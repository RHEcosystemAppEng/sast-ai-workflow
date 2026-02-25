"""
Confidence scoring utilities for SAST workflow.

Implements weighted confidence score calculation based on 4 components:
- Filter Confidence: Initial filtering decision confidence
- Agent Confidence: LLM's confidence in final verdict
- Evidence Strength: Quality and quantity of supporting evidence
- Investigation Depth: Thoroughness of exploration

Component weights and normalization caps are externalized to common.constants
for easy tuning without code changes.
"""

import logging
import re
from typing import Dict, Any, Optional
from dataclasses import dataclass

from dto.SASTWorkflowModels import PerIssueData
from common.constants import (
    CONFIDENCE_WEIGHT_FILTER,
    CONFIDENCE_WEIGHT_AGENT,
    CONFIDENCE_WEIGHT_EVIDENCE,
    CONFIDENCE_WEIGHT_INVESTIGATION,
    EVIDENCE_WEIGHT_FAISS_SCORE,
    EVIDENCE_WEIGHT_FILES_FETCHED,
    EVIDENCE_WEIGHT_EVIDENCE_COUNT,
    CONFIDENCE_MAX_FILES_FOR_NORMALIZATION,
    CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION,
    CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION,
)

logger = logging.getLogger(__name__)


def _validate_weight_configuration() -> None:
    """
    Validate that confidence scoring weights are properly configured.

    Raises:
        ValueError: If weights don't sum to 1.0
    """
    # Tolerance for floating-point rounding errors ONLY (e.g., 0.2 + 0.5 + 0.2 + 0.1 = 0.9999999999999999)
    # This is NOT intended to allow misconfigured weights - any real configuration error will exceed this threshold
    FLOATING_POINT_TOLERANCE = 1e-9

    # Validate main component weights sum to 1.0
    main_weights_sum = (
        CONFIDENCE_WEIGHT_FILTER +
        CONFIDENCE_WEIGHT_AGENT +
        CONFIDENCE_WEIGHT_EVIDENCE +
        CONFIDENCE_WEIGHT_INVESTIGATION
    )

    if abs(main_weights_sum - 1.0) > FLOATING_POINT_TOLERANCE:
        raise ValueError(
            f"Main confidence weights must sum to 1.0, got {main_weights_sum}. "
            f"Check CONFIDENCE_WEIGHT_* constants in common.constants"
        )

    # Validate evidence sub-component weights sum to 1.0
    evidence_weights_sum = (
        EVIDENCE_WEIGHT_FAISS_SCORE +
        EVIDENCE_WEIGHT_FILES_FETCHED +
        EVIDENCE_WEIGHT_EVIDENCE_COUNT
    )

    if abs(evidence_weights_sum - 1.0) > FLOATING_POINT_TOLERANCE:
        raise ValueError(
            f"Evidence sub-weights must sum to 1.0, got {evidence_weights_sum}. "
            f"Check EVIDENCE_WEIGHT_* constants in common.constants"
        )

    logger.debug(
        f"Confidence weight validation passed: "
        f"main_sum={main_weights_sum}, evidence_sum={evidence_weights_sum}"
    )


# Validate configuration at module import time to fail fast
_validate_weight_configuration()


@dataclass
class ConfidenceScoreBreakdown:
    """Detailed breakdown of confidence score components.

    final_confidence is a percentage (0-100).
    Component scores are kept in 0.0-1.0 scale for internal use.
    """
    final_confidence: float  # Percentage (0-100)
    filter_confidence: float  # 0.0-1.0
    agent_confidence: float  # 0.0-1.0
    evidence_strength: float  # 0.0-1.0
    investigation_depth: float  # 0.0-1.0

    # Evidence strength sub-components
    faiss_score: Optional[float] = None
    files_fetched_count: int = 0
    evidence_count: int = 0

    # Investigation depth components
    symbols_explored: int = 0


def calculate_evidence_strength(per_issue_data: PerIssueData) -> tuple[float, Dict[str, Any]]:
    """
    Calculate evidence strength component (0.0 to 1.0).

    Components (weights from common.constants):
    - FAISS similarity: EVIDENCE_WEIGHT_FAISS_SCORE (highest similarity score from vector search)
    - Files fetched: EVIDENCE_WEIGHT_FILES_FETCHED (number of source files retrieved)
    - Evidence count: EVIDENCE_WEIGHT_EVIDENCE_COUNT (code snippets, CVEs, references in justification)

    Args:
        per_issue_data: PerIssueData object containing analysis information

    Returns:
        Tuple of (evidence_strength_score, details_dict)
    """
    # 1. FAISS Similarity Score (already normalized 0-1)
    faiss_score = 0.0
    if (per_issue_data.analysis_response and
        per_issue_data.analysis_response.faiss_similarity_score is not None):
        faiss_score = float(per_issue_data.analysis_response.faiss_similarity_score)

    # 2. Files Fetched (normalize to 0-1, cap at CONFIDENCE_MAX_FILES_FOR_NORMALIZATION)
    files_fetched_count = len(per_issue_data.fetched_files)
    files_score = min(files_fetched_count / CONFIDENCE_MAX_FILES_FOR_NORMALIZATION, 1.0)

    # 3. Evidence Count (parse justifications for code blocks, CVEs, file references)
    evidence_count = 0
    if per_issue_data.analysis_response and per_issue_data.analysis_response.justifications:
        for justification in per_issue_data.analysis_response.justifications:
            # Count code blocks (triple backticks)
            evidence_count += justification.count('```') // 2  # Pairs of backticks
            # Count CVE references
            evidence_count += justification.count('CVE-')
            # Count file:line references (e.g., "file.c:123") with bounded regex to prevent ReDoS
            evidence_count += len(re.findall(r'[a-zA-Z0-9_./\-]{1,100}\.[a-zA-Z0-9]{1,10}:\d{1,10}', justification))

    evidence_score = min(evidence_count / CONFIDENCE_MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION, 1.0)

    # Weighted combination of evidence components (weights from common.constants)
    evidence_strength = (
        EVIDENCE_WEIGHT_FAISS_SCORE * faiss_score +
        EVIDENCE_WEIGHT_FILES_FETCHED * files_score +
        EVIDENCE_WEIGHT_EVIDENCE_COUNT * evidence_score
    )

    details = {
        'faiss_score': faiss_score,
        'files_fetched_count': files_fetched_count,
        'files_score': files_score,
        'evidence_count': evidence_count,
        'evidence_score': evidence_score
    }

    return evidence_strength, details


def calculate_investigation_depth(per_issue_data: PerIssueData) -> tuple[float, Dict[str, Any]]:
    """
    Calculate investigation depth component (0.0 to 1.0).

    Metrics (normalization cap from common.constants):
    - Number of symbols explored (from found_symbols set)
    - Explicit exploration depth counter

    Args:
        per_issue_data: PerIssueData object containing analysis information

    Returns:
        Tuple of (investigation_depth_score, details_dict)
    """
    # Use both found_symbols count and explicit exploration_depth
    symbols_explored = len(per_issue_data.found_symbols)
    explicit_depth = per_issue_data.exploration_depth

    # Combine both metrics (average of normalized values, cap from common.constants)
    symbols_score = min(symbols_explored / CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION, 1.0)
    depth_score = min(explicit_depth / CONFIDENCE_MAX_SYMBOLS_FOR_NORMALIZATION, 1.0)

    investigation_depth = (symbols_score + depth_score) / 2.0

    details = {
        'symbols_explored': symbols_explored,
        'explicit_depth': explicit_depth,
        'symbols_score': symbols_score,
        'depth_score': depth_score
    }

    return investigation_depth, details


def calculate_final_confidence(per_issue_data: PerIssueData) -> ConfidenceScoreBreakdown:
    """
    Calculate final confidence score using weighted formula.

    Formula (weights from common.constants):
    Final = (CONFIDENCE_WEIGHT_FILTER × Filter) + (CONFIDENCE_WEIGHT_AGENT × Agent) +
            (CONFIDENCE_WEIGHT_EVIDENCE × Evidence) + (CONFIDENCE_WEIGHT_INVESTIGATION × Investigation)

    Component scores are kept in 0.0-1.0 scale. Final confidence is converted to percentage (0-100).

    Args:
        per_issue_data: PerIssueData object containing all analysis information

    Returns:
        ConfidenceScoreBreakdown object with final score as percentage and components as 0-1 values
    """
    # Component 1: Filter Confidence (weight from common.constants)
    filter_confidence = 0.0
    if (per_issue_data.analysis_response and
        per_issue_data.analysis_response.filter_confidence is not None):
        filter_confidence = float(per_issue_data.analysis_response.filter_confidence)

    # Component 2: Agent Confidence (weight from common.constants)
    agent_confidence = 0.0
    if (per_issue_data.analysis_response and
        per_issue_data.analysis_response.agent_confidence is not None):
        agent_confidence = float(per_issue_data.analysis_response.agent_confidence)

    # Component 3: Evidence Strength (weight from common.constants)
    evidence_strength, evidence_details = calculate_evidence_strength(per_issue_data)

    # Component 4: Investigation Depth (weight from common.constants)
    investigation_depth, investigation_details = calculate_investigation_depth(per_issue_data)

    # Calculate weighted final confidence (0-1 scale, weights from common.constants)
    final_confidence_raw = (
        CONFIDENCE_WEIGHT_FILTER * filter_confidence +
        CONFIDENCE_WEIGHT_AGENT * agent_confidence +
        CONFIDENCE_WEIGHT_EVIDENCE * evidence_strength +
        CONFIDENCE_WEIGHT_INVESTIGATION * investigation_depth
    )

    # Convert ONLY final confidence to percentage (0-100)
    final_confidence = final_confidence_raw * 100.0

    # Create breakdown object
    breakdown = ConfidenceScoreBreakdown(
        final_confidence=final_confidence,  # Percentage (0-100)
        filter_confidence=filter_confidence,  # Keep as 0-1
        agent_confidence=agent_confidence,  # Keep as 0-1
        evidence_strength=evidence_strength,  # Keep as 0-1
        investigation_depth=investigation_depth,  # Keep as 0-1
        faiss_score=evidence_details.get('faiss_score'),
        files_fetched_count=evidence_details.get('files_fetched_count', 0),
        evidence_count=evidence_details.get('evidence_count', 0),
        symbols_explored=investigation_details.get('symbols_explored', 0)
    )

    logger.debug(
        f"Confidence calculation: final={final_confidence:.1f}%, "
        f"filter={filter_confidence:.3f}, agent={agent_confidence:.3f}, "
        f"evidence={evidence_strength:.3f}, investigation={investigation_depth:.3f}"
    )

    return breakdown


def inject_mock_confidence_data(per_issue_data: PerIssueData) -> None:
    """
    TEMPORARY: Inject mock confidence data for components not yet implemented.

    This function should be REMOVED once all nodes properly populate:
    - agent_confidence (from judge/finalize nodes)
    - fetched_files (from data_fetcher node)
    - exploration_depth (from analysis nodes)

    Args:
        per_issue_data: PerIssueData to inject mock data into
    """
    if not per_issue_data.analysis_response:
        logger.warning("Cannot inject mock data: analysis_response is None")
        return

    # Mock agent_confidence if not present (assume high confidence for final decisions)
    if per_issue_data.analysis_response.agent_confidence is None:
        # Use is_final status as heuristic: final decisions get higher confidence
        from dto.LLMResponse import FinalStatus
        if per_issue_data.analysis_response.is_final == FinalStatus.TRUE.value:
            per_issue_data.analysis_response.agent_confidence = 0.9
            logger.debug("Injected mock agent_confidence=0.9 (final decision)")
        else:
            per_issue_data.analysis_response.agent_confidence = 0.7
            logger.debug("Injected mock agent_confidence=0.7 (non-final decision)")

    # Mock fetched_files if empty (use source_code keys as proxy)
    if not per_issue_data.fetched_files and per_issue_data.source_code:
        per_issue_data.fetched_files = list(per_issue_data.source_code.keys())
        logger.debug(f"Injected mock fetched_files from source_code ({len(per_issue_data.fetched_files)} files)")

    # Mock exploration_depth if zero (use found_symbols as proxy)
    if per_issue_data.exploration_depth == 0 and per_issue_data.found_symbols:
        per_issue_data.exploration_depth = len(per_issue_data.found_symbols)
        logger.debug(f"Injected mock exploration_depth from found_symbols ({per_issue_data.exploration_depth})")


def calculate_aggregate_confidence_metrics(
    confidence_scores: Dict[str, ConfidenceScoreBreakdown]
) -> Dict[str, Any]:
    """
    Calculate aggregate statistics across all issues.

    Args:
        confidence_scores: Dict mapping issue_id to ConfidenceScoreBreakdown

    Returns:
        Dictionary of aggregate metrics and per-issue scores (percentages 0-100)
    """
    if not confidence_scores:
        return {
            'per_issue_scores': {},
            'mean_confidence': 0.0,
            'min_confidence': 0.0,
            'max_confidence': 0.0,
            'high_confidence_count': 0,
            'medium_confidence_count': 0,
            'low_confidence_count': 0,
            'total_issues': 0
        }

    # Extract per-issue scores for storage in tracker.metrics
    per_issue_scores = {
        issue_id: breakdown.final_confidence
        for issue_id, breakdown in confidence_scores.items()
    }

    final_scores = [breakdown.final_confidence for breakdown in confidence_scores.values()]

    mean_confidence = sum(final_scores) / len(final_scores)
    min_confidence = min(final_scores)
    max_confidence = max(final_scores)

    # Categorize by confidence level (percentage thresholds)
    high_confidence_count = sum(1 for score in final_scores if score >= 80.0)
    medium_confidence_count = sum(1 for score in final_scores if 50.0 <= score < 80.0)
    low_confidence_count = sum(1 for score in final_scores if score < 50.0)

    return {
        'per_issue_scores': per_issue_scores,  # issue_id -> percentage mapping
        'mean_confidence': mean_confidence,
        'min_confidence': min_confidence,
        'max_confidence': max_confidence,
        'high_confidence_count': high_confidence_count,
        'medium_confidence_count': medium_confidence_count,
        'low_confidence_count': low_confidence_count,
        'total_issues': len(confidence_scores)
    }