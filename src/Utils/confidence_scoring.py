"""
Confidence scoring utilities for SAST workflow.

Implements weighted confidence score calculation based on 4 components:
- Filter Confidence (20%): Initial filtering decision confidence
- Agent Confidence (50%): LLM's confidence in final verdict
- Evidence Strength (20%): Quality and quantity of supporting evidence
- Investigation Depth (10%): Thoroughness of exploration
"""

import logging
import re
from typing import Dict, Any, Optional
from dataclasses import dataclass

from dto.SASTWorkflowModels import PerIssueData

logger = logging.getLogger(__name__)

# Weights for confidence score components
FILTER_WEIGHT = 0.20
AGENT_WEIGHT = 0.50
EVIDENCE_WEIGHT = 0.20
INVESTIGATION_WEIGHT = 0.10

# Evidence strength sub-component weights
FAISS_SCORE_WEIGHT = 0.40
FILES_FETCHED_WEIGHT = 0.30
EVIDENCE_COUNT_WEIGHT = 0.30

# Normalization caps
MAX_FILES_FOR_NORMALIZATION = 10
MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION = 5
MAX_SYMBOLS_FOR_NORMALIZATION = 5


@dataclass
class ConfidenceScoreBreakdown:
    """Detailed breakdown of confidence score components."""
    final_confidence: float
    filter_confidence: float
    agent_confidence: float
    evidence_strength: float
    investigation_depth: float

    # Evidence strength sub-components
    faiss_score: Optional[float] = None
    files_fetched_count: int = 0
    evidence_count: int = 0

    # Investigation depth components
    symbols_explored: int = 0


def calculate_evidence_strength(per_issue_data: PerIssueData) -> tuple[float, Dict[str, Any]]:
    """
    Calculate evidence strength component (0.0 to 1.0).

    Components:
    - FAISS similarity: 40% (highest similarity score from vector search)
    - Files fetched: 30% (number of source files retrieved)
    - Evidence count: 30% (code snippets, CVEs, references in justification)

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

    # 2. Files Fetched (normalize to 0-1, cap at MAX_FILES_FOR_NORMALIZATION)
    files_fetched_count = len(per_issue_data.fetched_files)
    files_score = min(files_fetched_count / MAX_FILES_FOR_NORMALIZATION, 1.0)

    # 3. Evidence Count (parse justifications for code blocks, CVEs, file references)
    evidence_count = 0
    if per_issue_data.analysis_response and per_issue_data.analysis_response.justifications:
        for justification in per_issue_data.analysis_response.justifications:
            # Count code blocks (triple backticks)
            evidence_count += justification.count('```') // 2  # Pairs of backticks
            # Count CVE references
            evidence_count += justification.count('CVE-')
            # Count file:line references (e.g., "file.c:123")
            evidence_count += len(re.findall(r'\w+\.\w+:\d+', justification))

    evidence_score = min(evidence_count / MAX_EVIDENCE_ITEMS_FOR_NORMALIZATION, 1.0)

    # Weighted combination of evidence components
    evidence_strength = (
        FAISS_SCORE_WEIGHT * faiss_score +
        FILES_FETCHED_WEIGHT * files_score +
        EVIDENCE_COUNT_WEIGHT * evidence_score
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

    Metrics:
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

    # Combine both metrics (average of normalized values)
    symbols_score = min(symbols_explored / MAX_SYMBOLS_FOR_NORMALIZATION, 1.0)
    depth_score = min(explicit_depth / MAX_SYMBOLS_FOR_NORMALIZATION, 1.0)

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

    Formula:
    Final = (0.20 × Filter) + (0.50 × Agent) + (0.20 × Evidence) + (0.10 × Investigation)

    Args:
        per_issue_data: PerIssueData object containing all analysis information

    Returns:
        ConfidenceScoreBreakdown object with final score and component breakdown
    """
    # Component 1: Filter Confidence (20%)
    filter_confidence = 0.0
    if (per_issue_data.analysis_response and
        per_issue_data.analysis_response.filter_confidence is not None):
        filter_confidence = float(per_issue_data.analysis_response.filter_confidence)

    # Component 2: Agent Confidence (50%)
    agent_confidence = 0.0
    if (per_issue_data.analysis_response and
        per_issue_data.analysis_response.agent_confidence is not None):
        agent_confidence = float(per_issue_data.analysis_response.agent_confidence)

    # Component 3: Evidence Strength (20%)
    evidence_strength, evidence_details = calculate_evidence_strength(per_issue_data)

    # Component 4: Investigation Depth (10%)
    investigation_depth, investigation_details = calculate_investigation_depth(per_issue_data)

    # Calculate weighted final confidence
    final_confidence = (
        FILTER_WEIGHT * filter_confidence +
        AGENT_WEIGHT * agent_confidence +
        EVIDENCE_WEIGHT * evidence_strength +
        INVESTIGATION_WEIGHT * investigation_depth
    )

    # Create breakdown object
    breakdown = ConfidenceScoreBreakdown(
        final_confidence=final_confidence,
        filter_confidence=filter_confidence,
        agent_confidence=agent_confidence,
        evidence_strength=evidence_strength,
        investigation_depth=investigation_depth,
        faiss_score=evidence_details.get('faiss_score'),
        files_fetched_count=evidence_details.get('files_fetched_count', 0),
        evidence_count=evidence_details.get('evidence_count', 0),
        symbols_explored=investigation_details.get('symbols_explored', 0)
    )

    logger.debug(
        f"Confidence calculation: final={final_confidence:.3f}, "
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
        Dictionary of aggregate metrics
    """
    if not confidence_scores:
        return {
            'mean_confidence': 0.0,
            'min_confidence': 0.0,
            'max_confidence': 0.0,
            'high_confidence_count': 0,
            'medium_confidence_count': 0,
            'low_confidence_count': 0,
            'total_issues': 0
        }

    final_scores = [breakdown.final_confidence for breakdown in confidence_scores.values()]

    mean_confidence = sum(final_scores) / len(final_scores)
    min_confidence = min(final_scores)
    max_confidence = max(final_scores)

    # Categorize by confidence level
    high_confidence_count = sum(1 for score in final_scores if score >= 0.8)
    medium_confidence_count = sum(1 for score in final_scores if 0.5 <= score < 0.8)
    low_confidence_count = sum(1 for score in final_scores if score < 0.5)

    return {
        'mean_confidence': mean_confidence,
        'min_confidence': min_confidence,
        'max_confidence': max_confidence,
        'high_confidence_count': high_confidence_count,
        'medium_confidence_count': medium_confidence_count,
        'low_confidence_count': low_confidence_count,
        'total_issues': len(confidence_scores)
    }