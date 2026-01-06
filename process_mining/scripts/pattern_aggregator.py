#!/usr/bin/env python3
"""
Pattern Aggregation Script - Enhanced Cross-Package RHEL C Pattern Synthesis

This script implements aggregation rules:
- True positive filtering
- Pattern matching with multi-factor similarity
- Confidence calculation
- Issue type diversity penalty
- Structural specificity scoring
- Pattern variant support
- Stratified output by confidence tier

Usage:
    python scripts/pattern_aggregator.py \
        --patterns-dir patterns/train_patterns \
        --output patterns/rhel_c_patterns_train_final.json \
        --mode maximum-coverage
"""

import json
import argparse
import sys
import os
from pathlib import Path
from typing import Dict, List, Any, Optional, Set, Tuple
from collections import defaultdict
import difflib
import re

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from process_mining.src.common.process_mining_config import ProcessMiningConfig


class PatternAggregator:
    """Enhanced aggregator with improved pattern matching and quality filtering."""

    def __init__(
        self,
        mode: str = 'maximum-coverage',
        min_package_threshold: Optional[int] = None,
        min_confidence: Optional[float] = None,
        config_path: Optional[str] = None,
        debug: bool = False
    ):
        self.config = ProcessMiningConfig(config_path)
        agg_config = self.config.get_aggregation_config()

        self.mode = mode
        self.debug = debug

        self.min_package_threshold = (
            min_package_threshold
            if min_package_threshold is not None
            else agg_config.get('min_package_threshold', 2)
        )

        self.min_confidence = (
            min_confidence
            if min_confidence is not None
            else agg_config.get('min_confidence_threshold', 0.30)
        )

        self.true_positive_indicators = agg_config.get('true_positive_indicators', [
            "TRUE POSITIVE", "SAST is correct", "SAST is NOT wrong",
            "actual bug", "should be fixed", "confirmed bug", "real issue"
        ])

        pattern_matching = agg_config.get('pattern_matching', {})
        strong_match = pattern_matching.get('strong_match', {})
        self.sast_threshold = self._get_mode_threshold(strong_match.get('sast_assumption_similarity', 0.6))
        self.behavior_threshold = self._get_mode_threshold(strong_match.get('actual_behavior_similarity', 0.6))
        self.code_threshold = self._get_mode_threshold(strong_match.get('code_pattern_overlap', 0.35))
        self.mechanism_threshold = strong_match.get('mechanism_similarity', 0.75)

        weights = agg_config.get('confidence_weights', {})
        self.confidence_weights = {
            'coverage': weights.get('package_coverage', 0.30),
            'frequency': weights.get('frequency', 0.25),
            'consistency': weights.get('consistency', 0.25),
            'evidence_quality': weights.get('evidence_quality', 0.10),
            'structural_specificity': weights.get('structural_specificity', 0.10)
        }

        issue_penalty = agg_config.get('issue_type_penalty', {})
        self.max_issues_no_penalty = issue_penalty.get('max_without_penalty', 3)
        self.penalty_per_extra = issue_penalty.get('penalty_per_extra_type', 0.15)

        self.min_consistency_for_2pkg = agg_config.get('min_consistency_for_2pkg', 0.3)

        self.coverage_bonuses = agg_config.get('high_coverage_bonus', {
            '5_packages': 0.05,
            '10_packages': 0.10,
            '20_packages': 0.15
        })

        self.confidence_tiers = agg_config.get('confidence_tiers', {
            'high': 0.5,
            'medium': 0.4,
            'low': 0.3
        })

        self.related_issue_groups = [
            {'USE_AFTER_FREE', 'MEMORY_LEAK', 'RESOURCE_LEAK'},
            {'INTEGER_OVERFLOW', 'OVERRUN', 'BUFFER_SIZE', 'NEGATIVE_RETURNS'},
            {'UNINIT', 'VARARGS'},
            {'COMPILER_WARNING', 'CPPCHECK_WARNING'},
        ]

        self.all_patterns = []
        self.package_patterns = defaultdict(list)
        self.true_positives = []

    def _get_mode_threshold(self, base_threshold: float) -> float:
        """Adjust thresholds based on aggregation mode."""
        if self.mode == 'high-quality':
            return 0.80
        elif self.mode == 'balanced':
            return 0.70
        else: 
            return base_threshold

    def load_patterns(self, patterns_dir: Path) -> None:
        """Load all pattern JSON files and filter true positives."""
        pattern_files = list(patterns_dir.glob("*.json"))
        pattern_files = [f for f in pattern_files if not f.stem.startswith('_')]

        print(f"Found {len(pattern_files)} pattern files")

        for pattern_file in pattern_files:
            try:
                with open(pattern_file, 'r') as f:
                    data = json.load(f)

                package_name = pattern_file.stem.replace('_claude', '')

                if 'patterns' in data:
                    for pattern in data['patterns']:
                        pattern['source_package'] = package_name

                        if self._is_true_positive(pattern):
                            self.true_positives.append(pattern)
                        else:
                            self.all_patterns.append(pattern)
                            self.package_patterns[package_name].append(pattern)

            except Exception as e:
                print(f"Error loading {pattern_file}: {e}")

        print(f"Loaded {len(self.all_patterns)} false positive patterns")
        print(f"Filtered {len(self.true_positives)} true positives")
        print(f"From {len(self.package_patterns)} packages")

    def _is_true_positive(self, pattern: Dict) -> bool:
        """Check if pattern represents a true positive."""
        rca = pattern.get('root_cause_analysis', {})
        analyst_reasoning = pattern.get('example_entry', {}).get('analyst_reasoning', '')

        text_to_check = ' '.join([
            rca.get('sast_assumption', ''),
            rca.get('actual_behavior', ''),
            rca.get('why_sast_wrong', ''),
            analyst_reasoning
        ]).upper()

        for indicator in self.true_positive_indicators:
            if indicator.upper() in text_to_check:
                return True

        return False

    def compute_text_similarity(self, text1: str, text2: str) -> float:
        """Compute semantic similarity between two text strings."""
        if not text1 or not text2:
            return 0.0
        return difflib.SequenceMatcher(None, text1.lower(), text2.lower()).ratio()

    def jaccard_similarity(self, set1: Set, set2: Set) -> float:
        """Compute Jaccard similarity: intersection / union"""
        if not set1 and not set2:
            return 1.0
        if not set1 or not set2:
            return 0.0

        intersection = len(set1 & set2)
        union = len(set1 | set2)
        return intersection / union if union > 0 else 0.0

    def are_issue_types_related(self, types1: Set[str], types2: Set[str]) -> bool:
        """Check if issue types are semantically related."""
        for group in self.related_issue_groups:
            if (types1 & group) and (types2 & group):
                return True
        return False

    def patterns_match(self, p1: Dict, p2: Dict) -> Tuple[bool, float]:
        """
        Check if two patterns from DIFFERENT packages are the same general pattern.

        Returns (match, similarity_score)
        """
        if p1.get('source_package') == p2.get('source_package'):
            return False, 0.0

        issue_types1 = set(p1.get('issue_types', []))
        issue_types2 = set(p2.get('issue_types', []))

        if not (issue_types1 & issue_types2):
            if not self.are_issue_types_related(issue_types1, issue_types2):
                if self.debug:
                    print(f"  ✗ No issue type overlap or relation")
                return False, 0.0

        rca1 = p1.get('root_cause_analysis', {})
        rca2 = p2.get('root_cause_analysis', {})

        sast_sim = self.compute_text_similarity(
            rca1.get('sast_assumption', ''),
            rca2.get('sast_assumption', '')
        )

        behavior_sim = self.compute_text_similarity(
            rca1.get('actual_behavior', ''),
            rca2.get('actual_behavior', '')
        )

        mechanism_sim = self.compute_text_similarity(
            rca1.get('mechanism', ''),
            rca2.get('mechanism', '')
        )

        sp1 = p1.get('structural_pattern', {})
        sp2 = p2.get('structural_pattern', {})

        code_patterns1 = set(sp1.get('code_patterns', []))
        code_patterns2 = set(sp2.get('code_patterns', []))
        code_overlap = self.jaccard_similarity(code_patterns1, code_patterns2)

        funcs1 = set(sp1.get('required_functions', []))
        funcs2 = set(sp2.get('required_functions', []))
        func_overlap = self.jaccard_similarity(funcs1, funcs2)

        if self.debug:
            print(f"  SAST sim: {sast_sim:.2f}, Behavior sim: {behavior_sim:.2f}")
            print(f"  Mechanism sim: {mechanism_sim:.2f}, Code overlap: {code_overlap:.2f}")

        meets_text_similarity = (
            sast_sim >= self.sast_threshold and
            behavior_sim >= self.behavior_threshold
        )

        meets_structural_overlap = code_overlap >= self.code_threshold

        strong_match = meets_text_similarity and meets_structural_overlap

        partial_match = (
            (sast_sim >= 0.85 and behavior_sim >= 0.85 and code_overlap >= 0.2) or
            (code_overlap >= 0.7 and sast_sim >= 0.5)
        )

        mechanism_match = mechanism_sim >= self.mechanism_threshold and code_overlap >= 0.20

        similarity_score = (
            0.3 * sast_sim +
            0.3 * behavior_sim +
            0.2 * code_overlap +
            0.1 * func_overlap +
            0.1 * mechanism_sim
        )

        matches = strong_match or partial_match or mechanism_match

        if self.debug and matches:
            match_type = "strong" if strong_match else ("partial" if partial_match else "mechanism")
            print(f"  ✓ Match ({match_type}), similarity: {similarity_score:.2f}")

        return matches, similarity_score

    def group_similar_patterns(self) -> List[List[Dict]]:
        """Group patterns that are semantically similar across packages."""
        groups = []
        visited = set()

        for i, pattern in enumerate(self.all_patterns):
            if i in visited:
                continue

            group = [pattern]
            group_similarities = []
            visited.add(i)

            if self.debug:
                print(f"\n--- Pattern {i}: {pattern['source_package']} - {pattern.get('pattern_name', 'Unknown')} ---")

            for j, other_pattern in enumerate(self.all_patterns):
                if j in visited:
                    continue

                matches, similarity = self.patterns_match(pattern, other_pattern)

                if matches:
                    group.append(other_pattern)
                    group_similarities.append(similarity)
                    visited.add(j)

            packages_in_group = len(set(p['source_package'] for p in group))

            if packages_in_group >= self.min_package_threshold:
                consistency = self._compute_consistency_score(group)

                if packages_in_group == 2 and consistency < self.min_consistency_for_2pkg:
                    if self.debug:
                        print(f"  ✗ Rejected: 2 packages but consistency {consistency:.2f} < {self.min_consistency_for_2pkg}")
                    continue

                groups.append(group)
                if self.debug:
                    print(f"  ✓ Added group: {len(group)} patterns, {packages_in_group} packages, consistency: {consistency:.2f}")

        return groups

    def _compute_consistency_score(self, group: List[Dict]) -> float:
        """
        Compute consistency score based on pattern similarity within a group.

        Measures:
        1. Root cause analysis text similarity (40%)
        2. Structural pattern overlap via Jaccard similarity (40%)
        3. Issue type agreement via Jaccard similarity (20%)
        """
        if len(group) <= 1:
            return 1.0

        rca_similarities = []
        func_similarities = []
        issue_similarities = []

        for i in range(len(group)):
            for j in range(i + 1, len(group)):
                rca1 = group[i].get('root_cause_analysis', {})
                rca2 = group[j].get('root_cause_analysis', {})

                text1 = rca1.get('sast_assumption', '') + ' ' + rca1.get('actual_behavior', '')
                text2 = rca2.get('sast_assumption', '') + ' ' + rca2.get('actual_behavior', '')

                rca_similarities.append(self.compute_text_similarity(text1, text2))

                sp1 = group[i].get('structural_pattern', {})
                sp2 = group[j].get('structural_pattern', {})

                funcs1 = set(sp1.get('required_functions', []))
                funcs2 = set(sp2.get('required_functions', []))

                func_similarities.append(self.jaccard_similarity(funcs1, funcs2))

                types1 = set(group[i].get('issue_types', []))
                types2 = set(group[j].get('issue_types', []))

                issue_similarities.append(self.jaccard_similarity(types1, types2))

        avg_rca = sum(rca_similarities) / len(rca_similarities) if rca_similarities else 0.5
        avg_func = sum(func_similarities) / len(func_similarities) if func_similarities else 0.5
        avg_issue = sum(issue_similarities) / len(issue_similarities) if issue_similarities else 0.5

        consistency = 0.4 * avg_rca + 0.4 * avg_func + 0.2 * avg_issue
        return min(consistency, 1.0)

    def _calculate_issue_type_penalty(self, num_issues: int) -> float:
        """Calculate penalty for patterns with too many issue types."""
        if num_issues <= self.max_issues_no_penalty:
            return 1.0

        extra_issues = num_issues - self.max_issues_no_penalty
        penalty = 1.0 - (extra_issues * self.penalty_per_extra)
        return max(penalty, 0.4)

    def _calculate_structural_specificity(self, structural_pattern: Dict) -> float:
        """Calculate how specific/well-defined the structural pattern is."""
        num_functions = len(structural_pattern.get('required_functions', []))
        num_code_patterns = len(structural_pattern.get('code_patterns', []))
        required_imports = structural_pattern.get('required_imports', [])

        has_specific_api = any(
            lib for lib in required_imports
            if lib not in ['stdlib.h', 'stdio.h', 'string.h', 'unistd.h']
        )

        score = (
            0.4 * min(num_functions / 5.0, 1.0) +
            0.4 * min(num_code_patterns / 3.0, 1.0) +
            0.2 * (1.0 if has_specific_api else 0.5)
        )

        return score

    def _calculate_evidence_quality(self, patterns: List[Dict]) -> float:
        """Calculate evidence quality score across patterns in group."""
        scores = []

        for pattern in patterns:
            score = 0.5

            inv_steps = pattern.get('investigation_steps', [])
            if len(inv_steps) >= 3:
                score += 0.1

            if any('line' in str(step.get('evidence_found', '')).lower() for step in inv_steps):
                score += 0.1

            if any(len(str(step.get('evidence_found', ''))) > 50 for step in inv_steps):
                score += 0.1

            example = pattern.get('example_entry', {})
            if 'line' in example:
                score += 0.1

            if len(example.get('analyst_reasoning', '')) > 100:
                score += 0.1

            scores.append(min(score, 1.0))

        return sum(scores) / len(scores) if scores else 0.5

    def _get_coverage_bonus(self, num_packages: int) -> float:
        """Get confidence bonus based on package coverage."""
        if num_packages >= 20:
            return self.coverage_bonuses.get('20_packages', 0.15)
        elif num_packages >= 10:
            return self.coverage_bonuses.get('10_packages', 0.10)
        elif num_packages >= 5:
            return self.coverage_bonuses.get('5_packages', 0.05)
        return 0.0

    def _compute_final_confidence(
        self,
        package_coverage_pct: float,
        total_occurrences: int,
        total_patterns: int,
        group: List[Dict],
        merged_structural: Dict
    ) -> float:
        """Compute final confidence score with all enhancements."""
        coverage_score = package_coverage_pct / 100.0
        frequency_score = min(total_occurrences / total_patterns, 1.0)
        consistency_score = self._compute_consistency_score(group)
        evidence_score = self._calculate_evidence_quality(group)
        structural_score = self._calculate_structural_specificity(merged_structural)

        weights = self.confidence_weights
        base_confidence = (
            weights.get('coverage', 0.30) * coverage_score +
            weights.get('frequency', 0.25) * frequency_score +
            weights.get('consistency', 0.25) * consistency_score +
            weights.get('evidence_quality', 0.10) * evidence_score +
            weights.get('structural_specificity', 0.10) * structural_score
        )

        num_issues = len(set(sum([p.get('issue_types', []) for p in group], [])))
        issue_penalty = self._calculate_issue_type_penalty(num_issues)

        num_packages = len(set(p['source_package'] for p in group))
        coverage_bonus = self._get_coverage_bonus(num_packages)

        confidence = (base_confidence * issue_penalty) + coverage_bonus

        return min(confidence, 1.0)

    def merge_pattern_group(self, group: List[Dict], pattern_id: str) -> Dict:
        """Merge a group of similar patterns into a single cross-package pattern."""
        packages = [p['source_package'] for p in group]
        unique_packages = sorted(list(set(packages)))

        template = max(group, key=lambda p: p.get('confidence_assessment', {}).get('pattern_confidence', 0.5))

        total_occurrences = len(group)
        package_coverage_percentage = (len(unique_packages) / len(self.package_patterns)) * 100

        merged_structural = self._merge_structural_patterns(group)

        confidence = self._compute_final_confidence(
            package_coverage_percentage,
            total_occurrences,
            len(self.all_patterns),
            group,
            merged_structural
        )

        if len(group) >= 5:
            evidence_quality = "HIGH"
        elif len(group) >= 2:
            evidence_quality = "MEDIUM"
        else:
            evidence_quality = "LOW"

        if len(unique_packages) >= 5:
            generalizability = "HIGH"
        elif len(unique_packages) >= 2:
            generalizability = "MEDIUM"
        else:
            generalizability = "LOW"

        api_family = self._extract_api_family(group)

        merged = {
            "pattern_id": pattern_id,
            "pattern_name": template.get('pattern_name', 'Unknown Pattern'),
            "issue_types": sorted(list(set(sum([p.get('issue_types', []) for p in group], [])))),

            "cross_package_metadata": {
                "packages_observed": unique_packages,
                "total_occurrences": total_occurrences,
                "package_coverage_percentage": round(package_coverage_percentage, 2),
                "api_family": api_family
            },

            "example_entry": template.get('example_entry', {}),
            "investigation_steps": template.get('investigation_steps', []),
            "root_cause_analysis": template.get('root_cause_analysis', {}),
            "structural_pattern": merged_structural,

            "confidence_assessment": {
                "pattern_confidence": round(confidence, 2),
                "evidence_quality": evidence_quality,
                "generalizability": generalizability,
                "reasoning": f"Observed in {len(unique_packages)} packages ({package_coverage_percentage:.1f}% coverage), {total_occurrences} total occurrences, consistency: {self._compute_consistency_score(group):.2f}"
            }
        }

        return merged

    def _extract_api_family(self, group: List[Dict]) -> str:
        """Extract the API family from the patterns."""
        all_functions = []
        for pattern in group:
            sp = pattern.get('structural_pattern', {})
            all_functions.extend(sp.get('required_functions', []))

        if not all_functions:
            return 'general_c'

        functions_str = ' '.join(all_functions).lower()

        api_families = {
            'g_': 'glib',
            'gtk_': 'gtk',
            'pthread_': 'pthread',
            'malloc': 'memory_management',
            'read': 'io_operations',
            'write': 'io_operations',
            'socket': 'networking',
            'ssl_': 'openssl',
        }

        for prefix, family in api_families.items():
            if prefix in functions_str:
                return family

        return 'general_c'

    def _merge_structural_patterns(self, group: List[Dict]) -> Dict:
        """Merge structural patterns from multiple patterns."""
        all_imports = set()
        all_functions = set()
        all_code_patterns = set()

        for pattern in group:
            sp = pattern.get('structural_pattern', {})
            all_imports.update(sp.get('required_imports', []))
            all_functions.update(sp.get('required_functions', []))
            all_code_patterns.update(sp.get('code_patterns', []))

        template = group[0].get('structural_pattern', {})

        return {
            "required_imports": sorted(list(all_imports)),
            "required_functions": sorted(list(all_functions)),
            "code_patterns": sorted(list(all_code_patterns)),
            "context_requirements": template.get('context_requirements', 'C codebase')
        }

    def aggregate(self) -> Dict:
        """Main aggregation logic."""
        print(f"\nGrouping similar patterns across packages (mode: {self.mode})...")
        pattern_groups = self.group_similar_patterns()

        print(f"Found {len(pattern_groups)} cross-package pattern groups")

        high_confidence_patterns = []
        medium_confidence_patterns = []
        low_confidence_patterns = []

        for i, group in enumerate(pattern_groups, start=1):
            pattern_id = f"RHEL-C-{i:03d}"
            merged = self.merge_pattern_group(group, pattern_id)

            confidence = merged['confidence_assessment']['pattern_confidence']

            if confidence >= self.min_confidence:
                if confidence >= self.confidence_tiers.get('high', 0.5):
                    high_confidence_patterns.append(merged)
                elif confidence >= self.confidence_tiers.get('medium', 0.4):
                    medium_confidence_patterns.append(merged)
                else:
                    low_confidence_patterns.append(merged)

        all_patterns = high_confidence_patterns + medium_confidence_patterns + low_confidence_patterns

        print(f"Generated {len(all_patterns)} final patterns (above confidence threshold {self.min_confidence})")
        print(f"  High confidence (>={self.confidence_tiers.get('high', 0.5)}): {len(high_confidence_patterns)}")
        print(f"  Medium confidence (>={self.confidence_tiers.get('medium', 0.4)}): {len(medium_confidence_patterns)}")
        print(f"  Low confidence (>={self.confidence_tiers.get('low', 0.3)}): {len(low_confidence_patterns)}")

        avg_confidence = sum(p['confidence_assessment']['pattern_confidence'] for p in all_patterns) / len(all_patterns) if all_patterns else 0

        summary = {
            "total_input_files": len(self.package_patterns),
            "total_patterns_across_all_packages": len(self.all_patterns),
            "true_positives_filtered": len(self.true_positives),
            "unique_cross_package_patterns": len(all_patterns),
            "high_confidence_patterns": len(high_confidence_patterns),
            "medium_confidence_patterns": len(medium_confidence_patterns),
            "low_confidence_patterns": len(low_confidence_patterns),
            "average_pattern_confidence": round(avg_confidence, 2),
            "min_package_threshold": self.min_package_threshold,
            "min_confidence_threshold": self.min_confidence,
            "mode": self.mode,
            "aggregation_methodology": f"Enhanced aggregation with multi-factor similarity matching. Patterns included if observed in {self.min_package_threshold}+ packages with semantic similarity in root cause analysis and structural patterns."
        }

        return {
            "patterns": all_patterns,
            "high_confidence_patterns": high_confidence_patterns,
            "medium_confidence_patterns": medium_confidence_patterns,
            "low_confidence_patterns": low_confidence_patterns,
            "aggregation_summary": summary
        }


def main():
    parser = argparse.ArgumentParser(
        description="Aggregate patterns from multiple RHEL packages"
    )
    parser.add_argument(
        '--config',
        type=str,
        help='Path to process mining config YAML'
    )
    parser.add_argument(
        '--patterns-dir',
        type=Path,
        required=True,
        help='Directory containing pattern JSON files'
    )
    parser.add_argument(
        '--output',
        type=Path,
        required=True,
        help='Output file for aggregated patterns'
    )
    parser.add_argument(
        '--true-positives-output',
        type=Path,
        help='Output file for filtered true positives'
    )
    parser.add_argument(
        '--mode',
        type=str,
        choices=['maximum-coverage', 'balanced', 'high-quality'],
        default='maximum-coverage',
        help='Aggregation mode (default: maximum-coverage)'
    )
    parser.add_argument(
        '--min-packages',
        type=int,
        default=2,
        help='Minimum number of packages for a pattern'
    )
    parser.add_argument(
        '--min-confidence',
        type=float,
        default=0.30,
        help='Minimum confidence score'
    )
    parser.add_argument(
        '--debug',
        action='store_true',
        help='Enable debug output'
    )

    args = parser.parse_args()

    print(f"Pattern Aggregation - Cross-Package RHEL C Pattern Synthesis")
    print(f"=" * 70)
    print(f"Mode: {args.mode}")
    print(f"Patterns directory: {args.patterns_dir}")
    print(f"Output file: {args.output}")
    print(f"Min packages threshold: {args.min_packages}")
    print(f"Min confidence: {args.min_confidence}")
    print()

    aggregator = PatternAggregator(
        mode=args.mode,
        min_package_threshold=args.min_packages,
        min_confidence=args.min_confidence,
        config_path=args.config,
        debug=args.debug
    )

    aggregator.load_patterns(args.patterns_dir)

    result = aggregator.aggregate()

    args.output.parent.mkdir(parents=True, exist_ok=True)
    with open(args.output, 'w') as f:
        json.dump(result, f, indent=2)

    if args.true_positives_output and aggregator.true_positives:
        args.true_positives_output.parent.mkdir(parents=True, exist_ok=True)
        with open(args.true_positives_output, 'w') as f:
            json.dump({
                "true_positives": aggregator.true_positives,
                "summary": {
                    "total_count": len(aggregator.true_positives),
                    "description": "Patterns filtered out because they represent actual bugs (true positives), not false positives"
                }
            }, f, indent=2)
        print(f"📄 True positives saved to: {args.true_positives_output}")

    print(f"\n✅ Aggregation complete!")
    print(f"📄 Output saved to: {args.output}")
    print(f"\nSummary:")
    print(f"  - Input packages: {result['aggregation_summary']['total_input_files']}")
    print(f"  - Total input patterns: {result['aggregation_summary']['total_patterns_across_all_packages']}")
    print(f"  - True positives filtered: {result['aggregation_summary']['true_positives_filtered']}")
    print(f"  - Cross-package patterns: {result['aggregation_summary']['unique_cross_package_patterns']}")
    print(f"  - Average confidence: {result['aggregation_summary']['average_pattern_confidence']}")
    print(f"  - High confidence: {result['aggregation_summary']['high_confidence_patterns']}")
    print(f"  - Medium confidence: {result['aggregation_summary']['medium_confidence_patterns']}")
    print(f"  - Low confidence: {result['aggregation_summary']['low_confidence_patterns']}")


if __name__ == '__main__':
    main()
