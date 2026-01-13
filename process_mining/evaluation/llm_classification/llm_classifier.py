import json
import re
from typing import Dict, List, Optional
from pathlib import Path

try:
    from openai import OpenAI
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False
    OpenAI = None


class ClassificationResult:
    """Result of LLM classification for a single entry."""

    def __init__(
        self,
        predicted_classification: str,
        predicted_justification: str,
        cited_patterns: List[str],
        exploitability_assessment: str = "",
        raw_response: str = ""
    ):
        self.predicted_classification = predicted_classification
        self.predicted_justification = predicted_justification
        self.cited_patterns = cited_patterns
        self.exploitability_assessment = exploitability_assessment
        self.raw_response = raw_response

    def to_dict(self) -> Dict:
        """Convert to dictionary representation."""
        return {
            "predicted_classification": self.predicted_classification,
            "predicted_justification": self.predicted_justification,
            "cited_patterns": self.cited_patterns,
            "exploitability_assessment": self.exploitability_assessment
        }


class PatternBasedClassifier:
    """Classify SAST findings using LLM + issue-type-specific patterns."""

    ISSUE_TYPE_KEYWORDS = {
        'RESOURCE_LEAK': ['resource_leak', 'resourceleak'],
        'UNINIT': ['uninit'],
        'OVERRUN': ['overrun'],
        'INTEGER_OVERFLOW': ['integer_overflow', 'integeroverflow'],
        'USE_AFTER_FREE': ['use_after_free', 'useafterfree'],
        'CPPCHECK_WARNING': ['cppcheck_warning', 'cppcheck'],
    }

    def __init__(
        self,
        base_url: str = "http://vllm-public-sast-ai-models.public.apps.lab01.ibmcloud.appeng.rhecoeng.com/v1",
        api_key: str = "API_KEY",
        model: str = "nvidia/Llama-3.1-Nemotron-70B-Instruct-HF",
        max_tokens: int = 8000,
        temperature: float = 0.0,
        baseline_mode: bool = False
    ):
        """
        Initialize LLM classifier.

        Args:
            base_url: vLLM server URL
            api_key: API key for authentication
            model: Model name/ID
            max_tokens: Maximum tokens in response
            temperature: Sampling temperature (0.0 = deterministic)
            baseline_mode: If True, run without pattern library (baseline evaluation)
        """
        self.base_url = base_url
        self.api_key = api_key
        self.model = model
        self.max_tokens = max_tokens
        self.temperature = temperature
        self.baseline_mode = baseline_mode

        if OPENAI_AVAILABLE:
            self.client = OpenAI(
                base_url=base_url,
                api_key=api_key,
                timeout=120.0,
                max_retries=3
            )
        else:
            self.client = None

        self.patterns_by_issue_type: Dict[str, str] = {}
        self.prompt_template = ""

    def load_patterns(self, patterns_dir: Optional[Path] = None) -> None:
        """
        Load pattern files from directory, auto-detecting by filename.

        Filename convention:
        - *resource_leak*.json -> RESOURCE_LEAK
        - *uninit*.json -> UNINIT
        - *overrun*.json -> OVERRUN

        Args:
            patterns_dir: Path to directory containing pattern JSON files (None for baseline mode)
        """
        if patterns_dir is None:
            if not self.baseline_mode:
                raise ValueError("patterns_dir is required when not in baseline mode")
            print("Running in baseline mode without patterns")
            self.patterns_by_issue_type = {}
            return

        patterns_dir = Path(patterns_dir)
        if not patterns_dir.exists():
            raise FileNotFoundError(f"Patterns directory not found: {patterns_dir}")

        for json_file in patterns_dir.glob('*.json'):
            filename_lower = json_file.name.lower()
            for issue_type, keywords in self.ISSUE_TYPE_KEYWORDS.items():
                if any(kw in filename_lower for kw in keywords):
                    with open(json_file, 'r') as f:
                        self.patterns_by_issue_type[issue_type] = f.read()
                    print(f"Loaded {issue_type} patterns from {json_file.name}")
                    break

        print(f"Loaded patterns for {len(self.patterns_by_issue_type)} issue types: {list(self.patterns_by_issue_type.keys())}")

    def _get_patterns_for_issue_type(self, issue_type: str) -> Optional[str]:
        """
        Get pattern content for a specific issue type.

        Args:
            issue_type: The issue type to look up (e.g., 'RESOURCE_LEAK', 'UNINIT')

        Returns:
            Pattern file content as string, or None if not available
        """
        return self.patterns_by_issue_type.get(issue_type)

    def load_prompt_template(self, template_file: Path) -> None:
        """
        Load classification prompt template.

        Args:
            template_file: Path to prompt template file
        """
        with open(template_file, 'r') as f:
            self.prompt_template = f.read()

    def _build_classification_prompt(self, masked_entry: Dict) -> str:
        """
        Build a simple classification prompt with issue-type-specific patterns.

        Args:
            masked_entry: Entry dict with ground truth masked

        Returns:
            Complete prompt string
        """
        issue_type = masked_entry.get('issue_type', 'N/A')

        prompt = f"""Please analyze the following SAST (Static Application Security Testing) finding and determine if it is a TRUE_POSITIVE (real security vulnerability) or FALSE_POSITIVE (incorrect/benign finding).

## SAST Finding

**Issue Type**: {issue_type}
**CWE**: {masked_entry.get('cwe', 'N/A')}
**Package**: {masked_entry.get('package_name', 'N/A')}
**File**: {masked_entry.get('file_path', 'N/A')}:{masked_entry.get('line_number', '')}

**Error Trace**:
```
{masked_entry.get('error_trace', 'N/A')}
```

**Source Code Context**:
```c
{masked_entry.get('source_code', 'N/A')}
```
"""

        if not self.baseline_mode:
            patterns_content = self._get_patterns_for_issue_type(issue_type)
            if patterns_content:
                prompt += f"""
## Reference Patterns

The following pattern file for {issue_type} issues may be useful for your analysis. It contains known false positive and true positive patterns observed in similar codebases. Cite the pattern IDs if they apply.

```json
{patterns_content}
```
"""
            else:
                prompt += f"""
## Note

No specific patterns available for {issue_type} issues. Please analyze based on the code context alone.
"""

        prompt += """
## Your Response

You MUST respond with valid JSON in exactly this format:

```json
{
  "classification": "TRUE_POSITIVE" or "FALSE_POSITIVE",
  "justification": "Your detailed explanation of why this is a true or false positive",
  "cited_patterns": ["pattern_id_1", "pattern_id_2"]
}
```

CRITICAL REQUIREMENTS:
1. The "classification" field MUST be exactly "TRUE_POSITIVE" or "FALSE_POSITIVE" - no other values allowed
2. Output ONLY the JSON block - no text before or after the JSON
3. If no patterns were cited, use an empty array: "cited_patterns": []
4. Ensure the JSON is valid and properly formatted

Your response must start with ```json and end with ```
"""

        return prompt

    def classify_entry(self, masked_entry: Dict) -> ClassificationResult:
        """
        Classify a single SAST finding entry.

        Args:
            masked_entry: Entry dict with ground truth masked

        Returns:
            ClassificationResult with prediction and justification
        """
        prompt = self._build_classification_prompt(masked_entry)

        try:
            if not OPENAI_AVAILABLE or self.client is None:
                raise RuntimeError("OpenAI SDK not available. Install with: pip install openai")

            response = self.client.chat.completions.create(
                model=self.model,
                max_tokens=self.max_tokens,
                temperature=self.temperature,
                messages=[
                    {"role": "user", "content": prompt}
                ]
            )

            response_text = response.choices[0].message.content

            classification, justification, cited_patterns = self._parse_response(response_text)

            return ClassificationResult(
                predicted_classification=classification,
                predicted_justification=justification,
                cited_patterns=cited_patterns,
                exploitability_assessment="",
                raw_response=response_text
            )

        except Exception as e:
            print(f"Error classifying entry: {e}")
            return ClassificationResult(
                predicted_classification="ERROR",
                predicted_justification=f"Classification failed: {str(e)}",
                cited_patterns=[],
                exploitability_assessment="",
                raw_response=""
            )

    def _parse_response(self, response_text: str) -> tuple[str, str, List[str]]:
        """
        Parse LLM response to extract classification, justification, and cited patterns.

        Attempts JSON parsing first, then falls back to regex parsing.

        Args:
            response_text: Raw LLM response

        Returns:
            Tuple of (classification, justification, cited_patterns)
        """
        json_result = self._try_parse_json(response_text)
        if json_result:
            return json_result

        return self._parse_response_regex(response_text)

    def _try_parse_json(self, response_text: str) -> Optional[tuple[str, str, List[str]]]:
        """
        Try to parse JSON from the response.

        Args:
            response_text: Raw LLM response

        Returns:
            Tuple of (classification, justification, cited_patterns) or None if parsing fails
        """
        json_match = re.search(r'```(?:json)?\s*(\{[\s\S]*?\})\s*```', response_text)
        if json_match:
            json_str = json_match.group(1)
        else:
            json_match = re.search(r'\{[\s\S]*\}', response_text)
            if json_match:
                json_str = json_match.group()
            else:
                return None

        try:
            data = json.loads(json_str)

            classification = data.get('classification', '').upper().strip()
            if classification not in ('TRUE_POSITIVE', 'FALSE_POSITIVE'):
                classification = data.get('predicted_classification', '').upper().strip()

            if classification not in ('TRUE_POSITIVE', 'FALSE_POSITIVE'):
                return None 

            justification = data.get('justification', '') or data.get('reasoning', '') or 'No justification provided'
            cited_patterns = data.get('cited_patterns', []) or data.get('patterns', []) or []

            if isinstance(cited_patterns, str):
                if cited_patterns.lower() == 'none' or not cited_patterns:
                    cited_patterns = []
                else:
                    cited_patterns = [p.strip() for p in cited_patterns.split(',') if p.strip()]

            return classification, justification, cited_patterns

        except json.JSONDecodeError:
            return None

    def _parse_response_regex(self, response_text: str) -> tuple[str, str, List[str]]:
        """
        Parse LLM response using regex (fallback method).

        Args:
            response_text: Raw LLM response

        Returns:
            Tuple of (classification, justification, cited_patterns)
        """
        class_match = re.search(
            r'CLASSIFICATION:\s*(TRUE_POSITIVE|FALSE_POSITIVE)',
            response_text,
            re.IGNORECASE
        )
        classification = class_match.group(1).upper() if class_match else "UNKNOWN"

        just_match = re.search(
            r'JUSTIFICATION:\s*(.*?)(?=\n(?:CITED_PATTERNS):|$)',
            response_text,
            re.DOTALL | re.IGNORECASE
        )
        justification = just_match.group(1).strip() if just_match else "No justification provided"

        patterns_match = re.search(
            r'CITED_PATTERNS:\s*(.*?)(?=\n\n|$)',
            response_text,
            re.DOTALL | re.IGNORECASE
        )
        cited_patterns = []
        if patterns_match:
            patterns_text = patterns_match.group(1).strip()
            if patterns_text and patterns_text.lower() != "none":
                cited_patterns = [
                    p.strip()
                    for p in patterns_text.split(',')
                    if p.strip() and not p.strip().lower() == 'none'
                ]

        return classification, justification, cited_patterns


def main():
    """Test the classifier with a sample entry."""
    import sys
    from entry_parser import ValidationEntryParser

    if len(sys.argv) < 3:
        print("Usage: python llm_classifier.py <patterns_dir> <validation_file>")
        print("")
        print("  patterns_dir: Directory containing pattern JSON files")
        print("                Files are auto-detected by name:")
        print("                  *resource_leak*.json -> RESOURCE_LEAK")
        print("                  *uninit*.json -> UNINIT")
        print("                  *overrun*.json -> OVERRUN")
        print("")
        print("  validation_file: Path to validation .txt file")
        sys.exit(1)

    patterns_dir = Path(sys.argv[1])
    validation_file = Path(sys.argv[2])

    parser = ValidationEntryParser()
    entries = parser.parse_file(validation_file)

    if not entries:
        print("No entries found in validation file")
        sys.exit(1)

    classifier = PatternBasedClassifier()
    classifier.load_patterns(patterns_dir)

    print(f"\nClassifying entry: {entries[0].entry_id}")
    print(f"Issue type: {entries[0].issue_type}")
    print(f"Ground truth: {entries[0].ground_truth_classification}")

    result = classifier.classify_entry(entries[0].get_masked_entry())

    print(f"\n{'='*80}")
    print(f"CLASSIFICATION RESULTS")
    print(f"{'='*80}")
    print(f"Prediction: {result.predicted_classification}")
    print(f"\nJustification:\n{result.predicted_justification}")
    print(f"\nCited patterns: {', '.join(result.cited_patterns) if result.cited_patterns else 'None'}")
    print(f"{'='*80}")

    correct = result.predicted_classification == entries[0].ground_truth_classification
    print(f"\nCorrect: {correct}")


if __name__ == "__main__":
    main()
