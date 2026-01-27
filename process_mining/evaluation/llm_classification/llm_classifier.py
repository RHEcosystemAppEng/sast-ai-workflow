import argparse
import json
import re
import time
from collections import deque
from threading import Lock
from typing import Dict, List, Optional
from pathlib import Path

try:
    from openai import OpenAI
    OPENAI_AVAILABLE = True
except ImportError:
    OPENAI_AVAILABLE = False
    OpenAI = None


# Platform configurations
PLATFORM_CONFIGS = {
    "local": {
        "base_url": "http://vllm-public-sast-ai-models.public.apps.lab01.ibmcloud.appeng.rhecoeng.com/v1",
        "api_key": "API_KEY",
        "model": "nvidia/Llama-3.1-Nemotron-70B-Instruct-HF",
        "rate_limit": None,  # No rate limit for local
    },
    "nim": {
        "base_url": "https://integrate.api.nvidia.com/v1",
        "api_key": "YOUR_NIM_API_KEY",
        "model": "qwen/qwen3-next-80b-a3b-thinking",
        "rate_limit": 30,  # 30 requests per minute
    },
}


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

    # Keywords to match issue types from pattern filenames
    # Supports both old format (*resource_leak*.json) and new format (rhel_c_RESOURCE_LEAK_pattern_summary.json)
    ISSUE_TYPE_KEYWORDS = {
        'RESOURCE_LEAK': ['resource_leak', 'resourceleak', '_resource_leak_'],
        'UNINIT': ['uninit', '_uninit_'],
        'OVERRUN': ['overrun', '_overrun_'],
        'INTEGER_OVERFLOW': ['integer_overflow', 'integeroverflow', '_integer_overflow_'],
        'USE_AFTER_FREE': ['use_after_free', 'useafterfree', '_use_after_free_'],
        'CPPCHECK_WARNING': ['cppcheck_warning', 'cppcheck', '_cppcheck_warning_'],
        'ARRAY_VS_SINGLETON': ['array_vs_singleton', 'arrayvs_singleton', '_array_vs_singleton_'],
        'BAD_FREE': ['bad_free', 'badfree', '_bad_free_'],
        'BUFFER_SIZE': ['buffer_size', 'buffersize', '_buffer_size_'],
        'COMPILER_WARNING': ['compiler_warning', 'compilerwarning', '_compiler_warning_'],
        'COPY_PASTE_ERROR': ['copy_paste_error', 'copypasteerror', '_copy_paste_error_'],
        'NEGATIVE_RETURNS': ['negative_returns', 'negativereturns', '_negative_returns_'],
        'OVERLAPPING_COPY': ['overlapping_copy', 'overlappingcopy', '_overlapping_copy_'],
        'RETURN_LOCAL': ['return_local', 'returnlocal', '_return_local_'],
        'STRING_NULL': ['string_null', 'stringnull', '_string_null_'],
        'VARARGS': ['varargs', '_varargs_'],
        'Y2K38_SAFETY': ['y2k38_safety', 'y2k38safety', '_y2k38_safety_', 'y2k38'],
        'BAD_CHECK_OF_WAIT_COND': ['bad_check_of_wait_cond', 'badcheckofwaitcond', '_bad_check_of_wait_cond_'],
        'IDENTICAL_BRANCHES': ['identical_branches', 'identicalbranches', '_identical_branches_'],
        'LOCK_EVASION': ['lock_evasion', 'lockevasion', '_lock_evasion_'],
    }

    def __init__(
        self,
        platform: str = "local",
        base_url: Optional[str] = None,
        api_key: Optional[str] = None,
        model: Optional[str] = None,
        max_tokens: int = 8000,
        temperature: float = 0.0,
        baseline_mode: bool = False
    ):
        """
        Initialize LLM classifier.

        Args:
            platform: Platform to use ("local" for self-hosted vLLM, "nim" for NVIDIA NIM)
            base_url: Override platform's default URL
            api_key: Override platform's default API key
            model: Override platform's default model
            max_tokens: Maximum tokens in response
            temperature: Sampling temperature (0.0 = deterministic)
            baseline_mode: If True, run without pattern library (baseline evaluation)
        """
        if platform not in PLATFORM_CONFIGS:
            raise ValueError(f"Unknown platform: {platform}. Available: {list(PLATFORM_CONFIGS.keys())}")

        config = PLATFORM_CONFIGS[platform]
        self.platform = platform
        self.base_url = base_url or config["base_url"]
        self.api_key = api_key or config["api_key"]
        self.model = model or config["model"]
        self.max_tokens = max_tokens
        self.temperature = temperature
        self.baseline_mode = baseline_mode

        if OPENAI_AVAILABLE:
            self.client = OpenAI(
                base_url=self.base_url,
                api_key=self.api_key,
                timeout=120.0,
                max_retries=3
            )
        else:
            self.client = None

        # Rate limiting setup
        self.rate_limit = config.get("rate_limit")
        self._request_times: deque = deque()
        self._rate_limit_lock = Lock()

        print(f"Using platform: {platform} ({self.model})")
        if self.rate_limit:
            print(f"Rate limit: {self.rate_limit} requests/minute")

        self.patterns_by_issue_type: Dict[str, str] = {}
        self.prompt_template = ""

    def load_patterns(self, patterns_dir: Optional[Path] = None) -> None:
        """
        Load pattern files from directory, auto-detecting by filename.

        Supports both old and new filename conventions:
        Old format:
        - *resource_leak*.json -> RESOURCE_LEAK
        - *uninit*.json -> UNINIT
        - *overrun*.json -> OVERRUN

        New format (simplified patterns):
        - rhel_c_RESOURCE_LEAK_pattern_summary.json -> RESOURCE_LEAK
        - rhel_c_UNINIT_pattern_summary.json -> UNINIT
        - rhel_c_OVERRUN_pattern_summary.json -> OVERRUN

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

        prompt = f"""You are analyzing a SAST (Static Application Security Testing) finding to classify it as TRUE_POSITIVE (real security vulnerability) or FALSE_POSITIVE (incorrect/benign finding).

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

```json
{patterns_content}
```

## Analysis Plan (FOLLOW THESE STEPS IN ORDER)

You MUST follow this analysis plan step-by-step:

1. **Read the Error Trace**: Understand the flow and location of the reported issue.

2. **Read the Source Code Context**: Examine the relevant code attached above.

3. **Check Reference Patterns**: Review the patterns provided above and determine if any pattern matches this finding.

4. **Make Your Decision**:
   - **If a pattern applies**: Use the pattern to determine TRUE_POSITIVE or FALSE_POSITIVE. The pattern description becomes your justification.
   - **If no pattern applies**: Continue with your own code analysis to determine the classification.

**CRITICAL**: Always attempt to match patterns FIRST. Only perform independent analysis if no pattern is applicable.
"""
            else:
                prompt += f"""
## Analysis Plan

1. **Read the Error Trace**: Understand the flow and location of the reported issue.
2. **Read the Source Code Context**: Examine the relevant code attached above.
3. **Analyze the Code**: Determine if this is a real vulnerability or a false alarm based on the code context.

Note: No specific patterns available for {issue_type} issues.
"""
        else:
            prompt += """
## Analysis Plan

1. **Read the Error Trace**: Understand the flow and location of the reported issue.
2. **Read the Source Code Context**: Examine the relevant code attached above.
3. **Analyze the Code**: Determine if this is a real vulnerability or a false alarm based on the code context.
"""

        prompt += """
## Your Response

Respond with valid JSON in exactly this format:

```json
{
  "classification": "TRUE_POSITIVE" or "FALSE_POSITIVE",
  "justification": "Your explanation - if a pattern was used, include the pattern ID, its description, and how it matches this specific code",
  "cited_patterns": ["pattern_id_1", "pattern_id_2"]
}
```

REQUIREMENTS:
1. "classification" MUST be exactly "TRUE_POSITIVE" or "FALSE_POSITIVE" - no other values allowed
2. If you used a pattern for your decision:
   - The pattern ID MUST be in "cited_patterns"
   - The "justification" MUST include the pattern ID, its description, AND a brief explanation of how it matches this specific code
3. If no patterns were used, set "cited_patterns": []
4. Output ONLY the JSON block - no text before or after the JSON

Your response must start with ```json and end with ```
"""

        return prompt

    def _wait_for_rate_limit(self) -> None:
        """
        Wait if necessary to respect rate limits.

        Uses a sliding window to track requests in the last 60 seconds.
        Thread-safe for parallel workers.
        """
        if not self.rate_limit:
            return

        with self._rate_limit_lock:
            now = time.time()
            window_start = now - 60.0

            # Remove requests outside the 60-second window
            while self._request_times and self._request_times[0] < window_start:
                self._request_times.popleft()

            # If at rate limit, wait until oldest request exits the window
            if len(self._request_times) >= self.rate_limit:
                wait_time = self._request_times[0] - window_start + 0.1
                if wait_time > 0:
                    print(f"Rate limit reached, waiting {wait_time:.1f}s...")
                    time.sleep(wait_time)
                    # Clean up again after waiting
                    now = time.time()
                    window_start = now - 60.0
                    while self._request_times and self._request_times[0] < window_start:
                        self._request_times.popleft()

            # Record this request
            self._request_times.append(time.time())

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

            # Respect rate limits before making request
            self._wait_for_rate_limit()

            response = self.client.chat.completions.create(
                model=self.model,
                max_tokens=self.max_tokens,
                temperature=self.temperature,
                messages=[
                    {"role": "user", "content": prompt}
                ]
            )

            response_text = response.choices[0].message.content

            if response_text is None:
                raise ValueError("LLM returned empty response (content is None)")

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
    from entry_parser import ValidationEntryParser

    arg_parser = argparse.ArgumentParser(
        description="LLM-based SAST finding classifier",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Use local self-hosted Llama model (default)
  python llm_classifier.py patterns/ validation.txt

  # Use NVIDIA NIM platform with Qwen3 model
  python llm_classifier.py patterns/ validation.txt --platform nim

Pattern files are auto-detected by name:
  Old format: *resource_leak*.json -> RESOURCE_LEAK
  New format: rhel_c_RESOURCE_LEAK_pattern_summary.json -> RESOURCE_LEAK
        """
    )
    arg_parser.add_argument("patterns_dir", type=Path, help="Directory containing pattern JSON files")
    arg_parser.add_argument("validation_file", type=Path, help="Path to validation .txt file")
    arg_parser.add_argument(
        "--platform", "-p",
        choices=list(PLATFORM_CONFIGS.keys()),
        default="local",
        help="LLM platform to use (default: local)"
    )

    args = arg_parser.parse_args()

    entry_parser = ValidationEntryParser()
    entries = entry_parser.parse_file(args.validation_file)

    if not entries:
        print("No entries found in validation file")
        return

    classifier = PatternBasedClassifier(platform=args.platform)
    classifier.load_patterns(args.patterns_dir)

    print(f"\nClassifying entry: {entries[0].entry_id}")
    print(f"Issue type: {entries[0].issue_type}")
    print(f"Ground truth: {entries[0].ground_truth_classification}")

    result = classifier.classify_entry(entries[0].get_masked_entry())

    print(f"\n{'='*80}")
    print("CLASSIFICATION RESULTS")
    print(f"{'='*80}")
    print(f"Prediction: {result.predicted_classification}")
    print(f"\nJustification:\n{result.predicted_justification}")
    print(f"\nCited patterns: {', '.join(result.cited_patterns) if result.cited_patterns else 'None'}")
    print(f"{'='*80}")

    correct = result.predicted_classification == entries[0].ground_truth_classification
    print(f"\nCorrect: {correct}")


if __name__ == "__main__":
    main()
