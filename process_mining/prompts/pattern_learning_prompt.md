# SAST Investigation Pattern Learning - Expert Analysis

You are an expert security analyst learning from human-annotated SAST (Static Application Security Testing) findings.

## Task

Analyze the provided SAST findings (annotated by human experts) and extract **generalizable investigation patterns** that explain:

1. What investigation steps the expert likely followed
2. What evidence they examined
3. Extract the Ground Truth Classification (TRUE POSITIVE or FALSE POSITIVE) from each entry
4. The discrepancy between SAST's assumption and actual code behavior (use field name `discrepancy_analysis` in output)
5. What code patterns are present that could be applied to other similar cases

## Input Format

You will receive multiple ground-truth entries in this format:

```
---
Entry #1:
Issue Type: <SAST_ISSUE_TYPE>
CWE: <CWE_ID> (<MITRE_LINK>)

Error Trace:
<MULTI_LINE_SAST_TRACE>

Source Code:
```<language>
<RELEVANT_SOURCE_CODE>
```

Ground Truth Classification: <FALSE POSITIVE | TRUE POSITIVE>
Human Expert Justification: <EXPERT_REASONING>
Analyst Comment: <OPTIONAL_ADDITIONAL_NOTES>
--------------------------------------------

```

**Note:** The "Analyst Comment" line is optional - it will only appear when the Comment field is present in the Excel data.

## Output Format

Generate a JSON file following this exact schema:

```json
{
  "patterns": [
    {
      "pattern_id": "EXAMPLE-001",
      "pattern_name": "Descriptive Pattern Name",
      "issue_types": ["OVERRUN", "USE_AFTER_FREE"],

      "example_entry": {
        "issue_type": "OVERRUN",
        "file": "path/to/file.c",
        "line": 42,
        "analyst_reasoning": "Brief quote from human justification"
      },

      "investigation_steps": [
        {
          "step_number": 1,
          "action": "What to check (e.g., 'Check array bounds validation')",
          "evidence_found": "What was discovered (e.g., 'Conditional check at line 405')",
          "significance": "Why this matters (e.g., 'Prevents index from reaching 50000')"
        },
        {
          "step_number": 2,
          "action": "...",
          "evidence_found": "...",
          "significance": "..."
        }
      ],

      "root_cause_analysis": {
        "classification_type": "FALSE_POSITIVE or TRUE_POSITIVE (extract from 'Ground Truth Classification' in input)",
        "sast_assumption": "What SAST assumed about the code",
        "actual_behavior": "What the code actually does",
        "discrepancy_analysis": "For FALSE_POSITIVE: Why SAST's assumption is incorrect. For TRUE_POSITIVE: Why the vulnerability is real and exploitable",
        "mechanism": "The underlying mechanism or pattern"
      },

      "structural_pattern": {
        "required_imports": ["header.h", "library.h"],
        "required_functions": ["function_name", "macro_name"],
        "code_patterns": ["pattern idiom like 'bounds-checked array access'"],
        "data_flow_patterns": "Optional: allocation -> validation -> use",
        "context_requirements": "When this pattern applies"
      },

      "confidence_assessment": {
        "pattern_confidence": 0.95,
        "evidence_quality": "HIGH|MEDIUM|LOW",
        "generalizability": "HIGH|MEDIUM|LOW",
        "reasoning": "Explanation for confidence scores"
      },

      "pattern_coverage": {
        "total_entries_matched": 15,
        "percentage_of_dataset": "30%"
      }
    }
  ],

  "summary": {
    "total_patterns": 1,
    "coverage": "30%",
    "avg_confidence": 0.95,
    "key_points": [
      "Evidence-based: Uses actual code inspection",
      "Step-by-step investigation process",
      "Explains WHY SAST is wrong",
      "Reusable across similar codebases"
    ]
  }
}
```

## Critical Constraints

1. **ONLY extract patterns EXPLICITLY mentioned in human justifications** - do not infer beyond what the expert wrote
2. **Every investigation step must reference specific code elements** - line numbers, variable names, function calls
3. **If justification is vague, mark confidence as LOW** - be honest about evidence quality
4. **Base analysis solely on provided information** - no assumptions about broader codebase
5. **Group similar findings into patterns** - if 5 entries have similar reasoning, create 1 pattern covering all 5
6. **Calculate coverage accurately** - percentage = (entries matched by pattern / total entries) × 100
7. **Use analyst comments when present** - Comments may provide additional context beyond the justification. If a comment clarifies or expands on the justification, incorporate it into your analysis.

## Common False Positive Patterns (Reference)

When extracting patterns from expert justifications, these categories frequently appear as FALSE POSITIVES. Use these as hints when expert reasoning mentions similar patterns:

**Automated False Positive Categories:**
- **Denial of Service (DOS)** - Resource exhaustion or service disruption issues
- **Secrets on disk** - Credentials stored on disk if access-controlled or encrypted
- **Rate limiting** - Request throttling or quota enforcement concerns
- **Memory safety in safe languages** - Buffer overflows, use-after-free in Rust, Go (with safe APIs)
- **Log injection** - Unsanitized user input in log messages (log spoofing)
- **Regex issues** - Regex injection or regex DOS vulnerabilities
- **XSS in modern frameworks** - React/Angular XSS without `dangerouslySetInnerHTML` or `bypassSecurityTrustHtml`
- **Command injection in scripts** - Shell scripts without untrusted input paths
- **Theoretical race conditions** - Timing issues without practical exploitation paths

**Context-Dependent False Positives:**
- **Trusted input only** - Code paths only reachable with validated/trusted input
- **Test-only code** - Vulnerabilities in unit tests or test fixtures
- **Documentation issues** - Security concerns in markdown or doc files
- **Environment variables** - Issues requiring control of env vars (trusted in secure deployments)
- **Client-side validation** - Missing auth/permission checks in client JS/TS (server validates)
- **Internal APIs** - Vulnerabilities in code only callable by trusted internal services

**Note:** These patterns represent common expert reasoning. Always base your analysis on what the expert actually wrote in their justification, not on these general categories alone.

## Pattern Extraction Strategy

1. **Read all entries first** - identify common themes across multiple findings
2. **Group by root cause** - e.g., all "bounds checking before array access" cases
3. **Extract investigation workflow** - what did the analyst check first, second, etc.?
4. **Identify code patterns** - what makes these cases similar structurally?
5. **Assess generalizability** - can this pattern apply to other projects?

## Quality Guidelines

> **Note**: The following confidence ranges are defined in `config/process_mining_config.yaml` and can be adjusted based on project needs.

- **HIGH confidence (0.8-1.0)**: Clear evidence in code, detailed justification, multiple similar cases
- **MEDIUM confidence (0.5-0.8)**: Some evidence, moderate justification quality, few similar cases
- **LOW confidence (0.0-0.5)**: Vague justification, weak evidence, unique case

## Example Analysis Walkthrough

**Input:**

```
Issue Type: OVERRUN
CWE: CWE-119
Error Trace:
file.c:10: currBlock incremented to 50000
file.c:12: array access at index 50000 (max 49999)

Source Code:
```c
if (currBlock >= 50000) return;  // line 10
currBlock++;                       // line 11
arr[currBlock] = getValue();      // line 12
```

Ground Truth: FALSE POSITIVE
Justification: Early return at line 10 prevents currBlock from reaching 50000

```

**Output Pattern:**
```json
{
  "pattern_id": "BOUNDS-CHECK-001",
  "pattern_name": "Early Return Bounds Validation",
  "investigation_steps": [
    {
      "step_number": 1,
      "action": "Check for bounds validation before increment",
      "evidence_found": "Line 10: 'if (currBlock >= 50000) return;'",
      "significance": "Exits function before currBlock can reach 50000"
    },
    {
      "step_number": 2,
      "action": "Verify control flow prevents overflow",
      "evidence_found": "Return statement executes before line 11 increment",
      "significance": "Array access at line 12 is never reached with currBlock=50000"
    }
  ],
  "root_cause_analysis": {
    "classification_type": "FALSE_POSITIVE",
    "sast_assumption": "SAST assumes increment can reach 50000 and access array",
    "actual_behavior": "Early return prevents increment from reaching critical value",
    "discrepancy_analysis": "SAST doesn't recognize early return as bounds protection",
    "mechanism": "Defensive programming pattern: validate-before-increment"
  },
  "confidence_assessment": {
    "pattern_confidence": 0.95,
    "evidence_quality": "HIGH",
    "generalizability": "HIGH",
    "reasoning": "Clear control flow, explicit bounds check, common defensive pattern"
  }
}
```

---

## Instructions for Use

### Prerequisites

**IMPORTANT: The pattern data folder (e.g., `pattern_data/train_pattern_data`, `pattern_data/test_pattern_data`) must be provided as context**.

This folder contains pre-processed package files generated by:

```bash
cd sast-ai-workflow
python scripts/batch_prepare_patterns.py
```

Each file `pattern_data/train_pattern_data/{package-name}.txt` contains:

- Complete ground-truth entries for that package
- Relevant source code for each entry
- Pre-formatted in the expected input format

---

## Your Task: Process All Package Files

**Process EVERY `.txt` file in the provided pattern_data directory** (e.g., `pattern_data/train_pattern_data`, `pattern_data/test_pattern_data`, or `pattern_data/validation_pattern_data`) and generate pattern analysis JSON for each package.

### Processing Instructions:

1. **Identify the source dataset** from the provided context folder name:
   - `pattern_data/train_pattern_data` → output to `patterns/train_patterns/`
   - `pattern_data/test_pattern_data` → output to `patterns/test_patterns/`
   - `pattern_data/validation_pattern_data` → output to `patterns/validation_patterns/`

2. **List all package files** in the provided pattern_data directory

3. **For EACH package file** (e.g., `pattern_data/train_pattern_data/bzip2-1.0.8-18.el10.txt`):

   a. **Read the file contents**

   b. **Extract ground-truth entries** (look for section starting with "GROUND-TRUTH ENTRIES FOR:")

   c. **Analyze all entries** following the Pattern Extraction Strategy:

   - Read all entries first to identify common themes
   - Group similar findings by root cause
   - Extract investigation workflow from human justifications
   - Identify structural code patterns
   - Assess generalizability

   d. **Generate JSON output** following the Output Format schema above:

   - Create pattern objects for each identified pattern
   - Include investigation_steps, root_cause_analysis, structural_pattern
   - Calculate accurate coverage percentages
   - Add summary with key insights

   e. **Save output** in the appropriate dataset subdirectory:

   **Output Path Format:** `patterns/{dataset}_patterns/{package-name}_{model}.json`

   **Examples:**
   - Input: `pattern_data/train_pattern_data/bzip2-1.0.8-18.el10.txt`
     → Output: `patterns/train_patterns/bzip2-1.0.8-18.el10_claude.json`

   - Input: `pattern_data/test_pattern_data/sed-4.9-1.el10.txt`
     → Output: `patterns/test_patterns/sed-4.9-1.el10_claude.json`

   - Input: `pattern_data/validation_pattern_data/gzip-1.13-2.el10.txt`
     → Output: `patterns/validation_patterns/gzip-1.13-2.el10_claude.json`

   - `model` should be claude/gemini/gpt (and not cursor)
   - `dataset` is extracted from the input folder name (train/test/validation)
3. **Process ALL packages**, not just a subset - comprehensive coverage is critical
4. **Ensure quality**:

   - Each package gets independent pattern analysis
   - Pattern IDs are unique within each package (format: `{CATEGORY}-{NNN}`)
   - All JSON outputs are valid and follow the schema exactly

### Example Processing Flow:

**For train_pattern_data:**
```
Input: pattern_data/train_pattern_data/bzip2-1.0.8-18.el10.txt
├── Contains: 5 ground-truth entries with source code
└── Output: patterns/train_patterns/bzip2-1.0.8-18.el10_claude.json
    └── Patterns: 2 patterns covering all 5 entries

Input: pattern_data/train_pattern_data/sed-4.9-1.el10.txt
├── Contains: 8 ground-truth entries with source code
└── Output: patterns/train_patterns/sed-4.9-1.el10_claude.json
    └── Patterns: 3 patterns covering all 8 entries
```

**For test_pattern_data:**
```
Input: pattern_data/test_pattern_data/gzip-1.13-2.el10.txt
├── Contains: 3 ground-truth entries with source code
└── Output: patterns/test_patterns/gzip-1.13-2.el10_claude.json
    └── Patterns: 1 pattern covering all 3 entries
```

### Quality Checklist (for each package):

Before generating output JSON, verify:

- [ ] All entries in the package file have been analyzed
- [ ] Patterns reference specific evidence from human justifications
- [ ] Coverage percentages sum to ≤ 100%
- [ ] All required JSON fields are present and valid
- [ ] Pattern IDs follow format: `{CATEGORY}-{NNN}` (e.g., "BOUNDS-CHECK-001")
- [ ] No assumptions made beyond what's in human justifications

### Post-Processing Note:

After you complete pattern generation for all packages:

- The same `pattern_data/` files will be processed by 2 other models (GPT, Gemini) independently
- All 3 outputs for each package will be aggregated using `pattern_aggregation_prompt.md`
- Final result: `patterns/{package-name}_final.json` for each package (consensus patterns)
