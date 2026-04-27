# Pattern Extraction Pipeline

Standalone offline pipeline that extracts generalizable false positive patterns from historical SAST analysis data. The extracted patterns can be used to improve automated false positive detection.

## Overview

The pipeline reads ground-truth data or ignore.err files, groups entries by issue type, sends them to an LLM for pattern extraction, and outputs structured JSON with patterns, matching criteria, and confidence scores.

```
Input Files → Parse → Filter → Group by Issue Type → LLM Extraction → Checkpoint → Output JSON
```

## Usage

```bash
python -m pattern_extraction.extract_patterns \
    --input-dir test_pattern_data/ \
    --input-format ground_truth \
    --output-file extracted_patterns.json \
    --batch-size 5 \
    --checkpoint-interval 10 \
    --resume \
    -v
```

### Required Environment Variables

| Variable | Description |
|----------|-------------|
| `LLM_URL` | LLM API endpoint (or use `--llm-url`) |
| `LLM_MODEL_NAME` | Model name (or use `--llm-model`) |
| `LLM_API_KEY` | API key for authentication |

### CLI Options

| Option | Default | Description |
|--------|---------|-------------|
| `--input-dir` | *(required)* | Directory with input files |
| `--input-format` | `ground_truth` | Input format: `ground_truth` or `ignore_err` |
| `--output-file` | `extracted_patterns.json` | Output JSON file path |
| `--batch-size` | `5` | Packages per progress batch |
| `--entries-per-call` | `3` | Entries grouped per LLM invocation |
| `--checkpoint-dir` | `checkpoints/` | Directory for intermediate checkpoints |
| `--checkpoint-interval` | `10` | Save checkpoint every N packages |
| `--issue-types` | *(all)* | Filter to specific issue types |
| `--include-true-positives` | `false` | Include TRUE POSITIVE entries |
| `--resume` | `false` | Resume from checkpoints |
| `--max-retries` | `3` | Max LLM API retries per call |
| `-v` | `false` | Verbose/debug logging |

## Input Formats

### Ground Truth (`--input-format ground_truth`)

Structured `.txt` files from `test_pattern_data/` with labeled sections:

```
Entry #1:
Issue Type: INTEGER_OVERFLOW
CWE: CWE-190

Error Trace:
file.c:10: underflow: cast to signed type

Source Code (file.c):
...

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: Return value is checked.
```

### Ignore.err (`--input-format ignore_err`)

Raw `ignore.err` files with entries separated by double newlines:

```
Error: INTEGER_OVERFLOW (CWE-190):
file.c:10: tainted_data_argument: ...
# 10|->  int x = (int)offset;
Return value is properly bounded
```

## Output Format

```json
{
  "metadata": {
    "timestamp": "2026-04-15T...",
    "total_packages": 35,
    "processed_packages": 35,
    "total_entries_processed": 1162,
    "total_patterns_extracted": 287,
    "processing_time_seconds": 145.2
  },
  "patterns": [
    {
      "issue_type": "INTEGER_OVERFLOW",
      "cwe": "CWE-190",
      "pattern_summary": "Unsigned-to-signed cast with proper bounds checking",
      "code_pattern": "Cast where value is bounded or checked",
      "false_positive_indicators": [...],
      "matching_criteria": [...],
      "confidence": 0.85
    }
  ],
  "errors": []
}
```

## Configuration

The `PATTERN_EXTRACTION_MAX_SOURCE_CODE_CHARS` setting in `config/default_config.yaml` controls the maximum source code characters included per entry in the LLM prompt (default: 3000).

## Module Structure

| File | Purpose |
|------|---------|
| `extract_patterns.py` | CLI entry point with argparse |
| `pipeline.py` | Core pipeline orchestration |
| `parsers.py` | Input file parsers (ground truth + ignore.err) |
| `models.py` | Data models (ParsedFalsePositive, ExtractedPattern, etc.) |

## Checkpointing and Resume

The pipeline saves per-package checkpoints to the checkpoint directory. Use `--resume` to skip already-processed packages and continue from where a previous run left off. This is useful for large datasets or when recovering from interruptions.
