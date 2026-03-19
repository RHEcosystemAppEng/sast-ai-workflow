# Investigation Tools Reference

The research agent has access to five code-gathering tools. All tools are sandboxed to the local repository clone and their results are accumulated into the **CODE BANK** that feeds the analysis node.

---

## Tool Overview

| Tool | Type | Agent selects when |
|------|------|--------------------|
| [`fetch_code`](#fetch_code) | Custom | Function name is known; need its definition |
| [`read_file`](#read_file) | Custom | File path is known; need a section or the whole file |
| [`search_codebase`](#search_codebase) | Custom | Function/symbol location is unknown; need to find it by pattern |
| [`list_directory`](#list_directory) | Community | Repo structure is unfamiliar; need to discover file names |
| [`file_search`](#file_search) | Community | Need to find files by name pattern across the repo |

---

## `fetch_code`

**Purpose:** Extracts a named function's **definition** from the source tree. Takes the file where the function is referenced (e.g. the file from the SAST trace where the call occurs) and the function name; returns the full function body. Uses a hybrid approach: first tries AST-aware extraction via the repo handler (which may locate the definition in a different file than the reference file), then falls back to brace-counting heuristics on the given file.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `file_path` | `str` | required | Path relative to repo root (e.g. `src/auth.c`) |
| `function_name` | `str` | required | Exact function name (e.g. `validate_input`) |
| `context_lines` | `int` | `2` | Extra lines of context before/after the function |

**Agent selects this tool when:**
- A SAST trace names a function call at a specific file/line — the agent passes that file and function name to retrieve the **definition**
- A function call appears in already-gathered code (CODE BANK) but its definition is not yet there — the agent follows the call chain deeper

**Trade-offs:**
- More precise than `read_file` for named functions; the sophisticated path can locate the definition even if it lives in a different file than the reference file provided
- Falls back gracefully to brace-counting if sophisticated extraction fails; may return partial code for very large or macro-heavy functions
- On file-not-found, returns recovery suggestions pointing to `search_codebase` and `list_directory`

**Examples:**
```
# SAST trace shows sanitize_input() called in src/auth.c → get its definition
fetch_code(file_path="src/auth.c", function_name="sanitize_input")

# SAST trace shows process_data() called in lib/utils.c → get its definition
fetch_code(file_path="lib/utils.c", function_name="process_data", context_lines=5)
```

---

## `read_file`

**Purpose:** Read the full content of a file, or a specific line range, with line numbers displayed.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `file_path` | `str` | required | Path relative to repo root (e.g. `src/main.c`) |
| `start_line` | `int` | `None` | First line to read (1-based, inclusive) |
| `end_line` | `int` | `None` | Last line to read (1-based, inclusive) |

**Agent selects this tool when:**
- The file path is known and the agent needs to read a header, config file, or include
- A specific line range from a SAST trace needs to be read but the function name is unknown
- `fetch_code` failed to locate a function — the agent falls back to reading the raw file
- A small file needs to be reviewed in full

**Trade-offs:**
- More general than `fetch_code` but returns more tokens for large files; `start_line`/`end_line` should be used to limit scope when possible
- Strips the repo-name prefix automatically if present in the path

**Examples:**
```
read_file(file_path="src/config.h")
read_file(file_path="src/auth.c", start_line=50, end_line=100)
read_file(file_path="include/defs.h", start_line=1, end_line=50)
```

---

## `search_codebase`

**Purpose:** Search the repository for lines matching a regex pattern across files of a given extension (case-insensitive).

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `pattern` | `str` | required | Regex pattern (e.g. `sanitize.*input`, `malloc\|calloc`) |
| `file_pattern` | `str` | `"*.c"` | Glob for file types to search (e.g. `*.h`, `*.py`) |
| `max_results` | `int` | `20` | Maximum matches to return |

**Agent selects this tool when:**
- The location of a function definition is unknown and needs to be found by name
- All usages of a security-relevant API (e.g. `malloc`, `free`, `strcpy`) need to be located
- Sanitization or validation functions need to be discovered by naming convention
- `fetch_code` or `read_file` returned a file-not-found error — searching by pattern is the recovery path

**Trade-offs:**
- Searches file contents by regex — powerful but can return many false-positive matches for short patterns; anchored or more specific regexes reduce noise
- Skips hidden directories/files (`.git`, etc.)
- Stops at `max_results` — results may be truncated for very common patterns

**Examples:**
```
search_codebase(pattern="sanitize_input", file_pattern="*.c")
search_codebase(pattern="malloc|calloc|realloc", file_pattern="*.c")
search_codebase(pattern="def validate_.*", file_pattern="*.py")
```

---

## `list_directory`

**Purpose:** List files and subdirectories at a given path within the repository. Sandboxed to the repo root.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `dir_path` | `str` | `"."` | Directory path relative to repo root |

**Agent selects this tool when:**
- The repo structure is unfamiliar and the agent needs to discover what files exist before fetching
- The exact file path is unknown and related source files need to be located
- The agent needs to know which C files exist in a given module directory

**Trade-offs:**
- Returns names only, not file contents; a follow-up call to `fetch_code`, `read_file`, or `search_codebase` is needed to get actual code
- Sandboxed — cannot escape the repo root

**Examples:**
```
list_directory(dir_path="src")
list_directory(dir_path="lib/crypto")
list_directory()    # lists repo root
```

---

## `file_search`

**Purpose:** Find files by name pattern (glob) within the repository. Searches file **names**, not file **contents**.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `pattern` | `str` | required | Unix shell glob (e.g. `*.c`, `auth*.py`, `*sanitize*`) |
| `dir_path` | `str` | `"."` | Directory to search in |

**Agent selects this tool when:**
- Files with security-relevant names (e.g. `*auth*`, `*sanitize*`, `*crypto*`) need to be located
- All files of a given type need to be found across a large codebase with unknown structure
- The agent needs to confirm a file exists before calling `fetch_code` or `read_file`

**Trade-offs:**
- Matches file **names** only — use `search_codebase` for content-level search
- Conditionally included (may not be available in all configurations)
- Sandboxed to repo root

**Examples:**
```
file_search(pattern="*.c")
file_search(pattern="*sanitize*", dir_path="src")
file_search(pattern="auth*.h")
```

---

## Circuit Breakers

The investigation subgraph uses multiple independent safety limits to prevent runaway LLM usage. All limits result in a forced `NEEDS_REVIEW` verdict and a `stop_reason` string recorded in state and Langfuse.

| Limit | Constant | Default | Trigger Condition | `stop_reason` |
|-------|----------|---------|-------------------|---------------|
| Max LLM calls per research iteration | `MAX_MODEL_CALLS` | 15 | Research agent makes ≥ 15 model calls in one iteration | `"research_recursion_limit_hit"` |
| Max consecutive evaluation rejections | `MAX_REJECTION_STREAK` | 3 | Evaluator returns `NEEDS_MORE_RESEARCH` ≥ 3 times in a row | `"evaluation_rejection_streak"` |
| Max iterations without new code | `MAX_NO_PROGRESS_STREAK` | 2 | Code bank length does not grow for ≥ 2 consecutive iterations | `"no_progress_detected"` |
| Max total iterations | `max_iterations` (state) | configured at runtime | `iteration >= max_iterations` | `"max_iterations"` |
| Evaluation error (after retries) | — | — | Evaluation node raises unrecoverable exception | `"evaluation_error"` |

When any limit is hit, the circuit breaker node sets:
```python
proposed_verdict = "NEEDS_REVIEW"
is_complete      = True
stop_reason      = "<reason string>"
```

---

## State Schemas

### `SASTWorkflowTracker` (outer pipeline state)

Defined in `src/dto/SASTWorkflowModels.py`. This is the LangGraph `StateGraph` state for the outer pipeline (all six nodes share it).

| Field | Type | Description |
|-------|------|-------------|
| `config` | `Config \| None` | Full workflow configuration |
| `iteration_count` | `int` | Number of analysis cycles the batch has gone through |
| `issues` | `Dict[str, PerIssueData]` | Map of `issue_id → PerIssueData` |
| `metrics` | `dict` | Calculated metrics for the workflow run |

### `PerIssueData` (per-issue state within `SASTWorkflowTracker`)

| Field | Type | Description |
|-------|------|-------------|
| `issue` | `Issue` | The raw SAST issue object |
| `source_code` | `Dict[str, List[str]]` | File paths → code snippets |
| `similar_known_issues` | `str` | Similar known issues retrieved from vector DB |
| `analysis_response` | `AnalysisResponse \| None` | Final analysis verdict and justification |
| `found_symbols` | `Set[str]` | Symbols already fetched (deduplication) |
| `fetched_files` | `List[str]` | Files fetched during investigation |
| `exploration_depth` | `int` | Number of symbols explored beyond initial trace |
| `final_confidence_score` | `float \| None` | Confidence score (0.0–1.0) from `calculate_metrics` |

### `InvestigationState` (inner subgraph state)

Defined in `src/sast_agent_workflow/nodes/sub_agents/investigation/nodes/schemas.py`. Shared by all nodes in the investigation subgraph (research, analysis, evaluation, circuit breaker).

**Input / setup:**

| Field | Type | Description |
|-------|------|-------------|
| `issue_id` | `str` | Unique identifier for the SAST issue |
| `issue_cwe` | `str` | CWE identifier (e.g. `CWE-476`) |
| `issue_description` | `str` | Human-readable issue description |
| `initial_code` | `str` | Code snippet from the original SAST report |

**Research phase:**

| Field | Type | Description |
|-------|------|-------------|
| `research_messages` | `Sequence[BaseMessage]` | Messages from the ReAct research agent |
| `gathered_code` | `str` | Accumulated CODE BANK from all tool results |
| `fetched_files` | `Dict[str, List[str]]` | Successful fetches, keyed by tool name |
| `tool_call_history` | `List[str]` | All tool calls with parameters (deduplication) |

**Analysis phase:**

| Field | Type | Description |
|-------|------|-------------|
| `analysis` | `str` | Full analysis narrative |
| `analysis_prompt` | `str` | Prompt used for analysis (stored for summarization) |
| `proposed_verdict` | `str` | `FALSE_POSITIVE`, `TRUE_POSITIVE`, or `NEEDS_REVIEW` |
| `justifications` | `List[str]` | Key findings supporting the verdict |
| `confidence` | `str` | `HIGH`, `MEDIUM`, or `LOW` |

**Evaluation phase:**

| Field | Type | Description |
|-------|------|-------------|
| `evaluation_result` | `str` | `APPROVED` or `NEEDS_MORE_RESEARCH` |
| `evaluation_feedback` | `str` | Evaluator's feedback on what is missing |
| `required_information` | `List[str]` | Specific items the evaluator requests |

**Circuit breaker tracking:**

| Field | Type | Description |
|-------|------|-------------|
| `evaluation_rejection_streak` | `int` | Consecutive `NEEDS_MORE_RESEARCH` count |
| `no_progress_streak` | `int` | Consecutive iterations with no new code |
| `previous_code_length` | `int` | Code bank size from previous iteration |
| `stop_reason` | `str \| None` | Why investigation terminated early (`None` = normal) |

**Control:**

| Field | Type | Description |
|-------|------|-------------|
| `iteration` | `int` | Current loop iteration |
| `max_iterations` | `int` | Maximum allowed iterations |
| `is_complete` | `bool` | Set to `True` when investigation ends |
| `needs_reanalysis` | `bool` | Route back to analysis without new research |
| `reanalysis_count` | `int` | Times evaluator triggered reanalysis (Langfuse score) |
| `total_tool_calls` | `int` | Cumulative tool calls across all iterations (Langfuse score) |
