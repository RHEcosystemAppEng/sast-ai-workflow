# SAST Pattern Mining

## Overview

This directory contains prompt templates for extracting investigation patterns from human-annotated SAST findings using a single LLM approach.

## Workflow Summary

1. **Data Preparation**: Process ground-truth Excel files and fetch source code
2. **Pattern Learning**: Use a single LLM (Claude) to analyze ground-truth SAST findings and extract patterns for each package
3. **Pattern Aggregation**: Aggregate patterns across all packages into a final cross-package pattern file

## Files

- **[prompts/pattern_learning_prompt.md](prompts/pattern_learning_prompt.md)** - Main prompt for pattern extraction from individual packages
- **[prompts/pattern_aggregation_prompt.md](prompts/pattern_aggregation_prompt.md)** - Prompt for aggregating patterns across all packages
- **[examples/example_pattern_output.json](examples/example_pattern_output.json)** - Example final output

---

## Configuration

Pattern mining operations are controlled by [config/process_mining_config.yaml](config/process_mining_config.yaml). This file contains all thresholds, weights, and parameters used in pattern aggregation and data preparation.

### Key Configuration Sections

#### Pattern Aggregation

- **Confidence Formula Weights**: Control how coverage, frequency, consistency, and evidence quality contribute to final confidence scores (must sum to 1.0)
- **Pattern Matching Thresholds**: Minimum concept overlap for pattern similarity detection
- **Evidence Quality Thresholds**: Define what counts as HIGH/MEDIUM/LOW quality based on pattern count
- **Generalizability Thresholds**: Package coverage thresholds for pattern applicability
- **Key Concepts**: Keywords used for semantic pattern matching (e.g., ownership, transfer, reference, glib, error, etc.)
- **API Family Rules**: How functions are grouped by API family (g_object, g_task, g_error, pthread)

#### Pattern Preparation

- **Default Directories**: Input/output locations for ground-truth files and pattern data
- **RPM Source Handling**: Cache location and default source mode (rpm vs git)
- **False Positive Markers**: Which Excel values indicate false positives (y, yes, true)

#### Prompt Template Guidance

- **Confidence Ranges**: LLM guidance for HIGH/MEDIUM/LOW confidence assessment
- **Aggregation Thresholds**: LLM guidance for pattern inclusion criteria

### Customizing Configuration

1. **Edit the config file**: `process_mining/config/process_mining_config.yaml`
2. **Validate changes**: Ensure confidence weights sum to 1.0
3. **Use custom config**: Pass `--config /path/to/custom_config.yaml` to scripts

### CLI Overrides

Some config values can be overridden via CLI flags (CLI takes precedence):

**Train/Validation/Test Split:**
- `--train-ratio FLOAT`: Override train split ratio (split script)
- `--random-seed INT`: Override random seed (split script)
- `--split-tolerance FLOAT`: Override split ratio tolerance ±% (split script)
- `--fp-tolerance FLOAT`: Override FP/TP ratio tolerance ±% (split script)

**Pattern Aggregation:**
- `--min-packages N`: Override minimum package threshold (pattern aggregation)
- `--min-confidence F`: Override minimum confidence threshold (pattern aggregation)

**Data Preparation:**
- `--rpm-cache-dir DIR`: Override RPM cache location (data preparation)
- `--source-mode MODE`: Override source fetching mode - rpm or git (data preparation)
- `--output-dir DIR`: Override output directory (data preparation)

**Configuration Precedence**: CLI arguments > config file > hardcoded defaults

---

## Ground-Truth Dataset Management

### Dataset Overview

The ground-truth dataset consists of Excel files (`.xlsx`) containing human-annotated SAST findings:

- **Location**: `process_mining/data/ground-truth/`
- **Format**: One Excel file per package (e.g., `bash-5.2.26-3.el10.xlsx`)
- **Content**: SAST findings with human annotations indicating whether each finding is a false positive or true positive

**Key Statistics** (example dataset):

- Total packages: ~250 Excel files
- Valid packages: ~179 (after preprocessing)
- Total issues: ~4,287 annotated findings
- FP/TP Distribution: ~68% False Positives, ~32% True Positives
- Issue type diversity: 42 different issue types, with top 9 covering ~87% of data
- Package sizes: Range from 1 to 138 issues per package (median: 9, mean: 21.5)

### Required Excel Columns

For a ground-truth Excel file to be valid, it must contain:

1. **`Finding`** - The SAST finding output (issue type, CWE, error trace)
2. **`False Positive?`** - Annotation column indicating if the finding is a false positive
   - Valid values for **False Positive**: `y`, `yes`, `true` (case-insensitive)
   - Other non-empty values = True Positive
   - **Empty or missing values**: Entry is **skipped** (unknown classification)
3. **At least ONE justification column with data**:
   - **`Hint`** - Human expert's reasoning/justification
   - **`Comment`** - Additional comments or alternative justification

### Preprocessing: Identifying Invalid Files

Before splitting the dataset, run the preprocessing script to identify files with missing or incomplete annotations:

```bash
cd sast-ai-workflow
python process_mining/scripts/preprocess_ground_truth.py
```

**What it does:**

- Scans all `.xlsx` files in the ground-truth directory
- Validates each file against the required column criteria
- Generates three output files documenting validation results

**Output Files:**

1. **`invalid_files.txt`** - Simple list of invalid files (used by split script)

   ```
   # Files excluded from train/validation/test datasets due to validation failures
   # Generated: 2025-12-28 14:51:22
   # Total invalid: 70

   acl-2.3.2-1.el10.xlsx
   bluez-5.72-3.el10.xlsx
   dmidecode-3.5-3.el10.xlsx
   ...
   ```
2. **`invalid_files_detailed.json`** - Detailed breakdown with reasons

   ```json
   {
     "metadata": {
       "generation_timestamp": "2025-12-28T14:51:22",
       "total_files_scanned": 250,
       "valid_files": 180,
       "invalid_files": 70
     },
     "invalid_files": [
       {
         "filename": "acl-2.3.2-1.el10.xlsx",
         "package_name": "acl-2.3.2-1.el10",
         "reason": "Missing both 'Hint' and 'Comment' columns",
         "validation_category": "no_justification"
       },
       ...
     ],
     "summary_by_category": {
       "no_justification": 25,
       "empty_justification": 20,
       "missing_fp_column": 15,
       "empty_fp_annotations": 8,
       "read_error": 2
     }
   }
   ```
3. **`preprocessing_report.txt`** - Human-readable summary report

**Validation Categories:**

- `no_justification` - Missing both 'Hint' and 'Comment' columns
- `empty_justification` - Columns exist but contain no data
- `missing_fp_column` - 'False Positive?' column is missing
- `empty_fp_annotations` - 'False Positive?' column has no annotations
- `read_error` - File cannot be read or parsed

**CLI Options:**

```bash
# Standard run
python process_mining/scripts/preprocess_ground_truth.py

# Custom directory
python process_mining/scripts/preprocess_ground_truth.py --ground-truth-dir /path/to/data

# With output prefix (useful for versioning)
python process_mining/scripts/preprocess_ground_truth.py --output-prefix "2025-12-28_"
```

### Train/Validation/Test Splitting

After preprocessing, split the valid ground-truth data into three sets for model training, hyperparameter tuning, and final evaluation.

#### Split Strategy: Three-Way Stratified Split (70/10/20)

The split script creates:

- **Train set**: ~70% of packages
- **Validation set**: ~10% of packages
- **Test set**: ~20% of packages

**Key Properties:**

1. **Stratified Sampling**: Packages are grouped into strata based on:

   - **Size category**: Small (1-5 issues), Medium (6-20), Large (>20)
   - **FP ratio bucket**: Low (0-25% FP), Medium (25-75% FP), High (75-100% FP)
   - Creates 9 composite strata (3 × 3 combinations)
2. **Dual-Objective Optimization**: Both package count AND issue count must hit target ratios (±2%)

   - Ensures balanced distribution by both quantity and workload
3. **Adaptive FP/TP Balance**: All three sets match the overall FP ratio (±3% tolerance)

   - Dynamically calculates overall FP ratio (e.g., ~68%)
   - Validates that train, validation, and test sets preserve this balance
4. **Issue Type Diversity**: Top 9 issue types must appear in all three sets

   - Prevents training/validation/test on disjoint issue type distributions
5. **Package-Level Granularity**: Each package goes entirely into one set

   - Prevents data leakage across train/validation/test

#### Two-Stage Split Algorithm

The script uses a sequential two-stage approach:

**Stage 1**: Split into Train (70%) and Temp (30%)

- Apply stratified greedy allocation algorithm
- Minimize combined error for package count + issue count

**Stage 2**: Split Temp into Validation (33.3%) and Test (66.7%)

- 33.3% of 30% = 10% of original dataset (validation)
- 66.7% of 30% = 20% of original dataset (test)
- Apply same stratified allocation within temp set

#### Running the Split Script

```bash
cd sast-ai-workflow

# Standard run (70/10/20 split, random seed 42)
python process_mining/scripts/split_train_val_test.py

# Dry run to preview split without copying files
python process_mining/scripts/split_train_val_test.py --dry-run

# Different random seed (for exploring different splits)
python process_mining/scripts/split_train_val_test.py --random-seed 123

# Force proceed even if validation fails (not recommended)
python process_mining/scripts/split_train_val_test.py --force

# Ignore exclusions (process all files, including invalid ones)
python process_mining/scripts/split_train_val_test.py --ignore-exclusions

# Custom tolerance values (relax validation criteria)
python process_mining/scripts/split_train_val_test.py --split-tolerance 0.03 --fp-tolerance 0.05

# Override all split parameters
python process_mining/scripts/split_train_val_test.py --train-ratio 0.75 --random-seed 999 --split-tolerance 0.025
```

**What it does:**

1. Loads exclusion list from `invalid_files.txt` (generated by preprocessing)
2. Loads and analyzes all valid packages
3. Computes metadata (issue count, FP ratio, issue types, strata)
4. Performs two-stage stratified split
5. Validates split quality against all criteria
6. Copies files to `train/`, `validation/`, and `test/` subdirectories
7. Generates manifest files and detailed report

**Output Structure:**

```
ground-truth/
├── full_dataset/
│   └── *.xlsx (original files - 250 total)
├── invalid_files.txt (32 excluded files)
├── invalid_files_detailed.json
├── preprocessing_report.txt
├── train/
│   └── *.xlsx (~125 files - 70%)
├── validation/
│   └── *.xlsx (~18 files - 10%)
├── test/
│   └── *.xlsx (~36 files - 20%)
├── train_manifest.txt
├── validation_manifest.txt
├── test_manifest.txt
└── split_report.txt
```

**Manifest Files:**

Each manifest contains one package name per line (sorted):

```
bash-5.2.26-3.el10
bzip2-1.0.8-18.el10
coreutils-9.4-7.el10
...
```

#### Split Report

The `split_report.txt` provides detailed validation results:

```
================================================================================
TRAIN/VALIDATION/TEST SPLIT REPORT
================================================================================

## Overall Statistics
Total Packages: 179
Total Issues: 4287
Overall FP: 2907 (67.8%)
Overall TP: 1380 (32.2%)

## Train Split
Packages: 125 (69.8%)
Issues: 3001 (70.0%)
FP: 2040 (68.0%)
TP: 961 (32.0%)

## Validation Split
Packages: 18 (10.1%)
Issues: 429 (10.0%)
FP: 291 (67.8%)
TP: 138 (32.2%)

## Test Split
Packages: 36 (20.1%)
Issues: 857 (20.0%)
FP: 576 (67.2%)
TP: 281 (32.8%)

## Validation Results
FP/TP Ratio: [PASS]
  Overall FP Ratio: 67.8%
  Train FP Ratio: 68.0% (deviation: 0.2%)
  Validation FP Ratio: 67.8% (deviation: 0.0%)
  Test FP Ratio: 67.2% (deviation: 0.6%)
  Tolerance: ±3.0%

Split Ratio: [PASS]
  Train: Packages 69.8%, Issues 70.0% (target: 70.0% ±2.0%)
  Validation: Packages 10.1%, Issues 10.0% (target: 10.0% ±2.0%)
  Test: Packages 20.1%, Issues 20.0% (target: 20.0% ±2.0%)

Issue Type Diversity: [PASS]

## Top 9 Issue Type Distribution
Issue Type            Total    Train     Val    Test   Train %    Val %   Test %
----------------------------------------------------------------------------------
RESOURCE_LEAK          995      686       97     212    68.9%     9.7%    21.3%
OVERRUN                841      603       84     154    71.7%    10.0%    18.3%
UNINIT                 830      582       82     166    70.1%     9.9%    20.0%
...

## Stratification Breakdown
Stratum               Train Pkg    Val Pkg   Test Pkg  Train Issues    Val Issues   Test Issues
----------------------------------------------------------------------------------------------------
large_high                   26          3          8          1429           220           302
large_low                     4          1          0           149            63             0
large_medium                 19          3          5          1003           130           218
...

================================================================================
OVERALL: VALIDATION PASSED
================================================================================
```

#### Validation Criteria

**Primary (must pass):**

- **Adaptive FP/TP Balance**: All three sets match overall FP ratio (±3%)

  - Train FP ratio ≈ 67.8% (±3%)
  - Validation FP ratio ≈ 67.8% (±3%)
  - Test FP ratio ≈ 67.8% (±3%)
- **Three-Way Split Ratios** (±2% tolerance):

  - Train: 70% ± 2.2% (packages AND issues)
  - Validation: 10% ± 2% (packages AND issues)
  - Test: 20% ± 2% (packages AND issues)

**Secondary:**

- Top 9 issue types present in all three sets
- Stratum distribution across train/validation/test
- Package size distribution in each set

#### Finding a Good Random Seed

Because of the three-way split, finding a random seed that passes all validation criteria may require trying multiple values:

```bash
# Try different seeds until validation passes
for seed in 42 100 123 200 300 500 777 999 2025; do
  echo "=== Testing seed $seed ==="
  python process_mining/scripts/split_train_val_test.py --dry-run --random-seed $seed 2>&1 | grep "OVERALL:"
done
```

If no seed passes validation, you can:

1. Use `--force` to proceed anyway (check report to see how close it was)
2. Relax tolerance values via CLI flags (recommended) or config file

#### CLI Options

| Flag                       | Description                         | Default                               | Example                             |
| -------------------------- | ----------------------------------- | ------------------------------------- | ----------------------------------- |
| `--ground-truth-dir DIR` | Path to ground-truth directory      | From config                           | `--ground-truth-dir /custom/path` |
| `--train-ratio FLOAT`    | Train split ratio (affects Stage 1) | From config: `0.70`                 | `--train-ratio 0.75`              |
| `--random-seed INT`      | Random seed for reproducibility     | From config: `42`                   | `--random-seed 123`               |
| `--split-tolerance FLOAT`| Split ratio tolerance (±%)          | From config: `0.02` (±2%)           | `--split-tolerance 0.03`          |
| `--fp-tolerance FLOAT`   | FP/TP ratio tolerance (±%)          | From config: `0.03` (±3%)           | `--fp-tolerance 0.05`             |
| `--dry-run`              | Preview split without copying files | -                                     | `--dry-run`                       |
| `--force`                | Proceed even if validation fails    | -                                     | `--force`                         |
| `--config FILE`          | Path to custom config YAML          | `config/process_mining_config.yaml` | `--config custom.yaml`            |
| `--ignore-exclusions`    | Ignore `invalid_files.txt`        | -                                     | `--ignore-exclusions`             |

### Recommended Workflow for Dataset Management

1. **Preprocess** to identify invalid files:

   ```bash
   python process_mining/scripts/preprocess_ground_truth.py
   ```
2. **Review** the preprocessing report:

   ```bash
   cat process_mining/data/ground-truth/preprocessing_report.txt
   ```
3. **Preview** the split with dry run:

   ```bash
   python process_mining/scripts/split_train_val_test.py --dry-run
   ```
4. **Review** the split report to check validation status:

   ```bash
   cat process_mining/data/ground-truth/split_report.txt
   ```
5. **Execute** the split if validation passed:

   ```bash
   python process_mining/scripts/split_train_val_test.py
   ```
6. **Verify** output directories:

   ```bash
   ls -l process_mining/data/ground-truth/train/
   ls -l process_mining/data/ground-truth/validation/
   ls -l process_mining/data/ground-truth/test/
   ```

### Important Notes

- **Invalid files are preserved**: Files listed in `invalid_files.txt` remain in the `ground-truth/` directory but are automatically excluded from splits
- **No data deletion**: The split script only copies files to subdirectories; original files are never modified or deleted
- **Reproducibility**: Same random seed produces the same split (for comparison)
- **Auditability**: All exclusions and split decisions are documented in generated reports
- **Data leakage prevention**: Package-level splitting ensures no overlap between train/validation/test

---

## Complete Workflow

### Step 1: Batch Pre-Process All Packages

Run the batch processor to generate ready-to-use input files:

```bash
cd sast-ai-workflow
python process_mining/scripts/batch_prepare_patterns.py
```

This will:

- Process all Excel files in `process_mining/data/ground-truth/full_dataset/` (or train/validation/test subdirectories)
- Fetch source code for each entry
- Generate ready-to-paste files in dataset-specific output directories
- Create processing summary and error log

**Output Directory Structure:**

The script automatically creates output directories based on the source dataset:
- Files from `full_dataset/` → `process_mining/data/pattern_data/full_dataset_pattern_data/`
- Files from `train/` → `process_mining/data/pattern_data/train_pattern_data/`
- Files from `validation/` → `process_mining/data/pattern_data/validation_pattern_data/`
- Files from `test/` → `process_mining/data/pattern_data/test_pattern_data/`

Each output file is named: `{package-name}.txt` (one per package)

**Important Notes:**

- **By default**: The script processes all files
- **With `--resume`**: Skips files already listed in `process_mining/data/pattern_data/_processing_summary.json` (useful for interrupted runs)
- **With `--force`**: Re-processes ALL files, overwriting existing outputs (useful after changing code/format)
- **Without both flags**: Existing output files are overwritten, but the script doesn't track what was processed

**CLI Options:**

| Flag                    | Description                                                                                            | Example                           |
| ----------------------- | ------------------------------------------------------------------------------------------------------ | --------------------------------- |
| `--config FILE`       | Path to custom config YAML                                                                             | `--config custom.yaml`          |
| `--dataset NAME`      | Which dataset subdirectory to process: `full_dataset`, `train`, `validation`, `test` (default: full_dataset) | `--dataset train`               |
| `--pattern PATTERN`   | Glob pattern for Excel files (default:`*.xlsx`)                                                      | `--pattern "bzip2*.xlsx"`       |
| `--output-dir DIR`    | Output directory (default: auto-generated based on dataset)                                            | `--output-dir custom_output/`   |
| `--workers N`         | Number of parallel workers (default: 1)                                                                | `--workers 4`                   |
| `--limit N`           | Limit number of packages to process (for testing)                                                      | `--limit 5`                     |
| `--resume`            | Resume: skip already-processed files                                                                   | `--resume`                      |
| `--force`             | Force re-process all files (ignore resume cache)                                                       | `--force`                       |
| `--dry-run`           | Show what would be processed without processing                                                        | `--dry-run`                     |
| `--skip-source-code`  | Skip source code fetching (faster, less useful)                                                        | `--skip-source-code`            |
| `--source-mode MODE`  | Source fetching mode:`rpm` or `git` (default: from config)                                         | `--source-mode git`             |
| `--rpm-cache-dir DIR` | RPM cache directory (default: from config)                                                             | `--rpm-cache-dir /custom/cache` |
| `--clear-rpm-cache`   | Clear RPM cache before processing                                                                      | `--clear-rpm-cache`             |

**Common Usage Examples:**

```bash
# Process full dataset (default)
python process_mining/scripts/batch_prepare_patterns.py

# Process train set only
python process_mining/scripts/batch_prepare_patterns.py --dataset train

# Process validation set only
python process_mining/scripts/batch_prepare_patterns.py --dataset validation

# Process test set only
python process_mining/scripts/batch_prepare_patterns.py --dataset test

# Test with 5 packages first
python process_mining/scripts/batch_prepare_patterns.py --limit 5

# Parallel processing (4 workers, faster)
python process_mining/scripts/batch_prepare_patterns.py --workers 4 --dataset train

# Resume interrupted run (skip already-processed files)
python process_mining/scripts/batch_prepare_patterns.py --resume --dataset train

# Force re-process all files (useful after changing output format)
python process_mining/scripts/batch_prepare_patterns.py --force

# Skip source code fetching (faster, but less useful)
python process_mining/scripts/batch_prepare_patterns.py --skip-source-code

# Process specific files
python process_mining/scripts/batch_prepare_patterns.py --pattern "bzip2*.xlsx"

# Dry run (see what would be processed)
python process_mining/scripts/batch_prepare_patterns.py --dry-run

# Use Git instead of RPM for source code
python process_mining/scripts/batch_prepare_patterns.py --source-mode git

# Clear RPM cache and re-process
python process_mining/scripts/batch_prepare_patterns.py --clear-rpm-cache --force
```

### Step 2: Extract Patterns for Each Package

For each package file generated in Step 1, use the LLM to extract patterns:

1. Open the pattern learning prompt: [prompts/pattern_learning_prompt.md](prompts/pattern_learning_prompt.md)
2. For each package file in `process_mining/data/pattern_data/`:
   - Open `process_mining/data/pattern_data/{package-name}.txt`
   - Copy the file contents
   - Paste into Cursor/Claude with the pattern learning prompt
   - Save the JSON output as `process_mining/data/patterns/{package-name}_claude.json`

**Example:**

```bash
# Input: pattern_data/bzip2-1.0.8-18.el10.txt
# Output: patterns/bzip2-1.0.8-18.el10_claude.json
```

**Note:** You can process multiple packages in parallel by using multiple Claude sessions.

### Step 3: Aggregate Patterns Across All Packages

Once all individual package patterns are extracted, aggregate them into a single cross-package pattern file:

1. Open the aggregation prompt: [prompts/pattern_aggregation_prompt.md](prompts/pattern_aggregation_prompt.md)
2. Provide the prompt with access to all `patterns/*_claude.json` files
3. The LLM will identify common patterns across packages and create a final aggregated pattern file
4. Save the output as `patterns/rhel_c_patterns_final.json`

**Alternative (using script):**

The pattern aggregator script can be used to programmatically aggregate patterns:

```bash
# Use the pattern aggregator script
python process_mining/scripts/pattern_aggregator.py \
  --patterns-dir process_mining/data/patterns/ \
  --output process_mining/data/patterns/rhel_c_patterns_final.json \
  --min-packages 2 \
  --min-confidence 0.7
```

**Available Flags:**

| Flag                       | Description                                      | Default                                                             | Example                             |
| -------------------------- | ------------------------------------------------ | ------------------------------------------------------------------- | ----------------------------------- |
| `--config FILE`          | Path to custom config YAML                       | `process_mining/config/process_mining_config.yaml`                | `--config custom.yaml`            |
| `--patterns-dir DIR`     | Directory containing pattern JSON files          | From config: `process_mining/data/patterns/`                       | `--patterns-dir custom_patterns/` |
| `--output FILE`          | Output file for aggregated patterns              | From config: `process_mining/data/patterns/rhel_c_patterns_final.json` | `--output final_patterns.json`    |
| `--min-packages N`       | Minimum number of packages for pattern inclusion | From config: `2`                                                   | `--min-packages 3`                |
| `--min-confidence FLOAT` | Minimum confidence score (0.0-1.0)               | From config: `0.7`                                                 | `--min-confidence 0.8`            |

### Step 4: Review Results

```bash
# View processing summary from Step 1
cat process_mining/data/pattern_data/_processing_summary.json

# Check for any errors during data preparation
cat process_mining/data/pattern_data/_errors.log

# View the final aggregated patterns
cat process_mining/data/patterns/rhel_c_patterns_final.json
```

---

## Output Structure

### Individual Package Pattern Files

Each `process_mining/data/patterns/{package-name}_claude.json` contains:

- **patterns**: Array of pattern objects specific to that package
  - **pattern_id**: Unique identifier (e.g., "BOUNDS-CHECK-001")
  - **pattern_name**: Descriptive name
  - **issue_types**: List of SAST issue types
  - **investigation_steps**: Step-by-step analysis workflow
  - **root_cause_analysis**: Why SAST was wrong/right
  - **structural_pattern**: Code patterns, imports, functions
  - **confidence_assessment**: Pattern quality metrics
- **summary**: Statistics for this package

### Final Aggregated Pattern File

The `process_mining/data/patterns/rhel_c_patterns_final.json` contains:

- **patterns**: Array of cross-package patterns
  - All fields from individual patterns, plus:
  - **cross_package_metadata**:
    - **packages_observed**: List of packages where this pattern appears
    - **total_occurrences**: How many times observed across all packages
    - **package_coverage_percentage**: Percentage of packages with this pattern
    - **api_family**: The API family associated with this pattern (e.g., g_task, pthread)
- **aggregation_summary**: Overall statistics
  - **total_input_files**: Number of package pattern files processed
  - **unique_cross_package_patterns**: Patterns appearing in 2+ packages

---

## Common Issues

### Data Preparation

- **Missing source code**: If `--source-mode=rpm` fails, try `--source-mode=git`
- **RPM cache issues**: Use `--clear-rpm-cache` to start fresh
- **Skipped files**: Check `process_mining/data/pattern_data/_processing_summary.json` for validation failures
- **Files not regenerating after code changes**: By default, the script regenerates all files. Use `--resume` to skip already-processed files (tracked in `_processing_summary.json`). To force regeneration of all files even in resume mode, use `--force`

### Pattern Extraction

- **Vague justifications**: Review ground-truth data and add more detail
- **Low coverage**: Pattern may be too specific; consider grouping similar cases
- **High confidence but appears in only 1 package**: Pattern is package-specific, not generalizable

### Pattern Aggregation

- **Too few cross-package patterns**: Lower `--min-packages` threshold
- **Patterns seem unrelated**: Increase `--min-confidence` to filter out weak patterns
- **Large output file**: Normal for 100+ packages; focus on high-confidence patterns

---

## Advanced Usage

### Selective Re-processing

```bash
# Re-process only specific packages
python process_mining/scripts/batch_prepare_patterns.py --pattern "systemd*.xlsx" --force

# Skip already-processed packages
python process_mining/scripts/batch_prepare_patterns.py --resume
```

### Custom Aggregation

```bash
# Only aggregate patterns appearing in 5+ packages
python process_mining/scripts/pattern_aggregator.py --min-packages 5

# Higher confidence threshold
python process_mining/scripts/pattern_aggregator.py --min-confidence 0.85
```
