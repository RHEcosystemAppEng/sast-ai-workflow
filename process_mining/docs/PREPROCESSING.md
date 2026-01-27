# Ground-Truth Dataset Preprocessing Guide

This guide covers the preprocessing workflow for SAST pattern mining ground-truth data, including validation, train/validation/test splitting, and dataset management.

---

## Ground-Truth Dataset Management

### Dataset Overview

The ground-truth dataset consists of two complementary data sources:

#### 1. Human-Annotated Excel Files

Human-reviewed SAST findings with detailed annotations:

- **Source**: Original data obtained from the [Clean Data](https://drive.google.com/drive/folders/1NwykVVNSWfUzXU8U3SvI_LEyefhgK9tk) folder in the SAST-AI Google Drive
- **Location**: `process_mining/data/ground-truth/full_dataset/`
- **Format**: One Excel file (`.xlsx`) per package (e.g., `bash-5.2.26-3.el10.xlsx`)
- **Content**: SAST findings with human annotations indicating whether each finding is a false positive or true positive, along with expert justifications

#### 2. Known False Positives (ignore.err files)

Expert-confirmed false positives from production systems:

- **Source**: Original data obtained from the [known-false-positives](https://gitlab.cee.redhat.com/osh/known-false-positives/) GitLab repository
- **Location**: `process_mining/data/ground-truth/known_non_issue/`
- **Format**: One directory per package containing an `ignore.err` file with false positive entries
- **Content**: SAST findings that have been reviewed and confirmed as false positives by Red Hat engineers, with concise justifications

#### Combined Dataset Statistics

After preprocessing and validation:

- **Total packages**: 208 (from 246 original Excel files, after validation)
  - Valid Excel files: 210 (36 excluded due to validation failures)
  - Packages with both Excel and known FP data: 152 (73.1%)
  - Excel-only packages: 56 (26.9%)
- **Exclusions**:
  - 36 Excel files excluded by preprocessing script: missing annotations, empty justifications, or **source code unavailable in Brew**
  - 32 ignore.err-only packages excluded by split script: no matching Excel file at the same NVR (cannot map to source code)
- **Total issues**: ~7,600 annotated findings (after excluding ignore.err-only packages)
  - Excel issues: 4,455 (~58%)
  - Paired known FP issues: ~3,145 (~42%)
- **FP/TP distribution**: 82.9% False Positives, 17.1% True Positives
- **Issue type diversity**: Top 9 issue types cover majority of data
- **Package sizes**: Range from 1 to 138+ issues per package

### Required Excel Columns

For a ground-truth Excel file to be valid, it must contain:

1. **`Finding`** - The SAST finding output (issue type, CWE, error trace)
2. **`False Positive?`** - Annotation column indicating if the finding is a false positive
   - Valid values for **False Positive**: `y`, `yes`, `true` (case-insensitive)
   - Valid values for **True Positive**: `n`, `no`, `false` (case-insensitive)
   - **Unrecognized, empty, or missing values**: Entry is **skipped** to avoid misclassification
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
- **Checks source code availability**: Verifies that the package NVR can be fetched from Brew
- Excludes packages with unreachable source code (cannot fetch from Brew)
- Generates three output files documenting validation results

**Output Files:**

1. **`invalid_files.txt`** - Simple list of invalid files (used by split script)

   ```
   # Files excluded from train/validation/test datasets due to validation failures
   # Reasons: missing annotations, empty justifications, source code unavailable
   # Generated: 2026-01-05 15:20:15
   # Total invalid: 36

   acl-2.3.2-1.el10.xlsx
   bluez-5.72-3.el10.xlsx
   dmidecode-3.5-3.el10.xlsx
   ...
   ```
2. **`invalid_files_detailed.json`** - Detailed breakdown with reasons

   ```json
   {
     "metadata": {
       "generation_timestamp": "2026-01-05T15:20:15",
       "total_files_scanned": 246,
       "valid_files": 210,
       "invalid_files": 36
     },
     "invalid_files": [
       {
         "filename": "acl-2.3.2-1.el10.xlsx",
         "package_name": "acl-2.3.2-1.el10",
         "reason": "Missing both 'Hint' and 'Comment' columns",
         "validation_category": "missing_justification"
       },
       {
         "filename": "dmidecode-3.5-3.el10.xlsx",
         "package_name": "dmidecode-3.5-3.el10",
         "reason": "Source code unavailable in Brew",
         "validation_category": "source_unavailable"
       },
       ...
     ],
     "summary_by_category": {
       "empty_justification": 29,
       "source_unavailable": 4,
       "empty_fp_annotations": 2,
       "missing_justification": 1,
       "read_error": 0
     }
   }
   ```
3. **`preprocessing_report.txt`** - Human-readable summary report

**Validation Categories:**

- `no_justification` - Missing both 'Hint' and 'Comment' columns
- `empty_justification` - Columns exist but contain no data
- `missing_fp_column` - 'False Positive?' column is missing
- `empty_fp_annotations` - 'False Positive?' column has no annotations
- `source_unavailable` - Package NVR cannot be fetched from Brew (source code unavailable)
- `read_error` - File cannot be read or parsed

**Source Code Availability Check:**

The preprocessing script verifies that each package's source code can be fetched from Red Hat Brew using its NVR (Name-Version-Release). This check is critical because:

- Pattern extraction requires access to the actual source code
- Source code is needed to map SAST findings to specific code locations
- Unreachable source code would cause failures during the pattern extraction phase
- Packages with unavailable source code are marked as `source_unavailable` and excluded

**CLI Options:**

```bash
# Standard run
python process_mining/scripts/preprocess_ground_truth.py

# Custom directory
python process_mining/scripts/preprocess_ground_truth.py --ground-truth-dir /path/to/data

# With output prefix (useful for versioning)
python process_mining/scripts/preprocess_ground_truth.py --output-prefix "2025-12-28_"
```

---

## Known False Positives Dataset Management

### Overview

The known false positives (FP) dataset provides additional SAST false positive examples with expert justifications from production systems.

- **Source**: Original data obtained from the [known-false-positives](https://gitlab.cee.redhat.com/osh/known-false-positives/) GitLab repository
- **Location**: `process_mining/data/ground-truth/known_non_issue/`
- **Format**: Each package directory contains an `ignore.err` file with false positive entries
- **Content**: SAST findings that have been reviewed and confirmed as false positives by Red Hat engineers
- **Current statistics**:
  - Total packages: 184
  - Total false positive entries: 3,906
  - Average entries per package: 21.1
  - Match rate with Excel dataset: 74.2% (152 packages have both Excel and known FP data)

### Preprocessing Workflow

The preprocessing workflow for known FPs involves three main steps:

#### 1. Extract RHEL-Relevant Packages

The first step filters the known-false-positives repository to include only packages that are relevant to our ground-truth dataset:

```bash
cd sast-ai-workflow
python process_mining/scripts/create_known_fps_list.py
```

**What it does:**

- Scans the `full_dataset/` directory to extract unique base package names (e.g., `systemd`, `bash`, `glibc`)
- Matches these against package directories in the `known-false-positives` repository
- Filters for packages that have valid `ignore.err` files with error entries
- Generates a list of matched packages and a detailed report

**Output Files:**

1. **`known_non_issue_packages.txt`** - Simple list of matched package names (used by copy script)

   ```
   ModemManager
   acl
   adcli
   audit
   ...
   ```
2. **`known_fps_matching_report.txt`** - Detailed matching statistics

   ```
   SUMMARY
   Total ground-truth packages (full_dataset): 248
   Total known-FP packages matched: 184
   Match rate: 74.2%
   Total false positive entries: 3884
   Average entries per package: 21.1
   ```

#### 2. Copy Package Directories

After identifying relevant packages, copy their directories to the ground-truth location:

```bash
python process_mining/scripts/copy_known_fps.py
```

**What it does:**

- Reads the package list from `known_non_issue_packages.txt`
- Copies each package directory from `known-false-positives` repository
- Extracts only the `ignore.err` file from each package
- Creates package subdirectories in `known_non_issue/`

**Output Structure:**

```
known_non_issue/
├── ModemManager/
│   └── ignore.err
├── bash/
│   └── ignore.err
├── systemd/
│   └── ignore.err
└── ... (184 packages total)
```

#### 3. Validate Known FP Dataset

Run the preprocessing validation to ensure all copied packages have valid ignore.err files:

```bash
cd ../../..  # Navigate to sast-ai-workflow/
python process_mining/scripts/preprocess_ground_truth.py --ground-truth-dir process_mining/data/ground-truth/known_non_issue
```

**What it does:**

- Validates that each package directory contains an `ignore.err` file
- Checks that ignore.err files contain comment lines (starting with `#`) with justifications
- Generates validation reports similar to the main ground-truth preprocessing

**Output Files:**

```
known_non_issue/
├── invalid_known_fps.txt
├── invalid_known_fps_detailed.json
└── known_fps_preprocessing_report.txt
```

**Example Report:**

```
KNOWN NON-ISSUES (FALSE POSITIVES) PREPROCESSING REPORT
Total Packages Scanned: 184
Valid Packages: 184 (100.0%)
Invalid Packages: 0 (0.0%)
```

#### 4. Process Known FPs with NVR Enrichment

**IMPORTANT**: This step is required before splitting! The split script needs NVR-enriched files.

The raw `known_non_issue/` data uses simple package names (e.g., `bash/ignore.err`), but the split script requires files with full NVR (Name-Version-Release) information to match them with Excel files. This preprocessing step pairs known FP files with their corresponding Excel files and creates NVR-enriched versions:

```bash
cd sast-ai-workflow
python process_mining/scripts/preprocess_known_non_issues.py
```

**What it does:**

- Scans `full_dataset/` directory to extract package NVRs from Excel filenames
- Pairs `known_non_issue/<package>/ignore.err` files with Excel files by base package name
- Creates NVR-enriched files in `processed_known_non_issues/<package-NVR>_ignore.err`
- Generates a detailed pairing report

**Input/Output:**

```
# Input structure:
known_non_issue/
├── bash/
│   └── ignore.err
├── acl/
│   └── ignore.err
...

# Output structure:
processed_known_non_issues/
├── bash-5.2.26-3.el10_ignore.err
├── acl-2.3.2-1.el10_ignore.err
...
```

**Why this is needed:**

- The split script matches packages by their full NVR to ensure correct pairing
- This step identifies which known FP packages have corresponding Excel files ([paired])
- **Important**: Known FP packages without corresponding Excel files are excluded because there's no way to determine their NVR for source code mapping

### Known FP Data Format

Each `ignore.err` file contains SAST error entries with expert justifications:

```
Error: CPPCHECK_WARNING (CWE-476: [#def1])
bash-5.2/lib/readline/display.c:2518: warning[nullPointer]: Possible null pointer dereference: local_prompt
# False Positive - local_prompt is checked for NULL before this line
# The static analyzer doesn't track the control flow correctly

Error: RESOURCE_LEAK (CWE-772: [#def2])
bash-5.2/execute_cmd.c:1234: leaked_storage: Variable fd going out of scope leaks the storage it points to
# False Positive - fd is closed in the error handling path
# The analyzer doesn't recognize the cleanup in the signal handler
```

**Key Characteristics:**

- Each error entry starts with `Error:` followed by issue type and CWE
- Justification lines start with `#` and explain why it's a false positive
- Entries are separated by blank lines
- Justifications are written by Red Hat security engineers

### Recommended Workflow

1. **Extract relevant packages:**

   ```bash
   cd sast-ai-workflow
   python process_mining/scripts/create_known_fps_list.py
   ```
2. **Review matching report:**

   ```bash
   cat process_mining/data/ground-truth/known_fps_matching_report.txt
   ```
3. **Copy package directories:**

   ```bash
   python process_mining/scripts/copy_known_fps.py
   ```
4. **Validate copied data:**

   ```bash
   python process_mining/scripts/preprocess_ground_truth.py --ground-truth-dir process_mining/data/ground-truth/known_non_issue
   ```
5. **Review validation report:**

   ```bash
   cat process_mining/data/ground-truth/known_non_issue/known_fps_preprocessing_report.txt
   ```
6. **Process known FPs with NVR enrichment (REQUIRED before splitting):**

   ```bash
   python process_mining/scripts/preprocess_known_non_issues.py
   ```

### Integration with Excel Dataset

The known FP dataset is designed to work alongside the human-annotated Excel files:

**Complementary Data Sources:**

- **Excel files**: Comprehensive human annotations with detailed `Hint` and `Comment` columns, covering both false positives AND true positives
- **Known FP files**: Focused false positive examples with concise expert justifications from production systems

**Key Differences:**

- Excel files provide both FP and TP examples for comprehensive training
- Known FP files contain ONLY false positives (100% FP)

**Combined Usage:**

- 152 packages (73.1% of dataset) have both Excel and known FP data, providing dual perspectives on false positives
- The preprocessing and splitting scripts handle both formats automatically
- Packages with only ignore.err (no Excel) are excluded due to missing NVR information

### Important Notes

- **Case-insensitive matching**: Package name matching is case-insensitive to handle naming variations
- **Entry counting**: Only lines starting with `Error:` are counted as entries
- **Justification format**: Comment lines (starting with `#`) contain the expert reasoning

---

## Train/Validation/Test Splitting

After preprocessing both the Excel files and known FP datasets, split the combined ground-truth data into three sets for model training, hyperparameter tuning, and final evaluation.

### Split Strategy: Three-Way Stratified Split (60/20/20)

The split script creates:

- **Train set**: ~60% of packages
- **Validation set**: ~20% of packages
- **Test set**: ~20% of packages

**Key Properties:**

1. **Stratified Sampling**: Packages are grouped into strata based on:

   - **Size category**: Small (1-5 issues), Medium (6-20), Large (>20)
   - **FP ratio bucket**: Low (0-25% FP), Medium (25-75% FP), High (75-100% FP)
   - Creates 9 composite strata (3 × 3 combinations)
2. **Dual-Objective Optimization**: Both package count AND issue count must hit target ratios (±3%)

   - Ensures balanced distribution by both quantity and workload
3. **Adaptive FP/TP Balance**: All three sets match the overall FP ratio (±3% tolerance)

   - Dynamically calculates overall FP ratio (e.g., ~82.9%)
   - Validates that train, validation, and test sets preserve this balance
4. **Issue Type Diversity**: Top 9 issue types must appear in all three sets

   - Prevents training/validation/test on disjoint issue type distributions
5. **Package-Level Granularity**: Each package goes entirely into one set

   - Prevents data leakage across train/validation/test

#### Two-Stage Split Algorithm

The script uses a sequential two-stage approach:

**Stage 1**: Split into Train (60%) and Temp (40%)

- Apply stratified greedy allocation algorithm
- Minimize combined error for package count + issue count

**Stage 2**: Split Temp into Validation (50%) and Test (50%)

- 50% of 40% = 20% of original dataset (validation)
- 50% of 40% = 20% of original dataset (test)
- Apply same stratified allocation within temp set

#### Running the Split Script

```bash
cd sast-ai-workflow

# Standard run (60/20/20 split, random seed 42)
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
python process_mining/scripts/split_train_val_test.py --train-ratio 0.65 --random-seed 999 --split-tolerance 0.025
```

**What it does:**

1. Loads exclusion list from `invalid_files.txt` (generated by preprocessing)
2. Discovers and pairs Excel files with their corresponding ignore.err files **at the NVR level**
   - Matches by exact NVR (e.g., `acl-2.3.2-1.el10.xlsx` ↔ `acl-2.3.2-1.el10_ignore.err`)
   - **Filters out ignore.err-only packages**: Packages with only ignore.err files (no matching Excel at the same NVR) are excluded
   - These packages cannot be used because they lack the NVR needed for source code mapping
   - Excluded packages are logged with warnings during processing
3. Loads and analyzes all valid packages
4. Computes metadata (issue count, FP ratio, issue types, strata)
5. Performs two-stage stratified split
6. Validates split quality against all criteria
7. Copies files to `train/`, `validation/`, and `test/` subdirectories
8. Generates manifest files and detailed report

**Output Structure:**

```
ground-truth/
├── full_dataset/
│   └── *.xlsx (original Excel files - 246 total)
├── known_non_issue/
│   └── <package>/ (original known FP packages - 184 total)
│       └── ignore.err
├── processed_known_non_issues/
│   └── <package-NVR>_ignore.err (NVR-enriched files - 152 paired + 32 ignored)
│       Only the 152 paired files (with matching Excel) are used in splits
├── invalid_files.txt (36 excluded Excel files)
├── invalid_files_detailed.json
├── preprocessing_report.txt
├── train/
│   ├── excel/
│   │   └── *.xlsx (Excel files)
│   └── known_non_issue/
│       └── <package-NVR>_ignore.err (only paired files)
│   (~125 packages total - 60%)
├── validation/
│   ├── excel/
│   │   └── *.xlsx
│   └── known_non_issue/
│       └── <package-NVR>_ignore.err (only paired files)
│   (~42 packages total - 20%)
├── test/
│   ├── excel/
│   │   └── *.xlsx
│   └── known_non_issue/
│       └── <package-NVR>_ignore.err (only paired files)
│   (~41 packages total - 20%)
├── train_manifest.txt
├── validation_manifest.txt
├── test_manifest.txt
└── split_report.txt
```

**Manifest Files:**

Each manifest contains package names with their pairing type:

```
# Package Manifest - NVR-Based Processing
# Format: package_name [paired|excel_only]
#
# [paired]: Package has both Excel and NVR-enriched ignore.err files
# [excel_only]: Package has only Excel file
# Note: Packages with only ignore.err (no Excel) are excluded from splits

acl [paired]
bash [paired]
bolt [excel_only]
chrpath [paired]
cpio [paired]
...
```

#### Split Report

The `split_report.txt` provides detailed validation results:

```
================================================================================
TRAIN/VALIDATION/TEST SPLIT REPORT
================================================================================

## Overall Statistics
Total Packages: 208 (only paired and excel-only)
Total Issues: ~7,600
Overall FP: ~6,300 (82.9%)
Overall TP: ~1,300 (17.1%)

## Pairing Statistics
Paired Packages (Excel + ignore.err): 152 (73.1%)
Excel-Only Packages: 56 (26.9%)
Excluded by split script: 32 ignore.err-only packages (no matching Excel at same NVR = no source code mapping)
Total Excel Issues: 4,455 (~58%)
Total Paired ignore.err Issues: ~3,145 (~42%)

## Train Split
Packages: ~125 (60%)
  Paired: ~91, Excel-only: ~34
Issues: ~4,560 (60%)
  Excel: ~2,673, ignore.err: ~1,887
FP: ~3,782 (83%)
TP: ~778 (17%)

## Validation Split
Packages: ~42 (20%)
  Paired: ~30, Excel-only: ~12
Issues: ~1,520 (20%)
  Excel: ~891, ignore.err: ~629
FP: ~1,260 (83%)
TP: ~260 (17%)

## Test Split
Packages: ~41 (20%)
  Paired: ~31, Excel-only: ~10
Issues: ~1,520 (20%)
  Excel: ~891, ignore.err: ~629
FP: ~1,260 (83%)
TP: ~260 (17%)

## Validation Results
FP/TP Ratio: [PASS]
  Overall FP Ratio: 82.9%
  Train FP Ratio: 83.0% (deviation: 0.1%)
  Validation FP Ratio: 82.9% (deviation: 0.0%)
  Test FP Ratio: 82.9% (deviation: 0.0%)
  Tolerance: ±3.0%

Split Ratio: [PASS]
  Train: Packages 60.1%, Issues 60.0% (target: 60.0% ±3.0%)
  Validation: Packages 20.2%, Issues 20.0% (target: 20.0% ±3.0%)
  Test: Packages 19.7%, Issues 20.0% (target: 20.0% ±3.0%)

Issue Type Diversity: [PASS]

## Top 9 Issue Type Distribution
Issue Type                   Total    Train      Val     Test   Train %     Val %    Test %
----------------------------------------------------------------------------------------------------
RESOURCE_LEAK                 1845     1015      450      380    55.0%    24.4%    20.6%
OVERRUN                       1659     1000      392      267    60.3%    23.6%    16.1%
UNINIT                        1387      788      166      433    56.8%    12.0%    31.2%
INTEGER_OVERFLOW              1110      663      163      284    59.7%    14.7%    25.6%
...

## Stratification Breakdown
Stratum               Train Pkg    Val Pkg   Test Pkg  Train Issues    Val Issues   Test Issues
----------------------------------------------------------------------------------------------------
large_high                   49         15         14          3465          1066          1292
large_low                     5          1          1           185            63            78
large_medium                 11          5          3           653           219           242
...

================================================================================
OVERALL: VALIDATION PASSED
================================================================================
```

#### Validation Criteria

**Primary (must pass):**

- **Adaptive FP/TP Balance**: All three sets match overall FP ratio (±3%)

  - Train FP ratio ≈ 82.9% (±3%)
  - Validation FP ratio ≈ 82.9% (±3%)
  - Test FP ratio ≈ 82.9% (±3%)
- **Three-Way Split Ratios** (±3% tolerance):

  - Train: 60% ± 3% (packages AND issues)
  - Validation: 20% ± 3% (packages AND issues)
  - Test: 20% ± 3% (packages AND issues)

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

| Flag                        | Description                         | Default                               | Example                             |
| --------------------------- | ----------------------------------- | ------------------------------------- | ----------------------------------- |
| `--ground-truth-dir DIR`  | Path to ground-truth directory      | From config                           | `--ground-truth-dir /custom/path` |
| `--train-ratio FLOAT`     | Train split ratio (affects Stage 1) | From config:`0.60`                  | `--train-ratio 0.65`              |
| `--random-seed INT`       | Random seed for reproducibility     | From config:`42`                    | `--random-seed 123`               |
| `--split-tolerance FLOAT` | Split ratio tolerance (±%)         | From config:`0.03` (±3%)           | `--split-tolerance 0.04`          |
| `--fp-tolerance FLOAT`    | FP/TP ratio tolerance (±%)         | From config:`0.03` (±3%)           | `--fp-tolerance 0.05`             |
| `--dry-run`               | Preview split without copying files | -                                     | `--dry-run`                       |
| `--force`                 | Proceed even if validation fails    | -                                     | `--force`                         |
| `--config FILE`           | Path to custom config YAML          | `config/process_mining_config.yaml` | `--config custom.yaml`            |
| `--ignore-exclusions`     | Ignore `invalid_files.txt`        | -                                     | `--ignore-exclusions`             |

### Recommended Workflow for Complete Dataset Management

#### Phase 1: Preprocess Excel Files

1. **Preprocess Excel files** to identify invalid files:

   ```bash
   cd sast-ai-workflow
   python process_mining/scripts/preprocess_ground_truth.py
   ```
2. **Review** the preprocessing report:

   ```bash
   cat process_mining/data/ground-truth/preprocessing_report.txt
   ```

#### Phase 2: Setup Known False Positives

3. **Extract relevant known FP packages** (if not already done):

   ```bash
   cd sast-ai-workflow
   python process_mining/scripts/create_known_fps_list.py
   ```
4. **Copy known FP packages**:

   ```bash
   python process_mining/scripts/copy_known_fps.py
   ```
5. **Validate known FP dataset**:

   ```bash
   python process_mining/scripts/preprocess_ground_truth.py --ground-truth-dir process_mining/data/ground-truth/known_non_issue
   ```
6. **Process known FPs with NVR enrichment** (REQUIRED before splitting):

   ```bash
   python process_mining/scripts/preprocess_known_non_issues.py
   ```

#### Phase 3: Split Combined Dataset

7. **Preview the split** with dry run (uses both Excel and processed known FP data):

   ```bash
   python process_mining/scripts/split_train_val_test.py --dry-run
   ```
8. **Review** the split report to check validation status:

   ```bash
   cat process_mining/data/ground-truth/split_report.txt
   ```
9. **Execute** the split if validation passed:

   ```bash
   python process_mining/scripts/split_train_val_test.py
   ```
10. **Verify** output directories contain both file types:

```bash
   ls -l process_mining/data/ground-truth/train/
   find process_mining/data/ground-truth/train/ -name "*.xlsx" | wc -l
   find process_mining/data/ground-truth/train/ -type d -name "*" | wc -l
```

### Important Notes

- **Combined dataset handling**: The split script automatically processes both Excel files from `full_dataset/` and NVR-enriched ignore.err files from `processed_known_non_issues/`
- **Ignore.err-only packages are excluded**: Known FP packages without corresponding Excel files (~32 packages) are excluded from all splits because there's no way to determine their NVR for source code mapping
- **Invalid files are preserved**: Files listed in `invalid_files.txt` remain in the `full_dataset/` directory but are automatically excluded from splits
- **Reproducibility**: Same random seed produces the same split (for comparison across experiments)
- **Auditability**: All exclusions, pairing statistics, and split decisions are documented in generated reports
- **Data leakage prevention**: Package-level splitting ensures no overlap between train/validation/test sets
- **Balanced representation**: The stratification algorithm ensures both Excel and known FP data are well-represented across all splits

---

## Pattern Data Preparation for LLM Training

After splitting the dataset, the next step is to prepare pattern data files that combine ground-truth annotations with source code for LLM-based pattern learning.

### Overview

The `batch_prepare_patterns.py` script processes each package in the train/validation/test splits and generates LLM-ready input files. Each output file contains:

- All ground-truth entries from Excel files (FP and TP annotations with expert justifications)
- All known false positive entries from ignore.err files
- Source code snippets for each finding (fetched from Red Hat Brew)
- Formatted in a structure suitable for LLM pattern learning

### Key Features

1. **Dual-Source Processing**: Automatically combines entries from both Excel and ignore.err files for paired packages
2. **Source Code Fetching**: Retrieves source code from Red Hat Brew using RPM source handler
3. **Manifest-Based Filtering**: Uses dataset manifests to ensure only valid packages are processed
4. **Validation**: Automatically skips invalid files (respects preprocessing validation results)
5. **Parallel Processing**: Supports multi-threaded processing for faster batch operations
6. **Resume Capability**: Can resume interrupted runs
7. **Progress Tracking**: Generates detailed processing summaries with statistics

### Running the Script

#### Basic Usage

Process the training set (most common):

```bash
cd sast-ai-workflow

# Process training set (default)
python process_mining/scripts/batch_prepare_patterns.py --dataset train

# Process validation set
python process_mining/scripts/batch_prepare_patterns.py --dataset validation

# Process test set
python process_mining/scripts/batch_prepare_patterns.py --dataset test
```

#### Advanced Options

```bash
# Parallel processing with 4 workers (faster)
python process_mining/scripts/batch_prepare_patterns.py --dataset train --workers 4

# Resume interrupted run
python process_mining/scripts/batch_prepare_patterns.py --dataset train --resume

# Test with limited packages
python process_mining/scripts/batch_prepare_patterns.py --dataset train --limit 5 --dry-run

# Skip source code fetching
python process_mining/scripts/batch_prepare_patterns.py --dataset train --skip-source-code

# Clear RPM cache before processing
python process_mining/scripts/batch_prepare_patterns.py --dataset train --clear-rpm-cache

# Custom output directory
python process_mining/scripts/batch_prepare_patterns.py --dataset train --output-dir /custom/path
```

### What the Script Does

1. **Loads Manifest**: Reads the dataset-specific manifest (`train_manifest.txt`, `validation_manifest.txt`, or `test_manifest.txt`) to determine which packages to process and their pairing status

2. **Discovers Package Data**: Scans the dataset directory for:
   - Excel files in `dataset/excel/`
   - NVR-enriched ignore.err files in `dataset/known_non_issue/`

3. **Validates Files**: Checks each package against validation criteria:
   - Excel files must have justification columns with data
   - Excel files must have FP annotations
   - ignore.err files are validated for structure (optional)

4. **Processes Each Package**:
   - Reads Excel entries (issue type, CWE, error trace, FP/TP classification, expert justification)
   - Reads ignore.err entries (issue type, CWE, error trace, expert justification)
   - Fetches source code from Red Hat Brew using the package NVR
   - Maps source code to error trace locations
   - Combines all entries into a single formatted file

5. **Generates Output Files**: Creates one `.txt` file per package in the pattern data directory

6. **Generates Summary**: Creates `_processing_summary.json` with detailed statistics

### Output Structure

```
process_mining/data/
├── pattern_data/
│   ├── train_pattern_data/               # Training set pattern data
│   │   ├── bash-5.2.26-3.el10.txt
│   │   ├── acl-2.3.2-1.el10.txt
│   │   ├── systemd-256.7-1.el10.txt
│   │   ├── ...
│   │   ├── _processing_summary.json       # Summary statistics
│   │   └── _errors.log                    # Error log (if any errors)
│   ├── validation_pattern_data/           # Validation set pattern data
│   │   └── ...
│   └── test_pattern_data/                 # Test set pattern data
│       └── ...
```

### Pattern Data File Format

Each output file follows this format:

```
================================================================================
GROUND-TRUTH ENTRIES FOR: bash-5.2.26-3.el10
================================================================================

Package: bash-5.2.26-3.el10
Total Entries: 45
False Positives: 38
True Positives: 7

---
Entry #1:
Issue Type: RESOURCE_LEAK
CWE: CWE-772

Error Trace:
bash-5.2/execute_cmd.c:1234: alloc_fn: Storage is returned from allocation function "xmalloc"
bash-5.2/execute_cmd.c:1234: var_assign: Assigning: "temp" = storage returned from "xmalloc(size)"
bash-5.2/execute_cmd.c:1250: leaked_storage: Variable "temp" going out of scope leaks the storage it points to

Source Code (bash-5.2/execute_cmd.c):
```c
1230: /* Execute a simple command that is not a pipeline. */
1231: static int
1232: execute_simple_command (simple_com, pipe_in, pipe_out, async, fds_to_close)
1233: {
1234:   temp = (char *)xmalloc (size);
1235:   strcpy (temp, name);
1236:
1237:   if (error_check) {
1238:     verify_allocation (temp);
1239:   }
1240:
1245:   process_command (temp);
1246:
1250:   return result;
1251: }
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: The memory is freed in the cleanup handler registered at function entry. The static analyzer doesn't track the cleanup registration pattern used throughout the bash codebase.
Analyst Comment: "other justification". 


---
Entry #2:
...
```

### Processing Summary

The `_processing_summary.json` file contains:

```json
{
  "processing_timestamp": "2026-01-05T20:30:15",
  "total_packages": 146,
  "successful": 143,
  "partial_success": 2,
  "failed": 1,
  "skipped_invalid": 0,

  "pairing_statistics": {
    "paired_packages": 94,
    "excel_only_packages": 34,
    "ignore_err_only_packages": 18
  },

  "statistics": {
    "total_entries": 4912,
    "excel_entries": 2532,
    "ignore_err_entries": 2380,
    "total_false_positives": 4115,
    "total_true_positives": 797,
    "entries_with_source_code": 4823,
    "entries_without_source_code": 89
  },

  "processed_files": ["bash-5.2.26-3.el10", "acl-2.3.2-1.el10", ...],

  "partial_success_files": [
    {
      "file": "systemd-256.7-1.el10",
      "entry_count": 45,
      "errors": ["Source code fetch error for 2 entries"]
    }
  ],

  "failed_files": [
    {
      "file": "problematic-package",
      "errors": ["All entries failed to fetch source code"]
    }
  ],

  "processing_time_seconds": 1847.32,
  "avg_time_per_package": 12.65
}
```

### Source Code Fetching

The script uses the **RPM source handler** by default, which:

1. Downloads source RPMs from Red Hat Brew based on package NVR
2. Extracts source files to a temporary cache directory
3. Maps error trace file paths to actual source files
4. Extracts relevant code snippets with context lines

**Cache Management**:
- RPM cache location: `/tmp/rpm_cache` (configurable)
- Cache persists across runs for faster re-processing
- Use `--clear-rpm-cache` to start fresh

**Alternative: Git mode**:
```bash
python process_mining/scripts/batch_prepare_patterns.py --dataset train --source-mode git
```

### Validation and Error Handling

**Automatic Validation**:
- Excel files are validated for required columns and data
- ignore.err files are validated for structure (optional)
- Packages failing validation are automatically skipped

**Error Recovery**:
- If source code fetch fails for some entries, the package is marked as `partial_success`
- If source code fetch fails for ALL entries, the package is marked as `failed` and no output file is generated
- All errors are logged to `_errors.log`

**Resume Functionality**:
```bash
# If processing is interrupted, resume from where it left off
python process_mining/scripts/batch_prepare_patterns.py --dataset train --resume
```

### CLI Options Reference

| Flag | Description | Default | Example |
|------|-------------|---------|---------|
| `--dataset` | Which dataset to process | `train` | `--dataset validation` |
| `--output-dir DIR` | Output directory | From config | `--output-dir /custom/path` |
| `--workers N` | Number of parallel workers | `1` | `--workers 4` |
| `--limit N` | Limit number of packages (testing) | None | `--limit 10` |
| `--resume` | Resume interrupted run | - | `--resume` |
| `--force` | Force re-process all files | - | `--force` |
| `--dry-run` | Preview without processing | - | `--dry-run` |
| `--skip-source-code` | Skip source code fetching | - | `--skip-source-code` |
| `--source-mode MODE` | Source fetching mode (`rpm` or `git`) | `rpm` | `--source-mode git` |
| `--rpm-cache-dir DIR` | RPM cache directory | `/tmp/rpm_cache` | `--rpm-cache-dir /data/cache` |
| `--clear-rpm-cache` | Clear RPM cache before processing | - | `--clear-rpm-cache` |
| `--ignore-err-mode MODE` | How to process ignore.err (`both` or `paired_only`) | `both` | `--ignore-err-mode paired_only` |
| `--config FILE` | Path to config YAML | Default config | `--config custom.yaml` |

### Recommended Workflow

#### Phase 4: Prepare Pattern Data for LLM Training

After completing the train/validation/test split (Phase 3), prepare pattern data files:

11. **Process training set** (with parallel processing for speed):

    ```bash
    cd sast-ai-workflow
    python process_mining/scripts/batch_prepare_patterns.py --dataset train --workers 4
    ```

12. **Review processing summary**:

    ```bash
    cat process_mining/data/pattern_data/train_pattern_data/_processing_summary.json
    ```

13. **Verify output files**:

    ```bash
    # Check number of pattern data files
    ls process_mining/data/pattern_data/train_pattern_data/*.txt | wc -l

    # View a sample file
    head -100 process_mining/data/pattern_data/train_pattern_data/<package-NVR>
    ```

### Troubleshooting

**Issue: "Manifest file not found"**
- **Cause**: Dataset split hasn't been run yet
- **Solution**: Run `split_train_val_test.py` first to create train/validation/test directories and manifests

**Issue: "All entries failed to fetch source code"**
- **Cause**: VPN not connected or Brew is unavailable
- **Solution**: Connect to Red Hat VPN and verify Brew connectivity

**Issue: "Package has neither Excel nor ignore.err"**
- **Cause**: Manifest references a package that doesn't exist in the split
- **Solution**: This is a warning and can be safely ignored (packages are skipped)

**Issue: Processing is very slow**
- **Cause**: Sequential processing or slow network
- **Solution**: Use `--workers 4` or more for parallel processing

### Important Notes

- **VPN Required**: Source code fetching requires connection to Red Hat VPN
- **Network-Intensive**: Downloads source RPMs from Brew (can be several GB total)
- **Cache Recommended**: RPM cache persists across runs to avoid re-downloading
- **Manifest-Driven**: Only processes packages listed in the dataset manifest (ensures consistency with split)
- **No Excel File Modification**: Original Excel and ignore.err files are never modified
- **Idempotent**: Safe to re-run (use `--force` to regenerate existing files)
