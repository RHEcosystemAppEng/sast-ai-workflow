"""Shared fixtures for pattern extraction tests."""

import pytest


@pytest.fixture
def sample_ground_truth_content():
    """Full ground truth file content with 2 entries (1 FP, 1 TP)."""
    return """================================================================================
GROUND-TRUTH ENTRIES FOR: test-pkg-1.0-1.el10
================================================================================

Package: test-pkg-1.0-1.el10
Total Entries: 2
False Positives: 1
True Positives: 1

---
Entry #1:
Issue Type: INTEGER_OVERFLOW
CWE: CWE-190

Error Trace:
test-pkg-1.0/src/main.c:49: tainted_data_argument: The value "offset" is considered tainted.
test-pkg-1.0/src/main.c:64: underflow: The cast to a signed type could result in a negative number.
#   62|        }
#   63|
#   64|->    if (lseek(fd, offset, SEEK_SET) == -1)
#   65|          return -1;
#   66|        {

Source Code (test-pkg-1.0/src/main.c):
```c
31| int
32| process_file(const char *filename)
33| {
34|    int fd;
35|    off_t offset;
36|
37|    fd = open(filename, O_RDWR);
38|    if (fd == -1) return 1;
39|
40|    offset = get_offset();
41|    if (lseek(fd, offset, SEEK_SET) == -1)
42|        return -1;
43|
44|    close(fd);
45|    return 0;
46| }
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: For regular files, the offset must be non-negative. lseek returns -1 on error which is checked.
Analyst Comment: The return value of lseek is properly checked against -1.

---


---
Entry #2:
Issue Type: UNINIT
CWE: CWE-457

Error Trace:
test-pkg-1.0/src/util.c:100: var_decl: Declaring variable "buf" without initializer.
test-pkg-1.0/src/util.c:105: uninit_use: Using uninitialized value "buf".
#  103|
#  104|
#  105|->     memcpy(dest, buf, sizeof(buf));
#  106|        return 0;
#  107|   }

Source Code (test-pkg-1.0/src/util.c):
```c
98| void copy_data(char *dest) {
99|     char buf[256];
100|     // buf is never initialized
101|     memcpy(dest, buf, sizeof(buf));
102| }
```

Ground Truth Classification: TRUE POSITIVE
Human Expert Justification: Variable 'buf' is declared without an initializer and used directly in memcpy.
Analyst Comment: RHEL SAST Automation: genuine uninitialized use.
---

"""


@pytest.fixture
def sample_ground_truth_fp_only_content():
    """Ground truth file content with only false positive entries."""
    return """================================================================================
GROUND-TRUTH ENTRIES FOR: fp-pkg-1.0
================================================================================

Package: fp-pkg-1.0
Total Entries: 1
False Positives: 1
True Positives: 0

---
Entry #1:
Issue Type: RESOURCE_LEAK
CWE: CWE-772

Error Trace:
fp-pkg-1.0/src/io.c:50: alloc_fn: Storage is returned from allocation function "malloc".
fp-pkg-1.0/src/io.c:80: leaked_storage: Variable "ptr" going out of scope leaks the storage.
#   78|
#   79|
#   80|->   return result;
#   81|   }

Source Code (fp-pkg-1.0/src/io.c):
```c
45| int process() {
46|     char *ptr = malloc(256);
47|     if (!ptr) return -1;
48|     int result = do_work(ptr);
49|     free(ptr);
50|     return result;
51| }
```

Ground Truth Classification: FALSE POSITIVE
Human Expert Justification: ptr is freed on line 49 before returning. The SAST tool missed the free() call.

---

"""


@pytest.fixture
def sample_ignore_err_content():
    """Raw ignore.err file content with 2 entries."""
    return """Error: INTEGER_OVERFLOW (CWE-190):
test-pkg-1.0/src/main.c:49: tainted_data_argument: The value "offset" is considered tainted.
test-pkg-1.0/src/main.c:64: underflow: The cast to a signed type could result in a negative number.
#   62|        }
#   63|
#   64|->    if (lseek(fd, offset, SEEK_SET) == -1)
#   65|          return -1;
#   66|        {
For regular files, the offset must be non-negative. lseek returns -1 on error.

Error: UNINIT (CWE-457):
test-pkg-1.0/src/util.c:100: var_decl: Declaring variable "buf" without initializer.
test-pkg-1.0/src/util.c:105: uninit_use: Using uninitialized value "buf".
#  103|
#  104|
#  105|->     memcpy(dest, buf, sizeof(buf));
#  106|        return 0;
#  107|   }
The buffer is initialized by prior call to read() which is not visible in the trace."""


@pytest.fixture
def sample_ignore_err_no_cwe_content():
    """ignore.err entry without CWE."""
    return """Error: DEADCODE:
test-pkg-1.0/src/main.c:30: dead_code: Code is unreachable.
#   30|->    return 0;
This code is intentionally unreachable as a safety guard."""
