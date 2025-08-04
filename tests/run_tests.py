#!/usr/bin/env python3
"""
Test runner for SAST-AI workflow report reader functionality.

This script automatically discovers and runs all unit tests in the tests/ directory
that follow the test_*.py naming convention.

Usage:
    python run_tests.py
    python run_tests.py --verbose
    python run_tests.py --module test_sarif_reader
    python run_tests.py --pattern "test_*integration*"
"""

import argparse
import os
import sys
import unittest

# Add project root to path for imports
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "src"))
sys.path.insert(0, PROJECT_ROOT)

# Get the tests directory
TESTS_DIR = os.path.dirname(__file__)


class ColoredTextTestResult(unittest.TextTestResult):
    """Enhanced test result with colored output"""

    def __init__(self, stream, descriptions, verbosity):
        super().__init__(stream, descriptions, verbosity)
        self.verbosity = verbosity  # Explicitly store verbosity
        self.successes = []

    def addSuccess(self, test):
        super().addSuccess(test)
        self.successes.append(test)
        if self.verbosity > 1:
            self.stream.writeln(f"✅ {self.getDescription(test)}")

    def addError(self, test, err):
        super().addError(test, err)
        if self.verbosity > 1:
            self.stream.writeln(f"❌ {self.getDescription(test)} - ERROR")

    def addFailure(self, test, err):
        super().addFailure(test, err)
        if self.verbosity > 1:
            self.stream.writeln(f"❌ {self.getDescription(test)} - FAILED")

    def addSkip(self, test, reason):
        super().addSkip(test, reason)
        if self.verbosity > 1:
            self.stream.writeln(f"⚠️ {self.getDescription(test)} - SKIPPED: {reason}")


class TestRunner:
    """Custom test runner with enhanced reporting"""

    def __init__(self, verbosity=1):
        self.verbosity = verbosity

    def run_tests(self, test_module=None, test_pattern=None):
        """Run all tests, specific module tests, or pattern-based tests"""

        print("=" * 80)
        print("SAST-AI WORKFLOW REPORT READER TESTS")
        print("=" * 80)

        if test_module:
            suite = self._load_specific_module(test_module)
            print(f"Running tests from module: {test_module}")
        elif test_pattern:
            suite = self._load_tests_by_pattern(test_pattern)
            print(f"Running tests matching pattern: {test_pattern}")
        else:
            suite = self._load_all_tests()
            print("Running all report reader tests...")

        print()

        # Create custom test runner
        stream = sys.stdout
        runner = unittest.TextTestRunner(
            stream=stream, verbosity=self.verbosity, resultclass=ColoredTextTestResult
        )

        # Check if we have any tests to run
        test_count = suite.countTestCases()
        if test_count == 0:
            if test_module:
                raise ValueError(
                    f"No tests found in module '{test_module}'."
                    "Make sure the module exists and contains test classes."
                )
            elif test_pattern:
                raise ValueError(
                    f"No tests found matching pattern '{test_pattern}'."
                    "Try a different pattern or check available modules with --list-modules."
                )
            else:
                raise ValueError(
                    "No tests found in the tests directory. "
                    "Make sure test files exist and follow the test_*.py naming convention."
                )

        print(f"Found {test_count} test(s) to run")
        print()

        # Run tests
        result = runner.run(suite)

        # Print summary
        self._print_summary(result)

        return result.wasSuccessful()

    def _load_all_tests(self):
        """Load all test cases using automatic discovery"""
        loader = unittest.TestLoader()
        # Use unittest's built-in test discovery
        suite = loader.discover(start_dir=TESTS_DIR, pattern="test_*.py")
        return suite

    def _load_specific_module(self, module_name):
        """Load tests from a specific module using discovery"""
        loader = unittest.TestLoader()

        # Check if the module file exists
        module_file = os.path.join(TESTS_DIR, f"{module_name}.py")
        if not os.path.exists(module_file):
            available_modules = [
                f[:-3] for f in os.listdir(TESTS_DIR) if f.startswith("test_") and f.endswith(".py")
            ]
            raise ValueError(
                f"Unknown test module: {module_name}. Available modules: {available_modules}"
            )

        # Use discovery but limit to the specific module file
        suite = loader.discover(start_dir=TESTS_DIR, pattern=f"{module_name}.py")
        return suite

    def _load_tests_by_pattern(self, pattern):
        """Load tests matching a specific pattern"""
        loader = unittest.TestLoader()
        # Use discovery with the specified pattern
        suite = loader.discover(start_dir=TESTS_DIR, pattern=pattern)
        return suite

    def _print_summary(self, result):
        """Print detailed test summary"""
        print("\n" + "=" * 80)
        print("TEST SUMMARY")
        print("=" * 80)

        total_tests = result.testsRun
        successful = (
            len(result.successes)
            if hasattr(result, "successes")
            else (total_tests - len(result.failures) - len(result.errors))
        )
        failures = len(result.failures)
        errors = len(result.errors)
        skipped = len(result.skipped)

        print(f"Total Tests:     {total_tests}")
        print(f"✅ Successful:   {successful}")
        print(f"❌ Failed:       {failures}")
        print(f"💥 Errors:       {errors}")
        print(f"⚠️ Skipped:      {skipped}")

        if total_tests > 0:
            success_rate = (successful / total_tests) * 100
            print(f"Success Rate:    {success_rate:.1f}%")

        print()

        # Show discovered test files
        print("\nDiscovered Test Files:")
        test_files = [
            f for f in os.listdir(TESTS_DIR) if f.startswith("test_") and f.endswith(".py")
        ]
        for test_file in sorted(test_files):
            print(f"  📝 {test_file}")

        print()

        if result.failures:
            print("FAILURES:")
            for test, traceback in result.failures:
                print(f"❌ {test}: {traceback.split('AssertionError:')[-1].strip()}")
            print()

        if result.errors:
            print("ERRORS:")
            for test, traceback in result.errors:
                error_msg = (
                    traceback.split("\n")[-2]
                    if traceback.split("\n")[-2]
                    else traceback.split("\n")[-3]
                )
                print(f"💥 {test}: {error_msg}")
            print()

        if result.wasSuccessful():
            print("🎉 ALL TESTS PASSED! 🎉")
        else:
            print("❌ SOME TESTS FAILED")

        print("=" * 80)


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="Run SAST-AI report reader tests")
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")

    # Create mutually exclusive group for module and pattern
    test_group = parser.add_mutually_exclusive_group()
    test_group.add_argument("--module", "-m", help="Run tests from specific module only")
    test_group.add_argument(
        "--pattern", help='Run tests matching a specific pattern (e.g., "test_*integration*")'
    )

    parser.add_argument(
        "--list-modules", "-l", action="store_true", help="List available test modules"
    )

    args = parser.parse_args()

    if args.list_modules:
        print("Available test modules:")
        # Discover all test_*.py files in the tests directory
        test_files = [
            f for f in os.listdir(TESTS_DIR) if f.startswith("test_") and f.endswith(".py")
        ]
        for test_file in test_files:
            module_name = test_file[:-3]  # Remove .py
            print(f"  - {module_name}")
        print(f"\nTotal: {len(test_files)} test modules found")
        return 0

    verbosity = 2 if args.verbose else 1
    runner = TestRunner(verbosity=verbosity)

    try:
        success = runner.run_tests(args.module, args.pattern)
        return 0 if success else 1

    except ValueError as e:
        print(f"❌ {e}")
        return 1
    except Exception as e:
        print(f"Error running tests: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
