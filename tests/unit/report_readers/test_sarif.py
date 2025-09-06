import json
import os
import tempfile
import unittest
from unittest.mock import Mock

from fixtures import SAMPLE_SARIF

from src.common.config import Config
from src.report_readers.sarif_reader import SarifReportReader


class TestSarifReportReader(unittest.TestCase):
    """Test cases for SarifReportReader"""

    def setUp(self):
        """Set up test environment"""
        self.reader = SarifReportReader()
        self.config = Mock(spec=Config)

    def test__can_handle__valid_sarif_file_returns_true(self):
        """Test can_handle returns True for valid SARIF files"""
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertTrue(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__json_file_with_sarif_content_returns_true(self):
        """Test can_handle returns True for .json files with SARIF content"""
        with tempfile.NamedTemporaryFile(suffix=".json", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertTrue(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__unsupported_file_extension_returns_false(self):
        """Test can_handle returns False for unsupported file extensions"""
        with tempfile.NamedTemporaryFile(suffix=".txt", delete=False) as temp_file:
            temp_file.write(b"some text")
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertFalse(result)
            finally:
                os.unlink(temp_file.name)

    def test__can_handle__url_instead_of_file_returns_false(self):
        """Test can_handle returns False for URLs"""
        result = self.reader.can_handle("https://example.com/report.sarif", self.config)
        self.assertFalse(result)

    def test_given_nonexistent_file_when_checking_can_handle_then_returns_false(self):
        """Test can_handle returns False for non-existent files"""
        result = self.reader.can_handle("/nonexistent/file.sarif", self.config)
        self.assertFalse(result)

    def test_given_invalid_json_file_when_checking_can_handle_then_returns_false(self):
        """Test can_handle returns False for invalid JSON"""
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            temp_file.write("invalid json content")
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertFalse(result)
            finally:
                os.unlink(temp_file.name)

    def test_given_valid_json_but_not_sarif_when_checking_can_handle_then_returns_false(self):
        """Test can_handle returns False for valid JSON that's not SARIF"""
        non_sarif_data = {"some": "data", "but": "not sarif"}

        with tempfile.NamedTemporaryFile(suffix=".json", mode="w", delete=False) as temp_file:
            json.dump(non_sarif_data, temp_file)
            temp_file.flush()

            try:
                result = self.reader.can_handle(temp_file.name, self.config)
                self.assertFalse(result)
            finally:
                os.unlink(temp_file.name)

    def test_given_valid_sarif_file_when_reading_report_then_parses_issues_correctly(self):
        """Test basic SARIF report parsing"""
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)

                self.assertEqual(len(issues), 3)  # SAMPLE_SARIF has 3 results
                issue = issues[0]

                self.assertEqual(issue.id, "def1")
                self.assertTrue(
                    "RESOURCE_LEAK" in issue.issue_type or "Resource Leak" in issue.issue_type
                )
                self.assertEqual(issue.issue_cwe, "CWE-772")
                self.assertIn("cwe.mitre.org", issue.issue_cwe_link)
                self.assertIn("full_url", issue.trace)

            finally:
                os.unlink(temp_file.name)

    def test_given_sarif_with_multiple_results_when_reading_report_then_returns_all_issues(self):
        """Test parsing SARIF with multiple issues"""
        # Use SAMPLE_SARIF which already has multiple results
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(SAMPLE_SARIF, temp_file)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)

                self.assertEqual(len(issues), 3)  # SAMPLE_SARIF has 3 issues
                self.assertEqual(issues[0].id, "def1")
                self.assertEqual(issues[1].id, "def2")
                self.assertEqual(issues[2].id, "def3")

            finally:
                os.unlink(temp_file.name)

    def test_given_missing_file_when_reading_report_then_raises_file_not_found_error(self):
        """Test read_report raises FileNotFoundError for missing files"""
        with self.assertRaises(FileNotFoundError):
            self.reader.read_report("/nonexistent/file.sarif", self.config)

    def test_given_invalid_json_file_when_reading_report_then_raises_value_error(self):
        """Test read_report raises ValueError for invalid JSON"""
        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            temp_file.write("invalid json")
            temp_file.flush()

            try:
                with self.assertRaises(ValueError) as context:
                    self.reader.read_report(temp_file.name, self.config)

                self.assertIn("Invalid JSON", str(context.exception))
            finally:
                os.unlink(temp_file.name)

    def test_given_sarif_run_data_when_extracting_tool_info_then_returns_correct_details(self):
        """Test tool information extraction"""
        run_data = SAMPLE_SARIF["runs"][0]
        tool_info = self.reader._extract_tool_info(run_data)

        self.assertEqual(tool_info["name"], "TestTool")
        self.assertEqual(tool_info["version"], "1.0.0")

    def test_given_incomplete_tool_data_when_extracting_tool_info_then_returns_unknown_values(self):
        """Test tool info extraction with missing data"""
        run_data = {"tool": {"driver": {}}}
        tool_info = self.reader._extract_tool_info(run_data)

        self.assertEqual(tool_info["name"], "Unknown")
        self.assertEqual(tool_info["version"], "Unknown")

    def test_given_sarif_run_data_when_building_rules_map_then_creates_correct_mapping(self):
        """Test rules map building"""
        run_data = SAMPLE_SARIF["runs"][0]
        rules_map = self.reader._build_rules_map(run_data)

        self.assertIn("RESOURCE_LEAK: leaked_storage", rules_map)
        # Note: The first rule in the new fixture data is for resource leak

    def test_given_rule_with_cwe_property_when_extracting_cwe_info_then_returns_correct_cwe(self):
        """Test CWE extraction from rule properties"""
        result = {"ruleId": "TEST-001"}
        rule = {"properties": {"cwe": "CWE-89"}}

        cwe_info = self.reader._extract_cwe_info(result, rule)

        self.assertEqual(cwe_info["cwe"], "CWE-89")
        self.assertIn("89.html", cwe_info["cwe_link"])

    def test_given_result_message_with_cwe_when_extracting_cwe_info_then_returns_correct_cwe(self):
        """Test CWE extraction from result message"""
        result = {"message": {"text": "This is related to CWE-120 buffer overflow"}}
        rule = {}

        cwe_info = self.reader._extract_cwe_info(result, rule)

        self.assertEqual(cwe_info["cwe"], "CWE-120")
        self.assertIn("120.html", cwe_info["cwe_link"])

    def test_given_message_with_cwe_pattern_when_extracting_cwe_info_then_returns_correct_cwe(self):
        """Test CWE extraction for CWE identifiers"""
        result = {"message": {"text": "This relates to CWE-120 buffer overflow"}}
        rule = {}

        cwe_info = self.reader._extract_cwe_info(result, rule)

        self.assertEqual(cwe_info["cwe"], "CWE-120")
        self.assertIn("120.html", cwe_info["cwe_link"])

    def test_given_important_location_kinds_when_checking_skip_location_then_returns_false(self):
        """Test location filtering based on SARIF kinds"""
        # Important location should not be skipped
        important_location = {
            "kinds": ["memory", "acquire"],
            "location": {"message": {"text": "Buffer allocated here"}},
        }
        self.assertFalse(self.reader._should_skip_location(important_location))

        # Control flow location should be skipped
        control_flow_location = {
            "kinds": ["branch", "true"],
            "location": {"message": {"text": "Taking true branch"}},
        }
        self.assertTrue(self.reader._should_skip_location(control_flow_location))

        # Test precedence: control flow kinds override message content
        # Control flow kinds should be skipped even with vulnerability message
        mixed_control_location = {
            "kinds": ["branch", "true"],
            "location": {"message": {"text": "Memory leak detected here"}},  # Vulnerability message
        }
        self.assertTrue(self.reader._should_skip_location(mixed_control_location))

        # Test that important kinds don't override message-based skipping
        # Important kinds + control flow message = message decides (skip)
        mixed_location = {
            "kinds": ["memory", "acquire"],
            "location": {
                "message": {"text": "Condition x > 0, taking true branch"}  # Control flow message
            },
        }
        self.assertTrue(self.reader._should_skip_location(mixed_location))  # Message causes skip

        # Important kinds + vulnerability message = don't skip
        vuln_mixed_location = {
            "kinds": ["memory", "acquire"],
            "location": {"message": {"text": "Memory leak detected here"}},  # Vulnerability message
        }
        self.assertFalse(self.reader._should_skip_location(vuln_mixed_location))

    def test_given_vulnerability_message_when_checking_skip_location_then_returns_false(self):
        """Test location filtering based on message content"""
        # Vulnerability-related message should not be skipped
        vuln_location = {"location": {"message": {"text": "Memory leak detected here"}}}
        self.assertFalse(self.reader._should_skip_location(vuln_location))

        # Control flow message should be skipped
        control_location = {
            "location": {"message": {"text": "Condition x > 0, taking true branch"}}
        }
        self.assertTrue(self.reader._should_skip_location(control_location))

    def test_given_dirty_rule_id_when_cleaning_then_returns_clean_id(self):
        """Test rule ID cleaning functionality"""
        dirty_rule_id = "BUFFER_OVERFLOW: note[some_annotation]"
        clean_id = self.reader._clean_rule_id(dirty_rule_id)
        self.assertEqual(clean_id, "BUFFER_OVERFLOW")

    def test_given_cwe_identifier_when_generating_link_then_returns_cwe_url(self):
        """Test CWE link generation for CWE"""
        link = self.reader._generate_cwe_link("CWE-120")
        self.assertEqual(link, "https://cwe.mitre.org/data/definitions/120.html")

    def test_given_cve_identifier_when_generating_link_then_returns_empty_string(self):
        """Test CVE link generation for CVE"""
        link = self.reader._generate_cwe_link("CVE-2021-1234")
        self.assertEqual(link, "")

    def test_given_sarif_reader_when_getting_format_name_then_returns_sarif(self):
        """Test format name getter"""
        self.assertEqual(self.reader.get_format_name(), "SARIF")

    def test_given_unsupported_sarif_version_when_checking_can_handle_then_handles_gracefully(self):
        """Test handling of unsupported SARIF versions"""
        import copy

        unsupported_sarif = copy.deepcopy(SAMPLE_SARIF)
        unsupported_sarif["version"] = "1.0.0"  # Unsupported version

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(unsupported_sarif, temp_file)
            temp_file.flush()

            try:
                # Should still try to process despite version warning
                result = self.reader.can_handle(temp_file.name, self.config)
                # Depending on implementation, might still return True with warning
                self.assertIsInstance(result, bool)
            finally:
                os.unlink(temp_file.name)

    def test_given_sarif_with_empty_runs_when_reading_report_then_returns_empty_list(self):
        """Test handling of SARIF with empty runs array"""
        empty_runs_sarif = {"version": "2.1.0", "runs": []}

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(empty_runs_sarif, temp_file)
            temp_file.flush()

            try:
                issues = self.reader.read_report(temp_file.name, self.config)
                self.assertEqual(len(issues), 0)
            finally:
                os.unlink(temp_file.name)

    def test_given_malformed_sarif_results_when_reading_report_then_handles_gracefully(self):
        """Test handling of malformed results in SARIF"""
        import copy

        malformed_sarif = copy.deepcopy(SAMPLE_SARIF)
        # Add a malformed result missing required fields
        malformed_result = {"message": "incomplete result"}
        malformed_sarif["runs"][0]["results"].append(malformed_result)

        with tempfile.NamedTemporaryFile(suffix=".sarif", mode="w", delete=False) as temp_file:
            json.dump(malformed_sarif, temp_file)
            temp_file.flush()

            try:
                # Should handle gracefully and return valid results
                issues = self.reader.read_report(temp_file.name, self.config)
                # Should get at least one good issue
                self.assertGreaterEqual(len(issues), 1)
            finally:
                os.unlink(temp_file.name)


if __name__ == "__main__":
    unittest.main()
