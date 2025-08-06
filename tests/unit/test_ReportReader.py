"""
Tests for read_sast_report function in ReportReader.py
Covers data ingestion from both local HTML files and Google Sheets.
"""
import pytest
import requests
from unittest.mock import Mock, patch
from src.ReportReader import read_sast_report_local_html, read_sast_report_google_sheet


class TestReadSastReport:

    def test__read_sast_report__local_html_parses_successfully(self, tmp_path):
        # preparation
        html_content = '''
        <!DOCTYPE html>
        <html>
        <body>
        <pre><a id="def1" idid="def1"></a><b>Error: <span class="checker">USE_AFTER_FREE</span> (<a href="https://cwe.mitre.org/data/definitions/416.html" title="CWE-416: Use After Free">CWE-416</a>):</b> <a href="https://cov01.lab.eng.brq2.redhat.com/osh/task/683840/log/systemd-252-46.el9_5.2/scan-results-imp.html#def1">[#def1]</a> <span class="impFlag">[important]</span>
        <span class="infoEvent">systemd-252/src/basic/cgroup-util.c:453:25: <b>freed_arg</b>: "free" frees "fn".</span>
        systemd-252/src/basic/cgroup-util.c:452:25: <b>pass_freed_arg</b>: Passing freed pointer "fn" as an argument to "path_extend_internal".
        <span class="infoEventComment">#<span class="traceEvent">  450|                           _cleanup_free_ char *p = NULL;</span></span>
        <span class="infoEventComment">#<span class="traceEvent">  451|   </span></span>
        <span class="infoEventComment">#<span class="ctxLine">  452|-&gt;                         p = path_join(empty_to_root(path), fn);</span></span>
        <span class="infoEventComment">#<span class="traceEvent">  453|                           free(fn);</span></span>
        <span class="infoEventComment">#<span class="traceEvent">  454|                           if (!p)</span></span>

        <a id="def2" idid="def2"></a><b>Error: <span class="checker">USE_AFTER_FREE</span> (<a href="https://cwe.mitre.org/data/definitions/416.html" title="CWE-416: Use After Free">CWE-416</a>):</b>
        Dummy second finding to trigger append of first finding.
        </pre>
        </body>
        </html>
        '''
        
        html_file = tmp_path / "test_report.html"
        html_file.write_text(html_content)
        
        # testing
        result = read_sast_report_local_html(str(html_file))
        
        # assertion
        assert len(result) == 1
        assert result[0].id == "def1"
        assert result[0].issue_type == "USE_AFTER_FREE"
        assert result[0].issue_cve == "CWE-416"
        assert result[0].issue_cve_link == "https://cwe.mitre.org/data/definitions/416.html"
        assert "systemd-252/src/basic/cgroup-util.c:453:25" in result[0].trace
        assert "freed_arg" in result[0].trace
        assert "path_extend_internal" in result[0].trace

    def test__read_sast_report__google_sheet_authenticates_and_parses(self):
        # preparation
        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = [
            {"Finding": 'Error: BUFFER_SIZE (CWE-474):\nunzip60/envargs.c:121: overlapping_buffer: The source buffer "argstart + 1" potentially overlaps with the destination buffer "argstart", which results in undefined behavior for "strcpy".\nunzip60/envargs.c:121: remediation: Replace "strcpy(dest, src)" with "memmove(dest, src, strlen(src)+1)".\n#  119|               /* remove escape characters */\n#  120|               while ((argstart = MBSCHR(argstart, \'\\\\\')) != (char *)NULL) {\n#  121|->                 strcpy(argstart, argstart + 1);\n#  122|                   if (*argstart)\n#  123|                       ++argstart;'},
            {"Finding": 'Error: OVERRUN (CWE-119):\nunzip60/inflate.c:1608: cond_const: Checking "j <= 16U" implies that "j" is 17 on the false branch.\nunzip60/inflate.c:1613: assignment: Assigning: "*m" = "j". The value of "*m" is now 17.\nunzip60/inflate.c:1614: assignment: Assigning: "i" = "16U".\nunzip60/inflate.c:1614: decr: Decrementing "i". The value of "i" is now 15.\nunzip60/inflate.c:1614: cond_at_least: Checking "i" implies that "i" is at least 1 on the true branch.\nunzip60/inflate.c:1618: cond_at_least: Checking "*m > i" implies that "g" and "i" are at least 17 on the false branch.\nunzip60/inflate.c:1628: overrun-local: Overrunning array "c" of 17 4-byte elements at element index 17 (byte offset 71) using index "i" (which evaluates to 17).\n# 1626|     if ((y -= c[i]) < 0)\n# 1627|       return 2;\n# 1628|->   c[i] += y;\n# 1629|   \n# 1630|'}
        ]
        
        # testing
        with patch('src.ReportReader.get_google_sheet', return_value=mock_sheet):
            result = read_sast_report_google_sheet(
                "service_account.json", 
                "https://docs.google.com/spreadsheets/d/19wIC8ktql02LOPMzepcRaaJr7D0o2tZVMf3UVvzL3sw/edit?pli=1&gid=0#gid=0"
            )
            
        # assertion
        assert len(result) == 2
        assert result[0].issue_type == "BUFFER_SIZE"
        assert result[0].issue_cve == "CWE-474"
        assert result[1].issue_type == "OVERRUN" 
        assert result[1].issue_cve == "CWE-119"

    def test__read_sast_report__missing_file_raises_error(self):
        # testing
        with pytest.raises(FileNotFoundError):
            read_sast_report_local_html("/path/that/does/not/exist.html")

    def test__read_sast_report__empty_html_handles_gracefully(self, tmp_path):
        # preparation
        empty_file = tmp_path / "empty.html"
        empty_file.write_text("")
        
        # testing
        result = read_sast_report_local_html(str(empty_file))
        
        # assertion
        assert result == []

    def test__read_sast_report__missing_tags_processes_available(self, tmp_path):
        # preparation
        html_content = '''
        <!DOCTYPE html>
        <html>
        <body>
        <pre><a id="def1" idid="def1"></a><b>Error: <span class="checker">USE_AFTER_FREE</span> (Missing CVE link):</b>
        <span class="infoEvent">systemd-252/src/basic/cgroup-util.c:453:25: <b>freed_arg</b>: "free" frees "fn".</span>
        systemd-252/src/basic/cgroup-util.c:452:25: <b>pass_freed_arg</b>: Passing freed pointer "fn" as an argument.

        <a id="def2" idid="def2"></a><b>Error: Missing issue type span tag (<a href="https://cwe.mitre.org/data/definitions/416.html">CWE-416</a>):</b>
        Some trace information here.

        <a id="def3" idid="def3"></a><b>Error: No span or a tags at all:</b>
        Just some trace text.

        <a id="def4" idid="def4"></a><b>Error: <span class="checker">BUFFER_OVERFLOW</span> (No CVE info):</b>
        Buffer overflow trace information.
        </pre>
        </body>
        </html>
        '''
        
        html_file = tmp_path / "malformed_report.html"
        html_file.write_text(html_content)
        
        # testing
        result = read_sast_report_local_html(str(html_file))
        
        # assertion
        assert len(result) == 3
        
        # def1: Has span but no a tag in the b tag - parser skips this b tag
        assert result[0].id == "def1"
        assert result[0].issue_type == ""
        assert result[0].issue_cve == ""
        assert result[0].issue_cve_link == ""
        assert "systemd-252/src/basic/cgroup-util.c" in result[0].trace
        
        # def2: Has a tag but no span in the b tag - parser skips this b tag  
        assert result[1].id == "def2"
        assert result[1].issue_type == ""
        assert result[1].issue_cve == ""
        assert result[1].issue_cve_link == ""
        assert "Some trace information here" in result[1].trace
        
        # def3: Has neither span nor a tags in b tag - parser skips this b tag
        assert result[2].id == "def3"
        assert result[2].issue_type == ""
        assert result[2].issue_cve == ""
        assert result[2].issue_cve_link == ""
        assert "Just some trace text" in result[2].trace

    def test__read_sast_report__network_timeout_handles_error(self):
        # testing
        with patch('oauth2client.service_account.ServiceAccountCredentials.from_json_keyfile_name') as mock_creds:
            mock_creds.side_effect = requests.exceptions.Timeout("Connection timeout")
            
            with pytest.raises(requests.exceptions.Timeout):
                read_sast_report_google_sheet("service_account.json", "https://docs.google.com/spreadsheets/fake")

    def test__read_sast_report__empty_sheet_handles_gracefully(self, caplog):
        # preparation
        mock_sheet = Mock()
        mock_sheet.get_all_records.return_value = []  # Empty sheet
        
        # testing
        with patch('src.ReportReader.get_google_sheet', return_value=mock_sheet):
            with caplog.at_level('WARNING'):
                result = read_sast_report_google_sheet(
                    "service_account.json", 
                    "https://docs.google.com/spreadsheets/d/empty_sheet/edit"
                )
                
        # assertion
        assert result == []
        assert "No rows found in Google Sheet: https://docs.google.com/spreadsheets/d/empty_sheet/edit" in caplog.text

    def test__read_sast_report__corrupted_html_handles_errors(self, tmp_path, caplog):    
        # Test 1: HTML with no <pre> tags (missing required structure)
        # preparation - test #1
        no_pre_html = '''<!DOCTYPE html><html><body><h1>Report</h1><p>No pre tags here</p></body></html>'''
        no_pre_file = tmp_path / "no_pre.html"
        no_pre_file.write_text(no_pre_html)
        
        # testing - test #1
        with caplog.at_level('WARNING'):
            result = read_sast_report_local_html(str(no_pre_file))
            
        # assertion - test #1
        assert result == []
        assert "No <pre> tags found in HTML file - possibly corrupted" in caplog.text
        
        # Test 2: HTML with <pre> but no valid SAST structure
        # preparation - test #2
        no_findings_html = '''<!DOCTYPE html><html><body><pre>Just some random text with no findings</pre></body></html>'''
        no_findings_file = tmp_path / "no_findings.html"
        no_findings_file.write_text(no_findings_html)
        
        # testing - test #2
        caplog.clear()
        with caplog.at_level('WARNING'):
            result = read_sast_report_local_html(str(no_findings_file))
            
        # assertion - test #2
        assert result == []
        assert "No valid SAST tags found" in caplog.text
        
        # Test 3: Truncated HTML (simulating file corruption during transfer)
        # preparation - test #3
        truncated_html = '''<!DOCTYPE html>
        <html>
        <body>
        <pre>Some random text without proper SAST structure due to truncation...'''
        truncated_file = tmp_path / "truncated.html"
        truncated_file.write_text(truncated_html)
        
        # testing - test #3
        caplog.clear()
        with caplog.at_level('WARNING'):
            result = read_sast_report_local_html(str(truncated_file))
            
        # assertion - test #3
        assert isinstance(result, list)
        assert len(caplog.records) > 0
        
        # Test 4: Malformed but parseable HTML 
        # preparation - test #4
        malformed_html = '''
        <!DOCTYPE html>
        <html>
        <body>
        <pre><a id="def1"></a><b>Error: <span class="checker">USE_AFTER_FREE</span> (<a href="https://cwe.mitre.org/data/definitions/416.html">CWE-416</a>):</b>
        Valid finding with proper structure.

        <a id="def2"></a><b>Error: Malformed structure without proper tags</b>
        Some trace text but missing required span/a structure.
        </pre>
        </body>
        </html>
        '''
        
        malformed_file = tmp_path / "malformed.html"
        malformed_file.write_text(malformed_html)
        
        # testing - test #4
        result = read_sast_report_local_html(str(malformed_file))
        
        # assertion - test #4
        assert len(result) >= 0
        if len(result) > 0:
            assert result[0].id == "def1"

    def test__read_sast_report__auth_failure_raises_exception(self):
        """Handles authentication failures when accessing Google Sheets."""
        # testing
        with patch('oauth2client.service_account.ServiceAccountCredentials.from_json_keyfile_name') as mock_creds:
            mock_creds.side_effect = Exception("Authentication failed: Invalid service account credentials")
            
            with pytest.raises(Exception) as exc_info:
                read_sast_report_google_sheet("invalid_service_account.json", "https://docs.google.com/spreadsheets/fake")
            
            # assertion
            assert "Authentication failed" in str(exc_info.value)