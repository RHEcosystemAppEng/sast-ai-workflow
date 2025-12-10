import json
import logging
import os
import re
from typing import Any, Dict, List

from common.config import Config
from dto.Issue import Issue
from report_readers.base_reader import BaseReportReader

logger = logging.getLogger(__name__)


class SarifReportReader(BaseReportReader):
    """
    Reader for SARIF (Static Analysis Results Interchange Format) files.
    Supports SARIF v2.1.0 specification.
    """

    SUPPORTED_SARIF_VERSIONS = ["2.1.0"]
    SARIF_FILE_EXTENSIONS = [".sarif", ".json"]

    # Patterns for control flow messages to skip (verbose execution flow)
    CONTROL_FLOW_SKIP_PATTERNS = [
        r"Condition .*, taking (true|false) branch",
        r"Continuing loop",
        r"Falling through to end of if statement",
        r"Entering loop body",
        r"Exiting loop",
        r"Assuming.*taking.*branch",
        r"^Condition\s+.*,\s+taking\s+",
        r"Loop condition is",
        r"^Following.*branch",
        r"^Jumping back to the beginning of the loop",
        r"Starting defect path here",
    ]

    # Patterns for vulnerability-semantic messages to keep (important for security analysis)
    VULNERABILITY_KEEP_PATTERNS = [
        r"Storage is returned from allocation function",  # alloc_fn
        r"Assigning:",  # var_assign
        r"Resource .* is not freed",  # noescape
        r"leaks the storage",  # leaked_storage
        r"freed.*memory",  # memory operations
        r"null pointer",  # null pointer issues
        r"buffer overflow",  # buffer issues
        r"use after free",  # use after free
        r"double free",  # double free
        r"memory leak",  # memory leaks
        r"uninitialized",  # uninitialized variables
        r"tainted",  # taint analysis
        r"sanitiz",  # sanitization
        r"validation",  # input validation
    ]

    def __init__(self):
        """Initialize SARIF reader with default filtering configuration."""
        super().__init__()

    def can_handle(self, file_path: str, config: Config) -> bool:
        """
        Determine if this reader can handle the given file.

        Checks both file extension and content structure for SARIF format.
        """
        try:
            # Check if it's a URL (not supported for SARIF currently)
            if file_path.startswith("https://") or file_path.startswith("http://"):
                return False

            # Check file extension
            file_ext = os.path.splitext(file_path.lower())[1]
            if file_ext not in self.SARIF_FILE_EXTENSIONS:
                return False

            # Check if file exists
            if not os.path.exists(file_path):
                return False

            # Check file content structure
            return self._is_sarif_file(file_path)

        except Exception as e:
            logger.debug(f"Error checking if file is SARIF format: {e}")
            return False

    def read_report(self, file_path: str, config: Config) -> List[Issue]:
        """
        Read and parse SARIF file into Issue objects.
        """
        logger.info(f"Reading SARIF file: {file_path}")

        try:
            with open(file_path, "r", encoding="utf-8") as f:
                sarif_data = json.load(f)

            return self._parse_sarif_data(sarif_data)

        except json.JSONDecodeError as e:
            logger.error(f"Invalid JSON in SARIF file {file_path}: {e}")
            raise ValueError(f"Invalid JSON in SARIF file: {e}")
        except FileNotFoundError:
            logger.error(f"SARIF file not found: {file_path}")
            raise FileNotFoundError(f"SARIF file not found: {file_path}")
        except Exception as e:
            logger.error(f"Error reading SARIF file {file_path}: {e}")
            raise

    def _is_sarif_file(self, file_path: str) -> bool:
        """
        Check if the file contains valid SARIF structure.
        """
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                data = json.load(f)

            # Check for required SARIF properties
            if not isinstance(data, dict):
                return False

            # Check for SARIF version
            version = data.get("version")
            if not version or version not in self.SUPPORTED_SARIF_VERSIONS:
                logger.warning(
                    f"Unsupported SARIF version: {version}."
                    "Supported versions: {self.SUPPORTED_SARIF_VERSIONS}"
                )
                return version is not None  # Still try to process if version exists

            # Check for runs array
            runs = data.get("runs")
            if not isinstance(runs, list):
                return False

            return True

        except Exception:
            return False

    def _parse_sarif_data(self, sarif_data: Dict[str, Any]) -> List[Issue]:
        """
        Parse SARIF data structure into Issue objects.
        """
        issues = []

        runs = sarif_data.get("runs", [])

        # Sequential issue counter starting from 1
        issue_counter = 1

        for run_index, run in enumerate(runs):
            # Get tool information
            tool_info = self._extract_tool_info(run)

            # Get rules for rule lookup
            rules_map = self._build_rules_map(run)

            # Process results
            results = run.get("results", [])

            for result_index, result in enumerate(results):
                try:
                    issue = self._create_issue_from_result(
                        result, rules_map, tool_info, issue_counter
                    )
                    if issue:
                        issues.append(issue)
                        issue_counter += 1
                except Exception as e:
                    logger.warning(
                        f"Error processing SARIF result {result_index} in run {run_index}: {e}"
                    )
                    continue

        logger.info(f"Successfully parsed {len(issues)} issues from SARIF file")
        return issues

    def _extract_tool_info(self, run: Dict[str, Any]) -> Dict[str, str]:
        """
        Extract tool information from SARIF run.
        """
        tool_info = {"name": "Unknown", "version": "Unknown"}

        tool = run.get("tool", {})
        driver = tool.get("driver", {})

        if "name" in driver:
            tool_info["name"] = driver["name"]

        if "semanticVersion" in driver:
            tool_info["version"] = driver["semanticVersion"]
        elif "version" in driver:
            tool_info["version"] = driver["version"]

        return tool_info

    def _build_rules_map(self, run: Dict[str, Any]) -> Dict[str, Dict[str, Any]]:
        """
        Build a map of rule IDs to rule definitions.
        """
        rules_map = {}

        tool = run.get("tool", {})
        driver = tool.get("driver", {})
        rules = driver.get("rules", [])

        for rule in rules:
            rule_id = rule.get("id")
            if rule_id:
                rules_map[rule_id] = rule

        # Also check extensions for additional rules
        extensions = tool.get("extensions", [])
        for extension in extensions:
            ext_rules = extension.get("rules", [])
            for rule in ext_rules:
                rule_id = rule.get("id")
                if rule_id:
                    rules_map[rule_id] = rule

        return rules_map

    def _create_issue_from_result(
        self,
        result: Dict[str, Any],
        rules_map: Dict[str, Dict[str, Any]],
        tool_info: Dict[str, str],
        issue_counter: int,
    ) -> Issue:
        """
        Create an Issue object from a SARIF result.
        """
        # Generate unique issue ID
        issue_id = f"def{issue_counter}"

        issue = Issue(id=issue_id)

        # Get rule information
        rule_id = result.get("ruleId", "")
        rule = rules_map.get(rule_id, {})

        # Set issue type from rule or result
        issue.issue_type = self._extract_issue_type(result, rule, rule_id)

        # Set issue label (using rule name or short description)
        issue.issue_label = self._extract_issue_label(result, rule)

        # Extract CVE/CWE information
        cwe_info = self._extract_cwe_info(result, rule)
        issue.issue_cwe = cwe_info["cwe"]
        issue.issue_cwe_link = cwe_info["cwe_link"]

        # Build comprehensive trace information
        issue.trace = self._build_trace_info(result, rule, tool_info)

        return issue

    def _extract_issue_type(
        self, result: Dict[str, Any], rule: Dict[str, Any], rule_id: str
    ) -> str:
        """
        Extract issue type from result or rule.
        """
        # Try to get from rule first
        if rule:
            # Check rule name
            rule_name = rule.get("name", "")
            if rule_name:
                return self._clean_rule_id(rule_name)

            # Check short description
            short_desc = rule.get("shortDescription", {})
            if isinstance(short_desc, dict) and "text" in short_desc:
                return short_desc["text"][:100]  # Limit length

        # Fallback to rule ID
        if rule_id:
            return self._clean_rule_id(rule_id)

        # Last resort - check message
        message = result.get("message", {})
        if isinstance(message, dict) and "text" in message:
            return message["text"]

        return "SARIF Issue"

    def _clean_rule_id(self, rule_id: str) -> str:
        """
        Clean rule ID by removing 'note[...]' patterns and other unwanted suffixes.
        Also removes everything after the first colon to keep only the base issue type.
        """
        if not rule_id:
            return rule_id

        # Remove 'note[...]' pattern
        cleaned = re.sub(r":\s*note\[.*?\]", "", rule_id)

        # Remove everything after the first colon to keep only the base issue type
        # This ensures "RESOURCE_LEAK: leaked_storage" becomes "RESOURCE_LEAK"
        if ":" in cleaned:
            cleaned = cleaned.split(":")[0]

        # Remove any trailing whitespace
        cleaned = cleaned.strip()

        return cleaned

    def _extract_issue_label(self, result: Dict[str, Any], rule: Dict[str, Any]) -> str:
        """
        Extract issue label from rule information.
        """
        if rule:
            # Try rule name first
            rule_name = rule.get("name", "")
            if rule_name:
                return rule_name

            # Try short description
            short_desc = rule.get("shortDescription", {})
            if isinstance(short_desc, dict) and "text" in short_desc:
                return short_desc["text"]

        # Fallback to level or kind
        level = result.get("level", "")
        if level:
            return f"SARIF {level.title()}"

        return "SARIF Result"

    def _extract_cwe_info(self, result: Dict[str, Any], rule: Dict[str, Any]) -> Dict[str, str]:
        """
        Extract CWE information from result or rule.
        """
        cwe_info = {"cwe": "", "cwe_link": ""}

        # Check result properties for CWE
        if "properties" in result and isinstance(result["properties"], dict):
            for key, value in result["properties"].items():
                if isinstance(value, str):
                    # Check for direct CWE values
                    if key.lower() == "cwe" and value.startswith("CWE-"):
                        cwe_info["cwe"] = value.upper()
                        cwe_info["cwe_link"] = self._generate_cwe_link(value.upper())
                        return cwe_info
                    # Check for CWE patterns in any property value
                    vuln_match = re.search(r"(CWE-\d+)", value, re.IGNORECASE)
                    if vuln_match:
                        vuln_id = vuln_match.group(1).upper()
                        cwe_info["cwe"] = vuln_id
                        cwe_info["cwe_link"] = self._generate_cwe_link(vuln_id)
                        return cwe_info

        # Check rule properties for CWE information
        if "properties" in rule and isinstance(rule["properties"], dict):
            for key, value in rule["properties"].items():
                if isinstance(value, str):
                    # Check for direct CWE values
                    if key.lower() == "cwe" and value.startswith("CWE-"):
                        cwe_info["cwe"] = value.upper()
                        cwe_info["cwe_link"] = self._generate_cwe_link(value.upper())
                        return cwe_info
                    # Check for patterns in tags
                    if key.lower() == "tags":
                        vuln_match = re.search(r"(CWE-\d+)", value, re.IGNORECASE)
                        if vuln_match:
                            vuln_id = vuln_match.group(1).upper()
                            cwe_info["cwe"] = vuln_id
                            cwe_info["cwe_link"] = self._generate_cwe_link(vuln_id)
                            return cwe_info

            # Check rule descriptions
            if "shortDescription" in rule and "text" in rule["shortDescription"]:
                vuln_match = re.search(
                    r"(CWE-\d+)", rule["shortDescription"]["text"], re.IGNORECASE
                )
                if vuln_match:
                    vuln_id = vuln_match.group(1).upper()
                    cwe_info["cwe"] = vuln_id
                    cwe_info["cwe_link"] = self._generate_cwe_link(vuln_id)
                    return cwe_info

        # Check result message as a last resort
        if "message" in result and "text" in result["message"]:
            vuln_match = re.search(r"(CWE-\d+)", result["message"]["text"], re.IGNORECASE)
            if vuln_match:
                vuln_id = vuln_match.group(1).upper()
                cwe_info["cwe"] = vuln_id
                cwe_info["cwe_link"] = self._generate_cwe_link(vuln_id)

        return cwe_info

    def _generate_cwe_link(self, cwe_id: str) -> str:
        """Generate a link to the CWE online database."""
        if cwe_id.startswith("CWE-"):
            cwe_number = cwe_id.split("-")[-1]
            return f"https://cwe.mitre.org/data/definitions/{cwe_number}.html"
        return ""

    def _build_trace_info(
        self, result: Dict[str, Any], rule: Dict[str, Any], tool_info: Dict[str, str]
    ) -> str:
        """
        Build comprehensive trace information from SARIF result in shellcheck-style format.
        """
        # --- Build the trace ---
        trace_parts = []

        # Add code flow execution trace if available
        code_flows = result.get("codeFlows", [])
        code_flow_context = self._build_compact_code_flow(code_flows)

        if code_flow_context:
            trace_parts.append(code_flow_context)
        else:
            # --- Extracting the exact finding line and message ---
            finding_location = self._format_location(result)
            trace_parts.append(finding_location)

        # Add code context
        code_context = self._build_code_context(result["locations"][0])
        trace_parts.append(code_context)
        # Join parts and strip trailing whitespace for consistency
        return "\n".join(trace_parts).rstrip()

    def _build_code_context(self, location: Dict[str, Any]) -> str:
        """
        Build code context showing lines around the issue with line numbers.
        """
        try:
            phys_loc = location.get("physicalLocation", {})
            if not phys_loc:
                return ""

            region = phys_loc.get("region", {})
            start_line = region.get("startLine")

            if not start_line:
                return ""

            # Try to get code snippet from SARIF
            snippet = region.get("snippet", {})
            if isinstance(snippet, dict) and "text" in snippet:
                code_text = snippet["text"].strip()
                if code_text.startswith("Problem detected in this context:\n"):
                    code_text = code_text[len("Problem detected in this context:\n") :]
                if code_text:
                    # Add '# ' prefix to each line to match expected trace format
                    lines = code_text.split("\n")
                    formatted_lines = []
                    for line in lines:
                        if line.strip():  # Only format non-empty lines
                            # Check if line already starts with '# ' to avoid double prefixing
                            if not line.startswith("#"):
                                formatted_lines.append(f"#  {line.strip()}")
                            else:
                                formatted_lines.append(line)
                        else:
                            formatted_lines.append(line)
                    return "\n".join(formatted_lines)

            return ""

        except Exception as e:
            logger.debug(f"Error building code context: {e}")
            return ""

    def _should_skip_location(self, location: Dict[str, Any]) -> bool:
        """
        Check if a thread flow location should be skipped.

        Uses SARIF 'kinds' property when available for semantic filtering,
        falls back to message pattern matching.
        """
        # Extract kinds if available (SARIF 2.1.0 semantic categorization)
        kinds = location.get("kinds", [])

        # Fallback to message-based filtering
        message = location.get("location", {}).get("message", {})
        msg_text = ""
        if isinstance(message, dict) and "text" in message:
            msg_text = message["text"]

        return self._should_skip_by_kinds(kinds) or self._should_skip_by_message(msg_text)

    def _should_skip_by_kinds(self, kinds: List[str]) -> bool:
        """
        Determine if location should be skipped based on SARIF kinds.

        Keep: vulnerability-semantic kinds (memory, resource, taint, danger)
        Skip: control-flow kinds (branch, enter, exit, true, false)
        """
        if not kinds:
            return False

        # Important kinds to keep (vulnerability-related)
        important_kinds = {
            "acquire",
            "release",  # Resource management
            "memory",
            "resource",  # Memory/resource operations
            "taint",  # Taint analysis
            "danger",
            "caution",  # Security warnings
            "lock",  # Synchronization
            "value",  # Value tracking (assignments)
            "scope",  # Variable scope (may indicate leaks)
        }

        # Control flow kinds to skip
        control_flow_kinds = {
            "branch",
            "true",
            "false",  # Branch decisions
            "enter",
            "exit",  # Scope entry/exit
            "implicit",  # Implicit control flow
            "unreachable",  # Unreachable code
        }

        # If any important kind is present, keep the location
        if any(kind in important_kinds for kind in kinds):
            return False

        # If only control flow kinds are present, skip
        if any(kind in control_flow_kinds for kind in kinds):
            return True

        # For unknown kinds or 'call'/'return', use conservative approach
        # Keep 'call'/'return' as they might indicate important function calls
        return False

    def _should_skip_by_message(self, message: str) -> bool:
        """
        Fallback message-based filtering when kinds are not available.
        """
        if not message:
            return True  # Skip empty messages

        # First check if this is a vulnerability-semantic message we want to keep
        for pattern in self.VULNERABILITY_KEEP_PATTERNS:
            if re.search(pattern, message, re.IGNORECASE):
                return False  # Don't skip - this is important

        # Then check if this is a control flow message we want to skip
        for pattern in self.CONTROL_FLOW_SKIP_PATTERNS:
            if re.search(pattern, message, re.IGNORECASE):
                return True  # Skip control flow messages

        # For messages that don't match either pattern, keep them
        return False

    def _extract_step_type(self, location: Dict[str, Any]) -> str:
        """
        Extract step type from SARIF threadFlowLocation.

        Uses only what's actually available in SARIF:
        1. Check properties bag for tool-specific type info
        2. Use SARIF kinds as-is (they're meaningful)
        3. Fall back to generic label
        """
        # Try to get type from properties (tool-specific data)
        properties = location.get("properties", {})
        if "type" in properties:
            return properties["type"]
        if "kind" in properties:
            return properties["kind"]

        # Try to get type from location properties
        loc_properties = location.get("location", {}).get("properties", {})
        if "type" in loc_properties:
            return loc_properties["type"]

        # Use SARIF kinds directly (they're semantically meaningful)
        kinds = location.get("kinds", [])
        if kinds:
            # Return the most specific kind, preferring vulnerability-related ones
            priority_kinds = [
                "memory",
                "resource",
                "taint",
                "danger",
                "acquire",
                "release",
                "value",
            ]
            for priority_kind in priority_kinds:
                if priority_kind in kinds:
                    return priority_kind
            # If no priority kinds, return the first one
            return kinds[0]

        # Default fallback
        return "step"

    def _build_compact_code_flow(self, code_flow: List[Dict[str, Any]]) -> str:
        """
        Build a compact representation of the code flow.
        """
        try:
            if not code_flow:
                return ""

            flow_steps = []

            thread_flows = code_flow[0].get("threadFlows", [])
            if not thread_flows:
                return ""

            locations = thread_flows[0].get("locations", [])

            # Show key steps in the flow
            for i, loc in enumerate(locations):
                # Skip if location should be filtered out
                if self._should_skip_location(loc):
                    continue

                step_info = self._format_flow_step(loc, i + 1)
                if step_info:
                    flow_steps.append(step_info)

            return "\n".join(flow_steps)

        except Exception as e:
            logger.debug(f"Error building compact code flow: {e}")
            return ""

    def _format_flow_step(self, location: Dict[str, Any], step: int) -> str:
        """
        Format a single step in the code flow in ERR format: file:line:column: type: message
        """
        try:
            # Get the actual location
            phys_loc = location.get("location", {}).get("physicalLocation", {})
            if not phys_loc:
                return ""

            artifact_loc = phys_loc.get("artifactLocation", {})
            uri = artifact_loc.get("uri", "")

            region = phys_loc.get("region", {})
            start_line = region.get("startLine")
            start_col = region.get("startColumn")

            # Get message for this step
            message = location.get("location", {}).get("message", {})
            msg_text = ""
            if isinstance(message, dict) and "text" in message:
                msg_text = message["text"]

            # Extract step type
            step_type = self._extract_step_type(location)

            # Format as ERR style: file:line:column: type: message
            if uri and start_line and msg_text:
                if start_col:
                    return f"{uri}:{start_line}:{start_col}: {step_type}: {msg_text}"
                else:
                    return f"{uri}:{start_line}: {step_type}: {msg_text}"

            return ""

        except Exception as e:
            logger.debug(f"Error formatting flow step: {e}")
            return ""

    def _format_location(self, result: Dict[str, Any]) -> str:
        """
        Format location information for display (kept for backwards compatibility).
        """
        try:
            file_path = result["locations"][0]["physicalLocation"]["artifactLocation"]["uri"]
            start_line = result["locations"][0]["physicalLocation"]["region"]["startLine"]
            message_text = result["message"]["text"]
            return f"{file_path}:{start_line}: {message_text}"
        except Exception as e:
            logger.debug(f"Error formatting location: {e}")
            return ""
