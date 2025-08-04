class Issue:
    def __init__(self, issue_id):
        self.id = issue_id
        self.issue_type = ""
        self.issue_label = ""
        self.issue_cwe = ""
        self.issue_cwe_link = ""
        self.trace = ""
        self.parsing_errors = False  # Track if parsing failed for this issue

    def __repr__(self):
        return (
            f"id ={self.id}\n"
            f"type ={self.issue_type}\n"
            f"label ={self.issue_label}\n"
            f"cwe ={self.issue_cwe}\n"
            f"URL ={self.issue_cwe_link}\n"
            f"Trace ={self.trace}\n"
            f"Parsing Errors ={self.parsing_errors}"
        )
