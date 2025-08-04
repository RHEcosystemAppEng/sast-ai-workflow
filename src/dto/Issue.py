from pydantic import BaseModel

class Issue(BaseModel):
    """
    Represents a single issue.
    """
    # This field is required for instantiation.
    id: str
    
    issue_type: str = ""
    issue_label: str = ""
    issue_cve: str = ""
    issue_cve_link: str = ""
    trace: str = ""
    parsing_errors: bool = False  # Track if parsing failed for this issue

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
