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

    def __repr__(self):
        return (
            f"id ={self.id}\n"
            f"type ={self.issue_type}\n"
            f"label ={self.issue_label}\n"
            f"cve ={self.issue_cve}\n"
            f"URL ={self.issue_cve_link}\n"
            f"Trace ={self.trace}"
        )
