class Issue:
    def __init__(self, issue_id):
        self.id = issue_id
        self.issue_type = ""
        self.issue_label = ""
        self.issue_cve = ""
        self.issue_cve_link = ""
        self.trace = ""

    def __repr__(self):
        return (
            f"id ={self.id}\n"
            f"type ={self.issue_type}\n"
            f"label ={self.issue_label}\n"
            f"cve ={self.issue_cve}\n"
            f"URL ={self.issue_cve_link}\n"
            f"Trace ={self.trace}"
        )
