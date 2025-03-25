import typing


@typing.runtime_checkable
class RepoHandlerProtocol(typing.Protocol):
    def get_source_code_from_error_trace(self) -> str: ...
