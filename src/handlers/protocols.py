import typing


@typing.runtime_checkable
class RepoHandlerProtocol(typing.Protocol):
    def get_source_code_blocks_from_error_trace(self) -> dict: ...

    def get_source_code_of_called_expressions(self) -> str: ...

    def extract_missing_functions_or_macros(self, instructions) -> str: ...
