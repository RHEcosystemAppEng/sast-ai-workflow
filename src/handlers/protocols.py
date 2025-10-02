import typing
from typing import Set, Tuple


@typing.runtime_checkable
class RepoHandlerProtocol(typing.Protocol):
    def get_source_code_blocks_from_error_trace(self, error_trace: str) -> dict: ...

    def get_source_code_of_called_expressions(self) -> str: ...

    def extract_missing_functions_or_macros(
        self, instructions, found_symbols: Set[str]
    ) -> Tuple[str, Set[str]]: ...

    def reset_found_symbols(self): ...
