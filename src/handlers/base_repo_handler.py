import re
import os
import clang.cindex 

from collections import defaultdict
from clang.cindex import TranslationUnit, CursorKind

from common.config import Config
from Utils.repo_utils import get_repo_and_branch_from_url, download_repo



class RepoHandler:
    def __init__(self, config: Config) -> None:
        self.url, self.branch = get_repo_and_branch_from_url(config.GIT_REPO_PATH)
        
        self.repo_local_path = config.GIT_REPO_PATH
        if config.DOWNLOAD_GIT_REPO:
            # downloading git repository for given project
            self.repo_local_path = download_repo(config.GIT_REPO_PATH)
        else:
            print("Skipping github repo download as per configuration.")

        # This variable holds the prefix for source code files as they appear in the report.
        # It helps in locating the correct files by removing this prefix to the paths found in the error traces.
        self._report_file_prefix = ""

        # This list contains specific arguments to be passed to the Clang compiler.
        # These arguments are used to configure the parsing and analysis of the source code.
        # Example arguments could include macro definitions or include paths that are necessary for parsing the code correctly.
        #
        # Subclasses should override this list with project-specific flags.
        # For example:
        # self.clang_args = [
        #     "-DDEFINE_NAME=value",
        #     "-I/path/to/includes",
        # ]
        self.clang_args = []


        clang.cindex.Config.set_library_file(config.LIBCLANG_PATH)

    def get_source_code_from_error_trace(self, error_trace: str) -> str:
        """Parse an error trace and extracts relevant functions bodies/ """

        source_files = set(re.findall(r'([^\s]+\.(?:c|h)):(\d+):', error_trace))
        error_code_sources = defaultdict(set)
        
        for file_path, line_number in source_files:
            file_path = file_path.removeprefix(self._report_file_prefix)
            local_file_path = os.path.join(self.repo_local_path, file_path)
            if not os.path.exists(local_file_path):
                print(f"Skipping missing file: {local_file_path}")
                continue
            
            source_code = self.get_source_code_by_line(local_file_path, int(line_number))
            if source_code:
                error_code_sources[file_path].add(source_code)

        return {file: "\n".join(code_sections) for file, code_sections in error_code_sources.items()}
    
    def get_source_code_by_line(self, file_path: str, line: int) -> str:
        if not os.path.exists(file_path):
            print(f"File not found: {file_path}")
            return None

        translation_unit = TranslationUnit.from_source(file_path, 
                                                    options=TranslationUnit.PARSE_INCOMPLETE,
                                                    args=['-Xclang', '-fsyntax-only'] + self.clang_args)

        source_code = None

        def visit(node):
            nonlocal source_code
            if node.kind in {
                CursorKind.FUNCTION_DECL,
                CursorKind.CXX_METHOD,
                CursorKind.CONSTRUCTOR,
                CursorKind.DESTRUCTOR,
                CursorKind.FUNCTION_TEMPLATE,
                CursorKind.CLASS_DECL,
                CursorKind.STRUCT_DECL,
                CursorKind.CLASS_TEMPLATE,
                CursorKind.NAMESPACE}:
                start_line = node.extent.start.line
                end_line = node.extent.end.line
                if start_line <= line <= end_line:
                    # Read function from file
                    with open(file_path, "r") as f:
                        lines = f.readlines()
                        source_code = "".join(lines[start_line-1:end_line])
            
            for child in node.get_children():
                visit(child)

        visit(translation_unit.cursor)

        if source_code is None:
            # If the code is not inside a code block, returning 100 lines before and after
            print(f"No function found in {file_path} near line {line}")
            with open(file_path, "r") as f:
                lines = f.readlines()
                source_code = "".join(lines[min(0, line - 100):max(line + 100, len(lines))])
        
        return source_code
