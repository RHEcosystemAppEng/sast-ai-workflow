import re
import os
import clang.cindex
import subprocess

from collections import defaultdict
from clang.cindex import TranslationUnit, CursorKind, Cursor

from common.config import Config
from Utils.repo_utils import get_repo_and_branch_from_url, download_repo
from Utils.file_utils import load_json_file



class CRepoHandler:
    """
    A handler for managing and analyzing C language Git repositories.

    This class provides functionality to extract relevant source code functions
    based on either an error trace or specific line numbers.
    """
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
        self.clang_args = ['-include', config.CONFIG_H_PATH] if config.CONFIG_H_PATH else []

        self._compile_commands_json = {}
        self._compile_commands_json_path = config.COMPILE_COMMANDS_JSON_PATH

        clang.cindex.Config.set_library_file(config.LIBCLANG_PATH)
        self.index = clang.cindex.Index.create()

    @property
    def compile_commands_json(self):
        if not self._compile_commands_json:
            compile_commands_json = load_json_file(self._compile_commands_json_path)
            self._compile_commands_json = self._convert_relative_path_to_absolute(compile_commands_json)
        return self._compile_commands_json

    def _convert_relative_path_to_absolute(self, compile_commands_json) -> dict[str, str]:
        """Convert all relative header search paths in the provided compile_command.json to absolute paths"""
        reformatted_compile_commands_json = {}
        for section in compile_commands_json:
            file_path = section["file"].replace("../", self.repo_local_path)
            if os.path.exists(file_path):
                reformatted_compile_command = section["command"].replace("../", self.repo_local_path)
                reformatted_compile_commands_json[file_path] = reformatted_compile_command
        return reformatted_compile_commands_json

    def get_source_code_from_error_trace(self, error_trace: str) -> dict:
        """Parse an error trace and extracts relevant functions bodies"""

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
        """Extract the full source code section (function, macro, or class) including the specified line in the source file"""
        if not os.path.exists(file_path):
            print(f"File not found: {file_path}")
            return None

        args = self._get_clang_args_from_file(file_path)
        translation_unit = self.index.parse(file_path,
                                            options=TranslationUnit.PARSE_INCOMPLETE,
                                            args=['-Xclang', '-fsyntax-only'] + args)

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
                        numbered_lines = [f"{i + start_line}| {line}" for i, line in enumerate(lines[start_line-1:end_line])]
                        source_code = "".join(numbered_lines)
            
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
    
    def get_source_code_of_called_expressions(self, instructions) -> str:
        def get_path(path: str):
            path = path.removeprefix(self.repo_local_path)
            path = path.removeprefix(self._report_file_prefix)
            path = path.split(":")[0]
            return path

        source_code_dict = defaultdict(list)
        for instruction in instructions:
            path = get_path(instruction.reffering_source_code_path)
            source_code_dict[path].append(instruction.expression_name)

        missing_source_code = ""
        for source_code_path, expressions_list in source_code_dict.items():
            source_code = ''
            try:
                source_code = self.extract_definition_from_source_code(expressions_list, source_code_path)
            except Exception as e:
                print(f"Failed to retrieve {expressions_list} from {source_code_path}.\nError:{e}")
            if source_code:
                for file_path, exps_dict in source_code.items():
                    missing_source_code += f'code of {file_path} file:\n{"\n\n".join([exp for exp in exps_dict.values()])}'
        return missing_source_code

    def extract_definition_from_source_code(self, function_names: set[str], source_code_file_path: str) -> dict[str, str]:
        """Extract the definitions of functions or macros that are referenced in the source code"""

        source_code_file_path = os.path.join(self.repo_local_path, source_code_file_path)
        args = ['-Xclang', '-fsyntax-only'] + self._get_clang_args_from_file(source_code_file_path) + self._get_includes_of_file(source_code_file_path)

        translation_unit = self.index.parse(
            source_code_file_path, 
            options=TranslationUnit.PARSE_DETAILED_PROCESSING_RECORD,
            args=args)
        
        source_code_dict = defaultdict(dict)

        for cursor in translation_unit.cursor.walk_preorder():
            if cursor.kind in [CursorKind.MACRO_INSTANTIATION, CursorKind.CALL_EXPR] and cursor.spelling in function_names:
                if cursor.get_definition() and cursor.get_definition().location:
                    file_path = cursor.get_definition().location.file.name
                    source_code_dict[file_path].update({cursor.spelling: self._get_source_code_from_cursor(cursor.get_definition())})
                else:
                    if cursor.kind == CursorKind.MACRO_INSTANTIATION:
                        file_path, line = self._get_macro_definition_file_location(macro_name=cursor.spelling)
                    else:
                        file_path, line = self._get_function_definition_file_location(cursor.spelling)
                    if file_path:
                        source_code_dict[file_path].update({cursor.spelling: self.get_source_code_by_line(file_path=file_path, line=int(line)+1)})

        for diag in translation_unit.diagnostics:
            print(f"[{diag.severity}] {diag.spelling}")

        found_expressions = set([expression for expressions in source_code_dict.values() for expression in expressions.keys()])
        if len(found_expressions) < len(function_names):
            for expression in set(function_names).difference(found_expressions):
                file_path, line = self._get_function_definition_file_location(expression)
                if not file_path:
                    file_path, line = self._get_function_definition_file_location(expression)
                if file_path:
                    source_code_dict[file_path].update({cursor.spelling: self.get_source_code_by_line(file_path=file_path, line=int(line)+1)})
                    found_expressions.add(expression)
            missing_functions = set(function_names).difference(found_expressions)
            print(f"Missing source code of {missing_functions}")
            
        return source_code_dict
    
    def _get_clang_args_from_file(self, file_path: str) -> list[str]: 
        """Extract the used macros in the source code file for constructing Clang arguments"""
        if self.clang_args:
            return self.clang_args
        
        pattern = re.compile(r'^\s*#\s*(if|ifdef|ifndef)\b(.*)', re.MULTILINE)
        macros = set()
        with open(file_path) as f:
            expressions = pattern.findall(f.read())
        
        for expr in expressions:
            matches = re.findall(r'defined\s*\(\s*(\w+)\s*\)|defined\s+(\w+)|\b([A-Z_][A-Z0-9_]*)\b', expr[1])
            for match in matches[0]:
                if match:
                    macros.add(f"-D{match}")
        
        return list(macros)

    def _get_includes_of_file(self, file_path):
        """Get list of header search directories paths of a file"""
        return [arg for arg in self.compile_commands_json[file_path].split() if '-I' in arg] + [f"-I{self.repo_local_path}"] 

    def _get_source_code_from_cursor(self, cursor: Cursor) -> str:
        """Extract the source code section referenced by the given Clang cursor."""
        with open(cursor.location.file.name , "r") as f:
            lines = f.readlines()
            numbered_lines = [f"{i + cursor.extent.start.line}| {line}" for i, line in enumerate(lines[cursor.extent.start.line-1:cursor.extent.end.line])]
            source_code = "".join(numbered_lines)
        return source_code

    def _get_function_definition_file_location(self, function_name):
        """Get the definition location of a function in the repo if exists"""
        file_path, code_line_number = "", ""
        command = ['grep ' +
                   '-nHr ' +
                    r'"^[a-zA-Z_][a-zA-Z0-9_[:space:]\*]*' +
                    function_name +
                    r'[[:space:]]*\([^;{]*\)[[:space:]]*" ' + self.repo_local_path]
        
        try:
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
                shell=True
            )
        except Exception as e:
            print(e)

        if result:
            file_path, code_line_number = result.stdout.strip().split(':')[:2]

        return file_path, code_line_number

    def _get_macro_definition_file_location(self, macro_name):
        """Get the definition location of a MACRO in the repo if exists"""
        file_path, code_line_number = "", ""
        command = fr'grep -nHr "#define\s*{macro_name}.*" {self.repo_local_path}'
        try:
            result = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False
            )
        except Exception as e:
            print(e)

        if result:
            file_path, code_line_number = result.stdout.strip().split(':')[:2]

        return file_path, code_line_number
    