#!/usr/bin/env python3
"""
Integration test script for project context initialization.

This script runs the project_context initialization on a real repository
and displays the discovered metadata in a human-readable format.
"""

import sys
import time
from pathlib import Path

# Add src to path so we can import the module
sys.path.insert(0, str(Path(__file__).parent / "src"))

from sast_agent_workflow.agent.project_context import initialize_project_context


def print_section(title: str, width: int = 80):
    """Print a formatted section header."""
    print("\n" + "=" * width)
    print(f"  {title}")
    print("=" * width)


def print_subsection(title: str):
    """Print a formatted subsection header."""
    print(f"\n--- {title} ---")


def format_size(bytes_size: int) -> str:
    """Format byte size to human-readable format."""
    for unit in ['B', 'KB', 'MB', 'GB']:
        if bytes_size < 1024.0:
            return f"{bytes_size:.1f} {unit}"
        bytes_size /= 1024.0
    return f"{bytes_size:.1f} TB"


def main(repo_path: str):
    """Run project context initialization and display results."""

    # Verify repository exists
    if not Path(repo_path).exists():
        print(f"❌ Error: Repository path does not exist: {repo_path}")
        sys.exit(1)

    print_section("PROJECT CONTEXT INITIALIZATION TEST", 80)
    print(f"Repository: {repo_path}")
    print(f"Testing on: {Path(repo_path).name}")

    # Initialize project context (timed)
    print("\n⏱️  Starting initialization...")
    start_time = time.time()

    try:
        context = initialize_project_context(repo_path)
        elapsed_time = time.time() - start_time

        print(f"✅ Initialization completed in {elapsed_time:.2f} seconds")

        # Validate <15 second requirement
        if elapsed_time < 15.0:
            print(f"   ✓ Performance target met (< 15 seconds)")
        else:
            print(f"   ⚠️  WARNING: Exceeded 15 second target!")

    except Exception as e:
        print(f"❌ Initialization failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)

    # Display results
    print_section("DISCOVERED PROJECT STRUCTURE", 80)

    print_subsection(f"Directory Tree ({len(context.structure)} directories)")

    if context.structure:
        # Sort directories by depth then alphabetically
        sorted_dirs = sorted(context.structure.items(),
                           key=lambda x: (x[0].count('/'), x[0]))

        for idx, (dir_path, contents) in enumerate(sorted_dirs[:20], 1):
            depth = dir_path.count('/')
            indent = "  " * depth
            display_path = dir_path if dir_path else "/ (root)"
            item_count = len(contents)

            print(f"{indent}📁 {display_path}")
            print(f"{indent}   └─ {item_count} items")

            # Show first 10 items
            if contents:
                preview = contents[:10]
                for item in preview:
                    # Determine if it's likely a directory or file
                    icon = "📄" if "." in item else "📁"
                    print(f"{indent}      {icon} {item}")

                if len(contents) > 10:
                    remaining = len(contents) - 10
                    print(f"{indent}      ... and {remaining} more items")

        if len(context.structure) > 20:
            remaining_dirs = len(context.structure) - 20
            print(f"\n   ... and {remaining_dirs} more directories")
    else:
        print("   (No directories discovered)")

    # Display security files
    print_section("SECURITY-RELATED FILES", 80)

    print_subsection(f"Found {len(context.security_files)} security-related files")

    if context.security_files:
        print("\nFiles containing security patterns (sanitize, validate, clean, escape, etc.):\n")

        # Group by directory
        from collections import defaultdict
        by_dir = defaultdict(list)

        for file_path in context.security_files[:50]:  # Show first 50
            dir_path = str(Path(file_path).parent)
            file_name = Path(file_path).name
            by_dir[dir_path].append(file_name)

        for dir_path in sorted(by_dir.keys())[:15]:  # Show first 15 directories
            files = by_dir[dir_path]
            print(f"  📁 {dir_path}/")
            for file_name in sorted(files)[:10]:  # Show first 10 files per dir
                print(f"     🔒 {file_name}")
            if len(files) > 10:
                print(f"     ... and {len(files) - 10} more files")

        if len(context.security_files) > 50:
            remaining = len(context.security_files) - 50
            print(f"\n   ... and {remaining} more security files")
    else:
        print("   (No security-related files found)")

    # Display detected frameworks
    print_section("DETECTED FRAMEWORKS", 80)

    if context.frameworks:
        print(f"\n✅ Detected {len(context.frameworks)} framework(s):\n")
        for framework in context.frameworks:
            print(f"   🔧 {framework}")
    else:
        print("\n   ℹ️  No frameworks detected")
        print("   (This may be a library, system component, or uses unrecognized frameworks)")

    # Display summary statistics
    print_section("SUMMARY STATISTICS", 80)

    print(f"""
  Total Directories Discovered:  {len(context.structure)}
  Total Security Files Found:    {len(context.security_files)}
  Frameworks Detected:           {len(context.frameworks)}
  Initialization Time:           {elapsed_time:.2f} seconds
  Performance Status:            {'✅ PASS' if elapsed_time < 15.0 else '⚠️  SLOW'}
    """)

    # Display sample project context usage
    print_section("PROJECT CONTEXT OBJECT", 80)

    print("\nThis ProjectContext object would be shared across all agent investigations:")
    print(f"""
  context.structure         → Dict with {len(context.structure)} directories
  context.security_files    → List with {len(context.security_files)} file paths
  context.frameworks        → List: {context.frameworks or ['(none)']}
    """)

    print("\nExample: Agent can use this to narrow search scope:")
    if context.security_files:
        example_file = context.security_files[0]
        example_dir = str(Path(example_file).parent)
        print(f"""
  Instead of:  search_codebase('sanitize.*', repo_path)  # Expensive!
  Agent uses:  search_codebase('sanitize.*', '{example_dir}/')  # Targeted!
        """)

    print_section("TEST COMPLETE", 80)
    print("\n✅ Project context initialization successful!\n")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python test_project_context_integration.py <repo_path>")
        print("\nExample:")
        print("  python test_project_context_integration.py /path/to/repository")
        sys.exit(1)

    repo_path = sys.argv[1]
    main(repo_path)
