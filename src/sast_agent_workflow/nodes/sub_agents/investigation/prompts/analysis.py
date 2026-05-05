"""Analysis node prompt builder.

All prompt text lives in analysis_context/. This module is a thin render
layer: it loads the template and language-specific YAML files, then uses
Jinja2 to substitute variables and return the final prompt string.

Adding support for a new language requires only a new YAML file in
analysis_context/ — no Python changes needed.
"""

import logging
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict

import yaml
from jinja2 import Environment, StrictUndefined

logger = logging.getLogger(__name__)

_CONTEXT_DIR = Path(__file__).parent / "analysis"

# ---------------------------------------------------------------------------
# Lazy singletons — cached on first call via lru_cache
# ---------------------------------------------------------------------------


@lru_cache(maxsize=1)
def _get_jinja_env() -> Environment:
    return Environment(keep_trailing_newline=True, undefined=StrictUndefined)


@lru_cache(maxsize=1)
def _get_template() -> Dict[str, str]:
    with open(_CONTEXT_DIR / "prompt_template.yaml", "r") as f:
        return yaml.safe_load(f)


@lru_cache(maxsize=None)
def _get_language_context(language: str) -> Dict[str, str]:
    """Load the YAML context file for a specific language, cached per language.

    Falls back to 'generic' if language is empty or no matching file exists.
    """
    path = _CONTEXT_DIR / f"{language}.yaml"
    if not path.exists():
        logger.warning("No analysis context for language '%s', falling back to generic.", language)
        language = "generic"
        path = _CONTEXT_DIR / f"{language}.yaml"
    try:
        with open(path, "r") as f:
            return yaml.safe_load(f) or {}
    except Exception as e:
        logger.error("Failed to load analysis context from %s: %s", path, e)
        raise


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def build_analysis_prompt(state: Dict[str, Any]) -> str:
    """
    Build the analysis prompt for verdict decision.

    Args:
        state: Current investigation state.

               Required keys: issue_description
               Optional keys: gathered_code, needs_reanalysis,
                              evaluation_feedback, analysis, repo_language

    Returns:
        Formatted prompt string for the analysis LLM.
    """
    language = state.get("repo_language") or "generic"
    lang_ctx = _get_language_context(language)

    t = _get_template()

    evaluator_feedback_block = ""
    if state.get("needs_reanalysis") and state.get("evaluation_feedback"):
        evaluator_feedback_block = _render(
            t["evaluator_feedback"],
            feedback_text=state["evaluation_feedback"],
        )
    full_template = "\n".join(
        [
            t["header"],
            t["finding"],
            t["mandatory_steps"],
            t["guidelines"],
            t["required_output"],
        ]
    )

    return _render(
        full_template,
        issue_description=state["issue_description"],
        gathered_code=state.get("gathered_code", "(No code gathered)"),
        previous_analysis=state.get("analysis") or "None - this is the first analysis.",
        evaluator_feedback_block=evaluator_feedback_block,
        context_hints=lang_ctx.get("context_hints", "").rstrip(),
        issue_patterns=lang_ctx.get("issue_patterns", "").rstrip(),
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _render(template_str: str, **context: Any) -> str:
    return _get_jinja_env().from_string(template_str).render(**context)
