"""Evaluation node prompt builder.

All prompt text lives in evaluation_context/prompt_template.yaml. This module
is a thin render layer: it loads the template and uses Jinja2 to substitute
variables and return the final prompt string.
"""

import logging
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict

import yaml
from jinja2 import Environment, StrictUndefined

logger = logging.getLogger(__name__)

_CONTEXT_DIR = Path(__file__).parent / "evaluation"

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


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def build_evaluation_prompt(state: Dict[str, Any]) -> str:
    """
    Build the evaluation prompt for verdict quality assessment.

    Args:
        state: Current investigation state containing analysis and gathered code.

               Required keys: iteration, max_iterations, issue_description,
                              gathered_code, analysis, proposed_verdict, confidence
               Optional keys: evaluation_rejection_streak, no_progress_streak,
                              justifications

    Returns:
        Formatted prompt string for the evaluation LLM.
    """
    justifications = state.get("justifications", [])
    justifications_str = "\n".join(f"- {j}" for j in justifications)

    t = _get_template()
    full_template = "\n".join(
        [
            t["header"],
            t["progress"],
            t["standards"],
            t["decisions"],
            t["footer"],
        ]
    )

    return _render(
        full_template,
        iteration=state["iteration"],
        max_iterations=state["max_iterations"],
        rejection_streak=state.get("evaluation_rejection_streak", 0),
        no_progress_streak=state.get("no_progress_streak", 0),
        issue_description=state["issue_description"],
        gathered_code=state["gathered_code"],
        analysis=state["analysis"],
        proposed_verdict=state["proposed_verdict"],
        confidence=state["confidence"],
        justifications=justifications_str,
    )


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _render(template_str: str, **context: Any) -> str:
    return _get_jinja_env().from_string(template_str).render(**context)
