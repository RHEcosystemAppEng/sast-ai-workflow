"""Investigation workflow nodes and subgraph.

Exports the five core investigation subgraph nodes:
- create_research_node: ReAct agent with LangChain v1 middleware pattern
- create_analysis_node: LLM-based verdict decision using structured output
- create_evaluation_node: Quality critique deciding if more research needed
- create_circuit_breaker_node: Graceful termination on safety limits
- should_continue: Conditional routing (end / research / reanalyze / circuit_breaker)
"""

from .analysis import create_analysis_node
from .circuit_breaker import create_circuit_breaker_node
from .evaluation import create_evaluation_node
from .research import create_research_node
from .router import should_continue

__all__ = [
    "create_research_node",
    "create_analysis_node",
    "create_evaluation_node",
    "create_circuit_breaker_node",
    "should_continue",
]
