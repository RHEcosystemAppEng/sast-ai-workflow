from typing import Type
from pydantic import BaseModel
from langchain_openai.chat_models.base import ChatOpenAI
from langchain_nvidia_ai_endpoints import ChatNVIDIA
from langchain.output_parsers import OutputFixingParser, PydanticOutputParser
from langchain_core.exceptions import OutputParserException, LangChainException
from langchain.output_parsers.prompts import NAIVE_FIX
from langchain_core.prompts.prompt import PromptTemplate
from langchain_core.runnables import RunnableSerializable


ERROR_MESSAGE = ("Parsing failed after {max_retries} retries: {exception}"
                 "\nFailed on input: {input}"
                "\nThis indicates a persistent issue with the model or the input data."
                "\nPlease investigate the root cause to resolve this problem.")
WARNING_MESSAGE = "\033[91mWARNING: An error occurred during model output parsing. Model type is {model_type}. Retrying now. \033[0m"

def robust_structured_output(
    llm: ChatNVIDIA | ChatOpenAI,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int = 1,
) -> BaseModel:
    """
    Determines the type of LLM and delegates to the appropriate handler.
    """
    if isinstance(llm, ChatOpenAI):
        return _handle_chat_openai(llm, schema, input, prompt_chain, max_retries)
    elif isinstance(llm, ChatNVIDIA):
        return _handle_chat_nvidia(llm, schema, input, prompt_chain, max_retries)
    else:
        raise ValueError(f"Unsupported LLM type: {type(llm)}")

def _handle_chat_openai(
    llm: ChatOpenAI,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int
) -> BaseModel:
    """
    Handles structured output for ChatOpenAI.
    """
    structured_llm = llm.with_structured_output(
        schema, method="json_mode", include_raw=True
    )
    llm_chain = (
            prompt_chain
            | structured_llm
    )
    result = llm_chain.invoke(input)

    if result.get("parsed") is not None:
        return result.get("parsed")

    print(WARNING_MESSAGE.format(model_type=schema))
    raw_content = result.get("raw").content
    # print(f"{raw_content=}")

    parser = PydanticOutputParser(pydantic_object=schema)
    fix_prompt = PromptTemplate.from_template(NAIVE_FIX + " Please don't change the content of the answer itself, but just the structure and data type of it.")
    fixing_parser = OutputFixingParser.from_llm(parser=parser, llm=llm, max_retries=max_retries, prompt=fix_prompt)

    try:
        fixed_result = fixing_parser.invoke(raw_content)
        return fixed_result
    except OutputParserException as e:
        raise OutputParserException(ERROR_MESSAGE.format(max_retries=max_retries, exception=e, input=input))

def _handle_chat_nvidia(
    llm: ChatNVIDIA,
    schema: Type[BaseModel],
    input: str,
    prompt_chain: RunnableSerializable,
    max_retries: int,
) -> BaseModel:
    """
    Handles structured output for ChatNVIDIA with retry logic.
    ChatNVIDIA not soppurted include_raw=True, instead it return None when failed to parse.
    """
    for attempt in range(max_retries):
        structured_llm = llm.with_structured_output(
            schema, method="json_mode"
        )
        llm_chain = (
            prompt_chain
            | structured_llm
        )
        try:
            result = llm_chain.invoke(input)
            if result is not None:
                return result
            else:
                last_exception = "No exception was raised, but the response is None"
        except Exception as e:
            last_exception = e
        print(WARNING_MESSAGE.format(model_type=schema))

    raise LangChainException(ERROR_MESSAGE.format(max_retries=max_retries, exception=last_exception, input=input))
