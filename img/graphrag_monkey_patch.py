# Monkey patch the graphrag package to support local models deployed via Ollama. Only tested for graphrag version 0.3.2.
# See https://chishengliu.com/posts/graphrag-local-ollama/ for details.
# 
# Usage:
# * How to patch the `graphrag.index` CLI:
#   * Save https://github.com/microsoft/graphrag/blob/v0.3.2/graphrag/index/__main__.py as "index.py"
#   * Replace line 8 with:
#     ```python
#     from graphrag.index.cli import index_cli
#     from graphrag_monkey_patch import patch_graphrag
#     patch_graphrag()
#     ```
#   * Then, you can use `python index.py` instead of `python -m graphrag.index` to do the indexing.
# * How to patch the `graphrag.query` CLI:
#   * Save https://github.com/microsoft/graphrag/blob/v0.3.2/graphrag/query/__main__.py as "query.py"
#   * Replace line 9 with:
#     ```python
#     from graphrag.query.cli import run_global_search, run_local_search
#     from graphrag_monkey_patch import patch_graphrag
#     patch_graphrag()
#     ```
#   * Then, you can use `python query.py` instead of `python -m graphrag.query` to do the queries.

def patch_graphrag():
    patch_openai_embeddings_llm()
    patch_query_embedding()
    patch_global_search()

def patch_openai_embeddings_llm():
    # Reference: https://github.com/microsoft/graphrag/issues/370#issuecomment-2211821370
    from graphrag.llm.openai.openai_embeddings_llm import OpenAIEmbeddingsLLM
    import ollama

    async def _execute_llm(self, input, **kwargs):
        embedding_list = []
        for inp in input:
            embedding = ollama.embeddings(model=self.configuration.model, prompt=inp)
            embedding_list.append(embedding["embedding"])
        return embedding_list

    OpenAIEmbeddingsLLM._execute_llm = _execute_llm

def patch_query_embedding():
    # Reference: https://github.com/microsoft/graphrag/issues/345#issuecomment-2230317752
    from graphrag.query.llm.oai.embedding import OpenAIEmbedding
    import ollama
    from tenacity import (
        AsyncRetrying,
        RetryError,
        Retrying,
        retry_if_exception_type,
        stop_after_attempt,
        wait_exponential_jitter,
    )

    def _embed_with_retry(self, text, **kwargs):
        try:
            retryer = Retrying(
                stop=stop_after_attempt(self.max_retries),
                wait=wait_exponential_jitter(max=10),
                reraise=True,
                retry=retry_if_exception_type(self.retry_error_types),
            )
            for attempt in retryer:
                with attempt:
                    embedding = (ollama.embeddings(model=self.model, prompt=text)["embedding"] or [])
                    return (embedding, len(text))
        except RetryError as e:
            self._reporter.error(
                message="Error at embed_with_retry()",
                details={self.__class__.__name__: str(e)},
            )
            return ([], 0)
        else:
            # TODO: why not just throw in this case?
            return ([], 0)

    async def _aembed_with_retry(self, text, **kwargs):
        try:
            retryer = AsyncRetrying(
                stop=stop_after_attempt(self.max_retries),
                wait=wait_exponential_jitter(max=10),
                reraise=True,
                retry=retry_if_exception_type(self.retry_error_types),
            )
            async for attempt in retryer:
                with attempt:
                    embedding = (ollama.embeddings(model=self.model, prompt=text)["embedding"] or [])
                    return (embedding, len(text))
        except RetryError as e:
            self._reporter.error(
                message="Error at embed_with_retry()",
                details={self.__class__.__name__: str(e)},
            )
            return ([], 0)
        else:
            # TODO: why not just throw in this case?
            return ([], 0)

    OpenAIEmbedding._embed_with_retry = _embed_with_retry
    OpenAIEmbedding._aembed_with_retry = _aembed_with_retry

def patch_global_search():
    # Reference: https://github.com/microsoft/graphrag/issues/575#issuecomment-2252045859
    from graphrag.query.structured_search.global_search.search import GlobalSearch
    import logging
    import time
    from graphrag.query.llm.text_utils import num_tokens
    from graphrag.query.structured_search.base import SearchResult
    log = logging.getLogger(__name__)

    async def _map_response_single_batch(self, context_data, query, **llm_kwargs):
        """Generate answer for a single chunk of community reports."""
        start_time = time.time()
        search_prompt = ""
        try:
            search_prompt = self.map_system_prompt.format(context_data=context_data)
            search_messages = [ {"role": "user", "content": search_prompt + "\n\n### USER QUESTION ### \n\n" + query} ]
            async with self.semaphore:
                search_response = await self.llm.agenerate(
                    messages=search_messages, streaming=False, **llm_kwargs
                )
                log.info("Map response: %s", search_response)
            try:
                # parse search response json
                processed_response = self.parse_search_response(search_response)
            except ValueError:
                # Clean up and retry parse
                try:
                    # parse search response json
                    processed_response = self.parse_search_response(search_response)
                except ValueError:
                    log.warning(
                        "Warning: Error parsing search response json - skipping this batch"
                    )
                    processed_response = []

            return SearchResult(
                response=processed_response,
                context_data=context_data,
                context_text=context_data,
                completion_time=time.time() - start_time,
                llm_calls=1,
                prompt_tokens=num_tokens(search_prompt, self.token_encoder),
            )

        except Exception:
            log.exception("Exception in _map_response_single_batch")
            return SearchResult(
                response=[{"answer": "", "score": 0}],
                context_data=context_data,
                context_text=context_data,
                completion_time=time.time() - start_time,
                llm_calls=1,
                prompt_tokens=num_tokens(search_prompt, self.token_encoder),
            )

    GlobalSearch._map_response_single_batch = _map_response_single_batch