# Semantic Scholar to Org

`night/semantic-scholar-to-org` passes clipboard URLs to the shell's
`semantic-scholar-to-org` through BrishGarden and inserts the returned Org.

arXiv inputs are resolved to the correct Semantic Scholar paper through Semantic
Scholar's HTTP redirect. The source remains Semantic Scholar; URL identification
uses no LLM or search.

When a Semantic Scholar key is configured, the API is tried first. Its failures
are reported with `ecgray`. `semantic_scholar_scraper=auto` (default) then uses
DeepSeek first when its key is available, falling back to `full-html2` only if
needed. Set it to `deepseek` or `full-html2` to select one backend explicitly.
Without a Semantic Scholar key, the API is skipped.

Missing citation counts are omitted. DeepSeek summaries are labeled `@summary`.
If a page exposes only its title and reference count, the function reports partial
metadata and omits unavailable fields and empty abstract sections.

The implementation, regressions, and detailed usage live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/semantic_scholar.zsh`
- `python/orgmode/semantic_scholar_html_to_json.py`
- `commonlisp/semantic_scholar_json_to_org.lisp`
- `zshlang/tests/semantic-scholar-to-org.py`
- `docs/semantic-scholar-to-org.md`
- `docs/deepseek-fetch.md`

Run `brishz-restart` after shell changes. The Elisp wrapper needs no reload.

Verified by @OpenAI on 2026-10-20.
