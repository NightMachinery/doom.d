# Semantic Scholar to Org

`night/semantic-scholar-to-org` passes clipboard URLs to the shell's
`semantic-scholar-to-org` through BrishGarden and inserts the returned Org.

arXiv inputs are resolved to the correct Semantic Scholar paper through Semantic
Scholar's HTTP redirect. The source remains Semantic Scholar; URL identification
uses no LLM or search.

When a Semantic Scholar key is configured, the API is tried first. Its failures
are reported with `ecgray`. `semantic_scholar_scraper=auto` (default) then uses
DeepSeek first when its key is available, recovering incomplete results through
Semantic Scholar's public page data before trying `full-html2`. Without a
DeepSeek key it starts with page data. Without a Semantic Scholar key, the
Graph API is skipped.

Use `semantic_scholar_scraper=page-data` for direct retrieval without either key.
The other selections are `deepseek` (with page-data recovery, no browser) and
`full-html2` (HTML first, then page-data recovery).

Successful conversions require a title, authors, and a full abstract. Title-only
pages, TLDRs, social previews, and related-paper descriptions cannot satisfy
that requirement. If recovery fails, the command returns an error with no Org
output. Publication details and PDF links are retained when available; missing
citation counts are omitted and zero counts are preserved.

The implementation, regressions, and detailed usage live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/semantic_scholar.zsh`
- `python/orgmode/semantic_scholar_html_to_json.py`
- `commonlisp/semantic_scholar_json_to_org.lisp`
- `zshlang/tests/semantic-scholar-to-org.py`
- `docs/semantic-scholar-to-org.md`
- `docs/deepseek-fetch.md`

Run `brishz-restart` after shell changes. The Elisp wrapper needs no reload.

Verified by @OpenAI on 2026-10-20.
