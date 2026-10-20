# Semantic Scholar to Org

`night/semantic-scholar-to-org` passes clipboard URLs to the shell's
`semantic-scholar-to-org` through BrishGarden and inserts the returned Org.

The shell scraper works without `SemanticScholar_API_key`: it uses `full-html2`
and validates citation metadata, JSON-LD, and visible fields. If that fails and
`deepseek_api_key` is available, it tries DeepSeek Fetch. Full public paper URLs
with their title slug and ID work better than bare-ID URLs in the live check.

When a Semantic Scholar key is configured, the API can enrich scraped metadata;
API failures retain the scrape. Missing citation counts are omitted. DeepSeek
may expose a short summary rather than the complete abstract, and the generated
heading labels it `@summary` accordingly.

The implementation, regressions, and detailed usage live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/semantic_scholar.zsh`
- `python/orgmode/semantic_scholar_html_to_json.py`
- `commonlisp/semantic_scholar_json_to_org.lisp`
- `zshlang/tests/semantic-scholar-to-org.py`
- `docs/semantic-scholar-to-org.md`
- `docs/deepseek-fetch.md`

Run `brishz-restart` after shell changes. The Elisp wrapper needs no reload.

Verified by @OpenAI on 2026-10-20.
