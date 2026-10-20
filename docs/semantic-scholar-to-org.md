# Semantic Scholar to Org

`night/semantic-scholar-to-org` passes clipboard URLs to the shell's
`semantic-scholar-to-org` through BrishGarden and inserts the returned Org.

arXiv inputs are resolved to the correct Semantic Scholar paper through Semantic
Scholar's HTTP redirect. The source remains Semantic Scholar; URL identification
uses no LLM or search.

The shell wrapper delegates fetching, normalization, and rendering to the
native Go `org-paper-metadata` helper, built automatically on first use.
The old Lisp formatter and Python parser are deprecated compatibility shims.

`semantic_scholar_scraper=auto` tries public page data first, then the configured
Graph API, configured DeepSeek Fetch, and finally `full-html2`. Backend failures
are reported through `ecgray`. The first attempt needs no key.

Use `semantic_scholar_scraper=page-data` for direct retrieval without either key.
Other selections are `api` (Graph API only), `deepseek` (with page-data recovery,
no browser), and `full-html2` (HTML first, then page-data recovery).

Successful conversions require a title, authors, and a full abstract. Title-only
pages, TLDRs, social previews, and related-paper descriptions cannot satisfy
that requirement. If recovery fails, the command returns an error with no Org
output. Publication details and PDF links are retained when available; missing
citation counts are omitted and zero counts are preserved.

The drawer records source and successful-fetch time, key-reference count,
explicit open-access status, and a separate publication-update date when one
is supplied. Open access adds no heading tag. A supplied TLDR appears in a
labeled quote block before the full abstract, inside the same folded
`@abstract` heading.

For existing notes, `org-paper-metadata-updater '<directory-root>'` previews
refreshes of paper headings, skipping entries fetched within seven days.
Add `--diff` for a patch or `--write` to apply changes. Loose links are ignored;
personal prose, custom properties, and unrelated subheadings are preserved.
The updater uses the same Go package directly. Its detailed usage is in
`$NIGHTDIR/docs/org-paper-metadata-updater.md`.

The implementation, regressions, and detailed usage live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/semantic_scholar.zsh`
- `golang/org-paper-metadata/`
- `zshlang/tests/semantic-scholar-to-org.py`
- `docs/semantic-scholar-to-org.md`
- `docs/deepseek-fetch.md`

Run `brishz-restart` after shell changes. The Elisp wrapper needs no reload.

Verified by @OpenAI on 2026-10-20.
