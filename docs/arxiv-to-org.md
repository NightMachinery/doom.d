# arXiv to Org

`night/arxiv-to-org` passes clipboard text to the shell's `arxiv-to-org`
through BrishGarden, then inserts the returned Org with heading adjustment.

For paper `2505.00661`, the arXiv Atom API returned HTTP 429 while the public
abstract page was available. The repair belongs in the shell's `arxiv-json`:
failed or empty API requests now fall back to validated citation metadata from
the abstract page, retaining the JSON shape consumed by the existing converter.

The shell implementation and its offline regressions live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/arxiv.zsh`
- `python/orgmode/arxiv_html_to_json.py`
- `zshlang/tests/arxiv-to-org.py`
- `docs/arxiv-metadata.md`

Restart BrishGarden with `brishz-restart` after updating the shell definitions.
The Elisp wrapper does not need a change or reload for this repair.
