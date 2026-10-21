# Semantic Scholar to Org

`night/semantic-scholar-to-org` passes clipboard URLs to the shell's
`semantic-scholar-to-org` through BrishGarden and inserts the returned Org.

arXiv inputs are resolved to the correct Semantic Scholar paper through Semantic
Scholar's HTTP redirect. Page-data canonical-ID mappings are followed as well.
The source remains Semantic Scholar; URL identification
uses no LLM or search.

The shell wrapper delegates fetching, normalization, and rendering to the
native Go `org-paper-metadata` helper, built automatically on first use.
The old Lisp formatter and Python parser are deprecated compatibility shims.

`semantic_scholar_scraper=auto` tries public page data first, then the configured
Graph API and configured DeepSeek Fetch. HTML retrieval is disabled by default;
select `full-html2` explicitly to use it. Backend failures
are reported through `ecgray`. The first attempt needs no key.

Use `semantic_scholar_scraper=page-data` for direct retrieval without either key.
Other selections are `api` (Graph API only), `deepseek` (with page-data recovery,
no browser), and `full-html2` (HTML first, then page-data recovery).

Successful conversions require a title, authors, and a full abstract. Title-only
pages, TLDRs, social previews, and related-paper descriptions cannot satisfy
that requirement. If recovery fails, the command returns an error with no Org
output. Publication details and PDF links are retained when available; missing
citation counts are omitted and zero counts are preserved.

The drawer records successful-fetch time, key-reference count,
explicit open-access status, and a separate publication-update date when one
is supplied. Open access adds no heading tag. A supplied TLDR appears in a
plain quote block before the full abstract, inside the same folded
`@abstract` heading. A folded `@bibtex` sibling follows when a source citation
is available. Its
`:orgPaperMetadataGenerated: bibtex` property identifies tool-managed citations.
Use `--no-bibtex` to skip export and output, or `--bibtex` to enable it explicitly.
BibTeX defaults to DBLP, then DOI, then Semantic Scholar, using the resolved
paper’s exact external IDs. A published DOI takes precedence over a DBLP CoRR
preprint. Anonymous Semantic Scholar enrichment supplies the IDs and a cached
fallback citation. No title search, fuzzy matching or LLM identification is used.
DBLP direct exports can recover through configured DeepSeek Fetch; returned
BibTeX must match the requested DBLP key. `--bibtex-browser` opts into bounded
`full-html2` recovery from DBLP’s official Trier mirror as a last resort after
programmatic sources and optional DeepSeek fail; it is disabled by default.
Missing DOIs are resolved from exact DBLP keys through its official SPARQL API
before publisher export. Bot checks, errors and missing IDs fall through. Citations are never
synthesized. If every provider fails, the helper warns and omits new sections
while preserving saved citations. Stdin JSON parsing remains offline.

For existing notes, `org-paper-metadata-updater '<directory-root>'` previews
refreshes of paper headings, skipping entries fetched within seven days.
Add `--diff` for a patch or `--write` to apply changes. Loose links are ignored;
personal prose, custom properties (including `url`), and unrelated subheadings
are preserved. Legacy abstracts are refreshed without changing saved BibTeX.
The updater reports each fetch before it starts and limits retrieval to one
minute per paper by default. `--no-bibtex` removes only marked generated
citations, including from recent entries without fetching metadata. Notes or
IDs added beneath that heading survive cleanup. Old marked `@misc` citations
with the retired serializer’s key pattern are removed even from recent entries
or after a failed fetch. A successful citation export replaces them on refresh. Recognizable legacy
citation
blocks move unchanged into folded, unmarked `@bibtex` sections; those are
preserved by `--no-bibtex`.
Known venues use abbreviations such as `@ECAI`, with old generated long tags
replaced on refresh. Personal tags survive anywhere in the heading. Recognized
arXiv drawer links become versionless URLs that refer to the latest version,
including on recent entries without retrieval or a new timestamp. Heading URLs,
prose and saved BibTeX are unaffected by URL normalization.

Only public identifiers/URLs leave the machine. `--scraper page-data` selects
Semantic Scholar metadata; citation providers are independent. To confine all
requests to Semantic Scholar, add
`--bibtex-source semantic-scholar --dblp-metadata=false`. Other
citation selections are `auto`, `dblp`, and `doi`, also configurable through
`semantic_scholar_bibtex_source`. Automatic citations may contact DBLP, DOI
registration agencies and configured DeepSeek Fetch. Note text and local paths
stay local. JSON includes `bibtexSource`; Org drawers do not add that property.

DBLP publication metadata enrichment is enabled by default through its SPARQL
JSON API, independently of BibTeX. Exact DOI matches can replace a preprint key
with the published record. Available fields include canonical title and ordered
authors, publication year, venue, publication type, full proceedings title,
publisher, pages, volume/issue, series, ISBN/ISSN and a DBLP URL. SS abstract,
TLDR, counts and dates remain. Missing fields do not erase saved data; failures
warn and retain SS metadata. Use `--dblp-metadata=false` to disable enrichment,
or `semantic_scholar_dblp_metadata=n`. `--no-bibtex` keeps this enrichment active
without retrieving citation text. Recent entries retain the weekly skip.
A confirmed DBLP export bot challenge produces one notice and skips that export
for the rest of the run. SPARQL enrichment and DOI/SS citations stay active;
missing records do not disable DBLP, and genuine final failures still warn.

The updater uses the same Go package directly. Its detailed usage is in
`$NIGHTDIR/docs/org-paper-metadata-updater.md`.

The implementation, regressions, and detailed usage live under `$NIGHTDIR`:

- `zshlang/auto-load/others/scraping/semantic_scholar.zsh`
- `golang/org-paper-metadata/`
- `zshlang/tests/semantic-scholar-to-org.py`
- `docs/semantic-scholar-to-org.md`
- `docs/deepseek-fetch.md`

Run `brishz-restart` after shell changes. The Elisp wrapper needs no reload.

Verified by @OpenAI on 2026-10-21.
