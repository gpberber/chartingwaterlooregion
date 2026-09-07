---
name: review-post
description: Pre-publication review of a post - checks the writing, every chart and table against the house style, reproducibility requirements, and metadata. Use before flipping draft to false, or when asked to review a post.
argument-hint: <slug>
---

# Review post

Argument: `$ARGUMENTS` = the post slug. Read `posts/<slug>/index.qmd`, `posts/<slug>/README.md`,
and the scripts in `posts/<slug>/R/`. Render the post first (`quarto render posts/<slug> --profile draft`)
so you can compare prose against actual output in `_freeze/posts/<slug>/index/execute-results/html.json`
and look at the figures in `_freeze/posts/<slug>/index/figure-html/`.

Produce a findings list grouped by the headings below. Each finding: file and line, what is wrong,
the fix. Say "none" for a heading with no findings. Do not fix anything unless asked; the user
decides. Then offer to apply the fixes.

## 1. Writing

- Every number in the prose matches the rendered output (recompute from the data if unsure).
- The opening paragraph states the question and the answer; a reader who stops there knows the finding.
- Headings are plain-language findings, not variable names or "Analysis".
- Jargon (CSI, UCR, clearance rate) is explained the first time it appears.
- No "we can see that", "interestingly", "it is worth noting".
- Dates and periods are explicit ("2015 to 2024", not "the study period") at least once.

## 2. Charts (apply the cwr-charts skill rules)

For every `ggplot` chunk:
- Chunk label starts with `fig-`; has `fig-cap` and `fig-alt`; `fig-height`/`fig-width` set if the chart needed tuning.
- One chart per chunk, no `print()` of tables in the same chunk.
- Uses colours and theme from `R/theme_cwr.R` only (flag any literal colour string, `theme_bw`, `theme_classic`, `purple4`).
- Title states the finding; subtitle has units and period; caption via `cwr_caption()` with the source.
- Direct labels where feasible; legend only if justified; redundant axis text removed when bars are labelled.
- Focus row/series is bold or blue; comparison red; rest grey.
- `linewidth` not `size` for lines; `|>` not `%>%`; no `sapply`/`gsub`/`grepl` (stringr/purrr instead).

## 3. Tables

- `gt` only (flag `kable`, `knitr::kable`, `flextable`, raw data frames printed).
- Chunk label starts with `tbl-`, has `tbl-cap`; one table per chunk.
- Source note present; column labels are words, not snake_case.

## 4. Reproducibility

- Setup chunk has `here::i_am("posts/<slug>/index.qmd")` and sources `R/theme_cwr.R`.
- All file paths through `here()`; no absolute paths, no `setwd()`.
- Every `library()` in the post or its scripts is listed in `R/packages.R`.
- `R/01_get_data.R` recreates everything in `data-raw/` (download or `cwr_data_download()`), and
  `R/02_clean_data.R` recreates everything in `data/`.
- No file over 25 MB in `data/` unless it is in `.gitignore` and in a release.
- README lists each source with URL, licence, and access date. The licence must be the source's own
  (e.g. Statistics Canada Open Licence, Open Government Licence – Kitchener), never "CC BY" or "MIT";
  files obtained by request rather than from an open-data portal need their terms confirmed.
- A post that uses WRPS occurrence data (`load_wrps_occurrences()` or `wat_region_occurrences`)
  shows `wrps_disclaimer` in its Data and methods section; WRPS requires it on any publication.
- The post does not override `license:` in its YAML (site default is CC BY, set in `posts/_metadata.yml`).
- The post ends with the "Reproducibility and data download" callout (download link, dictionary
  table, session info) and its setup chunk sources `R/data_bundle.R` and sets `data_bundle_version`.
- `data/tables.csv` lists every table the post reads (including any from `datasets/`), with
  description, source and licence filled in; `data/dictionary.csv` describes every column.
  Run `Rscript -e 'source(here::here("R","data_bundle.R")); cwr_dictionary_check("<slug>")'`
  and report the result. If the data changed since the last bundle, `data_bundle_version` must be bumped.
- No `Sys.getenv()` secret is required, or the README says which and how to get it.

## 5. Metadata

- `title`, `description` (one sentence, used on the listing card), `date`, `categories`, `image` set and the file exists.
- The first category is `Snapshot` or `Deep dive`, followed by 2 to 3 topic categories. A Snapshot
  is a few simple charts and basic analysis read in under 6 minutes (roughly 1,000 words of prose
  plus a handful of charts); anything longer or more involved is a Deep dive. Count the prose words
  outside code chunks, note the number of charts and tables, and flag a mismatch, suggesting either
  relabelling or cutting the post down.
- `draft: true` still present (flag if the user intends to publish and it is false already: fine; if publishing, it must be flipped by /publish).
- No leftover `format:` overrides that duplicate `posts/_metadata.yml`.

## 6. Safety

- Run `bash _dev/check_repo_safety.sh` after staging the post folder (`git add posts/<slug>`) and report the result; unstage afterwards if the user did not ask to commit.
