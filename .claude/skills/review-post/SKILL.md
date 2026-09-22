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
- The opening paragraph sets up the question the post answers. It need not give the answer away:
  posts here are often written so the reader guesses along and findings are revealed as they come.
- Headings are plain-language findings, not variable names or "Analysis".
- Jargon (CSI, UCR, clearance rate) is explained the first time it appears.
- No "we can see that", "interestingly", "it is worth noting".
- Dates and periods are explicit ("2015 to 2024", not "the study period") at least once.

## 2. Charts (apply the cwr-charts skill rules)

For every `ggplot` chunk:
- No chart still carries a placeholder title or subtitle ("Title: the finding, in one line",
  "Subtitle: what is measured..."); any title that still only describes the data - a working
  title Claude drafted, such as "Where commuters from Kitchener work" - is flagged for Greg to keep
  or rewrite as a finding (style rule 1); and no stock template text is left unwritten: the opening
  paragraph comment and "First finding as a plain-language heading" have to be replaced or
  removed before a post goes live (posts scaffolded before 2026-09-21 may also carry a
  "Second finding" heading and an empty `tbl-first` chunk; flag those too).
- No chart shows counts from long-form census data (style rule 9a): commuting, labour,
  education, housing, immigration, average or aggregate income and the other long-form topics are
  a 25% sample (median income is 100% data), so labels, axes
  and alt text show shares or rates. Check every census table the post reads, and flag any count
  that comes from one.
- Sample data is disclosed (style rule 9b). A chart from a survey (LFS, CCHS, a poll) has the
  survey's note written out in `cwr_caption(notes = )`. The census long form has no chart note:
  instead the README's Reliability table has a printed "Long-form census" row naming the tables
  the post draws from it and saying the figures are estimates for people in private households
  from the long form sent to 25% of households; flag a leftover long-form note on a chart. The
  post's Data sources prose says the data are a sample too. For long-form census shares:
  - the cleaning script works out every charted share's interval with `R/census_ci.R` where the
    table publishes bounds, and the data carry `percent_lower`, `percent_upper`, `cv` and `quality`;
  - each chart either draws the intervals (where they could change the reading) or states the widest
    in the stock note (its number from `cwr_ci_widest()`); flag a chart that does neither, and any close ranking or
    comparison the intervals do not support, especially one the prose leans on;
  - no share rated E or F (`quality`) is drawn: its row reads "Not reported" with a note key, the
    stock "Not reported" note is in the caption, and the alt text says why;
  - no count is charted unless rounded and given with its interval, and no share divides a
    long-form figure by a short-form one (style rule 9a);
  - a chart from a table with no published intervals that ranks or compares shares carries
    the stock "No confidence intervals are published" note;
  - the README's Reliability table has printed "Long-form census", "Sample estimates" and
    "Response rates" rows (second column empty), `data/census_quality.csv` exists with every
    area's long-form total non-response rate under 50%, and the Sampling note says what each
    chart does with the intervals. No interval is described as covering "sampling error only".
- Every chart note is a string written out in the post's own code (style rule 1d): flag any note
  added by an argument or looked up from a list in `R/`, which Greg could not edit in the post.
- No chart draws a figure flagged unusable in `data/quality_flags.csv` (E use with caution, F too
  unreliable, x suppressed, `..` not available) as a number - not as a zero, not joined across by a line as if it
  existed. Figures carrying Statistics Canada's D rating, "acceptable" - its lowest published
  rating, and not a caution, which is E - are charted as they are and disclosed in the README's
  Reliability table that the post prints; they need no note on the chart, so flag one that has
  gained a note nobody asked for (style rule 9c).
- Chart text shortens North Dumfries through `cwr_short_name()` (axis labels, map labels, direct
  labels, notes), and no horizontal stacked bar uses a legend where `cwr_stack_keys()` would name
  the segments on the bars (style rules 2b and 3d).
- No chart is a pie or a donut (`geom_arc_bar`, `coord_polar` on a bar, any package drawing one).
  The About page promises readers there are none; this is the check that keeps that true.
- Numbers written into a chart's `alt` text are computed from the data, not typed. Typed figures go
  stale silently, and only for readers who cannot see the chart to check them.
- The chart goes through `cwr_figure(p, "fig-<slug>", alt = )` in a chunk with
  `#| output: asis`, and `alt` is filled in with what the chart shows. Flag any chart that uses
  knitr's `fig-cap`/`fig-width` options instead (it would have no phone version). Flag a chart
  with categories on its y axis that types `height` or `phone_height`: those are set from the
  rows so bars are one thickness across the site.
  Flag a chart that wraps its category labels by hand (a `cwr_wrap()` labeller, `<br>` labels, a
  wrapping `scale_y_discrete()` in `phone =`, or `row_height`/`geom_col(width = 0.58)` set for
  wrapped names): `cwr_figure()` wraps them itself, at 30 characters on the desktop and 15 on the
  phone, and sets the row depth and bar thickness to match.
- `caption =` is optional; do not ask for one unless the chart needs a note that cannot live in
  the image. Flag a caption that only repeats the chart's own title or subtitle.
- Numbering follows the post type by itself. In a **Deep dive**, check every reference to a chart
  in the prose is written `@fig-<slug>` - flag any literal "Figure 3", which breaks silently the
  moment a chart is inserted above it. In a **Snapshot**, charts carry no number, so flag prose
  that refers to one ("as Figure 2 showed"); refer to the chart by what it showed instead.
- Both PNGs exist in `posts/<slug>/figures/` (`fig-<slug>.png` and `fig-<slug>-phone.png`).
  Read the `-phone` one scaled to about 320 px: titles wrap, nothing collides with the axis.
- One chart per chunk, no `print()` of tables in the same chunk.
- Uses colours and theme from `R/theme_cwr.R` only (flag any literal colour string, `theme_bw`, `theme_classic`, `purple4`).
- Title states the finding; subtitle has units and period; caption via `cwr_caption()` naming the
  publisher of the numbers. `credit = TRUE` (which adds "| *Charting Waterloo Region*") belongs only on a
  chart whose figures the post worked out itself - a rate, an index, a model, several sources combined.
  Flag it on a chart that just plots published figures.
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
- No chart text, alt text, note, Key term or README line calls the Region's cities and townships
  "municipalities" (style rule 2c): they are districts; "municipality" means the Region as a whole.
  Flag every instance outside a quotation from a source.
- The post has a `## Key terms` section just before `## Data sources and reliability`, printing the
  README's `## Key terms` table with `cwr_key_terms_table("<slug>")` in an `#| output: asis` chunk.
  It lists only terms used in the main body of a chart - flag a statistical term, a census geography
  term, or a term found only in a title, note or prose. Every term has a definition with its source in brackets (the render's WARNING line lists any that
  do not - Greg defines or deletes them), and every chart note, axis or legend label, alt text and
  sentence of prose that says who or what is counted agrees with those definitions. Check this line
  by line against the table: it is the check that would have caught "people who work from home are
  counted in their own municipality" in the commuting post.
- The post has a `## Data sources and reliability` section with two subheadings: `### Sources`,
  which prints the README's source table with `cwr_sources_table("<slug>")`, and `### Reliability`,
  which prints the README's Reliability table with `cwr_reliability_table("<slug>")`, each in an
  `#| output: asis` chunk rather than a copy typed into the post. A `## Other sources` section
  follows whenever anything non-data is cited. Key terms, Data sources and reliability, and Other
  sources are each a folded box, `::: {#key-terms .callout-note collapse="true"}` (ids
  `#data-sources-and-reliability`, `#other-sources`), as in the template; flag a post where they are
  still bare headings. A post still headed `## Data sources` (written before
  2026-09-18) is flagged for the new structure.
- A post that uses WRPS occurrence data (`load_wrps_occurrences()` or `wat_region_occurrences`)
  shows `wrps_disclaimer` in its Data sources section; WRPS requires it on any publication.
- The post does not override `license:` in its YAML (site default is CC BY, set in `posts/_metadata.yml`).
- The post ends with the "Reproducibility and data download" callout (download link, session
  info) and its setup chunk sources `R/data_bundle.R` and sets `data_bundle_version`. The callout
  must NOT print the data dictionary: it belongs in the post's README and the download zip only.
  Flag any `cwr_dictionary_table()` call or `tbl-dictionary` chunk left in a post.
- `data/tables.csv` lists every table the post reads (including any from `datasets/`), with
  description, source, licence and `sample` filled in; `data/dictionary.csv` describes every column,
  and names any confidence bound as one. The README's Data sources table has its Sample column filled
  for every row, and a sampled source has a Sampling note in the README's Notes.
  Run `Rscript -e 'source(here::here("R","data_bundle.R")); cwr_dictionary_check("<slug>")'`
  and report the result. The post's README has a generated `## Data dictionary` section matching
  the current data; refresh it with `cwr_dictionary_readme("<slug>")` if it is missing or stale. If the data changed since the last bundle, `data_bundle_version` must be bumped.
- **No figure flagged E, F, x or `..` is used.** Every `cwr_quality_flags()` call in
  `R/02_clean_data.R` assigns its result, and the code after it uses that result rather than the
  data it was given - a call whose result is thrown away reports E figures and then uses them.
  Flag any unassigned call, and any data the post reads from a flagged table that bypasses the
  check. Greg never uses figures flagged E ("use with caution"); the function blanks them, and
  nothing downstream may put them back (a join to the raw file, a hand-typed value, a fill).
- **Data quality.** Every source table with quality flags goes through `cwr_quality_flags()` in
  `R/02_clean_data.R`, on the rows the post keeps, and its footnotes are saved by `01_get_data.R`
  and passed as `notes =` (Statistics Canada tables always have both). Re-run the cleaning script
  and list **every** row of `data/quality_flags.csv` in the findings, with the level, the symbol or
  footnote, and which chart it reaches - Greg asked to be told about every flag that applies, so do
  not summarise them away. Each one needs a row in the README's `## Reliability` table (closely
  related flags may share one) ending with its hidden code, `<!-- flags: table:symbol -->` or
  `table:note<n>` for a footnote; the render stops on a flag with no code, and warns on a code
  the data no longer has - report either. A source with no flag column needs its own
  documented caveats in that table too. A table that was read but never checked is a finding.
  List every row whose "Left out of the post because" column is filled, so Greg sees what the post
  leaves out and why. If a flag in `data/quality_flags.csv` has no row at all, it may be one Greg
  deleted: remind him to put it back with the reason in that column rather than leave it out of
  the record.
- No `Sys.getenv()` secret is required, or the README says which and how to get it.

## 5. Metadata

- `title`, `description` (one sentence, used on the listing card), `date`, `categories`, `image` set and the file exists.
- **Search.** The title and description are Greg's prose, so these are suggestions, never edits;
  quote the current text and say what a searcher would type. Flag a title with no word a person
  would search for (a place, a topic: "population", "rent", "commuting"), and a description that
  names neither the place (Waterloo Region, or the district) nor the topic, or runs well
  outside about 120 to 160 characters (Google often shows it under the title in results, cut to
  fit). A playful title can stay playful if the description carries the search words. Report the
  character count of the description.
- **Sharing picture.** `R/seo_post_render.R` makes a 1200 x 630 sharing picture from the post's
  first chart, or the chart named by `share-figure: fig-<id>` in the YAML (`none` keeps the
  thumbnail). After rendering, look at `_site/posts/<slug>/figures/share.png` and say whether that
  chart is the one that best sells the post; if another would, suggest `share-figure:`.
- The first category is `Snapshot` or `Deep dive`, followed by 2 to 3 topic categories. A Snapshot
  is a few simple charts and basic analysis read in under 5 minutes (roughly 800 words of prose
  plus a handful of charts); anything longer or more involved is a Deep dive. Count the prose words
  outside code chunks, note the number of charts and tables, and flag a mismatch, suggesting either
  relabelling or cutting the post down.
- `draft: true` still present (flag if the user intends to publish and it is false already: fine; if publishing, it must be flipped by /publish).
- No leftover `format:` overrides that duplicate `posts/_metadata.yml`.

## 6. Safety

- Run `bash _dev/check_repo_safety.sh` after staging the post folder (`git add posts/<slug>`) and report the result; unstage afterwards if the user did not ask to commit.
