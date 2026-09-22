# Language

Post: https://chartingwaterlooregion.ca/posts/language/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_98100180_coords.csv` | Table 98-10-0180-01, mother tongue in detail, 2021 census - thirty-two cells fetched by coordinate rather than the whole table | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810018001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-15 |

<!-- Sample: "None (full count)" for administrative data, a census short-form question or a
     full register; otherwise name the sample, e.g. "Census long form, 25% sample of households"
     or the survey and its sample size, from the survey's own documentation. This column is
     printed in the post's Data sources section, so readers see it. -->

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`. Nothing in this post needs one: every
raw input above is downloaded by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **Men+ and Women+:** because the non-binary population is small, non-binary people are counted in the Men+ and Women+ categories. (table 98-10-0180) <!-- flags: 98-10-0180:note3 --> | Does not apply: every chart uses the total for all genders, so the Men+ and Women+ split is not used. |
| **Language data:** Statistics Canada's Languages Reference Guide covers the quality of the language questions and how they compare with other sources. (table 98-10-0180) <!-- flags: 98-10-0180:note7 --> | A pointer to the guide, not an issue in itself. |

<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one district and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     Then end the Issue cell with a hidden code for every flag the row covers, for example
       ... (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 -->
     A symbol is table:symbol (17-10-0155-01:E), a footnote is table:note plus its number
     (98-10-0459:note1), several are comma-separated. Readers never see the code. The post
     will not render while any flag in data/quality_flags.csv lacks a row with its code, and a
     row left out of the post with a reason still counts.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("language") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("language")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/language/R/01_get_data.R")            # fills data-raw/
source("posts/language/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/language` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/language/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- The charts were first drawn for the welcome post and moved here on 2026-09-19, with their data
  and scripts.
- Detailed mother tongue (98-10-0180-01) is not downloaded at all. The whole table is 618 MB zipped
  - every census subdivision in Canada crossed with 538 languages - and this post needs thirty-two
  numbers from it, so `R/01_get_data.R` fetches them one cell at a time through Statistics Canada's
  coordinate service. The cell references are written out in that script beside the names Statistics
  Canada gives them.
- Sampling: none of the figures are from a sample. Mother tongue is a short-form question, asked of
  everyone.
