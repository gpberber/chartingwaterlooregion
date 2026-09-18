# Commuting in Waterloo Region

Post: https://chartingwaterlooregion.ca/posts/commuting/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_98100462.csv` | Table 98-10-0462-01, commuting destination by mode, age and gender, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of households | 2026-09-15 |
| `table_98100459_region.csv`, `table_98100459_members.csv` | Table 98-10-0459-01, commuting flow from place of residence to place of work, 2021 census, cut to commutes with one end in the Region; the members file is the table's own list of places and codes | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810045901) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of households | 2026-09-17 |
| `csd_boundaries.gpkg` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), cut to the Region and the places its residents commute to | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (boundaries) | 2026-09-17 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **Commuting flows:** Statistics Canada randomly rounds each flow to a multiple of 5 to protect confidentiality, so small flows, and the shares worked out from them, are approximate. (table 98-10-0459) <!-- flags: 98-10-0459:note1 --> | |
| **Men+ and Women+:** because the non-binary population is small, non-binary people are counted in the Men+ and Women+ categories. (tables 98-10-0459, 98-10-0462) <!-- flags: 98-10-0459:note3, 98-10-0462:note3 --> | Does not apply: every chart uses the total for all genders, so the Men+ and Women+ split is not used. |

<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one municipality and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("commuting") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("commuting")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/commuting/R/01_get_data.R")            # fills data-raw/
source("posts/commuting/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/commuting` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/commuting/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- Sampling: both commuting tables come from the 2021 census long-form questionnaire, which went to
  a 25% sample of households, so every figure is an estimate and the charts show shares rather than
  counts. Every chart says so in a note. Table 98-10-0462 publishes a 95% confidence interval for
  each count (its `Statistics` dimension); the cleaning script keeps only the count, and no chart
  draws an interval, because the charts show shares worked out here, and Statistics Canada publishes
  no interval for those. Table 98-10-0459 publishes no intervals. Counts are also randomly rounded
  to a multiple of 5.
- The commuting table is cut down as it is downloaded, to Waterloo Region and its seven
  municipalities. Whole, it covers every census subdivision in Canada and runs to hundreds of
  megabytes, and nothing outside those rows is used.
