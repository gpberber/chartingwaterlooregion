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
| **Sample estimates:** the commuting questions are on the census long-form questionnaire, which went to one household in four, so every figure is an estimate. Where Statistics Canada publishes a 95% confidence interval for a count, the interval for a share worked out from it is approximated with the US Census Bureau's formula for a proportion; a share whose sampling error is more than a third of its value is marked as unreliable. These intervals cover sampling error only, not non-response or rounding, and are least exact for very small shares and small places. No intervals are published for the commuting flows, so close rankings there may be sampling error. (tables 98-10-0459, 98-10-0462) | |
| **Commuting flows:** Statistics Canada randomly rounds each flow to a multiple of 5 to protect confidentiality, so small flows, and the shares worked out from them, are approximate. (table 98-10-0459) <!-- flags: 98-10-0459:note1 --> | |
| **Not applicable:** three counts are zero (commuters from North Dumfries and Wellesley who work in another province, and Wellesley's transit commuters), and a zero count has no confidence interval, so its bounds are marked "..." (not applicable). (table 98-10-0462) <!-- flags: 98-10-0462:... --> | Does not apply: a share of zero has no interval, so these bounds are never used; the zero itself is charted as published. |
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
  each count (its `Statistics` dimension). `R/02_clean_data.R` turns those
  into approximate 95% intervals for the shares in `commuting.csv` and `commuting_mode.csv`
  (`percent_lower`, `percent_upper`): each count's standard error is backed out of its published
  interval, and the share's is worked out with the US Census Bureau's proportion formula, which
  allows for the part being counted inside the total. `cv` is the coefficient of variation, and
  `unreliable` marks a share whose CV is over 33%. The intervals cover sampling error only, not
  non-response, imputation or rounding, and are weakest for very small and very large shares.
  The first chart states the widest of its intervals in a note; the mode chart marks its unreliable
  shares (transit in North Dumfries and Woolwich), which are kept because they are realistic.
  Table 98-10-0459 publishes no intervals, so the destination and feeder rankings carry a note that
  close shares may differ only by sampling error. The Reliability table's "Sample estimates" row
  is the post's methodology note. The method is in `R/census_ci.R`.
  Counts are also randomly rounded to a multiple of 5, so a share built on a count under about 50
  is rough whatever its interval.
- The commuting table is cut down as it is downloaded, to Waterloo Region and its seven
  municipalities. Whole, it covers every census subdivision in Canada and runs to hundreds of
  megabytes, and nothing outside those rows is used.
