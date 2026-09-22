# Commuting in Waterloo Region

Post: https://chartingwaterlooregion.ca/posts/commuting/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_98100462.csv` | Table 98-10-0462-01, commuting destination by mode, age and gender, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-15 |
| `table_98100459_region.csv`, `table_98100459_members.csv` | Table 98-10-0459-01, commuting flow from place of residence to place of work, 2021 census, cut to commutes with one end in the Region; the members file is the table's own list of places and codes | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810045901) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-17 |
| `census_tnr.csv` | Long-form and short-form total non-response rates for the Region and its seven municipalities, read from each area's Census Profile page by `cwr_census_tnr()` | [Statistics Canada, Census Profile 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=E) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (rates about the census's own collection) | 2026-09-22 |
| `table_98100572.csv` | Table 98-10-0572-01, long-form data quality indicators for commuting: non-response and imputation rates per question, cut to the Region's eight geographies | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810057201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households (the rates are weighted estimates) | 2026-09-22 |
| `csd_boundaries.gpkg` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), cut to the Region and the places its residents commute to | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (boundaries) | 2026-09-17 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **Long-form census:** every chart in this post is drawn from the 2021 census long-form questionnaire (tables 98-10-0462 and 98-10-0459). Its figures are estimates for people in private households, from the 2021 census long-form questionnaire sent to 25% of households; people living in collective dwellings, such as nursing and seniors' homes and student residences, are not included. Each responding household stands for about four, its weight adjusted by Statistics Canada for households that did not respond and matched to the full census counts of age, household size and other characteristics; answers left blank are filled in from similar households. (tables 98-10-0459, 98-10-0462) | |
| **Sample estimates:** because the figures are estimates, each share worked out from table 98-10-0462 has a 95% confidence interval, built from the intervals Statistics Canada publishes for its counts by Statistics Canada's own method. The intervals cover sampling error and the variability from households that did not respond; they do not cover any bias if those households differ from the ones that did, answers filled in for blank questions, people the census missed or counted twice, misreported answers, or rounding. A share whose sampling error is 16.6% of its value or more - rated "use with caution" (E) or "too unreliable to be published" (F) on Statistics Canada's scale - is not reported: public transit in North Dumfries (F), Wilmot (E) and Woolwich (E). Table 98-10-0459 publishes no intervals, so close rankings drawn from it may differ only by sampling error. (tables 98-10-0459, 98-10-0462) | |
| **Response rates:** 1.5% to 5.0% of the households sent the long form in each municipality returned nothing usable (3.3% for the Region; Statistics Canada advises caution only at 50% or more), and 1.3% to 4.6% of the answers to the place-of-work and mode-of-commuting questions were missing or filled in. (Census Profile 2021; table 98-10-0572) | |
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
  a 25% sample of private households, so every figure is a weighted estimate for everyone in
  private households (not a count of respondents, and without people in collective dwellings), and
  the charts show shares. The Reliability table's "Long-form census" and "Sample estimates" rows
  say so in the post; the charts carry no sampling note of their own. Table 98-10-0462 publishes
  a 95% confidence interval for each count (its `Statistics` dimension). `R/02_clean_data.R` turns
  those into 95% intervals for the shares in `commuting.csv` and `commuting_mode.csv`
  (`percent_lower`, `percent_upper`) with `R/census_ci.R`, which follows Statistics Canada's own
  method: each count's variance is backed out of its published modified Wilson interval (Student's
  t on 32 degrees of freedom), the share's standard error comes from the US Census Bureau's
  proportion formula, which allows for the part being counted inside the total, and the share's
  interval is a modified Wilson interval. `cv` is the coefficient of variation, and `quality` rates
  it on Statistics Canada's scale: E from 16.6%, F above 33.3%. E and F shares are blanked, as this
  site never uses either; the mode chart names them "Not reported" with a note. The first chart states
  the widest of its intervals in a note. The intervals are least exact for very small shares and
  small places, and too wide near 100% where the Census Bureau formula falls back to its cautious
  form (Wilmot's car share). Table 98-10-0459 publishes no intervals, so the destination and feeder
  rankings carry a note that close shares may differ only by sampling error. Counts are also
  randomly rounded to a multiple of 5, so a share built on a count under about 50 is rough whatever
  its interval. Response: `data/census_quality.csv` holds each area's long-form total non-response
  rate and the non-response and imputation rates for the place-of-work and mode questions (from
  `census_tnr.csv` and table 98-10-0572); all are low.
- The commuting table is cut down as it is downloaded, to Waterloo Region and its seven
  municipalities. Whole, it covers every census subdivision in Canada and runs to hundreds of
  megabytes, and nothing outside those rows is used.
