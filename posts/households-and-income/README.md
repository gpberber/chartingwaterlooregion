# Households and income

Post: https://chartingwaterlooregion.ca/posts/households-and-income/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_98100057.csv` | Table 98-10-0057-01, household income statistics by household type, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810005701) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (medians are 100% data, from tax records) | 2026-09-15 |
| `table_98100070.csv` | Table 98-10-0070-01, income statistics for detailed income sources and taxes, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810007001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (medians are 100% data, from tax records) | 2026-09-15 |
| `table_18100004.csv` | Table 18-10-0004-01, consumer price index, monthly, not seasonally adjusted | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000401) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (price index) | 2026-09-15 |
| `table_98100041.csv` | Table 98-10-0041-01, structural type of dwelling and household size, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810004101) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-15 |

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
| **Incomes restated in today's dollars:** from April 2020, Statistics Canada estimated some parts of the Consumer Price Index by special methods, among them child care, air travel and travel tours. The all-items index used to restate 2020 incomes includes those parts. (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 --> | |

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
     cwr_dictionary_readme("households-and-income") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("households-and-income")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/households-and-income/R/01_get_data.R")            # fills data-raw/
source("posts/households-and-income/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/households-and-income` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/households-and-income/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- The charts were first drawn for the welcome post and moved here on 2026-09-19, with their data
  and scripts.
- The census tables are cut down as they are downloaded: to Waterloo Region's eight geographies,
  and for the CPI to the Ontario all-items index. Whole, they run to millions of rows and hundreds
  of megabytes, and nothing outside those rows is used.
- Income is what the 2021 census reports for 2020, restated in 2026 dollars with the Ontario
  all-items consumer price index: the average of the twelve months of 2020 against the average of
  the 2026 months published so far. The CPI is not published for Kitchener-Cambridge-Waterloo, so
  the province is the closest published basket.
- Sampling: none of the figures are from a sample. The census medians are published for 100% of the
  population: since 2016, census income comes from tax and benefit records linked to every
  respondent, and only averages and aggregates are restricted to the 25% long-form sample (2021
  Income Reference Guide). Household size and dwelling type are short-form questions, asked of
  everyone. The CPI is a price index, not a survey of people.
