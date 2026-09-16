# Commuting in Waterloo Region

Post: https://chartingwaterlooregion.ca/posts/commuting/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|---|---|---|---|---|
| `table_98100462.csv` | Table 98-10-0462-01, commuting destination by mode, age and gender, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | 2026-09-15 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

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
- The commuting table is cut down as it is downloaded, to Waterloo Region and its seven
  municipalities. Whole, it covers every census subdivision in Canada and runs to hundreds of
  megabytes, and nothing outside those rows is used.
