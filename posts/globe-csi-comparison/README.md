# Title: to be written

Post: https://chartingwaterlooregion.ca/posts/globe-csi-comparison/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|---|---|---|---|---|
| `datasets/crime/data/crime_severity_index.rds` (shared crime dataset) | Crime Severity Index, Canada and provinces, Table 35-10-0026-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510002601) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | August 2026 |
| `datasets/crime/data/crime_severity_index.rds` (shared crime dataset) | Crime Severity Index, Ontario police services (Waterloo Regional Police Service), Table 35-10-0188-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018801) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | August 2026 |
| `datasets/crime/data/criminal_incidents.parquet`, `criminal_incident_totals.parquet` (shared crime dataset) | Incident rates by violation, Ontario police services (Waterloo Regional Police Service), Table 35-10-0180-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | August 2026 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("globe-csi-comparison") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("globe-csi-comparison")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/globe-csi-comparison/R/01_get_data.R")            # fills data-raw/
source("posts/globe-csi-comparison/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/globe-csi-comparison` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/globe-csi-comparison/`).
- This post has no raw files of its own. Its data comes from the shared crime dataset (`datasets/crime/`, version `data-dataset-crime-v1` plus the 2026-09-17 CSI change), and `R/02_clean_data.R` filters and summarises it into `data/`. `R/01_get_data.R` has nothing to fetch.
- No API keys are needed. <!-- If one is, say which and how to get it. -->
