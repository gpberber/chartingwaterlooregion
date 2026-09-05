# {{title}}

Shared dataset used by several posts. Load it in a post with:

```r
source(here::here("datasets", "{{slug}}", "R", "load.R"))
```

## Pipeline

| Script | What it does | Run when |
|---|---|---|
| `R/01_get_data.R` | Downloads raw inputs into `data-raw/` (or fetches them from a GitHub Release) | when the source updates |
| `R/02_clean_data.R` | Turns `data-raw/` into the tidy files in `data/` | after `01_get_data.R` |
| `R/helpers.R` | Functions and definitions shared by the pipeline and by posts | sourced by the others |
| `R/load.R` | Reads `data/` into named objects for a post | in every post that uses this dataset |

## Sources

| Data | Source | Table / URL | Licence |
|---|---|---|---|
| | | | |

## Files in `data/`

| File | Contents | In git? |
|---|---|---|
| | | Yes / No: GitHub Release `data-dataset-{{slug}}-v1` |

## Reproduce

```r
source("R/packages.R"); install_missing()
source("datasets/{{slug}}/R/01_get_data.R")
source("datasets/{{slug}}/R/02_clean_data.R")
```

## Versions

| Tag | Date | Change |
|---|---|---|
| `data-dataset-{{slug}}-v1` | {{date}} | First release |
