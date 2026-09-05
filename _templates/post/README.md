# {{title}}

Post: https://gpberber.github.io/chartingwaterlooregion/posts/{{slug}}/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|---|---|---|---|---|
| | | | | |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/{{slug}}/R/01_get_data.R")            # fills data-raw/
source("posts/{{slug}}/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/{{slug}}` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/{{slug}}/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
