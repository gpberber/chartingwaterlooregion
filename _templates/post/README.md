# {{title}}

Post: https://chartingwaterlooregion.ca/posts/{{slug}}/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| | | | | | |

<!-- Sample: "None (full count)" for administrative data, a census short-form question or a
     full register; otherwise name the sample, e.g. "Census long form, 25% sample of households"
     or the survey and its sample size, from the survey's own documentation. This column is
     printed in the post's Data sources section, so readers see it. -->

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("{{slug}}") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("{{slug}}")
```

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
- Sampling: <!-- Which sources are samples (see the Sample column above) and what that means for
  the figures; whether the source publishes confidence intervals, and whether the charts draw them,
  state them in a note, or neither and why. "None of the data are sampled" if so. -->
