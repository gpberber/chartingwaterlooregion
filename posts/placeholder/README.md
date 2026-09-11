# A placeholder while the site is being built

Post: https://chartingwaterlooregion.ca/posts/placeholder/

A temporary post carrying a single chart, so the site is not empty while the first real posts are
written. Delete the whole folder once they are published.

## Data sources

| File in `data/` | What it is | Source (link) | Licence | Accessed |
|----|----|----|----|----|
| `queue_daily.csv` | Daily phone-queue metrics for the City of Kitchener contact centre, 1 March 2019 to 30 July 2023, with calendar and holiday flags | [City of Kitchener Open Data](https://open-kitchenergis.opendata.arcgis.com/documents/KitchenerGIS::phone-queue-metrics/about) | Open Government Licence - The Corporation of the City of Kitchener | Feb 2025 |

The statutory-holiday flags in that file come from the [University of Waterloo holidays
dataset](https://github.com/uWaterloo/Datasets/blob/master/Holidays/holidays.csv), licensed as
stated in that repository.

## Reproduce

`data/queue_daily.csv` is a copy of the cleaned file built by the `kitchener-phone-wait-times`
post, kept here so this placeholder is self-contained and can be deleted in one piece. To rebuild
it from the original source, run that post's scripts from the project root in R:

``` r
source("R/packages.R"); install_missing()                      # once
source("posts/kitchener-phone-wait-times/R/01_get_data.R")     # fills data-raw/
source("posts/kitchener-phone-wait-times/R/02_clean_data.R")   # fills data/queue_daily.csv
```

Then `quarto render posts/placeholder` from a terminal.

## Notes

- This post has no data download bundle, so it has no `data/tables.csv` or `data/dictionary.csv`.
- No API keys are needed.
