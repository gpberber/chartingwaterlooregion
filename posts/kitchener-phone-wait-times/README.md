# Kitchener phone wait times

Post: https://chartingwaterlooregion.ca/posts/kitchener-phone-wait-times/

A Snapshot post: how quickly the City of Kitchener's contact centre answers the phone, 2019 to 2023.

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|---|---|---|---|---|
| `kitchener_phone_queue_metrics.csv` | Daily phone queue metrics for the City of Kitchener contact centre, Feb 2019 to Jul 2023 | City of Kitchener Open Data (https://open-kitchenergis.opendata.arcgis.com/documents/KitchenerGIS::phone-queue-metrics/about) | Open Government Licence - The Corporation of the City of Kitchener | Feb 2025 |
| `holidays.csv` | Ontario statutory holidays | https://github.com/uWaterloo/Datasets/blob/master/Holidays/holidays.csv | See repository | Feb 2025 |

The raw metrics file is attached to GitHub Release `data-raw-kitchener-phone-wait-times-v1`; the
holidays file is downloaded directly. Both are fetched by `R/01_get_data.R`.

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()                      # once
source("posts/kitchener-phone-wait-times/R/01_get_data.R")     # fills data-raw/
source("posts/kitchener-phone-wait-times/R/02_clean_data.R")   # fills data/queue_daily.csv
```

Then `quarto render posts/kitchener-phone-wait-times` from a terminal.

## Notes

- The longer exploration of the same data is a separate R project outside this repository
  (`../Kitchener Phone Queues/`), moved out of the blog in September 2026.
- No API keys are needed.
- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/kitchener-phone-wait-times/`).
