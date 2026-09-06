# Kitchener phone queues

Post: https://gpberber.github.io/chartingwaterlooregion/posts/kitchener-phone-queues/

## Data sources

| File in `data-raw/` | What it is | Source | Licence | Accessed |
|---|---|---|---|---|
| `kitchener_phone_queue_metrics.csv` | Daily phone queue metrics for the City of Kitchener contact centre, Feb 2019 to Jul 2023 | City of Kitchener Open Data (phone queue metrics dataset) <!-- TODO: confirm URL --> | Open Government Licence – Kitchener <!-- TODO: confirm --> | Feb 2025 |
| `holidays.csv` | Ontario statutory holidays | https://github.com/uWaterloo/Datasets/blob/master/Holidays/holidays.csv | See repository | Feb 2025 |

The raw metrics file is attached to GitHub Release `data-raw-kitchener-phone-queues-v1`; the holidays
file is downloaded directly. Both are fetched by `R/01_get_data.R`.

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()                   # once
source("posts/kitchener-phone-queues/R/01_get_data.R")      # fills data-raw/
source("posts/kitchener-phone-queues/R/02_clean_data.R")    # fills data/queue_data_clean.csv
```

Then `quarto render posts/kitchener-phone-queues` from a terminal.

## Notes

- `R/02_explore.Rmd` and `R/03_time_series_analysis.Rmd` are exploration notebooks, not part of the site.
- No API keys are needed.
- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/kitchener-phone-queues/`).
