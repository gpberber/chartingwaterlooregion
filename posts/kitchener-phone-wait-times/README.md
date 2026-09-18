# Kitchener phone wait times

Post: https://chartingwaterlooregion.ca/posts/kitchener-phone-wait-times/

A Snapshot post: how quickly the City of Kitchener's contact centre answers the phone, 2019 to 2023.

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|----|----|----|----|----|
| `kitchener_phone_queue_metrics.csv` | Daily phone queue metrics for the City of Kitchener contact centre, Feb 2019 to Jul 2023 | [City of Kitchener Open Data](https://open-kitchenergis.opendata.arcgis.com/documents/KitchenerGIS::phone-queue-metrics/about) | Open Government Licence - The Corporation of the City of Kitchener | Feb 2025 |
| `holidays.csv` | Ontario statutory holidays | [University of Waterloo](https://github.com/uWaterloo/Datasets/blob/master/Holidays/holidays.csv) | See repository | Feb 2025 |

The raw metrics file is attached to GitHub Release `data-raw-kitchener-phone-wait-times-v1`; the holidays file is downloaded directly. Both are fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|

<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one municipality and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Reproduce

From the project root in R:

``` r
source("R/packages.R"); install_missing()                      # once
source("posts/kitchener-phone-wait-times/R/01_get_data.R")     # fills data-raw/
source("posts/kitchener-phone-wait-times/R/02_clean_data.R")   # fills data/queue_daily.csv
```

Then `quarto render posts/kitchener-phone-wait-times` from a terminal.

## Notes

- The longer exploration of the same data is a separate R project outside this repository (`../Kitchener Phone Queues/`), moved out of the blog in September 2026.
- No API keys are needed.
- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/kitchener-phone-wait-times/`).

## Data dictionary

Every column of every table in `data/tables.csv`. Types and example values are read from the data itself; descriptions and units come from `data/dictionary.csv`. **This section is generated - edit `data/dictionary.csv`, not the table below**, then run `cwr_dictionary_readme("kitchener-phone-wait-times")` from `R/data_bundle.R` (building the download bundle does it too). The same dictionary ships as `data_dictionary.csv` inside the bundle.

| Table | Column | Type | Description | Units | Values |
|---|---|---|---|---|---|
| queue_daily | date | date | Calendar date | YYYY-MM-DD | 2019-03-01 to 2023-07-30 |
| queue_daily | year | number | Calendar year |  | 2019 to 2023 |
| queue_daily | month | text | Three-letter month abbreviation (Jan to Dec) |  | Mar; Apr; May; ... (12 distinct values) |
| queue_daily | year_month | date | First day of the month the date falls in | YYYY-MM-DD | 2019-03-01 to 2023-07-01 |
| queue_daily | day_of_week | text | Three-letter day of the week (Mon to Sun) |  | Fri; Sat; Sun; ... (7 distinct values) |
| queue_daily | holiday | true/false | TRUE if the date is an Ontario statutory holiday |  | TRUE / FALSE |
| queue_daily | weekend_or_holiday | true/false | TRUE if the date is a Saturday or Sunday or a statutory holiday |  | TRUE / FALSE |
| queue_daily | calls_presented | number | Calls that entered the queue | calls | 24 to 1925 |
| queue_daily | avg_queue_time | number | Average time callers spent waiting in the queue (answered and abandoned calls) | seconds | 5 to 832 |
| queue_daily | max_queue_time | number | Longest single wait in the queue that day | seconds | 10 to 5908 |
| queue_daily | calls_handled | number | Calls answered by an agent | calls | 11 to 1835 |
| queue_daily | avg_speed_of_answer | number | Average time answered calls waited before an agent picked up | seconds | 4 to 1263 |
| queue_daily | avg_handle_time | number | Average length of an answered call from answer to hang-up including hold time | seconds | 51 to 231 |
| queue_daily | max_handle_time | number | Longest single call that day | seconds | 79 to 5001 |
| queue_daily | calls_abandoned | number | Calls where the caller hung up while waiting in the queue | calls | 0 to 480 |
| queue_daily | target_seconds | number | The service-level target the City measures against (30 on every day) | seconds | 30 to 30 |
| queue_daily | answered_within_target | number | Calls answered within the 30-second target | calls | 10 to 413 |
| queue_daily | abandoned_within_target | number | Calls abandoned before the 30-second target had elapsed | calls | 0 to 322 |
| queue_daily | pct_handled_within_target | number | answered_within_target as a share of calls_handled | percent (0 to 100) | 14.9 to 100 |
| queue_daily | pct_within_target_excl_abandoned | number | answered_within_target as a share of calls_presented minus calls_abandoned | percent (0 to 100) | 8.8 to 100 |
| queue_daily | pct_within_target_abandoned_positive | number | answered_within_target plus abandoned_within_target as a share of calls_presented | percent (0 to 100) | 15.8 to 100 |
| queue_daily | pct_answered_within_target | number | answered_within_target as a share of calls_presented; the measure used in the post, where an abandoned call counts as a miss | percent (0 to 100) | 8.2 to 100 |
| queue_daily | percentage_of_calls_handled | number | calls_handled as a share of calls_presented | percent (0 to 100) | 45.8 to 100 |
| queue_daily | percentage_of_calls_abandoned | number | calls_abandoned as a share of calls_presented | percent (0 to 100) | 0 to 54.2 |
