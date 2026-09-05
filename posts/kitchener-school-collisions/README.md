# Collisions near Kitchener schools

Post: https://gpberber.github.io/chartingwaterlooregion/posts/kitchener-school-collisions/

## Data sources

| File in `data-raw/` | What it is | Source | Licence | Accessed |
|---|---|---|---|---|
| `kitchener_collisions.csv` | All reported collisions in Kitchener, Jan 2015 to Jun 2022 | City of Kitchener Open Data (traffic collisions dataset) <!-- TODO: confirm URL --> | Open Government Licence – Kitchener <!-- TODO: confirm --> | Dec 2024 |
| `traffic_collisions.geojson` | Same collisions with geometry | City of Kitchener Open Data <!-- TODO: confirm URL --> | as above | Feb 2025 |
| `public_schools.htm` | List of Waterloo Region District School Board schools with addresses | WRDSB website school directory | Public web page | Jan 2025 |
| `catholic_schools.pdf` | List of Waterloo Catholic District School Board schools with addresses | WCDSB website | Public document | Dec 2024 |

All four raw files are attached to GitHub Release `data-raw-kitchener-school-collisions-v1` and are
fetched by `R/01_get_data.R`. Geocoding of school addresses used OpenStreetMap Nominatim via
the `tidygeocoder` package; the geocoded result is committed in `data/kitchener_schools_geocoded.csv`
so the post can be rendered without re-geocoding.

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()                          # once
source("posts/kitchener-school-collisions/R/01_get_data.R")        # fills data-raw/ from the release
source("posts/kitchener-school-collisions/R/02a_clean_schools.R")  # geocodes schools (needs internet)
source("posts/kitchener-school-collisions/R/02b_clean_collisions.R")
source("posts/kitchener-school-collisions/R/03_summarise.R")       # fills data/
```

Then `quarto render posts/kitchener-school-collisions` from a terminal. The cleaned files in `data/`
are committed, so steps 02 and 03 are optional unless you want to rebuild them.

## Notes

- No API keys are needed. Nominatim geocoding is free but rate-limited; `02a_clean_schools.R` takes a few minutes.
- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/kitchener-school-collisions/`).
