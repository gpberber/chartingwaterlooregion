# Welcome to Charting Waterloo Region!

Post: https://chartingwaterlooregion.ca/posts/welcome/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Accessed |
|---|---|---|---|---|
| `table_17100155.csv` | Table 17-10-0155-01, population estimates on 1 July by census subdivision, 2021 boundaries | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015501) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | 2026-09-14 |
| `csd_boundaries/` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), the municipal polygons | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | 2026-09-14 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`. Nothing in this post needs one: every
raw input above is downloaded by `R/01_get_data.R`. The boundary file is about 150 MB, so it is
fetched once and left in `data-raw/`, which git ignores.

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/welcome/R/01_get_data.R")            # fills data-raw/
source("posts/welcome/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/welcome` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/welcome/`).
- No API keys are needed.
- Area is Statistics Canada's published 2021 census land area (the `LANDAREA` field of the
  boundary file), which excludes inland water. Density is that area divided into the 2025
  population estimate, so it is people per square kilometre of land.

## Data dictionary

Every column of every table in `data/tables.csv`. Types and example values are read from the data itself; descriptions and units come from `data/dictionary.csv`. **This section is generated - edit `data/dictionary.csv`, not the table below**, then run `cwr_dictionary_readme("welcome")` from `R/data_bundle.R` (building the download bundle does it too). The same dictionary ships as `data_dictionary.csv` inside the bundle.

| Table | Column | Type | Description | Units | Values |
|---|---|---|---|---|---|
| districts | csduid | number | Statistics Canada census subdivision identifier, seven digits; the first four (3530) are the census division code for Waterloo Region |  | 3530004 to 3530035 |
| districts | district | text | Name of the municipality |  | Kitchener; Cambridge; Waterloo; ... (7 distinct values) |
| districts | district_type | text | City or Township, from the census subdivision type code (CY or TP) |  | City; Township |
| districts | population | number | Estimated resident population on 1 July 2025 | people | 12413 to 323917 |
| districts | area_sq_km | number | Land area published by Statistics Canada in the 2021 census boundary file; excludes inland water | square kilometres | 64.056 to 326.5574 |
| districts | density_per_sq_km | number | population divided by area_sq_km, so people per square kilometre of land | people per square kilometre | 44.69285 to 2367.556 |
| districts | label_room_km | number | Radius of the largest circle that fits inside the municipality, a measure of how much room a map label has there | kilometres | 3.088863 to 8.220127 |
| districts | label_lon | number | Longitude of the centre of that largest inscribed circle, used to place the map label | decimal degrees (EPSG:4326) | -80.71725 to -80.33542 |
| districts | label_lat | number | Latitude of the centre of that largest inscribed circle, used to place the map label | decimal degrees (EPSG:4326) | 43.32466 to 43.58204 |
