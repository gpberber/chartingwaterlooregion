---

editor: 
  markdown: 
    wrap: 72
---

# Welcome to Charting Waterloo Region!

Post: https://chartingwaterlooregion.ca/posts/welcome/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|------------|------------|------------|------------|------------|------------|
| `table_17100155.csv` | Table 17-10-0155-01, population estimates on 1 July by census subdivision, 2021 boundaries | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015501) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (population estimates) | 2026-09-14 |
| `csd_boundaries/` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), the municipal polygons | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (boundaries) | 2026-09-14 |
| `table_32100370.csv` | Table 32-10-0370-01, cattle inventory on farms, 2021 Census of Agriculture, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210037001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_98100002.csv` | Table 98-10-0002-01, population and dwelling counts, 2021 census, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810000201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-18 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release (tag shown in the table) and fetched by `R/01_get_data.R`. Nothing in this post needs one: every raw input above is downloaded by `R/01_get_data.R`. The boundary file is about 150 MB, so it is fetched once and left in `data-raw/`, which git ignores.

## Reliability

| Issue | Left out of the post because |
|------------------------------------|------------------------------------|
| **Population, 2025:** the 2025 estimates are preliminary, and Statistics Canada will revise them. Every population and density figure in this post is for 2025. (table 17-10-0155-01) <!-- flags: 17-10-0155-01:note4 --> | Disclosed as "population estimate" in chart's subtitle. |
| **How population estimates are made:** they start from the census count, adjusted for people the census missed, and add the growth since from births, deaths and migration. (table 17-10-0155-01) <!-- flags: 17-10-0155-01:note3 --> | Describes the method behind every population estimate, not a weakness in these figures. |
| **Cattle counts, 2021:** Statistics Canada grades the quality of each township's cattle count separately, on a scale from A (excellent) to E (use with caution): Wellesley - excellent (A), Woolwich - very good (B), Wilmot - good (C) and North Dumfries - acceptable (D). (table 32-10-0370-01) <!-- flags: 32-10-0370-01:A, 32-10-0370-01:B, 32-10-0370-01:C, 32-10-0370-01:D --> |  |
| **Township population, 2021:** the population is the 2021 census count, which is not adjusted for people the census missed. (table 98-10-0002-01) <!-- flags: 98-10-0002-01:note1 --> |  |
| **Census of Agriculture, 2021 against earlier years:** the definition of a census farm changed in 2021, and farms were assigned to places more strictly, so Statistics Canada advises caution comparing 2021 figures with earlier Censuses of Agriculture. (table 32-10-0370-01) <!-- flags: 32-10-0370-01:note1, 32-10-0370-01:note2, 32-10-0370-01:note4 --> | Does not apply: the post uses 2021 figures only and compares them with no earlier census. |

```{=html}
<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one district and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->
```

## Reproduce

From the project root in R:

``` r
source("R/packages.R"); install_missing()          # once
source("posts/welcome/R/01_get_data.R")            # fills data-raw/
source("posts/welcome/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/welcome` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/welcome/`).
- No API keys are needed.
- Area is Statistics Canada's published 2021 census land area (the `LANDAREA` field of the boundary file), which excludes inland water. Density is that area divided into the 2025 population estimate, so it is people per square kilometre of land.
- The cattle and census population tables are cut to Waterloo Region as they are downloaded, unlike the other raw files; nothing outside those rows is used.
- Cattle (32-10-0370-01) are published for census consolidated subdivisions, groups of neighbouring census subdivisions, not for single districts. Each of the four townships is a consolidated subdivision on its own, under its own census subdivision code, so the township figures are the townships exactly (checked against the 2021 geographic attribute file, Statistics Canada 92-151); the City of Waterloo is folded into the Kitchener one, which is why no city figures are used. The figures are the table's "Total cattle": calves, steers, heifers, cows and bulls.
- Sampling: none of the figures are from a sample. The population estimates are Statistics Canada's demographic estimates, not a survey. The Census of Agriculture counts every farm, and the 2021 township populations beside the cattle are the census full count.

## Data dictionary

Every column of every table in `data/tables.csv`. Types and example values are read from the data itself; descriptions and units come from `data/dictionary.csv`. **This section is generated - edit `data/dictionary.csv`, not the table below**, then run `cwr_dictionary_readme("welcome")` from `R/data_bundle.R` (building the download bundle does it too). The same dictionary ships as `data_dictionary.csv` inside the bundle.

| Table | Column | Type | Description | Units | Values |
|---|---|---|---|---|---|
| districts | csduid | number | Statistics Canada census subdivision identifier, seven digits; the first four (3530) are the census division code for Waterloo Region |  | 3530004 to 3530035 |
| districts | district | text | Name of the census subdivision |  | Kitchener; Cambridge; Waterloo; ... (7 distinct values) |
| districts | district_type | text | City or Township, from the census subdivision type code (CY or TP) |  | City; Township |
| districts | population | number | Estimated resident population on 1 July 2025 | people | 12413 to 323917 |
| districts | area_sq_km | number | Land area published by Statistics Canada in the 2021 census boundary file; excludes inland water | square kilometres | 64.056 to 326.5574 |
| districts | density_per_sq_km | number | population divided by area_sq_km, so people per square kilometre of land | people per square kilometre | 44.69285 to 2367.556 |
| districts | label_room_km | number | Radius of the largest circle that fits inside the district, a measure of how much room a map label has there | kilometres | 3.088863 to 8.220127 |
| districts | label_lon | number | Longitude of the centre of that largest inscribed circle, used to place the map label | decimal degrees (EPSG:4326) | -80.71725 to -80.33542 |
| districts | label_lat | number | Latitude of the centre of that largest inscribed circle, used to place the map label | decimal degrees (EPSG:4326) | 43.32466 to 43.58204 |
| cattle | district | text | Name of the township |  | North Dumfries; Wellesley; Wilmot; ... (4 distinct values) |
| cattle | cattle | number | Cattle of every kind on farms in the township on 11 May 2021 - calves, steers, heifers, cows and bulls | head of cattle | 4612 to 34818 |
| cattle | cattle_status | text | Statistics Canada's quality rating for that cattle count: A excellent, B very good, C good, D acceptable |  | D; A; C; ... (4 distinct values) |
| cattle | population | number | Population counted in the township by the 2021 census on 11 May 2021 | people | 10619 to 26999 |
