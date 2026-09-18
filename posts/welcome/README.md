# Welcome to Charting Waterloo Region!

Post: https://chartingwaterlooregion.ca/posts/welcome/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_17100155.csv` | Table 17-10-0155-01, population estimates on 1 July by census subdivision, 2021 boundaries | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710015501) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (population estimates) | 2026-09-14 |
| `csd_boundaries/` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), the municipal polygons | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (boundaries) | 2026-09-14 |
| `table_98100057.csv` | Table 98-10-0057-01, household income statistics by household type, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810005701) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (medians are 100% data, from tax records) | 2026-09-15 |
| `table_98100070.csv` | Table 98-10-0070-01, income statistics for detailed income sources and taxes, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810007001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (medians are 100% data, from tax records) | 2026-09-15 |
| `table_18100004.csv` | Table 18-10-0004-01, consumer price index, monthly, not seasonally adjusted | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000401) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (price index) | 2026-09-15 |
| `table_98100041.csv` | Table 98-10-0041-01, structural type of dwelling and household size, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810004101) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-15 |
| `table_98100180_coords.csv` | Table 98-10-0180-01, mother tongue in detail, 2021 census - thirty-two cells fetched by coordinate rather than the whole table | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810018001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-15 |
| `table_32100370.csv` | Table 32-10-0370-01, cattle inventory on farms, 2021 Census of Agriculture, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210037001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_98100002.csv` | Table 98-10-0002-01, population and dwelling counts, 2021 census, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810000201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (census short form, full count) | 2026-09-18 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`. Nothing in this post needs one: every
raw input above is downloaded by `R/01_get_data.R`. The boundary file is about 150 MB, so it is
fetched once and left in `data-raw/`, which git ignores.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **Population, 2025:** the 2025 estimates are preliminary, and Statistics Canada will revise them. Every population and density figure in this post is for 2025. (table 17-10-0155-01) <!-- flags: 17-10-0155-01:note4 --> | |
| **Incomes restated in today's dollars:** from April 2020, Statistics Canada estimated some parts of the Consumer Price Index by special methods, among them child care, air travel and travel tours. The all-items index used to restate 2020 incomes includes those parts. (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 --> | |
| **How population estimates are made:** they start from the census count, adjusted for people the census missed, and add the growth since from births, deaths and migration. (table 17-10-0155-01) <!-- flags: 17-10-0155-01:note3 --> | Describes the method behind every population estimate, not a weakness in these figures. |
| **Men+ and Women+:** because the non-binary population is small, non-binary people are counted in the Men+ and Women+ categories. (table 98-10-0180) <!-- flags: 98-10-0180:note3 --> | Does not apply: every chart uses the total for all genders, so the Men+ and Women+ split is not used. |
| **Cattle, 2021:** Statistics Canada grades each township's cattle count separately, on a scale from A (excellent) to E (use with caution): Wellesley excellent (A), Woolwich very good (B), Wilmot good (C) and North Dumfries acceptable (D). (table 32-10-0370-01) <!-- flags: 32-10-0370-01:A, 32-10-0370-01:B, 32-10-0370-01:C, 32-10-0370-01:D --> | |
| **Township population, 2021:** the population beside the cattle is the 2021 census count, which is not adjusted for people the census missed. The population chart uses Statistics Canada's 2025 estimates, which are adjusted and four years later, so its township figures are higher. (table 98-10-0002-01) <!-- flags: 98-10-0002-01:note1 --> | |
| **Census of Agriculture, 2021 against earlier years:** the definition of a census farm changed in 2021, and farms were assigned to places more strictly, so Statistics Canada advises caution comparing 2021 figures with earlier Censuses of Agriculture. (table 32-10-0370-01) <!-- flags: 32-10-0370-01:note1, 32-10-0370-01:note2, 32-10-0370-01:note4 --> | Does not apply: the post uses 2021 figures only and compares them with no earlier census. |
| **Language data:** Statistics Canada's Languages Reference Guide covers the quality of the language questions and how they compare with other sources. (table 98-10-0180) <!-- flags: 98-10-0180:note7 --> | A pointer to the guide, not an issue in itself. |

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
- Most of the census tables here are cut down as they are downloaded, unlike the other raw files:
  to Waterloo Region's eight geographies, and for the CPI to the Ontario all-items index. Whole,
  they run to millions of rows and hundreds of megabytes, and nothing outside those rows is used.
- Detailed mother tongue (98-10-0180-01) is not downloaded at all. The whole table is 618 MB zipped
  - every census subdivision in Canada crossed with 538 languages - and this post needs thirty-two
  numbers from it, so `R/01_get_data.R` fetches them one cell at a time through Statistics Canada's
  coordinate service. The cell references are written out in that script beside the names Statistics
  Canada gives them.
- Income is what the 2021 census reports for 2020, restated in 2026 dollars with the Ontario
  all-items consumer price index: the average of the twelve months of 2020 against the average of
  the 2026 months published so far. The CPI is not published for Kitchener-Cambridge-Waterloo, so
  the province is the closest published basket.
- Cattle (32-10-0370-01) are published for census consolidated subdivisions, groups of
  neighbouring municipalities, not for municipalities. Each of the four townships is a
  consolidated subdivision on its own, under its own municipal code, so the township figures are
  the townships exactly (checked against the 2021 geographic attribute file, Statistics Canada
  92-151); the City of Waterloo is folded into the Kitchener one, which is why no city figures are
  used. The figures are the table's "Total cattle": calves, steers, heifers, cows and bulls.
- Sampling: none of the figures are from a sample. The census medians are published for 100% of the
  population: since 2016, census income comes from tax and benefit records linked to every
  respondent, and only averages and aggregates are restricted to the 25% long-form sample (2021
  Income Reference Guide). Household size and mother tongue are short-form questions, asked of
  everyone. The population estimates are Statistics Canada's demographic estimates, not a survey.
  The Census of Agriculture counts every farm, and the 2021 township populations beside the cattle
  are the census full count.

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
