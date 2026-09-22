# Commuting in Waterloo Region

Post: https://chartingwaterlooregion.ca/posts/commuting/

## Key terms

| Term | Definition |
|------------------------------------|------------------------------------|
| Commuters (who is counted) | "Population aged 15 years and over, in private households, with a job or absent from their job or business during the week of Sunday, May 2 to Saturday, May 8, 2021, and who reported having a usual place of work or no fixed workplace address." (2021 Census Dictionary, Main mode of commuting) The charts of where commuters work count only those "who reported having a usual place of work" (2021 Census Dictionary, Commuting destination); the chart of how they get to work also counts those with no fixed workplace address. People who worked at home or outside Canada are in neither. |
| Worked at home | "Worked at home (including farms) - Persons whose job is located in the same building as their place of residence, persons who live and work on the same farm and teleworkers who spend most of their work week working at home." One category of place of work status, which "refers to whether a person worked at home, worked outside Canada, had no fixed workplace address, or worked at a specific address (usual place of work)". (Statistics Canada, Dictionary, Census of Population, 2021, "Place of work status"; the same wording is the footnote on the Place of work status dimension of table 98-10-0467-01) |
| Home district | Workers who work in the township/city they live in. |
| Elsewhere in Region | Workers who work in a Region township/city other than their home district. |
| Outside Region | Workers who work outside the Region. |
| Car, truck or van | Travelling by car, truck or van as a driver or as a passenger. |
| Active transportation | Walking or bicycling. |

```{=html}
<!-- Only terms that appear in a chart - in its title or subtitle, or in its main body (segment
     and legend names, panel headings, axis and direct labels) - and that a reader needs defined
     to read it: never statistical terms (confidence interval, coefficient of variation), never
     census geography terms (census subdivision, census division), never a term used only in a
     note or the prose. A title or subtitle counts because the reader meets the term there before
     the chart, even when nothing inside the panel says it. Greg asks for any term he wants
     added. Quote the source's own definition and name the
     source in brackets at the end: the table's metadata, footnotes or category names, the census
     dictionary or reference guides, other official documentation. A term with no official
     definition is Greg's to define: its Definition cell is left empty, to be filled in or the row
     deleted. The post prints every row that has a definition, and the render warns about any
     that does not. Every chart note, label and sentence about who or what is counted is still
     checked against the source's definitions (cwr-charts rule 1d), whether or not the term is in
     this table. -->
```

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|------------|------------|------------|------------|------------|------------|
| `table_98100462.csv` | Table 98-10-0462-01, commuting destination by mode, age and gender, 2021 census | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-15 |
| `table_98100464.csv` | Table 98-10-0464-01, main mode of commuting by industry sectors, occupation broad category and gender, 2021 census: the 96 cells the mode chart uses (the Region and its seven districts; total, car, truck or van, public transit and active transportation; each count with its 95% confidence interval bounds), fetched from Statistics Canada's web data service | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046401) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-22 |
| `table_98100467.csv` | Table 98-10-0467-01, place of work status by highest level of education, age and gender, 2021 census: the 48 cells the work-at-home chart uses (the Region and its seven districts; all workers and those who worked at home; each count with its 95% confidence interval bounds), fetched from Statistics Canada's web data service | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810046701) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-22 |
| `table_2016321_region.csv`, `table_2016321_notes.csv` | Data table 98-400-X2016321, place of work status by industry, occupation and sex, 2016 census, cut to the Region and its seven districts at the table's totals; the notes file holds the table's data-quality note and its place-of-work-status footnote, read from the metadata in the download | [Statistics Canada](https://www150.statcan.gc.ca/n1/en/catalogue/98-400-X2016321) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households (2016) | 2026-09-22 |
| `table_98100459_region.csv`, `table_98100459_members.csv` | Table 98-10-0459-01, commuting flow from place of residence to place of work, 2021 census, cut to commutes with one end in the Region; the members file is the table's own list of places and codes | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810045901) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-17 |
| `census_tnr.csv` | Long-form and short-form total non-response rates for the Region and its seven districts, read from each area's Census Profile page by `cwr_census_tnr()` | [Statistics Canada, Census Profile 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=E) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (rates about the census's own collection) | 2026-09-22 |
| `table_98100572.csv` | Table 98-10-0572-01, long-form data quality indicators for commuting: non-response and imputation rates per question, cut to the Region's eight geographies | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810057201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households (the rates are weighted estimates) | 2026-09-22 |
| `csd_boundaries.gpkg` | 2021 census subdivision cartographic boundary file (`lcsd000b21a_e`), cut to the Region and the places its residents commute to | [Statistics Canada](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (boundaries) | 2026-09-17 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release (tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|------------------------------------|------------------------------------|
| **Long-form census:** every chart in this post is drawn from the 2021 census long-form questionnaire (tables 98-10-0462, 98-10-0464, 98-10-0467 and 98-10-0459), and the work-at-home chart's 2016 figures from the 2016 long form (table 98-400-X2016321), which also went to 25% of households. Its figures are estimates for people in private households, from the 2021 census long-form questionnaire sent to 25% of households; people living in collective dwellings, such as nursing and seniors' homes and student residences, are not included. Each responding household stands for about four, its weight adjusted by Statistics Canada for households that did not respond and matched to the full census counts of age, household size and other characteristics; answers left blank are filled in from similar households. (tables 98-10-0459, 98-10-0462, 98-10-0464, 98-10-0467, 98-400-X2016321) |  |
| **Sample estimates:** because the figures are estimates, each 2021 share worked out from tables 98-10-0462, 98-10-0464 and 98-10-0467 has a 95% confidence interval, built from the intervals Statistics Canada publishes for its counts by Statistics Canada's own method. The intervals cover sampling error and the variability from households that did not respond; they do not cover any bias if those households differ from the ones that did, answers filled in for blank questions, people the census missed or counted twice, misreported answers, or rounding. A share whose sampling error is 16.6% of its value or more - rated "use with caution" (E) or "too unreliable to be published" (F) on Statistics Canada's scale - is not reported: public transit in North Dumfries (F), Wilmot (E) and Woolwich (E), and active transportation in North Dumfries (E). Table 98-10-0459 publishes no intervals, so close rankings drawn from it may differ only by sampling error. (tables 98-10-0459, 98-10-0462, 98-10-0464, 98-10-0467) |  |
| **Response rates:** 1.5% to 5.0% of the households sent the long form in each district returned nothing usable (3.3% for the Region; Statistics Canada advises caution only at 50% or more), and 1.3% to 4.6% of the answers to the place-of-work and mode-of-commuting questions were missing or filled in. In the 2016 census, 2.9% to 4.8% of the long forms in each district returned nothing usable (4.4% for the Region). (Census Profile 2021; tables 98-10-0572, 98-400-X2016321) |  |
| **Commuting flows:** Statistics Canada randomly rounds each flow to a multiple of 5 to protect confidentiality, so small flows, and the shares worked out from them, are approximate. (table 98-10-0459) <!-- flags: 98-10-0459:note1 --> |  |
| **Not applicable:** three counts are zero (commuters from North Dumfries and Wellesley who work in another province, in table 98-10-0462, and Wellesley's transit commuters, in table 98-10-0464), and a zero count has no confidence interval, so its bounds are marked "..." (not applicable). (tables 98-10-0462, 98-10-0464) <!-- flags: 98-10-0462:..., 98-10-0464:... --> | Does not apply: a share of zero has no interval, so these bounds are never used; the zero itself is charted as published. |
| **2016 census:** the work-at-home chart's 2016 shares come from a 2016 census data table. The table publishes no confidence intervals, so those shares have none, and no rating of their reliability; the counts behind them are all 460 or more. Counts are randomly rounded to a multiple of 5 to protect confidentiality. (table 98-400-X2016321) <!-- flags: 98-400-X2016321:notequality --> |  |
| **Education:** table 98-10-0467 refers readers to the Education Reference Guide for the quality of its education figures. (table 98-10-0467) <!-- flags: 98-10-0467:note7 --> | Does not apply: the work-at-home chart uses the total for every level of education, so the education figures are not used. |
| **Men+ and Women+:** because the non-binary population is small, non-binary people are counted in the Men+ and Women+ categories. (tables 98-10-0459, 98-10-0462, 98-10-0464, 98-10-0467) <!-- flags: 98-10-0459:note3, 98-10-0462:note3, 98-10-0464:note4, 98-10-0467:note3 --> | Does not apply: every chart uses the total for all genders, so the Men+ and Women+ split is not used. |

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

## Data dictionary

```{=html}
<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("commuting") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->
```

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

``` r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("commuting")
```

## Reproduce

From the project root in R:

``` r
source("R/packages.R"); install_missing()          # once
source("posts/commuting/R/01_get_data.R")            # fills data-raw/
source("posts/commuting/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/commuting` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/commuting/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- Sampling: all three commuting tables come from the 2021 census long-form questionnaire, which went to a 25% sample of private households, so every figure is a weighted estimate for everyone in private households (not a count of respondents, and without people in collective dwellings), and the charts show shares. The Reliability table's "Long-form census" and "Sample estimates" rows say so in the post; the charts carry no sampling note of their own. Tables 98-10-0462 (destinations) and 98-10-0464 (modes) publish a 95% confidence interval for each count (their `Statistics` dimension). `R/02_clean_data.R` turns those into 95% intervals for the shares in `commuting.csv` and `commuting_mode.csv` (`percent_lower`, `percent_upper`) with `R/census_ci.R`, which follows Statistics Canada's own method: each count's variance is backed out of its published modified Wilson interval (Student's t on 32 degrees of freedom), the share's standard error comes from the US Census Bureau's proportion formula, which allows for the part being counted inside the total, and the share's interval is a modified Wilson interval. `cv` is the coefficient of variation, and `quality` rates it on Statistics Canada's scale: E from 16.6%, F above 33.3%. E and F shares are blanked, as this site never uses either; the mode chart names them "Not reported" with a note. The first chart states the widest of its intervals in a note. The intervals are least exact for very small shares and small places, and too wide near 100% where the Census Bureau formula falls back to its cautious form (Wilmot's car share). Table 98-10-0459 publishes no intervals, so the destination and feeder rankings carry a note that close shares may differ only by sampling error. Counts are also randomly rounded to a multiple of 5, so a share built on a count under about 50 is rough whatever its interval. Response: `data/census_quality.csv` holds each area's long-form total non-response rate and the non-response and imputation rates for the place-of-work and mode questions (from `census_tnr.csv` and table 98-10-0572); all are low.
- Working at home comes from table 98-10-0467 for 2021 (48 cells fetched by coordinate, like 98-10-0464) and from 2016 census data table 98-400-X2016321 for 2016, the only 2016 table of place of work status for census subdivisions. 2016 data tables are not in Statistics Canada's table database, so `R/01_get_data.R` downloads the whole table (a 640 MB CSV in a 20 MB zip) to a temporary folder and keeps the Region's eight rows. Both years' shares are of everyone employed in the census week (the total of place of work status, which includes people who worked outside Canada or had no fixed workplace address). The 2016 table has no intervals; its long-form global non-response rate (GNR) is kept in `data/census_quality.csv` as `gnr_long_2016`.
- The two work-at-home tables are not crossed with the same variables: 98-10-0467 gives place of work status by education, age and gender, while 98-400-X2016321 gives it by industry, occupation and sex. It makes no difference to the chart, which reads each table's fully aggregated cell (all industries, all occupations, both sexes in 2016; all levels of education, all ages, all genders in 2021), and both tables count the same population, the employed labour force aged 15 and over in private households. There is no 2016 equivalent crossed with education: the 2016 census has three place-of-work-status tables (98-400-X2016319, 98-400-X2016320 and 98-400-X2016321), all crossed with industry, and only 98-400-X2016321 goes below the census metropolitan area. So 2021 could be split by education, age or gender and 2016 could not, and a chart comparing the years can only show the totals.
- Mode of commuting comes from table 98-10-0464, not 98-10-0462: 98-10-0462 is crossed with commuting destination, so its mode figures cover only people with a usual place of work, while the mode question also covers people with no fixed workplace address (about 29,000 more commuters in the Region, 200,595 against 171,575). 98-10-0464 is the table Statistics Canada's commuting release in The Daily (2022-11-30) quotes. 98-10-0461 has 98-10-0462's limit, being crossed with distance to a usual place of work. 98-10-0464 has 90 million cells, so `R/01_get_data.R` fetches only the 96 it needs from the web data service, by coordinate; a zero count is not returned by the service and is filled in as zero.
- The commuting table is cut down as it is downloaded, to Waterloo Region and its seven districts. Whole, it covers every census subdivision in Canada and runs to hundreds of megabytes, and nothing outside those rows is used.
