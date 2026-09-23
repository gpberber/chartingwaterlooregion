# Working from home in Waterloo Region

Post: https://chartingwaterlooregion.ca/posts/work-from-home/

Split off the commuting post on 2026-09-23: the charts of who worked at home, in which
industries and in which districts moved here, and the commuting post kept the charts of where
people travel to work and how.

## Key terms

| Term | Definition |
|------------------------------------|------------------------------------|
| Worked at home | "Worked at home (including farms) - Persons whose job is located in the same building as their place of residence, persons who live and work on the same farm and teleworkers who spend most of their work week working at home." One category of place of work status, which "refers to whether a person worked at home, worked outside Canada, had no fixed workplace address, or worked at a specific address (usual place of work)". (Statistics Canada, Dictionary, Census of Population, 2021, "Place of work status"; the same wording is the footnote on the Place of work status dimension of table 98-10-0456-01) |
| Industry | "'Industry' refers to a generally homogeneous group of economic producing units, primarily engaged in a specific set of activities." A person who held more than one job between January 1, 2015 and May 2016 is counted in "the job held the longest during that period". The categories charted are the 20 sectors of the North American Industry Classification System (NAICS), 2012 in 2016 and 2017 in 2021; the 20 sectors are the same in both. (Statistics Canada, Dictionary, Census of Population, 2016, "Industry (based on the North American Industry Classification System [NAICS] 2012)") |
| Agriculture, forestry, fishing and hunting | "This sector comprises establishments primarily engaged in growing crops, raising animals, harvesting timber, harvesting fish and other animals from their natural habitats and providing related support activities." Establishments "primarily engaged in agricultural research or that supply veterinary services are not included in this sector". The first of the 20 NAICS sectors. (Statistics Canada, North American Industry Classification System (NAICS) Canada 2017 Version 3.0, "11 - Agriculture, forestry, fishing and hunting") |

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
| `table_98100456.csv`, `table_98100456_members.csv` | Table 98-10-0456-01, place of work status by industry sectors, occupation broad category and gender, 2021 census: the 189 cells three of the post's charts use - the Region and its seven districts at the table's industry total, and the Region at each of the 20 NAICS sectors, each as all workers and those who worked at home with the count's 95% confidence interval bounds, plus the seven districts at agriculture, forestry, fishing and hunting for the agriculture chart - fetched from Statistics Canada's web data service, with the members file giving each sector's name | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810045601) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households | 2026-09-22 |
| `table_2016321_region.csv`, `table_2016321_notes.csv` | Data table 98-400-X2016321, place of work status by industry, occupation and sex, 2016 census, cut to the Region and its seven districts at every industry (the table's own total and the 20 NAICS sectors), with occupation and sex at their totals: the total-industry rows are the work-at-home chart's 2016 figures and the sector rows are the industry chart; the notes file holds the table's data-quality note and its place-of-work-status footnote, read from the metadata in the download | [Statistics Canada](https://www150.statcan.gc.ca/n1/en/catalogue/98-400-X2016321) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households (2016) | 2026-09-22 |
| `census_tnr.csv` | Long-form and short-form total non-response rates for the Region and its seven districts, read from each area's Census Profile page by `cwr_census_tnr()` | [Statistics Canada, Census Profile 2021](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=E) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (rates about the census's own collection) | 2026-09-22 |
| `table_98100572.csv` | Table 98-10-0572-01, long-form data quality indicators for commuting: non-response and imputation rates per question, cut to the Region's eight geographies | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810057201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | Census long form, 25% sample of private households (the rates are weighted estimates) | 2026-09-22 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release (tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|------------------------------------|------------------------------------|
| **Long-form census:** every chart in this post is drawn from the census long-form questionnaire: 2021 from table 98-10-0456 and 2016 from data table 98-400-X2016321, both sent to 25% of households. Its figures are estimates for people in private households; people living in collective dwellings, such as nursing and seniors' homes and student residences, are not included. Each responding household stands for about four, its weight adjusted by Statistics Canada for households that did not respond and matched to the full census counts of age, household size and other characteristics; answers left blank are filled in from similar households. (tables 98-10-0456, 98-400-X2016321) |  |
| **Sample estimates:** because the figures are estimates, each 2021 share has a 95% confidence interval, built from the intervals Statistics Canada publishes for its counts by Statistics Canada's own method. The intervals cover sampling error and the variability from households that did not respond; they do not cover any bias if those households differ from the ones that did, answers filled in for blank questions, people the census missed or counted twice, misreported answers, or rounding. A share whose sampling error is 16.6% of its value or more - rated "use with caution" (E) or "too unreliable to be published" (F) on Statistics Canada's scale - is not reported: North Dumfries's share of workers in agriculture (E). The 2016 table publishes no intervals, so its shares have none and close rankings drawn from it may differ only by sampling error. (tables 98-10-0456, 98-400-X2016321) |  |
| **Response rates:** 1.5% to 5.0% of the households sent the long form in each district returned nothing usable (3.3% for the Region; Statistics Canada advises caution only at 50% or more), and 1.6% to 4.6% of the answers to the place-of-work question were missing or filled in. In the 2016 census, 2.9% to 4.8% of the long forms in each district returned nothing usable (4.4% for the Region). (Census Profile 2021; tables 98-10-0572, 98-400-X2016321) |  |
| **2016 census:** the 2016 figures on the chart of districts and on the industry charts come from a 2016 census data table. The table publishes no confidence intervals, so those shares have none, and no rating of their reliability; the charts say so in a note. Counts are randomly rounded to a multiple of 5 to protect confidentiality, which matters most for the smallest industries: the counts behind the district shares are all 460 or more, but management of companies and enterprises rests on 55 people working at home out of 465, where the rounding alone moves the share by about half a point either way. (table 98-400-X2016321) <!-- flags: 98-400-X2016321:notequality --> |  |
| **Men+ and Women+:** because the non-binary population is small, non-binary people are counted in the Men+ and Women+ categories. (table 98-10-0456) <!-- flags: 98-10-0456:note4 --> | Does not apply: every chart uses the total for all genders, so the Men+ and Women+ split is not used. |

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
     cwr_dictionary_readme("work-from-home") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->
```

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

``` r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("work-from-home")
```

## Reproduce

From the project root in R:

``` r
source("R/packages.R"); install_missing()          # once
source("posts/work-from-home/R/01_get_data.R")       # fills data-raw/
source("posts/work-from-home/R/02_clean_data.R")     # fills data/
```

Then `quarto render posts/work-from-home` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/work-from-home/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- Sampling: both tables come from the census long-form questionnaire, which went to a 25% sample of private households, so every figure is a weighted estimate for everyone in private households (not a count of respondents, and without people in collective dwellings), and the charts show shares. The Reliability table's "Long-form census" and "Sample estimates" rows say so in the post; the charts carry no sampling note of their own. Table 98-10-0456 publishes a 95% confidence interval for each count (its `Statistics` dimension). `R/02_clean_data.R` turns those into 95% intervals for the 2021 shares (`percent_lower`, `percent_upper`) with `R/census_ci.R`, which follows Statistics Canada's own method: each count's variance is backed out of its published modified Wilson interval (Student's t on 32 degrees of freedom), the share's standard error comes from the US Census Bureau's proportion formula, which allows for the part being counted inside the total, and the share's interval is a modified Wilson interval. `cv` is the coefficient of variation, and `quality` rates it on Statistics Canada's scale: E from 16.6%, F above 33.3%. E and F shares are blanked, as this site never uses either; the agriculture chart names North Dumfries "Not reported" with a note. Charts state the widest of their intervals in a note rather than drawing them. The 2016 data table publishes no intervals at all, so its shares have none and the charts that use them say so. Counts in both years are randomly rounded to a multiple of 5, so a share built on a count under about 50 is rough whatever its interval. Response: `data/census_quality.csv` holds each area's long-form total non-response rate, the non-response and imputation rates for the place-of-work question (from `census_tnr.csv` and table 98-10-0572) and 2016's long-form global non-response rate; all are low.
- Working at home comes from table 98-10-0456 for 2021 (189 cells fetched by coordinate, like 98-10-0464) and from 2016 census data table 98-400-X2016321 for 2016, the only 2016 table of place of work status for census subdivisions. 2016 data tables are not in Statistics Canada's table database, so `R/01_get_data.R` downloads the whole table (a 640 MB CSV in a 20 MB zip) to a temporary folder and keeps the Region's rows. Both years' shares are of everyone employed in the census week (the total of place of work status, which includes people who worked outside Canada or had no fixed workplace address). The 2016 table has no intervals; its long-form global non-response rate (GNR) is kept in `data/census_quality.csv` as `gnr_long_2016`.
- One 2021 table now serves both charts of working at home. 98-10-0467 (place of work status by education, age and gender) was used for the chart by district at first, and 98-10-0456 (by industry sector, occupation and gender) for the industry chart. Since none of education, age or gender is used in the post, the district chart was moved onto 98-10-0456 as well (Greg, 2026-09-22), which also means both years now come from tables crossed the same way. The two 2021 tables give the same district figures apart from Statistics Canada's random rounding: Wilmot is 2,970 workers at home in 98-10-0456 against 2,975 in 98-10-0467, a tenth of a percentage point, and two of the eight totals differ by 5 people. What 2016 cannot match is the crossing itself: the 2016 census has three place-of-work-status tables (98-400-X2016319, 98-400-X2016320 and 98-400-X2016321), all crossed with industry and none with education, and only 98-400-X2016321 goes below the census metropolitan area. So 2021 could be split by education, age or gender and 2016 could not, and a chart comparing the years can only show totals or the industry sectors both tables carry.
- The two industry charts are one ranking drawn twice: a lollipop of the 2016 shares, and further down a dumbbell that adds each sector's 2021 dot, the line between them and the change in points. They were one chart in two tabs until 2026-09-23, when Greg split them into separate charts and moved the 2016 one above the chart of districts. Both draw the same ten rows in the same order, ranked on 2016; each has its own x axis now that they are not shown one over the other. The ten are the sectors with the largest 2016 shares; each table's own industry total is left out, being the Region-wide share the chart above already gives. 2016 comes from data table 98-400-X2016321 and 2021 from table 98-10-0456, the two tables that cross place of work status with industry for this geography; both use the same 20 NAICS sectors (2012 in 2016, 2017 in 2021) and the sector names match exactly. 2021 publishes interval bounds and 2016 does not, so the dumbbell states the widest 2021 interval and says none is available for 2016. Two cautions on the 2016 ranking: finance and insurance (7.81%) and wholesale trade (7.79%) are separated by about two hundredths of a point, which is well inside what random rounding alone can move; and management of companies and enterprises is a small sector in 2016, 465 workers, so its 11.8% rests on a rounded count of 55.
- The agriculture chart is a different measure from the rest of the post: not where people work or how they get there, but which industry they work in. It is each district's workers in agriculture, forestry, fishing and hunting - the first NAICS sector, which takes in forestry, fishing and hunting as well as farming - as a share of all its employed residents, both taken at the place-of-work total, so a farm worker counts wherever the work is done. The figures are from table 98-10-0456 for 2021 only; 2016's table crosses industry with place of work status too, so the same chart could be drawn for 2016 if it were wanted. North Dumfries is not reported: 205 workers, an interval of 138 to 304, which Statistics Canada's scale rates E.
