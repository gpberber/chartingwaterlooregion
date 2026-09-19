# Title: to be written

Post: https://chartingwaterlooregion.ca/posts/globe-csi-comparison/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `datasets/crime/data/crime_severity_index.rds` (shared crime dataset) | Crime Severity Index, Canada and provinces, Table 35-10-0026-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510002601) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (police-reported incidents) | August 2026 |
| `datasets/crime/data/crime_severity_index.rds` (shared crime dataset) | Crime Severity Index, Ontario police services (Waterloo Regional Police Service), Table 35-10-0188-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018801) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (police-reported incidents) | August 2026 |
| `datasets/crime/data/criminal_incidents.parquet`, `criminal_incident_totals.parquet` (shared crime dataset) | Incident rates by violation, Canada, Table 35-10-0177-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510017701) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (police-reported incidents) | August 2026 |
| `datasets/crime/data/criminal_incidents.parquet`, `criminal_incident_totals.parquet` (shared crime dataset) | Incident rates by violation, Ontario and Ontario police services (Waterloo Regional Police Service), Table 35-10-0180-01 | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510018001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (police-reported incidents) | August 2026 |
| None: figures typed into `R/02_clean_data.R` from the PDF report | Share of Kitchener residents saying each type of crime increased over the past year, from *Crime Perception in Ontario Cities* (December 30, 2025), Kitchener pages 28-35 | [Liaison Strategies](https://liaison.ca) | No licence stated; published figures cited with attribution | Automated phone (IVR) survey of 800 Kitchener residents, October 22-23, 2025, weighted to 2021 census targets | 2026-09-18 |

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **2018 change in how police record crimes:** from January 2018, police record some incidents, notably sexual assaults and family violence, under a more victim-centred standard for deciding whether a reported crime took place. Statistics Canada advises caution comparing 2018 and later years with earlier ones. It applies to every series in this post, for all three places. (tables 35-10-0026-01, 35-10-0177-01, 35-10-0180-01, 35-10-0188-01) <!-- flags: 35-10-0026-01:note44, 35-10-0188-01:note23, 35-10-0177-01:note97, 35-10-0180-01:note69 --> | |
| **Assaults against a peace officer, 2010:** new offence codes introduced in 2009 caused some other assaults to be recorded as assaults against a peace officer in 2010. Statistics Canada advises caution comparing other years with 2010. (tables 35-10-0177-01, 35-10-0180-01) <!-- flags: 35-10-0177-01:note35, 35-10-0180-01:note17 --> | |
| **Ontario, 1998 to 2000:** some Ontario police services over-counted less serious incidents before 2001. The published figures have been adjusted for it, which lowers Ontario's crime rate in those years by about 2% to 3% a year. (tables 35-10-0026-01, 35-10-0180-01) <!-- flags: 35-10-0026-01:note7, 35-10-0180-01:note26 --> | |
| **Estimated detail:** for police services that reported only summary counts, the counts for individual offences were estimated from the pattern in services that reported in detail. (tables 35-10-0026-01, 35-10-0177-01, 35-10-0180-01, 35-10-0188-01) <!-- flags: 35-10-0026-01:note1, 35-10-0188-01:note1, 35-10-0177-01:note1, 35-10-0180-01:note1 --> | |
| **Revisions:** each year's release revises the previous year's figures. In 2012, population estimates from 2004 on were revised, which changes rates, and Montreal's child pornography counts for 2008 to 2011 were corrected, which changes Canada's totals. (tables 35-10-0026-01, 35-10-0177-01, 35-10-0180-01, 35-10-0188-01) <!-- flags: 35-10-0026-01:note2, 35-10-0188-01:note2, 35-10-0177-01:note2, 35-10-0180-01:note2, 35-10-0026-01:note33, 35-10-0188-01:note14, 35-10-0177-01:note47, 35-10-0180-01:note27, 35-10-0026-01:note34, 35-10-0177-01:note50 --> | |
| **Canada, 2020 on:** Canada's counts include incidents reported by the Canadian Forces Military Police, which are not assigned to any province, so they are slightly more than the provinces' counts added together. (table 35-10-0177-01) <!-- flags: 35-10-0177-01:note108 --> | |
| **Small police services:** the Crime Severity Index is not published for police services covering fewer than 1,000 people, and should be used with caution below 5,000. (tables 35-10-0026-01, 35-10-0188-01) <!-- flags: 35-10-0026-01:note21, 35-10-0188-01:note8 --> | Does not apply: Canada, Ontario and Waterloo Region's police service each cover far more than 5,000 people. |
| **What the Crime Severity Index measures:** each offence is weighted by how serious it is, from the sentences courts hand down, so the index reflects severity as well as volume. (tables 35-10-0026-01, 35-10-0188-01) <!-- flags: 35-10-0026-01:note20, 35-10-0188-01:note7 --> | Describes what the index is, not a weakness in these figures. |
| **Kitchener survey margin of error:** Liaison Strategies publishes one margin of error for its full Kitchener sample of 800, plus or minus 3.46 percentage points 19 times out of 20, and none for each answer. That is the margin for an answer given by half the sample, the widest there is, so for shares near 65% it slightly overstates the uncertainty. The chart draws it on every share. (Liaison Strategies, Crime Perception in Ontario Cities) | |

<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one municipality and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("globe-csi-comparison") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("globe-csi-comparison")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/globe-csi-comparison/R/01_get_data.R")            # fills data-raw/
source("posts/globe-csi-comparison/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/globe-csi-comparison` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/globe-csi-comparison/`).
- This post has no raw files of its own. Apart from the Kitchener survey figures, typed in from Liaison Strategies' PDF report, its data comes from the shared crime dataset (`datasets/crime/`, version `data-dataset-crime-v1` plus the 2026-09-17 CSI change), and `R/02_clean_data.R` filters and summarises it into `data/`. `R/01_get_data.R` has nothing to fetch.
- Sampling: the Statistics Canada tables are police-reported counts, not samples. The Kitchener perception figures come from Liaison Strategies' automated phone survey of 800 residents (October 22-23, 2025), weighted to 2021 census targets. Liaison publishes a 95% margin of error of plus or minus 3.46 points for the full sample, and the perception chart draws it as a bar on every share and states it in a note.
- No API keys are needed. <!-- If one is, say which and how to get it. -->
