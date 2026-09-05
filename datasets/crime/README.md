# Crime dataset

Shared data on crime, policing, and police spending for Waterloo Region compared with
Canada, Ontario, and Ontario's twelve largest municipal police services ("Big 12").
Several posts draw on it; none of them cleans the data itself.

Load it in a post with:

```r
source(here::here("datasets", "crime", "R", "load.R"))
```

## Pipeline

| Script | What it does | Run when |
|---|---|---|
| `R/00_setup.R` | Loads the packages the pipeline needs | sourced by the others |
| `R/01_get_data.R` | Downloads Statistics Canada tables (via `cansim`) and Ontario Financial Information Returns into `data-raw/`; reads the WRPS occurrence exports | Once a year, after the annual data release (usually summer) |
| `R/01b_get_ucr_codes.R` | Builds the UCR violation-code lookup from the two Statistics Canada reference PDFs in `data-raw/ucr_codes/` | When the UCR code manual changes |
| `R/02_clean_data.R` | Turns everything in `data-raw/` into the tidy files in `data/` | After `01_get_data.R` |
| `R/helpers.R` | Functions and definitions shared by the pipeline and by posts (date parsing, Waterloo Region geographies) | sourced by the others |
| `R/load.R` | Reads `data/` into named objects for a post | in every crime post |
| `R/exploration/` | Working notebooks used while developing posts; not rendered on the site | as needed |

## Sources

| Data | Source | Table / URL | Licence |
|---|---|---|---|
| Criminal incidents, Canada | Statistics Canada | 35-10-0177-01 | Statistics Canada Open Licence |
| Criminal incidents, Ontario police services | Statistics Canada | 35-10-0180-01 | Statistics Canada Open Licence |
| Crime Severity Index, CMAs and provinces | Statistics Canada | 35-10-0026-01 | Statistics Canada Open Licence |
| Crime Severity Index, Ontario police services | Statistics Canada | 35-10-0188-01 | Statistics Canada Open Licence |
| Hate crimes | Statistics Canada | 35-10-0191-01 | Statistics Canada Open Licence |
| Cybercrime | Statistics Canada | 35-10-0002-01 | Statistics Canada Open Licence |
| Homicide victims | Statistics Canada | 35-10-0071-01, 35-10-0068-01 | Statistics Canada Open Licence |
| Victims of violent crime by age and gender | Statistics Canada | 35-10-0049-01, 35-10-0050-01 | Statistics Canada Open Licence |
| Family and intimate-partner violence victims | Statistics Canada | 35-10-0200-01, 35-10-0202-01 | Statistics Canada Open Licence |
| Police personnel | Statistics Canada | 35-10-0077-01, 35-10-0076-01 | Statistics Canada Open Licence |
| UCR violation codes | Statistics Canada, CCJCSS reference PDFs | `data-raw/ucr_codes/` | Statistics Canada Open Licence |
| Municipal Financial Information Returns, 2000 to 2025 | Ontario Ministry of Municipal Affairs and Housing | https://efis.fma.csc.gov.on.ca/fir/ | Open Government Licence – Ontario <!-- TODO: confirm --> |
| WRPS occurrence data (Excel exports) | Waterloo Regional Police Service, obtained by request | `data-raw/raw_occurrence_data_files/` | **Terms not confirmed.** Not shared until they are. |

## Files in `data/`

| File | Contents | In git? |
|---|---|---|
| `criminal_incidents.parquet` | Region x year x violation incident counts and rates (475,000 rows; Parquet keeps it at 5 MB) | Yes |
| `criminal_incident_totals.parquet` | Totals by region x year x violation category | Yes |
| `criminal_incident_summary.rds` | Region x year headline rates | Yes |
| `crime_severity_index.rds` | CSI by region x year | Yes |
| `homicide_victims.rds`, `violent_victims.rds`, `family_ipv_victims.rds` | Victim counts | Yes |
| `hate_crimes.rds`, `cyber_crimes.rds` | Offence-type counts | Yes |
| `personnel.rds` | Officers and civilians by service x year | Yes |
| `police_fir.rds`, `big_12_financial_summary.rds` | Policing lines from municipal financial returns | Yes |
| `wat_region_occurrences.rds` | WRPS occurrences, cleaned | No: local only until terms confirmed |
| `ucr_codes.xlsx`, `wrps_expenditures.csv` | Small lookups | Yes |

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()                       # once
source("datasets/crime/R/01_get_data.R")                        # downloads raw data (several GB, slow)
source("datasets/crime/R/02_clean_data.R")                      # rebuilds data/
```

Every cleaned file except the WRPS-derived one is committed, so a post can be rendered straight
from a clone without running the pipeline. `01_get_data.R` downloads several gigabytes of raw
tables and takes a while; only run it to refresh the data.

## Versions

| Date | Change |
|---|---|
| 2026-09-05 | Statistics Canada tables as downloaded August 2026; Financial Information Returns 2000 to 2025 |

When the data is refreshed, add a row here and mention the date in any post that re-renders.
