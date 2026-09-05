# load.R
# ---------------------------------------------------------------------------
# Loads the cleaned crime dataset into named objects for a post. One line in
# the post's setup chunk gives every crime post the same tables with the same
# names, which is what makes charts comparable across posts:
#
#   source(here::here("datasets", "crime", "R", "load.R"))
#
# Requires the files in datasets/crime/data/, all of which are committed
# (the two large Statistics Canada tables are Parquet, which keeps them small).
# Rebuild them with 01_get_data.R then 02_clean_data.R only to refresh the data.
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(arrow)

crime_data_dir <- here("datasets", "crime", "data")

# Helper so each read below stays one line
read_crime <- function(file) {
  path <- file.path(crime_data_dir, file)
  if (!file.exists(path)) {
    stop(
      "Missing ", path, "\n",
      "Run source(here::here('datasets', 'crime', 'R', '01_get_data.R')) or the ",
      "full pipeline in datasets/crime/R/ (see datasets/crime/README.md)."
    )
  }
  if (grepl("\\.parquet$", path)) read_parquet(path) else read_rds(path)
}

# ---- Criminal incidents (Statistics Canada, Uniform Crime Reporting) ------
# One row per region x year x violation. Large (about 1 GB in memory); if a
# post needs only a slice, use open_dataset() + filter() + collect() instead:
#   incidents <- open_dataset(file.path(crime_data_dir, "criminal_incidents.parquet")) |>
#     filter(region == "WRPS") |> collect()
incidents        <- read_crime("criminal_incidents.parquet")
incident_totals  <- read_crime("criminal_incident_totals.parquet")   # totals by region x year x category
incident_summary <- read_crime("criminal_incident_summary.rds")      # region x year headline rates

# ---- Crime Severity Index ---------------------------------------------------
csi <- read_crime("crime_severity_index.rds")

# ---- Victims ----------------------------------------------------------------
homicide_victims <- read_crime("homicide_victims.rds")
violent_victims  <- read_crime("violent_victims.rds")
ipv_victims      <- read_crime("family_ipv_victims.rds")

# ---- Specific offence types -------------------------------------------------
hate_crimes  <- read_crime("hate_crimes.rds")
cyber_crimes <- read_crime("cyber_crimes.rds")

# ---- Police resources ---------------------------------------------------------
personnel         <- read_crime("personnel.rds")                  # officers and civilians by service x year
police_fir        <- read_crime("police_fir.rds")                 # municipal Financial Information Return, policing lines
big_12_financials <- read_crime("big_12_financial_summary.rds")   # Big 12 spending summary

# ---- Waterloo Region occurrences (WRPS) --------------------------------------
# 3 million police-reported occurrences, 2014 onward, from the yearly WRPS public
# release. The file is 161 MB so it lives in GitHub Release data-dataset-crime-v1
# rather than in git. It is not loaded here by default; a post that needs it calls:
#   wat_region_occurrences <- load_wrps_occurrences()
# and must show `wrps_disclaimer` (defined in helpers.R) in its Data and methods section.
load_wrps_occurrences <- function() {
  path <- file.path(crime_data_dir, "wat_region_occurrences.parquet")
  if (!file.exists(path)) {
    source(here("R", "data_helpers.R"))
    cwr_data_download("crime", kind = "data", root = "datasets")
  }
  read_parquet(path)
}

# ---- Shared definitions used across crime posts ------------------------------
source(here("datasets", "crime", "R", "helpers.R"))
