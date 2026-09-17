# 02_clean_data.R
# Turns the shared crime dataset (datasets/crime/data/) into the small tidy
# files in data/ that index.qmd reads. This post has no raw inputs of its own:
# every number comes from the crime dataset, so this script only filters and
# summarises it for the story.
# Run from the project root: source("posts/globe-csi-comparison/R/02_clean_data.R")
#
# The crime dataset's load.R is not sourced here because it reads every table,
# including the 1 GB incidents file, into memory. arrow::open_dataset() reads
# only the rows and columns asked for instead.

library(tidyverse)
library(janitor)
library(here)
library(arrow)

crime_dir <- here("datasets", "crime", "data")
data_dir <- here("posts", "globe-csi-comparison", "data")
dir.create(data_dir, showWarnings = FALSE)

# The three places compared. The dataset calls Waterloo Region's police
# service "WRPS"; the charts call it Waterloo Region.
regions <- c("Canada" = "Canada", "Ontario" = "Ontario", "WRPS" = "Waterloo Region")

# ---- Read (clean_names() right after every read) ---------------------------
csi_raw <- read_rds(file.path(crime_dir, "crime_severity_index.rds")) |>
  clean_names()

# Detailed violations (level 1-3 assaults and sexual assaults) are in the
# incidents file; subtotals (assaults against a peace officer, other assaults,
# sexual violations against children) are in the totals file. Both hold one
# row per region x year x violation.
wrps_incidents_raw <- open_dataset(file.path(crime_dir, "criminal_incidents.parquet")) |>
  filter(region == "WRPS", ucr_code %in% c("1310", "1320", "1330", "1410", "1420", "1430")) |>
  select(year, ucr_code, incidents, incidents_per_100k) |>
  collect() |>
  clean_names()

wrps_totals_raw <- open_dataset(file.path(crime_dir, "criminal_incident_totals.parquet")) |>
  filter(region == "WRPS", ucr_code %in% c("130", "135", "140")) |>
  select(year, ucr_code, incidents, incidents_per_100k) |>
  collect() |>
  clean_names()

# ---- Crime Severity Index: Canada, Ontario and Waterloo Region -------------
# Every year StatCan publishes (1998 on), both the overall and the violent index.
csi_comparison <- csi_raw |>
  mutate(region = as.character(region)) |>
  filter(region %in% names(regions)) |>
  mutate(region = recode(region, !!!regions)) |>
  select(year, region, csi, csi_violent) |>
  arrange(region, year)

# ---- Assaults and sexual offences in Waterloo Region, indexed ---------------
# Short display names for each UCR code, in the Globe and Mail's wording.
violation_names <- c(
  "1410" = "Aggravated assault",
  "1420" = "Assault with a weapon or causing bodily harm",
  "1430" = "Assault",
  "135"  = "Assaults against a peace officer",
  "140"  = "Other assaults",
  "1310" = "Aggravated sexual assault",
  "1320" = "Sexual assault with a weapon or causing bodily harm",
  "1330" = "Sexual assault",
  "130"  = "Sexual violations against children"
)

# Each rate is turned into an index: 100 in the base year, so 150 means the
# rate is half as high again as it was then. That lets offences with very
# different rates share one axis. The base is the first year in the dataset
# (1998, as in the Globe's charts) except for sexual violations against
# children, which the Globe bases on 2015 because Criminal Code changes make
# earlier years incomparable.
wrps_indexed <- bind_rows(wrps_incidents_raw, wrps_totals_raw) |>
  mutate(
    offence_group = if_else(ucr_code %in% c("1310", "1320", "1330", "130"), "Sexual offences", "Assaults"),
    violation = violation_names[ucr_code],
    base_year = if_else(ucr_code == "130", 2015, min(year))
  ) |>
  filter(year >= base_year) |>
  group_by(ucr_code) |>
  mutate(
    base_rate = incidents_per_100k[year == base_year],
    # an offence with no incidents in its base year has no index (it would be
    # a division by zero), so it is left blank and the chart leaves it out
    index = if_else(base_rate > 0, incidents_per_100k / base_rate * 100, NA_real_)
  ) |>
  ungroup() |>
  select(offence_group, year, ucr_code, violation, base_year, incidents, incidents_per_100k, index) |>
  arrange(offence_group, ucr_code, year)

assaults_indexed <- wrps_indexed |>
  filter(offence_group == "Assaults") |>
  select(-offence_group)

sexual_offences_indexed <- wrps_indexed |>
  filter(offence_group == "Sexual offences") |>
  select(-offence_group)

# ---- Write -----------------------------------------------------------------
write_csv(csi_comparison, file.path(data_dir, "csi_comparison.csv"))
write_csv(assaults_indexed, file.path(data_dir, "wrps_assaults_indexed.csv"))
write_csv(sexual_offences_indexed, file.path(data_dir, "wrps_sexual_offences_indexed.csv"))
