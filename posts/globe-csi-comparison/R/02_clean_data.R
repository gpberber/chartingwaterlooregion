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
# service "WRPS"; the charts call it "Region", the house label for Waterloo
# Region in chart text (cwr_region in R/theme_cwr.R, which this script does
# not load).
regions <- c("Canada" = "Canada", "Ontario" = "Ontario", "WRPS" = "Region")

# ---- Read (clean_names() right after every read) ---------------------------
csi_raw <- read_rds(file.path(crime_dir, "crime_severity_index.rds")) |>
  clean_names()

# Detailed violations (level 1-3 assaults and sexual assaults) are read from
# the incidents file; subtotals (assaults against a peace officer, other
# assaults, sexual violations against children) from the totals file. Both
# hold one row per region x year x violation. Each code is read from one file
# only: "assaults against a peace officer" (135) is stored in both, and
# reading it twice would double its rows.
incidents_raw <- open_dataset(file.path(crime_dir, "criminal_incidents.parquet")) |>
  filter(
    region %in% names(regions),
    ucr_code %in% c("1310", "1320", "1330", "1410", "1420", "1430")
  ) |>
  select(year, region, ucr_code, incidents, incidents_per_100k) |>
  collect() |>
  clean_names()

totals_raw <- open_dataset(file.path(crime_dir, "criminal_incident_totals.parquet")) |>
  filter(region %in% names(regions), ucr_code %in% c("130", "135", "140")) |>
  select(year, region, ucr_code, incidents, incidents_per_100k) |>
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

# ---- Assaults and sexual offences: Canada, Ontario and the Region -----------
# Short display names for each UCR code, in the Globe and Mail's wording,
# in the order the post shows them.
violation_names <- c(
  "1420" = "Assault with a weapon or causing bodily harm",
  "135"  = "Assaults against a peace officer",
  "1410" = "Aggravated assault",
  "1430" = "Assault",
  "140"  = "Other assaults",
  "130"  = "Sexual violations against children",
  "1320" = "Sexual assault with a weapon or causing bodily harm",
  "1330" = "Sexual assault",
  "1310" = "Aggravated sexual assault"
)

# The first year each offence is shown from. Sexual violations against
# children start in 2015, as in the Globe's chart, because Criminal Code
# changes make earlier years incomparable. Everything else starts with the
# data, in 1998.
first_year <- c("130" = 2015)

# Each rate is also turned into an index: 100 in the base year, so 150 means
# the rate is half as high again as it was then. That puts places with very
# different rates on one scale. The base is the first year shown, unless a
# place recorded no incidents that year: an index of zero is a division by
# zero. Then the base moves to the first year in which all three places
# recorded some, for all three places alike so their lines stay comparable.
# (Aggravated sexual assault is the one case: the Region recorded none in
# 1998 or 1999, so it is indexed to 2000.)
crime_rates <- bind_rows(incidents_raw, totals_raw) |>
  mutate(
    region = recode(region, !!!regions),
    violation = violation_names[ucr_code],
    offence_group = if_else(ucr_code %in% c("1310", "1320", "1330", "130"), "Sexual offences", "Assaults"),
    start_year = coalesce(first_year[ucr_code], min(year))
  ) |>
  filter(year >= start_year) |>
  group_by(ucr_code) |>
  mutate(
    base_year = min(year[year %in% year[incidents_per_100k > 0]
                         & !year %in% year[incidents_per_100k == 0]])
  ) |>
  group_by(ucr_code, region) |>
  mutate(index = incidents_per_100k / incidents_per_100k[year == base_year] * 100) |>
  ungroup() |>
  mutate(
    ucr_code = factor(ucr_code, levels = names(violation_names)),
    region = factor(region, levels = unname(regions))
  ) |>
  arrange(ucr_code, region, year) |>
  mutate(across(c(ucr_code, region), as.character)) |>
  select(offence_group, ucr_code, violation, region, year, incidents, incidents_per_100k, base_year, index)

# ---- Write -----------------------------------------------------------------
write_csv(csi_comparison, file.path(data_dir, "csi_comparison.csv"))
write_csv(crime_rates, file.path(data_dir, "crime_rates.csv"))
