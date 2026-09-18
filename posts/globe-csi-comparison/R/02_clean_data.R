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

source(here("R", "data_quality.R"))   # cwr_quality_flags()

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
  select(year, region, geo_uid, ucr_code, incidents, incidents_per_100k) |>
  collect() |>
  clean_names()

totals_raw <- open_dataset(file.path(crime_dir, "criminal_incident_totals.parquet")) |>
  filter(region %in% names(regions), ucr_code %in% c("130", "135", "140")) |>
  select(year, region, geo_uid, ucr_code, incidents, incidents_per_100k) |>
  collect() |>
  clean_names()

# ---- Data-quality flags -------------------------------------------------------
# cwr_quality_flags() (R/data_quality.R) reports the quality symbols and quality
# footnotes on the figures this post uses, records them in
# data/quality_flags.csv, and hands the figures back with any this site never
# uses (E use with caution, F, x, ..) blanked to NA. The sections below carry
# on with what it returns.
#
# The crime dataset's tables are pivoted wide by statistic, so they carry no
# flags themselves. The dataset lists every flagged figure in
# quality_flags.parquet and every footnote in quality_notes.csv
# (datasets/crime/R/03_quality_flags.R); here the post's figures are put back
# in long form, one row per figure, and the flags are joined on by year, place,
# UCR code and statistic. The log is started afresh so a table this script
# stops using does not linger in it.
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# Only the three places this post uses; read as text to match the file
flags <- open_dataset(file.path(crime_dir, "quality_flags.parquet")) |>
  filter(geo_uid %in% c("11124", "35", "35291")) |>
  select(tables, year, geo_uid, ucr_code, statistic, status, symbol, terminated) |>
  collect()

notes <- read_csv(file.path(crime_dir, "quality_notes.csv"),
                  col_types = cols(.default = col_character()))

# A footnote names the place or offence it applies to in Statistics Canada's
# own words ("Ontario [35]", "Total assaults against a peace officer"), and the
# dataset has relabelled both, so their published names are put back beside
# each figure for the check to match on. The tables write a place with and
# without its code in brackets, so both spellings are kept.
statcan_places <- tribble(
  ~geo_uid, ~geo,                                                      ~geo_short,
  "11124",  "Canada",                                                  "Canada",
  "35",     "Ontario [35]",                                            "Ontario",
  "35291",  "Waterloo Region (Kitchener), Ontario, municipal [35291]", "Waterloo Region (Kitchener), Ontario, municipal"
)

statcan_offences <- tribble(
  ~ucr_code, ~violations,
  "1310",    "Sexual assault, level 3, aggravated",
  "1320",    "Sexual assault, level 2, weapon or bodily harm",
  "1330",    "Sexual assault, level 1",
  "130",     "Total sexual violations against children",
  "1410",    "Assault, level 3, aggravated",
  "1420",    "Assault, level 2, weapon or bodily harm",
  "1430",    "Assault, level 1",
  "135",     "Total assaults against a peace officer",
  "140",     "Total other assaults"
)

# Joins the flags onto figures in long form (year, geo_uid, statistic, value,
# and ucr_code where there is one) and runs the check for one source table.
check_figures <- function(figures, table_number) {
  keys <- intersect(c("year", "geo_uid", "ucr_code", "statistic"), names(figures))
  figures <- figures |>
    mutate(year = as.character(year), geo_uid = as.character(geo_uid)) |>
    left_join(flags |> filter(tables == table_number) |> select(-tables), by = keys) |>
    left_join(statcan_places, join_by(geo_uid))
  if ("ucr_code" %in% names(figures)) {
    figures <- figures |> left_join(statcan_offences, join_by(ucr_code))
  }
  figures |>
    cwr_quality_flags(
      table_number,
      notes = notes |> filter(table == table_number) |> select(-table),
      log = quality_log,
      value_cols = "value"
    ) |>
    mutate(year = as.numeric(year), geo_uid = as.numeric(geo_uid))
}

# The Crime Severity Index: Canada and Ontario come from table 35-10-0026-01,
# Waterloo Region's police service from 35-10-0188-01
csi_statistics <- c(csi = "Crime severity index", csi_violent = "Violent crime severity index")

csi_long <- csi_raw |>
  mutate(region = as.character(region)) |>
  filter(region %in% names(regions)) |>
  select(year, region, geo_uid, csi, csi_violent) |>
  pivot_longer(c(csi, csi_violent), names_to = "measure", values_to = "value") |>
  mutate(statistic = unname(csi_statistics[measure]))

csi_checked <- bind_rows(
  csi_long |> filter(region != "WRPS") |> check_figures("35-10-0026-01"),
  csi_long |> filter(region == "WRPS") |> check_figures("35-10-0188-01")
) |>
  select(year, region, measure, value) |>
  pivot_wider(names_from = measure, values_from = value)

# The incident counts and rates, from the year each offence is first shown
# (first_year, below). Canada comes from table 35-10-0177-01, Ontario and
# Waterloo Region's police service from 35-10-0180-01.
incident_statistics <- c(incidents = "Actual incidents",
                         incidents_per_100k = "Rate per 100,000 population")

# The first year each offence is shown from. Sexual violations against
# children start in 2015, as in the Globe's chart, because Criminal Code
# changes make earlier years incomparable. Everything else starts with the
# data, in 1998.
first_year <- c("130" = 2015)

incidents_long <- bind_rows(incidents_raw, totals_raw) |>
  mutate(region = as.character(region)) |>
  filter(year >= coalesce(first_year[ucr_code], min(year))) |>
  pivot_longer(c(incidents, incidents_per_100k), names_to = "measure", values_to = "value") |>
  mutate(statistic = unname(incident_statistics[measure]))

incidents_checked <- bind_rows(
  incidents_long |> filter(region == "Canada") |> check_figures("35-10-0177-01"),
  incidents_long |> filter(region != "Canada") |> check_figures("35-10-0180-01")
) |>
  select(year, region, geo_uid, ucr_code, measure, value) |>
  pivot_wider(names_from = measure, values_from = value)

# ---- Crime Severity Index: Canada, Ontario and Waterloo Region -------------
# Every year StatCan publishes (1998 on), both the overall and the violent index.
csi_comparison <- csi_checked |>
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

# (first_year, the year each offence is shown from, is set in the quality
# section above, where the figures are first cut to those years.)

# Each rate is also turned into an index: 100 in the base year, so 150 means
# the rate is half as high again as it was then. That puts places with very
# different rates on one scale. The base is the first year shown, unless a
# place recorded no incidents that year: an index of zero is a division by
# zero. Then the base moves to the first year in which all three places
# recorded some, for all three places alike so their lines stay comparable.
# (Aggravated sexual assault is the one case: the Region recorded none in
# 1998 or 1999, so it is indexed to 2000.)
crime_rates <- incidents_checked |>
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
