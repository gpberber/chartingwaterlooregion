# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# the Vital Statistics page (index.qmd) reads, one file per chart. Run from the
# project root, after 01_get_data.R:
#   Rscript vital-statistics/R/02_clean_data.R
#
# Every file has a `geo` column holding "Region", "Ontario" or "Canada". The
# charts call Waterloo Region "Region" through `cwr_region` in R/theme_cwr.R;
# this script does not load that file, so it types the word. Where the figure
# is for the Kitchener - Cambridge - Waterloo census metropolitan area (CMA),
# that is recoded to "Region" too, and the chart carries the CMA note
# (cwr-charts rule 1b): the CMA is six of the Region's seven districts, all but
# Wellesley Township.
#
# The page shows five years of each series: the latest 60 months of a monthly
# one and the latest five figures of an annual one.

here::i_am("vital-statistics/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir <- here("vital-statistics", "data-raw")
data_dir <- here("vital-statistics", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Data-quality flags ------------------------------------------------------
# cwr_quality_flags() reports the quality symbols (E use with caution, F too
# unreliable, x suppressed...) and quality footnotes on the rows this script
# keeps, and records them in data/quality_flags.csv. It also hands back those
# rows with every figure this site never uses (E, F, x, ..) blanked to NA, so
# the script carries on with what it returns. The log is started afresh so a
# table this script stops using does not linger in it.
source(here("R", "data_quality.R"))
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# Statistics Canada sends some names with non-breaking spaces in them (table
# 33-10-0270 does), which look like spaces but do not match one. This turns
# them into ordinary spaces, trimmed.
tidy_names <- function(x) str_squish(str_replace_all(x, " ", " "))

# Reads a table saved by 01_get_data.R and its footnotes, runs the quality
# check on the rows `keep` selects, and returns those rows with clean names.
read_table <- function(file_stem, keep = \(d) d) {
  notes <- read_csv(file.path(raw_dir, str_c(file_stem, "_notes.csv")),
                    col_types = cols(.default = col_character())) |>
    mutate(across(everything(), tidy_names))
  raw <- read_csv(file.path(raw_dir, str_c(file_stem, ".csv")),
                  col_types = cols(VALUE = col_double(), REF_DATE = col_date(),
                                   .default = col_character())) |>
    mutate(across(where(is.character), tidy_names))
  raw |>
    keep() |>
    cwr_quality_flags(first(pull(raw, table)), notes = notes, log = quality_log) |>
    clean_names()
}

# Every source names the three places its own way. This puts them all on to
# the three the charts use, in the order they are drawn (the Region last, so
# its line sits on top).
places <- c("Canada", "Ontario", "Region")
recode_geo <- function(geo) {
  case_when(
    str_detect(geo, "Kitchener|^Waterloo, Ontario$|^35541$") ~ "Region",
    # "All census metropolitan areas and census agglomerations, Ontario" too
    str_detect(geo, "^Ontario|, Ontario$|^35$") ~ "Ontario",
    str_detect(geo, "^Canada$|, Canada$|^01$") ~ "Canada"
  ) |>
    factor(levels = places)
}

# The latest n months (or years) of each place's series
latest <- function(data, n, date = date) {
  data |>
    group_by(geo, .add = TRUE) |>
    slice_max({{ date }}, n = n) |>
    ungroup() |>
    arrange(geo, {{ date }})
}

# Sums a monthly series over the 12 months ending in each month; NA until 12
# months are available, and NA if any of the 12 is missing
sum_12 <- function(x) slider::slide_dbl(x, sum, .before = 11, .complete = TRUE)

# A 95% confidence interval is the estimate plus or minus 1.96 standard errors
z_95 <- qnorm(0.975)

# ---- 1. Unemployment and participation rates ----------------------------------------
# Labour Force Survey, three-month moving average, seasonally adjusted: the
# published rate and its published standard error, turned into a 95% interval.
lfs <- read_table("table_14100459") |>
  mutate(geo = recode_geo(geo)) |>
  select(geo, date = ref_date, measure = labour_force_characteristics, statistics, value)

lfs_rate <- function(which) {
  lfs |>
    filter(measure == which, statistics %in% c("Estimate", "Standard error of estimate")) |>
    pivot_wider(names_from = statistics, values_from = value) |>
    rename(percent = Estimate, se = `Standard error of estimate`) |>
    mutate(percent_lower = percent - z_95 * se, percent_upper = percent + z_95 * se) |>
    select(geo, date, percent, percent_lower, percent_upper) |>
    latest(60)
}

unemployment_rate <- lfs_rate("Unemployment rate")
write_csv(unemployment_rate, file.path(data_dir, "unemployment_rate.csv"))

participation_rate <- lfs_rate("Participation rate")
write_csv(participation_rate, file.path(data_dir, "participation_rate.csv"))

# ---- 2. Employment, change from a year earlier ------------------------------------
# The percentage change in employment from the same month a year before. The
# survey publishes the standard error of that year-over-year change (in
# thousands of people, like the estimate); dividing it by the year-earlier
# level puts it in percentage points.
employment_change <- lfs |>
  filter(measure == "Employment", statistics %in% c("Estimate", "Standard error of year-over-year change")) |>
  pivot_wider(names_from = statistics, values_from = value) |>
  rename(employment = Estimate, se_change = `Standard error of year-over-year change`) |>
  arrange(geo, date) |>
  group_by(geo) |>
  mutate(
    year_earlier = lag(employment, 12),
    percent = (employment / year_earlier - 1) * 100,
    percent_lower = percent - z_95 * se_change / year_earlier * 100,
    percent_upper = percent + z_95 * se_change / year_earlier * 100
  ) |>
  ungroup() |>
  filter(!is.na(percent)) |>
  select(geo, date, percent, percent_lower, percent_upper) |>
  latest(60)
write_csv(employment_change, file.path(data_dir, "employment_change.csv"))

# ---- 3. Unemployment and participation rates by age -----------------------------------
# For the CMA the survey publishes three-month moving averages not adjusted
# for the seasons, by age (14-10-0458). It has no ten-year age groups; 15 to
# 24, 25 to 54 and 55 to 64 cover 15 to 64 without a gap or an overlap.
# Ontario and Canada are published monthly (14-10-0017), so their three-month
# averages are worked out here the way Statistics Canada works out its own:
# the unemployed in the three months over the labour force in the same three
# months, and the labour force over the population for participation. That
# table has no 55 to 64 group, so its 55 to 59 and 60 to 64 counts are added
# together first. The lines rise and fall with the seasons (students looking
# for summer jobs, above all), so they are compared with each other, not
# month to month.
age_levels <- c("15 to 24 years", "25 to 54 years", "55 to 64 years")

by_age_cma <- read_table("table_14100458") |>
  mutate(
    geo = recode_geo(geo), date = ref_date, age = age_group,
    measure = labour_force_characteristics, percent = value,
    .keep = "none"
  )

# The sum over the three months ending in each month; NA until three are in
sum_3 <- function(x) slider::slide_dbl(x, sum, .before = 2, .complete = TRUE)

by_age_provinces <- read_table("table_14100017") |>
  mutate(
    geo = recode_geo(geo), date = ref_date,
    age = if_else(age_group %in% c("55 to 59 years", "60 to 64 years"), "55 to 64 years", age_group),
    characteristic = labour_force_characteristics, value,
    .keep = "none"
  ) |>
  group_by(geo, date, age, characteristic) |>
  summarise(value = sum(value), .groups = "drop") |>
  pivot_wider(names_from = characteristic, values_from = value) |>
  arrange(geo, age, date) |>
  group_by(geo, age) |>
  mutate(
    `Unemployment rate` = round(sum_3(Unemployment) / sum_3(`Labour force`) * 100, 1),
    `Participation rate` = round(sum_3(`Labour force`) / sum_3(Population) * 100, 1)
  ) |>
  ungroup() |>
  pivot_longer(c(`Unemployment rate`, `Participation rate`), names_to = "measure", values_to = "percent") |>
  filter(!is.na(percent)) |>
  select(geo, date, age, measure, percent)

rates_by_age <- bind_rows(by_age_cma, by_age_provinces) |>
  mutate(geo = factor(geo, levels = places), age = factor(age, levels = age_levels)) |>
  group_by(measure, age) |>
  latest(60) |>
  arrange(measure, age, geo, date)
write_csv(rates_by_age, file.path(data_dir, "rates_by_age.csv"))

# ---- 4. Employment Insurance regular beneficiaries -----------------------------------
# Beneficiaries per 100 people in the labour force, so the three places can
# be compared. Both series are seasonally adjusted; the labour force is the
# survey's three-month moving average from table 14-10-0459 (in thousands).
# Alternatives: per 100 people aged 15 to 64 from the population estimates, or
# the census-division table 14-10-0323 for the whole Region (not seasonally
# adjusted, and with no labour force to divide by).
labour_force <- lfs |>
  filter(measure == "Labour force", statistics == "Estimate") |>
  select(geo, date, labour_force = value)

ei_beneficiaries <- read_table("table_14100454") |>
  mutate(geo = recode_geo(geo), date = ref_date, beneficiaries = value, .keep = "none") |>
  inner_join(labour_force, join_by(geo, date)) |>
  mutate(per_100 = beneficiaries / (labour_force * 1000) * 100) |>
  latest(60)
write_csv(ei_beneficiaries, file.path(data_dir, "ei_beneficiaries.csv"))

# ---- 5. Business openings and closures ------------------------------------------------
# The number of openings and closures as published (the page charts the
# Region's), and each as a percentage of active businesses in the same month,
# so places of different sizes can be compared.
business_dynamics <- read_table("table_33100270") |>
  mutate(geo = recode_geo(geo), date = ref_date, measure = business_dynamics_measure, value, .keep = "none") |>
  pivot_wider(names_from = measure, values_from = value) |>
  pivot_longer(c(`Opening businesses`, `Closing businesses`), names_to = "measure", values_to = "businesses") |>
  mutate(
    measure = factor(if_else(measure == "Opening businesses", "Openings", "Closures"),
                     levels = c("Openings", "Closures")),
    percent = businesses / `Active businesses` * 100
  ) |>
  select(geo, date, measure, businesses, active_businesses = `Active businesses`, percent) |>
  group_by(measure) |>
  latest(60)
write_csv(business_dynamics, file.path(data_dir, "business_dynamics.csv"))

# ---- Population, for the charts per person ----------------------------------------------
# July 1 estimates. A month is divided by the estimate for July 1 of its own
# year; months after the latest estimate use the latest one, since the next
# is not published until the following winter.
population_total <- bind_rows(
  # the CMA, from the CMA table
  read_table("table_17100148", \(d) filter(d, str_detect(GEO, "Kitchener"))),
  # Ontario and Canada, from the provincial table
  read_table("table_17100005")
) |>
  mutate(geo = recode_geo(geo), year = year(ref_date), population = value, .keep = "none")

# CMHC surveys starts every month only in census metropolitan areas and census
# agglomerations (centres of 10,000 people or more), so starts are divided by
# the population living in them
population_centres <- read_table("table_17100148") |>
  mutate(geo = recode_geo(geo), year = year(ref_date), population = value, .keep = "none")

per_person <- function(data, population) {
  data |>
    mutate(year = year(date)) |>
    left_join(population, join_by(geo, year)) |>
    arrange(geo, date) |>
    group_by(geo) |>
    fill(population) |>
    ungroup() |>
    select(-year)
}

# ---- 6. Building permits ---------------------------------------------------------------
# The value of permits issued over the past 12 months (unadjusted, current
# dollars; the table is in thousands), per person. The 12-month total evens
# out the seasons and the odd month with one very large project.
building_permits <- read_table("table_34100292") |>
  mutate(
    geo = recode_geo(geo), date = ref_date,
    type = if_else(type_of_building == "Total residential", "Residential", "Non-residential"),
    value = value * 1000,
    .keep = "none"
  ) |>
  arrange(geo, type, date) |>
  group_by(geo, type) |>
  mutate(value_12 = sum_12(value)) |>
  ungroup() |>
  filter(!is.na(value_12)) |>
  per_person(population_total) |>
  mutate(
    dollars_per_person = value_12 / population,
    type = factor(type, levels = c("Residential", "Non-residential"))
  ) |>
  group_by(type) |>
  latest(60)
write_csv(building_permits, file.path(data_dir, "building_permits.csv"))

# ---- 7. New Housing Price Index --------------------------------------------------------
# As published: builders' prices for new homes, December 2016 = 100.
new_housing_price_index <- read_table("table_18100205") |>
  mutate(geo = recode_geo(geo), date = ref_date, index = value, .keep = "none") |>
  latest(60)
write_csv(new_housing_price_index, file.path(data_dir, "new_housing_price_index.csv"))

# ---- CMHC -----------------------------------------------------------------------------------
# CMHC figures carry no Statistics Canada symbols. The Rental Market Survey
# rates each figure's reliability in its Quality column, and its legend is
# given here so cwr_quality_flags() can apply it: "Fair (Use with Caution)" is
# treated like Statistics Canada's E and never used; a figure CMHC does not
# publish comes with no value. The Starts and Completions Survey is a count of
# every start, not a sample, and has no ratings.
cmhc_legend <- tribble(
  ~flag,                     ~meaning,                                               ~level,
  "Fair (Use with Caution)", "use with caution (never used on this site)",           "unusable",
  "Poor (Do Not Use)",       "too unreliable to be published",                       "unusable",
  "Good",                    "data reliability: good",                               "note",
  "Very good",               "data reliability: very good",                          "note",
  "Excellent",               "data reliability: excellent",                          "note"
)

read_cmhc <- function(file) {
  read_csv(file.path(raw_dir, file), col_types = cols(Value = col_double(), Date = col_date(),
                                                      .default = col_character())) |>
    clean_names() |>
    mutate(geo = recode_geo(geo_uid))
}

# ---- 8. Housing starts by dwelling type -----------------------------------------------
# Starts over the past 12 months per 1,000 people living in the centres CMHC
# surveys monthly. Twelve months smooth out the jumps when one large apartment
# building breaks ground.
housing_starts <- read_cmhc("cmhc_starts_dwelling_type.csv") |>
  filter(dwelling_type != "All") |>
  mutate(dwelling_type = str_replace(dwelling_type, "Semi-Detached", "Semi-detached")) |>
  arrange(geo, dwelling_type, date) |>
  group_by(geo, dwelling_type) |>
  mutate(starts_12 = sum_12(value)) |>
  ungroup() |>
  filter(!is.na(starts_12)) |>
  select(geo, date, dwelling_type, starts_12) |>
  per_person(population_centres) |>
  mutate(
    per_1000 = starts_12 / population * 1000,
    dwelling_type = factor(dwelling_type, levels = c("Single", "Semi-detached", "Row", "Apartment"))
  ) |>
  group_by(dwelling_type) |>
  latest(60)
write_csv(housing_starts, file.path(data_dir, "housing_starts.csv"))

# ---- 9. Housing starts by intended market ----------------------------------------------
# Each market's share of the starts over the past 12 months. Co-ops are a
# fraction of a per cent and are left out, so the three do not quite add to 100.
starts_intended_market <- read_cmhc("cmhc_starts_intended_market.csv") |>
  arrange(geo, intended_market, date) |>
  group_by(geo, intended_market) |>
  mutate(starts_12 = sum_12(value)) |>
  ungroup() |>
  filter(!is.na(starts_12)) |>
  select(geo, date, intended_market, starts_12) |>
  pivot_wider(names_from = intended_market, values_from = starts_12) |>
  pivot_longer(c(Rental, Condo, Homeowner), names_to = "intended_market", values_to = "starts_12") |>
  mutate(
    percent = starts_12 / All * 100,
    intended_market = factor(intended_market, levels = c("Rental", "Condo", "Homeowner"))
  ) |>
  select(geo, date, intended_market, starts_12, all_starts_12 = All, percent) |>
  group_by(intended_market) |>
  latest(60)
write_csv(starts_intended_market, file.path(data_dir, "starts_intended_market.csv"))

# ---- 10 to 12. Rental Market Survey -------------------------------------------------------
# One figure a year, from the October survey, for purpose-built rental
# apartments and rows, for all units ("Total") and by unit size. The quality
# check runs on the rows kept - the latest five surveys, for the unit sizes the
# page charts. A size is charted only where the Region has at least four usable
# figures of the five (Greg, 2026-09-26): its smaller samples leave studio
# vacancy rates, and studio and 3+ bedroom rent changes, mostly rated "Fair"
# or unpublished, so those are not kept.
read_rms <- function(file, bedrooms, table) {
  read_cmhc(file) |>
    filter(bedroom_type %in% bedrooms) |>
    mutate(year = year(date)) |>
    group_by(bedroom_type) |>   # five surveys for each unit size
    latest(5, year) |>
    cwr_quality_flags(table, flag_cols = "quality", legend = cmhc_legend, log = quality_log) |>
    select(geo, year, bedroom_type, value, quality)
}

vacancy_rate <- read_rms("cmhc_vacancy_rate.csv", c("Total", "1 Bedroom", "2 Bedroom", "3 Bedroom +"),
                         "CMHC RMS vacancy rate")
write_csv(vacancy_rate, file.path(data_dir, "vacancy_rate.csv"))

average_rent <- read_rms("cmhc_average_rent.csv", c("Total", "Studio", "1 Bedroom", "2 Bedroom", "3 Bedroom +"),
                         "CMHC RMS average rent")
write_csv(average_rent, file.path(data_dir, "average_rent.csv"))

# The same-sample change: the rise in average rent in units surveyed in both
# years, so new buildings entering the survey at higher rents do not inflate it
rent_change <- read_rms("cmhc_average_rent_change.csv", c("Total", "1 Bedroom", "2 Bedroom"),
                        "CMHC RMS rent change")
write_csv(rent_change, file.path(data_dir, "rent_change.csv"))

# ---- 13. Population growth -----------------------------------------------------------------
# The census-division table: the whole Region, Wellesley included, beside
# Ontario and Canada. Growth is the change from July 1 of one year to July 1
# of the next.
population <- read_table("table_17100152") |>
  mutate(geo = recode_geo(geo), year = year(ref_date), population = value, .keep = "none")

population_growth <- population |>
  arrange(geo, year) |>
  group_by(geo) |>
  mutate(percent = (population / lag(population) - 1) * 100) |>
  ungroup() |>
  filter(!is.na(percent)) |>
  latest(5, year)
write_csv(population_growth, file.path(data_dir, "population_growth.csv"))

# ---- 14. Components of population change ---------------------------------------------------
# Each component per 1,000 people, the population being the July 1 estimate
# at the start of the twelve months. `year` is the year the twelve months
# start: 2024 is July 1, 2024 to June 30, 2025. Migration is split as
# Statistics Canada publishes it: interprovincial (moves between provinces)
# and intraprovincial (moves between census divisions of one province). Both
# net to zero by definition where every move starts and ends inside the
# area - interprovincial for Canada, intraprovincial for Ontario and Canada -
# so those are left out (NA) rather than drawn as a line at zero. Emigration
# is left out: it is small and the same story everywhere.
population_components <- read_table("table_17100153") |>
  mutate(geo = recode_geo(geo), year = year(ref_date), component = components_of_population_growth, value, .keep = "none") |>
  pivot_wider(names_from = component, values_from = value) |>
  mutate(
    `Natural increase` = Births - Deaths,
    `Net interprovincial migration` = if_else(geo == "Canada", NA, `Net interprovincial migration`),
    `Net intraprovincial migration` = if_else(geo == "Region", `Net intraprovincial migration`, NA)
  ) |>
  select(geo, year, `Natural increase`, Immigrants, `Net non-permanent residents`,
         `Net interprovincial migration`, `Net intraprovincial migration`) |>
  pivot_longer(-c(geo, year), names_to = "component", values_to = "people") |>
  filter(!is.na(people)) |>
  left_join(population, join_by(geo, year)) |>
  mutate(
    per_1000 = people / population * 1000,
    component = factor(component, levels = c("Natural increase", "Immigrants", "Net non-permanent residents",
                                             "Net interprovincial migration", "Net intraprovincial migration"))
  ) |>
  arrange(component, geo, year)
write_csv(population_components, file.path(data_dir, "population_components.csv"))

# ---- 15. Crime Severity Index ----------------------------------------------------------------
# As published: police-reported crime weighted by seriousness, 2006 = 100.
crime_kinds <- c("All crime", "Violent crime", "Non-violent crime")
crime_severity <- read_table("table_35100026") |>
  mutate(
    geo = recode_geo(geo), year = year(ref_date),
    index = case_when(
      statistics == "Crime severity index" ~ "All crime",
      statistics == "Violent crime severity index" ~ "Violent crime",
      statistics == "Non-violent crime severity index" ~ "Non-violent crime"
    ),
    value,
    .keep = "none"
  ) |>
  mutate(index = factor(index, levels = crime_kinds)) |>
  arrange(index, geo, year)
write_csv(crime_severity, file.path(data_dir, "crime_severity.csv"))

# ---- 16. Police-reported incidents per 100,000 people -------------------------------------
# As published for all violations and for violent ones. Non-violent is all
# violations less violent ones: both rates share the same population, so the
# difference is the non-violent rate. "All violations" matches the Crime
# Severity Index's coverage (Criminal Code, traffic included, and federal
# statutes). Alternative: "Total, all Criminal Code violations (excluding
# traffic)", the rate Statistics Canada headlines as "the crime rate".
crime_rate <- read_table("table_35100177") |>
  mutate(geo = recode_geo(geo), year = year(ref_date), violations, value, .keep = "none") |>
  pivot_wider(names_from = violations, values_from = value) |>
  mutate(
    `All crime` = `Total, all violations`,
    `Violent crime` = `Total violent Criminal Code violations`,
    `Non-violent crime` = `All crime` - `Violent crime`
  ) |>
  pivot_longer(all_of(crime_kinds), names_to = "kind", values_to = "per_100k") |>
  mutate(kind = factor(kind, levels = crime_kinds)) |>
  select(geo, year, kind, per_100k) |>
  arrange(kind, geo, year)
write_csv(crime_rate, file.path(data_dir, "crime_rate.csv"))

# ---- 17. When each table was last released -------------------------------------------------
# One row per Statistics Canada table, with the date of the release the page's
# figures came from, for the "updated" date in each chart's subtitle.
# 01_get_data.R saves one small file per table beside its figures. CMHC's
# portal gives no release date, so its charts' subtitles carry none.
releases <- list.files(raw_dir, pattern = "_release\\.csv$", full.names = TRUE) |>
  map(\(file) read_csv(file, col_types = cols(.default = col_character()))) |>
  list_rbind() |>
  clean_names() |>
  mutate(released = as_date(ymd_hm(released))) |>
  arrange(table)
write_csv(releases, file.path(data_dir, "releases.csv"))

message("Done. Tidy files are in ", data_dir)
