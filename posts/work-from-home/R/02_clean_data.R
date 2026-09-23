# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads. Keep outputs under 25 MB each so they can be committed; if
# one must be bigger, /share-data moves it to a GitHub Release.
# Run from the project root: source("posts/work-from-home/R/02_clean_data.R")

here::i_am("posts/work-from-home/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir <- here("posts", "work-from-home", "data-raw")
data_dir <- here("posts", "work-from-home", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Data-quality flags ----------------------------------------------------
# cwr_quality_flags() reports the quality symbols (E use with caution, F too
# unreliable, x suppressed...) and quality footnotes on the rows this script
# keeps, and records them in data/quality_flags.csv. It is called once per
# table, on every row the post uses from that table. It also hands back those
# rows with every figure this site never uses (E, F, x, ..) blanked to NA, so
# the script carries on with what it returns. The log is started afresh so a
# table this script stops using does not linger in it.
source(here("R", "data_quality.R"))
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# The footnotes 01_get_data.R saved beside each table
table_notes <- function(file_stem) {
  read_csv(file.path(raw_dir, str_c(file_stem, "_notes.csv")),
           col_types = cols(.default = col_character()))
}

# ---- Confidence intervals for shares ---------------------------------------
# Place of work status is a long-form question, asked of one private household
# in four, so every count is a weighted estimate for everyone in private
# households. Table 98-10-0456 publishes a 95% confidence interval for each
# count, and
# R/census_ci.R turns those into intervals for the shares worked out here, by
# Statistics Canada's own method (how, and what they leave out, is explained
# there): cwr_var_from_bounds(), cwr_share_se() and cwr_add_share_ci(). The
# last also rates each share on Statistics Canada's scale and blanks any rated
# E ("use with caution") or F ("too unreliable"), which this site never uses.
# The 2016 data table publishes no intervals, so the 2016 shares get none.
source(here("R", "census_ci.R"))

# ---- The seven municipalities ----------------------------------------------
# The census table names each place but not what kind of place it is, and
# names two of them "Waterloo" - the city and the Region. So the seven are
# listed here by their census subdivision code, which is unambiguous, with the
# city or township type the charts colour by.
municipalities <- tribble(
  ~geo_uid,   ~district,         ~district_type,
  "3530004",  "North Dumfries",  "Township",
  "3530010",  "Cambridge",       "City",
  "3530013",  "Kitchener",       "City",
  "3530016",  "Waterloo",        "City",
  "3530020",  "Wilmot",          "Township",
  "3530027",  "Wellesley",       "Township",
  "3530035",  "Woolwich",        "Township"
)

# ---- Read ------------------------------------------------------------------
# Each figure in table 98-10-0456 comes as three rows - the count and the lower
# and upper bounds of its 95% confidence interval. This turns those three into
# one row per figure, with the bounds beside it (value, lower, upper), and
# works out the standard error they imply.
figure_per_row <- function(data, id_cols) {
  data |>
    mutate(statistic = case_when(
      str_detect(statistics_3, "lower bound") ~ "lower",
      str_detect(statistics_3, "upper bound") ~ "upper",
      .default = "value"
    )) |>
    pivot_wider(
      id_cols = all_of(id_cols),
      names_from = statistic,
      values_from = value
    ) |>
    mutate(se = sqrt(cwr_var_from_bounds(value, lower, upper)))
}

# Where people go to work: every destination, at total mode
# ---- Worked at home, 2016 and 2021 -------------------------------------------
# The share of employed people in each district who worked at home, in both
# censuses. The denominator is every employed person - the total of place of
# work status, which also counts people who worked outside Canada, had no fixed
# workplace address or had a usual place of work - so this is a share of all
# workers, not of commuters.
#
# The Region's own rows are kept beside the seven districts, for the post to
# quote; the chart draws only the districts.
#
# 2021, table 98-10-0456: the count and its interval bounds, fetched cell by
# cell by 01_get_data.R. One table serves both charts of working at home - the
# districts here, and the Region's industries further down - so it is read and
# checked for quality flags once, here, and split afterwards. The sector names
# are in the table's own member list, with the NAICS code in front of each.
industry_names <- read_csv(
  file.path(raw_dir, "table_98100456_members.csv"),
  col_types = cols(industry_member = col_integer(), industry = col_character())
) |>
  mutate(industry = str_squish(str_remove(industry, "^[0-9-]+ ")))

pow_2021 <- read_csv(
  file.path(raw_dir, "table_98100456.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double(),
                   industry_member = col_integer())
) |>
  clean_names() |>
  left_join(industry_names, join_by(industry_member)) |>
  # A count the web service did not return is a zero, as for 98-10-0464 (none
  # is missing in 2021)
  mutate(value = if_else(
    is.na(value) & is.na(status) & statistics_3 == "Count", 0, value
  )) |>
  cwr_quality_flags("98-10-0456", notes = table_notes("table_98100456"), log = quality_log)

# The share of each place's workers who worked at home, at the table's own
# industry total, turned into a share with its interval like the modes above
home_2021 <- pow_2021 |>
  filter(industry_member == 1L, geo_uid %in% c("3530", municipalities$geo_uid)) |>
  figure_per_row(c("geo_uid", "place_of_work_status_5")) |>
  mutate(
    total = value[str_starts(place_of_work_status_5, "Total")],
    se_total = se[str_starts(place_of_work_status_5, "Total")],
    percent = value / total * 100,
    .by = geo_uid
  ) |>
  filter(place_of_work_status_5 == "Worked at home") |>
  mutate(se_p = cwr_share_se(value, se, total, se_total)) |>
  cwr_add_share_ci(value) |>
  mutate(year = 2021L) |>
  select(geo_uid, year, workers = value, total, percent, percent_lower, percent_upper, cv, quality)

# 2016, data table 98-400-X2016321: one wide row per place, one column per
# place of work status. The 2016 tables publish no confidence intervals, so
# the 2016 shares have none, and no CV to rate them by; their counts are all in
# the hundreds or more. The file carries no cell symbols; its notes are checked
# like the other tables' footnotes.
home_2016_raw <- read_csv(
  file.path(raw_dir, "table_2016321_region.csv"),
  col_types = cols(.default = col_character())
) |>
  clean_names() |>
  mutate(
    geo_uid = geo_code_por,
    # The industry column's heading is long enough to be unwieldy; the member
    # names in it read "54 Professional, scientific and technical services",
    # so the NAICS code is dropped from the front for the chart's axis
    industry_2016 = str_squish(str_remove(
      dim_industry_north_american_industry_classification_system_naics_2012_21,
      "^[0-9-]+ "
    ))
  ) |>
  cwr_quality_flags("98-400-X2016321", notes = table_notes("table_2016321"), log = quality_log)

home_2016 <- home_2016_raw |>
  # The file now carries a row per industry as well (see the industry section
  # below), so the district comparison keeps the table's own industry total
  filter(str_starts(industry_2016, "Total")) |>
  # The two columns needed, found by the member names in their headings
  select(
    geo_uid,
    total = matches("place_of_work_status_5.*total_place_of_work_status"),
    workers = matches("place_of_work_status_5.*worked_at_home")
  ) |>
  mutate(
    across(c(total, workers), as.numeric),
    percent = workers / total * 100,
    year = 2016L
  )

work_at_home <- bind_rows(home_2016, home_2021) |>
  inner_join(
    # "Region" is what the charts call Waterloo Region (cwr_region in R/theme_cwr.R)
    bind_rows(tibble(geo_uid = "3530", district = "Region", district_type = "Region"), municipalities),
    join_by(geo_uid)
  ) |>
  select(district, district_type, year, workers, total, percent,
         percent_lower, percent_upper, cv, quality) |>
  arrange(district, year)

# 2016's long-form global non-response rate for each area (GNR, in the same
# file), for the Reliability table's response-rate row. 2021's are read from
# the Census Profile below.
gnr_2016 <- home_2016_raw |>
  filter(str_starts(industry_2016, "Total")) |>
  select(geo_uid, gnr_long_2016 = gnr) |>
  mutate(gnr_long_2016 = as.numeric(gnr_long_2016))

# ---- Working in agriculture, 2021 ------------------------------------------
# The share of each district's workers whose industry is agriculture, forestry,
# fishing and hunting - the first NAICS sector, fetched for the districts by
# 01_get_data.R. This is about the industry people work in, not where they work
# from, so both the sector and the district total are taken at the table's
# place-of-work total. The Region's own row is not charted, so it is left out
# here as well.
#
# The member id is not trusted on its own: 01_get_data.R asks for member 2, and
# this checks that member 2 really is the agriculture sector before the share
# is built.
agriculture_name <- industry_names |>
  filter(industry_member == 2L) |>
  pull(industry)
stopifnot(str_starts(agriculture_name, "Agriculture"))

agriculture <- pow_2021 |>
  filter(
    geo_uid %in% municipalities$geo_uid,
    str_starts(place_of_work_status_5, "Total"),
    industry_member %in% c(1L, 2L)
  ) |>
  figure_per_row(c("geo_uid", "industry_member")) |>
  mutate(
    total = value[industry_member == 1L],
    se_total = se[industry_member == 1L],
    percent = value / total * 100,
    .by = geo_uid
  ) |>
  filter(industry_member == 2L) |>
  mutate(se_p = cwr_share_se(value, se, total, se_total)) |>
  cwr_add_share_ci(value) |>
  inner_join(municipalities, join_by(geo_uid)) |>
  mutate(industry = agriculture_name) |>
  select(district, district_type, industry, workers = value, total, percent,
         percent_lower, percent_upper, cv, quality) |>
  arrange(desc(percent))

# ---- Worked at home by industry, 2016 and 2021 ------------------------------
# The share of each industry's own workers who worked at home, for the Region
# as a whole (census division 3530), for the chart that ranks industries. Both
# years cover the same 20 NAICS sectors; the tables' own industry totals are
# dropped, being the Region-wide share the chart above already shows.
#
# 2016, the same data table as above. It publishes no confidence intervals, so
# these shares have none and no CV to rate them by; the chart carries the note
# that says so. Counts are also randomly rounded to a multiple of 5, which
# matters most for the smallest sectors: management of companies and
# enterprises is 55 workers out of 465, so rounding alone moves its share by
# about half a point either way.
industry_2016 <- home_2016_raw |>
  filter(geo_uid == "3530", !str_starts(industry_2016, "Total")) |>
  select(
    industry = industry_2016,
    total = matches("place_of_work_status_5.*total_place_of_work_status"),
    workers = matches("place_of_work_status_5.*worked_at_home")
  ) |>
  mutate(
    across(c(total, workers), as.numeric),
    percent = workers / total * 100,
    year = 2016L
  )

# 2021, table 98-10-0456 again, this time the Region's 20 sectors. It publishes
# each count's interval bounds, so these shares do get intervals (the same
# method as the mode chart's).
industry_2021 <- pow_2021 |>
  filter(geo_uid == "3530", industry_member != 1L) |>
  figure_per_row(c("industry", "place_of_work_status_5")) |>
  mutate(
    total = value[str_starts(place_of_work_status_5, "Total")],
    se_total = se[str_starts(place_of_work_status_5, "Total")],
    percent = value / total * 100,
    .by = industry
  ) |>
  filter(place_of_work_status_5 == "Worked at home", !str_starts(industry, "Total")) |>
  mutate(se_p = cwr_share_se(value, se, total, se_total)) |>
  cwr_add_share_ci(value) |>
  mutate(year = 2021L) |>
  select(industry, year, workers = value, total, percent,
         percent_lower, percent_upper, cv, quality)

work_at_home_industry <- bind_rows(industry_2016, industry_2021) |>
  mutate(
    # Six of the sector names are far too long for a chart axis, most of all on
    # a phone, so the chart draws a shorter form of them (Greg, 2026-09-22) and
    # `industry` keeps the published name for the data file. Shortening belongs
    # here rather than in the chart (cwr-charts rule 8).
    # recode_values() replaced case_match() in dplyr 1.2.0; `default` takes the
    # published name, so the fourteen sectors not named here keep theirs.
    industry_short = recode_values(
      industry,
      "Agriculture, forestry, fishing and hunting" ~ "Agriculture",
      "Real estate and rental and leasing" ~ "Real estate",
      "Professional, scientific and technical services" ~ "Professional, scientific, technical",
      "Management of companies and enterprises" ~ "Management",
      "Other services (except public administration)" ~ "Other services",
      "Administrative and support, waste management and remediation services" ~ "Administrative and support",
      default = industry
    )
  ) |>
  arrange(industry, year) |>
  select(industry, industry_short, year, workers, total, percent,
         percent_lower, percent_upper, cv, quality)

# ---- Census response rates -------------------------------------------------
# How completely the long form was answered, for the README's Reliability
# table (cwr-charts rule 9b; R/data_quality.R explains both measures). One row
# per area: the long-form total non-response rate, the non-response and
# imputation rates for place of work status - the question every chart in this
# post rests on - and 2016's long-form global non-response rate, for the two
# charts that reach back to that census.
# Statistics Canada says an area whose total non-response rate is 50% or more
# should be used with caution, which this site treats as not usable at all;
# the script stops rather than carry such an area into the charts.
census_quality <- read_csv(file.path(raw_dir, "census_tnr.csv"), show_col_types = FALSE) |>
  clean_names() |>
  filter(questionnaire == "long") |>
  mutate(geo_uid = str_remove(dguid, "^2021A000[35]")) |>
  select(geo_uid, tnr_long = tnr_rate) |>
  left_join(
    read_csv(file.path(raw_dir, "table_98100572.csv"),
             col_types = cols(.default = col_character(), VALUE = col_double())) |>
      clean_names() |>
      filter(str_starts(long_form_data_quality_indicators_commuting_8, "Place of work status")) |>
      mutate(indicator = long_form_data_quality_indicators_commuting_8 |>
               str_to_lower() |>
               str_replace_all("[^a-z]+", "_")) |>
      pivot_wider(id_cols = geo_uid, names_from = indicator, values_from = value),
    join_by(geo_uid)
  ) |>
  left_join(gnr_2016, join_by(geo_uid)) |>
  left_join(
    # "Region" is what the charts call Waterloo Region (cwr_region in R/theme_cwr.R)
    bind_rows(tibble(geo_uid = "3530", district = "Region"), municipalities |> select(geo_uid, district)),
    join_by(geo_uid)
  ) |>
  relocate(district, .after = geo_uid)

high_non_response <- census_quality |>
  filter(tnr_long >= 50 | gnr_long_2016 >= 50) |>
  pull(district)
if (length(high_non_response) > 0) {
  stop("A long-form total non-response rate is 50% or more: ",
       str_flatten_comma(high_non_response),
       ". Statistics Canada says to use such data with caution, so this site does not use it.",
       call. = FALSE)
}

# ---- Write -----------------------------------------------------------------
write_csv(work_at_home, file.path(data_dir, "work_at_home.csv"))
write_csv(work_at_home_industry, file.path(data_dir, "work_at_home_industry.csv"))
write_csv(agriculture, file.path(data_dir, "agriculture.csv"))
write_csv(census_quality, file.path(data_dir, "census_quality.csv"))

message("Wrote ", nrow(work_at_home), " work-at-home rows, ",
        nrow(work_at_home_industry), " industry rows and ", nrow(agriculture),
        " agriculture rows to ", data_dir)
