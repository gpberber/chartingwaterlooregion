# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads. Keep outputs under 25 MB each so they can be committed; if
# one must be bigger, /share-data moves it to a GitHub Release.
# Run from the project root: source("posts/commuting/R/02_clean_data.R")

here::i_am("posts/commuting/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)
library(sf)

# cwr_label_point(): the roomiest point inside a shape, where its map label
# goes. Cleaning scripts do not load the chart theme, so the map helpers are
# sourced on their own.
source(here("R", "maps.R"))

raw_dir <- here("posts", "commuting", "data-raw")
data_dir <- here("posts", "commuting", "data")
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
# Commuting is a long-form question, asked of one private household in four, so
# every count is a weighted estimate for everyone in private households.
# Table 98-10-0462 publishes a 95% confidence interval for each count, and
# R/census_ci.R turns those into intervals for the shares worked out here, by
# Statistics Canada's own method (how, and what they leave out, is explained
# there): cwr_var_from_bounds(), cwr_share_se() and cwr_add_share_ci(). The
# last also rates each share on Statistics Canada's scale and blanks any rated
# E ("use with caution") or F ("too unreliable"), which this site never uses.
# Table 98-10-0459 publishes no intervals, so the flows get none.
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
# Table 98-10-0462 crosses where people who live in each municipality go to
# work with age, gender and mode of travel. Age and gender are held at their
# totals. Each figure comes as three rows - the count and the lower and upper
# bounds of its 95% confidence interval - and all three are kept (see
# "Confidence intervals for shares" above). Only every destination at total
# mode is used: the mode chart comes from table 98-10-0464 instead (see "Main
# mode of commuting" below). Every column is read as text except the value, so
# codes keep their leading digits exactly as published.
#
# The count and its two bounds become one row per figure, with the bounds
# beside it (value, lower, upper), for this table and for 98-10-0464 below.
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
commuting_raw <- read_csv(
  file.path(raw_dir, "table_98100462.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    geo_uid %in% municipalities$geo_uid,
    str_starts(age_15a, "Total"),
    str_starts(gender_3, "Total"),
    str_starts(main_mode_of_commuting_11a, "Total")
  ) |>
  cwr_quality_flags("98-10-0462", notes = table_notes("table_98100462"), log = quality_log) |>
  figure_per_row(c("geo_uid", "commuting_destination_5"))

# ---- Tidy ------------------------------------------------------------------
# Statistics Canada measures distance in units of geography rather than in
# kilometres, so its four categories are re-cut into the three that matter to a
# reader here: stayed home municipality, crossed into another one inside the
# Region, or left the Region. Waterloo Region is a census division, which is
# what makes that middle line drawable at all.
destinations <- tribble(
  ~commuting_destination_5,                                                                                             ~destination,
  "Commute within census subdivision (CSD) of residence",                                                               "In their own district",
  "Commute to a different census subdivision (CSD) within census division (CD) of residence",                           "Elsewhere in the Region",
  "Commute to a different census subdivision (CSD) and census division (CD) within province or territory of residence",  "Outside the Region",
  "Commute to a different province or territory",                                                                       "Outside the Region"
)

# The standard error of each municipality's total, for the intervals
destination_totals <- commuting_raw |>
  filter(str_starts(commuting_destination_5, "Total")) |>
  select(geo_uid, se_total = se)

commuting <- commuting_raw |>
  # An inner join drops the table's own "Total - Commuting destination" row,
  # which would otherwise be double-counted with the parts that make it up.
  inner_join(destinations, join_by(commuting_destination_5)) |>
  # "Outside the Region" is two of the table's categories added together; the
  # standard error of a sum is the square root of the summed squares, treating
  # the two as independent.
  summarise(workers = sum(value), se_x = sqrt(sum(se^2)), .by = c(geo_uid, destination)) |>
  mutate(percent = workers / sum(workers) * 100, .by = geo_uid) |>
  # The share's interval. The denominator is the sum of the parts, which the
  # table's rounding can leave a few people off its own total, so the total
  # row supplies only the standard error.
  left_join(destination_totals, join_by(geo_uid)) |>
  mutate(se_p = cwr_share_se(workers, se_x, sum(workers), se_total), .by = geo_uid) |>
  cwr_add_share_ci(workers) |>
  # And this one drops the Region's own rows, keeping the seven municipalities
  inner_join(municipalities, join_by(geo_uid)) |>
  select(district, district_type, destination, workers, percent,
         percent_lower, percent_upper, cv, quality) |>
  arrange(district, destination)

# ---- Main mode of commuting ------------------------------------------------
# Table 98-10-0464, which counts everyone the mode question covers: people with
# a usual place of work and people with no fixed workplace address.
# (98-10-0462's mode figures leave out the second group, since that table is
# crossed with destination; 01_get_data.R explains the choice.) 01_get_data.R
# fetched only the cells used - the total and three modes, with industry,
# occupation and gender at their totals - so every row is kept.
#
# The three modes are each other's siblings or cousins in Statistics Canada's
# hierarchy ("Public transit" and "Active transportation" sit under
# "Sustainable transportation"), so none of the three double-counts another;
# the "Other method" group is left out, so they do not add to 100.
commuting_mode <- read_csv(
  file.path(raw_dir, "table_98100464.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(geo_uid %in% municipalities$geo_uid) |>
  # A count the web service did not return is a zero: census tables leave
  # zero cells out. It has no status, unlike its bounds, which are marked
  # "..." (not applicable). Wellesley's transit count is the one in 2021.
  mutate(value = if_else(
    is.na(value) & is.na(status) & statistics_3 == "Count", 0, value
  )) |>
  cwr_quality_flags("98-10-0464", notes = table_notes("table_98100464"), log = quality_log) |>
  figure_per_row(c("geo_uid", "main_mode_of_commuting_11a")) |>
  # Each mode's share of all commuters in the district. The total row is
  # picked out as the denominator before the other modes are dropped.
  mutate(
    total = value[str_starts(main_mode_of_commuting_11a, "Total")],
    se_total = se[str_starts(main_mode_of_commuting_11a, "Total")],
    percent = value / total * 100,
    .by = geo_uid
  ) |>
  filter(!str_starts(main_mode_of_commuting_11a, "Total")) |>
  # The share's interval, as for the destinations above
  mutate(se_p = cwr_share_se(value, se, total, se_total)) |>
  cwr_add_share_ci(value) |>
  inner_join(municipalities, join_by(geo_uid)) |>
  select(district, district_type, mode = main_mode_of_commuting_11a, workers = value, percent,
         percent_lower, percent_upper, cv, quality) |>
  arrange(district, mode)

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
# 2021, table 98-10-0467: the count and its interval bounds, fetched cell by
# cell by 01_get_data.R, turned into a share with its interval like the modes
# above.
home_2021 <- read_csv(
  file.path(raw_dir, "table_98100467.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(geo_uid %in% c("3530", municipalities$geo_uid)) |>
  # A count the web service did not return is a zero, as for 98-10-0464 (none
  # is missing in 2021)
  mutate(value = if_else(
    is.na(value) & is.na(status) & statistics_3 == "Count", 0, value
  )) |>
  cwr_quality_flags("98-10-0467", notes = table_notes("table_98100467"), log = quality_log) |>
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

# ---- Worked at home by industry, 2016 ---------------------------------------
# The same 2016 table, read across its 20 NAICS sectors for the Region as a
# whole (census division 3530), for the chart that ranks industries by the
# share of their workers who worked at home. The table's own industry total is
# dropped: it is the Region-wide share the chart above already shows.
#
# 2016 publishes no confidence intervals, so these shares have none and no CV
# to rate them by; the chart carries the note that says so. Counts are also
# randomly rounded to a multiple of 5, which matters most for the smallest
# sectors: management of companies and enterprises is 55 workers out of 465, so
# rounding alone moves its share by about half a point either way.
work_at_home_industry <- home_2016_raw |>
  filter(geo_uid == "3530", !str_starts(industry_2016, "Total")) |>
  select(
    industry = industry_2016,
    total = matches("place_of_work_status_5.*total_place_of_work_status"),
    workers = matches("place_of_work_status_5.*worked_at_home")
  ) |>
  mutate(
    across(c(total, workers), as.numeric),
    percent = workers / total * 100,
    year = 2016L,
    # Six of the sector names are far too long for a chart axis, most of all on
    # a phone, so the chart draws a shorter form of them (Greg, 2026-09-22) and
    # `industry` keeps the published name for the data file. Shortening belongs
    # here rather than in the chart (cwr-charts rule 8).
    industry_short = case_match(
      industry,
      "Agriculture, forestry, fishing and hunting" ~ "Agriculture",
      "Real estate and rental and leasing" ~ "Real estate",
      "Professional, scientific and technical services" ~ "Professional, scientific, technical",
      "Management of companies and enterprises" ~ "Management",
      "Other services (except public administration)" ~ "Other services",
      "Administrative and support, waste management and remediation services" ~ "Administrative and support",
      .default = industry
    )
  ) |>
  arrange(desc(percent)) |>
  select(industry, industry_short, year, workers, total, percent)

# ---- Commuting flows --------------------------------------------------------
# Table 98-10-0459, already cut to people who live in the Region. One row per
# home municipality and place of work, including the thousands of places
# nobody from here commutes to.
#
# The place of work is identified by the second number in `coordinate` (the
# first is the place of residence). That number is a member id in the table's
# metadata, which is where the place's census subdivision code is kept - so
# places are matched on codes, never on names such as "Waterloo".
members <- read_csv(
  file.path(raw_dir, "table_98100459_members.csv"),
  col_types = cols(.default = col_character())
) |>
  clean_names() |>
  # Both dimensions list the same places under the same ids, but only the
  # place-of-residence dimension (1) carries the code
  filter(dimension_id == "1") |>
  # The code is published in square brackets, "[3523008]"; keep the digits
  mutate(work_csd = str_remove_all(classification_code, "[^0-9]")) |>
  select(member_id, work_csd, work = member_name)

# Every commute with one end in the Region: the rows for people who live here
# and the rows for people who work here. Both are wanted, so the file is read
# once and split below.
commutes_raw <- read_csv(
  file.path(raw_dir, "table_98100459_region.csv"),
  col_types = cols(.default = col_character())
) |>
  clean_names()

# The table is wide: one column per gender, each followed by its own symbol
# column. Only the total is used, so `values` limits the check to that column's
# symbols, and only rows with a commute in them feed the post.
commutes_raw <- commutes_raw |>
  filter(as.numeric(gender_3_total_gender_1) > 0) |>
  cwr_quality_flags("98-10-0459", values = "gender_3_total_gender_1",
                    notes = table_notes("table_98100459"), log = quality_log)

commutes <- commutes_raw |>
  mutate(
    # The last seven digits of DGUID are the home census subdivision code
    home_csd = str_sub(dguid, -7),
    member_id = str_extract(coordinate, "[0-9]+$"),
    workers = as.numeric(gender_3_total_gender_1)
  ) |>
  select(home_csd, home = geo, member_id, workers) |>
  inner_join(members, join_by(member_id)) |>
  filter(workers > 0) |>
  mutate(
    home_in_region = str_starts(home_csd, "3530"),
    work_in_region = str_starts(work_csd, "3530")
  )

# ---- Where the Region's own commuters go -----------------------------------
flows <- commutes |>
  filter(home_in_region) |>
  select(home_csd, member_id, work_csd, work, workers) |>
  # Each place's share of the home municipality's workers. The table has no
  # total row, so the denominator is the sum over every place of work. The
  # census rounds each count to a multiple of 5, so a share of a small place
  # is approximate.
  mutate(percent = workers / sum(workers) * 100, .by = home_csd) |>
  inner_join(municipalities |> select(home_csd = geo_uid, home = district),
             join_by(home_csd)) |>
  mutate(in_region = str_starts(work_csd, "3530")) |>
  # The largest destination outside the Region for each home municipality,
  # which the map marks. Ties would both be kept; there are none in 2021.
  mutate(
    top_outside = !in_region & workers == max(workers[!in_region]),
    .by = home_csd
  ) |>
  select(home_csd, home, work_csd, work, in_region, top_outside, workers, percent) |>
  arrange(home, desc(workers))

# ---- Where the people who work here come from ------------------------------
# The other direction: people who live outside the Region and work in one of
# its seven municipalities. The place they live in is named by the table's own
# GEO column rather than by the tribble above, which only covers the Region;
# the census names a municipality without saying what kind it is, so two places
# in Ontario can share a name, and the code is kept beside the name for anyone
# who needs to tell them apart.
inbound <- commutes |>
  filter(work_in_region, !home_in_region) |>
  select(home_csd, home, work_csd, workers) |>
  inner_join(municipalities |> select(work_csd = geo_uid, work = district),
             join_by(work_csd)) |>
  # Each place's share of everyone who commutes into the Region
  mutate(percent = workers / sum(workers) * 100) |>
  select(home_csd, home, work_csd, work, workers, percent) |>
  arrange(desc(workers))

# ---- Map shapes and points ------------------------------------------------
# The boundary file from 01_get_data.R holds the Region's seven municipalities
# and every place outside it that anyone from here commutes to.
boundaries <- st_read(file.path(raw_dir, "csd_boundaries.gpkg"), quiet = TRUE) |>
  clean_names() |>
  st_transform(cwr_map_crs)

# Where each place's label or dot goes: the roomiest point inside its shape,
# worked out by cwr_label_point() in R/maps.R, which every map on the site
# uses.

# One row per place the map marks: the seven municipalities and the top
# destination outside the Region for each of them. Stored as longitude and
# latitude, as spatial data usually is; the post projects them when it draws.
places <- boundaries |>
  filter(csduid %in% c(municipalities$geo_uid, flows$work_csd[flows$top_outside])) |>
  (\(shapes) bind_cols(
    sf::st_drop_geometry(shapes) |> select(csd = csduid, name = csdname),
    cwr_label_point(shapes)
  ))() |>
  select(csd, name, lon, lat) |>
  mutate(in_region = str_starts(csd, "3530"))

# The Region's seven outlines, simplified: a 50 m tolerance takes out detail no
# one can see at the size the map is drawn, and keeps the committed file small.
region_shapes <- boundaries |>
  filter(str_starts(csduid, "3530")) |>
  st_simplify(dTolerance = 50) |>
  st_transform(4326) |>
  select(csd = csduid, name = csdname)

# ---- Census response rates -------------------------------------------------
# How completely the long form was answered, for the README's Reliability
# table (cwr-charts rule 9b; R/data_quality.R explains both measures). One row
# per area: the long-form total non-response rate, and the non-response and
# imputation rates for the two commuting questions the charts rest on - place
# of work, which decides the destination, and main mode of commuting - plus
# 2016's long-form global non-response rate, for the work-at-home chart.
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
      filter(str_starts(long_form_data_quality_indicators_commuting_8, "Place of work status|Main mode of commuting")) |>
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
write_csv(commuting, file.path(data_dir, "commuting.csv"))
write_csv(commuting_mode, file.path(data_dir, "commuting_mode.csv"))
write_csv(work_at_home, file.path(data_dir, "work_at_home.csv"))
write_csv(work_at_home_industry, file.path(data_dir, "work_at_home_industry.csv"))
write_csv(census_quality, file.path(data_dir, "census_quality.csv"))
write_csv(flows, file.path(data_dir, "commuting_flows.csv"))
write_csv(inbound, file.path(data_dir, "commuting_inbound.csv"))
write_csv(places, file.path(data_dir, "flow_places.csv"))
write_sf(region_shapes, file.path(data_dir, "region_shapes.geojson"), delete_dsn = TRUE)

message("Wrote ", nrow(commuting), " commuting rows, ", nrow(commuting_mode),
        " mode rows, ", nrow(flows), " flows, ", nrow(inbound),
        " inbound flows and ", nrow(places), " places to ", data_dir)
