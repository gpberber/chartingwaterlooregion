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
# "Confidence intervals for shares" above). Two cuts of it are used: every destination at total mode (just below) and
# three modes at total destination ("Main mode of commuting" further down), so
# it is read once, cut to those rows for the seven municipalities, and checked
# as one table. Every column is read as text except the value, so codes keep
# their leading digits exactly as published.
#
# The three modes the mode chart shows; why these three is explained with the
# chart's data below.
modes <- c("Car, truck or van", "Public transit", "Active transportation")

commuting_462 <- read_csv(
  file.path(raw_dir, "table_98100462.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    geo_uid %in% municipalities$geo_uid,
    str_starts(age_15a, "Total"),
    str_starts(gender_3, "Total"),
    str_starts(main_mode_of_commuting_11a, "Total") |
      (str_starts(commuting_destination_5, "Total") & main_mode_of_commuting_11a %in% modes)
  ) |>
  cwr_quality_flags("98-10-0462", notes = table_notes("table_98100462"), log = quality_log) |>
  # One row per figure, with its bounds beside it: value, lower, upper
  mutate(statistic = case_when(
    str_detect(statistics_3, "lower bound") ~ "lower",
    str_detect(statistics_3, "upper bound") ~ "upper",
    .default = "value"
  )) |>
  pivot_wider(
    id_cols = c(geo_uid, commuting_destination_5, main_mode_of_commuting_11a),
    names_from = statistic,
    values_from = value
  ) |>
  mutate(se = sqrt(cwr_var_from_bounds(value, lower, upper)))

# Where people go to work: every destination, at total mode
commuting_raw <- commuting_462 |>
  filter(str_starts(main_mode_of_commuting_11a, "Total"))

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
# The same table, cut the other way: destination held at its total and mode of
# travel let through. Only three of the modes are kept, the three the chart
# shows. They are each other's siblings or cousins in Statistics Canada's
# hierarchy ("Public transit" and "Active transportation" sit under
# "Sustainable transportation"), so none of the three double-counts another;
# the "Other method" group is left out, so they do not add to 100.
# (`modes` itself is set in the Read section, where the table is cut.)
commuting_mode <- commuting_462 |>
  filter(str_starts(commuting_destination_5, "Total")) |>
  # Each mode's share of all commuters in the municipality. The total row is
  # picked out as the denominator before the other modes are dropped.
  mutate(
    total = value[str_starts(main_mode_of_commuting_11a, "Total")],
    se_total = se[str_starts(main_mode_of_commuting_11a, "Total")],
    percent = value / total * 100,
    .by = geo_uid
  ) |>
  filter(main_mode_of_commuting_11a %in% modes) |>
  # The share's interval, as for the destinations above
  mutate(se_p = cwr_share_se(value, se, total, se_total)) |>
  cwr_add_share_ci(value) |>
  inner_join(municipalities, join_by(geo_uid)) |>
  select(district, district_type, mode = main_mode_of_commuting_11a, workers = value, percent,
         percent_lower, percent_upper, cv, quality) |>
  arrange(district, mode)

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
# of work, which decides the destination, and main mode of commuting.
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
  left_join(
    # "Region" is what the charts call Waterloo Region (cwr_region in R/theme_cwr.R)
    bind_rows(tibble(geo_uid = "3530", district = "Region"), municipalities |> select(geo_uid, district)),
    join_by(geo_uid)
  ) |>
  relocate(district, .after = geo_uid)

high_non_response <- census_quality |>
  filter(tnr_long >= 50) |>
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
write_csv(census_quality, file.path(data_dir, "census_quality.csv"))
write_csv(flows, file.path(data_dir, "commuting_flows.csv"))
write_csv(inbound, file.path(data_dir, "commuting_inbound.csv"))
write_csv(places, file.path(data_dir, "flow_places.csv"))
write_sf(region_shapes, file.path(data_dir, "region_shapes.geojson"), delete_dsn = TRUE)

message("Wrote ", nrow(commuting), " commuting rows, ", nrow(commuting_mode),
        " mode rows, ", nrow(flows), " flows, ", nrow(inbound),
        " inbound flows and ", nrow(places), " places to ", data_dir)
