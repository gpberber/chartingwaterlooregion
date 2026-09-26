# 01_get_data.R
# Fetches every raw input for the Vital Statistics page into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root:
#   Rscript vital-statistics/R/01_get_data.R
#
# The page is refreshed by running this script, then 02_clean_data.R, then
# rendering the page on its own (see the README's "Updating the page").
#
# Every series is fetched for three places: Waterloo Region (or the Kitchener -
# Cambridge - Waterloo census metropolitan area where that is the smallest
# area published), Ontario and Canada. Only the latest few years of each are
# fetched - enough for the five years the page shows, plus the year before
# them that a 12-month total or a year-over-year change needs.

here::i_am("vital-statistics/R/01_get_data.R")

library(tidyverse)
library(here)
library(httr2)    # talks to Statistics Canada's web data service
library(cansim)   # Statistics Canada code lists and table footnotes
library(cmhc)     # CMHC's Housing Market Information Portal

raw_dir <- here("vital-statistics", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- Statistics Canada: fetching just the cells needed ---------------------
# Several of these tables are huge (building permits alone is 368 MB zipped),
# and the page needs a handful of series from each. So rather than download
# whole tables, each series is fetched from Statistics Canada's web data
# service (WDS) by its "coordinate": the member id of each of the table's
# dimensions, in order, joined by dots and padded with zeros to ten places.
# Member 1 is usually each dimension's total, but not always, so the ids are
# written out below beside their names, as the table's metadata gives them
# (getCubeMetadata), where they can be checked.
#
# The service is called directly with httr2 rather than through cansim, whose
# cell functions drop each figure's status (E, F, x ...), which the
# data-quality check in 02_clean_data.R needs.

wds <- "https://www150.statcan.gc.ca/t1/wds/rest/"

# What each status and symbol code means, from Statistics Canada's own code
# lists. Code 0 is "normal" / "none", which has no symbol.
status_codes <- get_cansim_code_set("status") |>
  select(statusCode, STATUS = statusRepresentationEn)
symbol_codes <- get_cansim_code_set("symbol") |>
  select(symbolCode, SYMBOL = symbolRepresentationEn)

# A table's dimensions and the names of their members, from its metadata
wds_members <- function(product_id) {
  request(str_c(wds, "getCubeMetadata")) |>
    req_body_json(list(list(productId = product_id))) |>
    req_perform() |>
    resp_body_json() |>
    pluck(1, "object", "dimension") |>
    # (the argument is `dim`, not `dimension`: inside tibble() a column
    # named dimension would hide it from the lines after)
    map(\(dim) {
      tibble(
        position = dim$dimensionPositionId,
        dimension = dim$dimensionNameEn,
        member_id = map_int(dim$member, "memberId"),
        member = map_chr(dim$member, "memberNameEn")
      )
    }) |>
    list_rbind()
}

# Fetches the latest `periods` figures of every combination of the members
# named in `members` - a list of member ids, one entry per dimension in the
# table's own order - and returns one row per figure, with each dimension's
# member name in a column of its own (named as the table names it), the
# reference date, the value, and its status and symbol. The table number is
# written with its dashes ("14-10-0459-01") so the output says which table it
# came from.
wds_table <- function(table, members, periods) {
  product_id <- as.numeric(str_remove_all(str_sub(table, 1, 10), "-"))
  dims <- wds_members(product_id)
  dim_names <- dims |> distinct(position, dimension) |> arrange(position) |> pull(dimension)
  stopifnot(length(members) == length(dim_names))

  # Every combination of the members asked for, and its coordinate
  cells <- exec(expand_grid, !!!set_names(members, dim_names)) |>
    mutate(COORDINATE = pmap_chr(across(everything()), \(...) {
      ids <- c(...)
      str_c(c(ids, rep(0, 10 - length(ids))), collapse = ".")
    }))

  figures <- request(str_c(wds, "getDataFromCubePidCoordAndLatestNPeriods")) |>
    req_body_json(map(pull(cells, COORDINATE), \(coordinate) {
      list(productId = product_id, coordinate = coordinate, latestN = periods)
    })) |>
    req_perform() |>
    resp_body_json() |>
    map(\(cell) {
      if (cell$status != "SUCCESS") stop("WDS could not return a cell of ", table)
      points <- pluck(cell, "object", "vectorDataPoint")
      tibble(
        COORDINATE = pluck(cell, "object", "coordinate"),
        REF_DATE = map_chr(points, "refPer"),
        VALUE = map_dbl(points, \(point) point$value %||% NA_real_),
        statusCode = map_chr(points, \(point) as.character(point$statusCode %||% 0)),
        symbolCode = map_chr(points, \(point) as.character(point$symbolCode %||% 0))
      )
    }) |>
    list_rbind() |>
    left_join(status_codes, join_by(statusCode)) |>
    left_join(symbol_codes, join_by(symbolCode)) |>
    select(-statusCode, -symbolCode)

  # Put the members' names back on, in place of their ids. The WDS returns a
  # coordinate padded to ten places, so it is matched on that form.
  named <- cells
  for (i in seq_along(dim_names)) {
    lookup <- dims |> filter(position == i) |> select(member_id, member)
    named <- named |>
      left_join(lookup, by = set_names("member_id", dim_names[i])) |>
      select(-all_of(dim_names[i])) |>
      rename(!!dim_names[i] := member)
  }

  named |>
    right_join(figures, join_by(COORDINATE)) |>
    rename(GEO = Geography) |>
    mutate(table = table, .before = 1)
}

# Saves a table's figures and its footnotes side by side in data-raw/, the
# footnotes for the data-quality check in 02_clean_data.R
save_table <- function(data, file_stem) {
  write_csv(data, file.path(raw_dir, str_c(file_stem, ".csv")))
  get_cansim_table_notes(first(pull(data, table))) |>
    write_csv(file.path(raw_dir, str_c(file_stem, "_notes.csv")))
  message("Saved ", file_stem, ": ", nrow(data), " figures")
}

# Seven years of months: the five the page shows, the twelve before them that
# a 12-month total or a year-over-year change needs, and a margin
months <- 84

# ---- Labour market -----------------------------------------------------------
# 14-10-0459-01: Labour Force Survey by census metropolitan area, three-month
# moving average, seasonally adjusted. Publishes Canada and Ontario on the same
# basis as the CMA, and a standard error beside every estimate.
#   Geography: 1 Canada, 18 Ontario, 29 Kitchener-Cambridge-Waterloo, Ontario
#   Labour force characteristics: 2 Labour force, 3 Employment, 5 Unemployment rate,
#                                 6 Participation rate
#   Statistics: 1 Estimate, 2 Standard error of estimate,
#               4 Standard error of year-over-year change
#   Data type: 1 Seasonally adjusted
wds_table(
  "14-10-0459-01",
  list(c(1, 18, 29), c(2, 3, 5, 6), c(1, 2, 4), 1),
  periods = months
) |>
  save_table("table_14100459")

# 14-10-0458-01: the same survey by CMA, three-month moving average, NOT
# seasonally adjusted, but split by age. Only CMAs: Canada and Ontario come
# from 14-10-0017-01 below. The CMA table has no ten-year age groups; these
# three cover 15 to 64 without a gap or an overlap. (It also has 25 to 44,
# 45 and over, 55 and over, and 65 and over.)
#   Geography: 22 Kitchener-Cambridge-Waterloo, Ontario
#   Labour force characteristics: 8 Unemployment rate, 9 Participation rate
#   Gender: 1 Total - Gender
#   Age group: 2 15 to 24 years, 6 25 to 54 years, 8 55 to 64 years
wds_table("14-10-0458-01", list(22, c(8, 9), 1, c(2, 6, 8)), periods = months) |>
  save_table("table_14100458")

# 14-10-0017-01: the survey by province, monthly, not seasonally adjusted, by
# age. 02_clean_data.R turns these into three-month moving averages so they
# match the CMA table above. This table has no 55 to 64 group, so its two
# five-year halves are fetched and added together.
#   Geography: 1 Canada, 7 Ontario
#   Labour force characteristics: 1 Population, 2 Labour force, 6 Unemployment
#   Gender: 1 Total - Gender
#   Age group: 2 15 to 24 years, 6 25 to 54 years, 17 55 to 59 years, 18 60 to 64 years
wds_table("14-10-0017-01", list(c(1, 7), c(1, 2, 6), 1, c(2, 6, 17, 18)), periods = months) |>
  save_table("table_14100017")

# 14-10-0454-01: Employment Insurance regular beneficiaries by census
# metropolitan category, monthly, seasonally adjusted. One dimension only.
#   Geography: 1 Canada, 89 Ontario, 99 Kitchener-Cambridge-Waterloo, Ontario
wds_table("14-10-0454-01", list(c(1, 89, 99)), periods = months) |>
  save_table("table_14100454")

# ---- Business and construction -----------------------------------------------
# 33-10-0270-01: experimental estimates of business openings and closures,
# monthly, seasonally adjusted.
#   Geography: 1 Canada, 16 Ontario, 25 Kitchener - Cambridge - Waterloo, Ontario
#   Industry: 1 Business sector industries [T004]
#   Business dynamics measure: 1 Active businesses, 2 Opening businesses,
#                              4 Closing businesses
wds_table("33-10-0270-01", list(c(1, 16, 25), 1, c(1, 2, 4)), periods = months) |>
  save_table("table_33100270")

# 34-10-0292-01: building permits, monthly, value in thousands of dollars.
# Unadjusted: 02_clean_data.R sums 12 months, which removes the seasons.
#   Geography: 1 Canada, 7 Ontario, 38 Kitchener-Cambridge-Waterloo, Ontario
#   Type of building: 4 Total residential, 33 Total non-residential
#   Type of work: 1 Types of work, total
#   Variables: 1 Value of permits
#   Seasonal adjustment, value type: 1 Unadjusted, current
wds_table("34-10-0292-01", list(c(1, 7, 38), c(4, 33), 1, 1, 1), periods = months) |>
  save_table("table_34100292")

# 18-10-0205-01: New Housing Price Index, monthly (December 2016 = 100).
#   Geography: 1 Canada, 17 Ontario, 23 Kitchener-Cambridge-Waterloo, Ontario
#   New housing price indexes: 1 Total (house and land)
wds_table("18-10-0205-01", list(c(1, 17, 23), 1), periods = months) |>
  save_table("table_18100205")

# ---- Population and safety -----------------------------------------------------
# 17-10-0152-01: population estimates on July 1 by census division. The
# Region is a census division (Waterloo, 3530), so this is the whole Region,
# Wellesley included, and the same table has Ontario and Canada. Six years
# give five years of growth.
#   Geography: 1 Canada, 152 Ontario, 176 Waterloo, Ontario
#   Gender: 1 Total - gender;  Age group: 1 All ages
wds_table("17-10-0152-01", list(c(1, 152, 176), 1, 1), periods = 6) |>
  save_table("table_17100152")

# 17-10-0153-01: components of population change by census division, for the
# twelve months from July 1 to June 30.
#   Geography: 1 Canada, 152 Ontario, 176 Waterloo, Ontario
#   Components: 1 Births, 2 Deaths, 3 Immigrants, 8 Net interprovincial
#               migration, 9 Net intraprovincial migration,
#               10 Net non-permanent residents
#   Gender: 1 Total - gender;  Age group: 1 All ages
wds_table("17-10-0153-01", list(c(1, 152, 176), c(1, 2, 3, 8, 9, 10), 1, 1), periods = 5) |>
  save_table("table_17100153")

# Populations to divide the building permits and housing starts by. Permits
# are counted across the whole province and country, so they are divided by
# the whole population; CMHC counts starts every month only in urban centres
# of 10,000 people or more - census metropolitan areas and census
# agglomerations - so starts are divided by the population of those.
# 17-10-0148-01: population estimates on July 1 by CMA and CA.
#   Geography: 2 All census metropolitan areas and census agglomerations, Canada;
#              25 Kitchener - Cambridge - Waterloo (CMA), Ontario;
#              193 All census metropolitan areas and census agglomerations, Ontario
#   Gender: 1 Total - gender;  Age group: 1 All ages
wds_table("17-10-0148-01", list(c(2, 25, 193), 1, 1), periods = 8) |>
  save_table("table_17100148")

# 17-10-0005-01: population estimates on July 1 by province.
#   Geography: 1 Canada, 7 Ontario
#   Gender: 1 Total - gender;  Age group: 1 All ages
wds_table("17-10-0005-01", list(c(1, 7), 1, 1), periods = 8) |>
  save_table("table_17100005")

# 35-10-0026-01: Crime Severity Index, annual (2006 = 100).
#   Geography: 1 Canada, 16 Ontario [35], 22 Kitchener-Cambridge-Waterloo, Ontario [35541]
#   Statistics: 1 Crime severity index, 3 Violent crime severity index,
#               5 Non-violent crime severity index
wds_table("35-10-0026-01", list(c(1, 16, 22), c(1, 3, 5)), periods = 5) |>
  save_table("table_35100026")

# 35-10-0177-01: police-reported incidents by violation, annual.
#   Geography: 1 Canada, 16 Ontario [35], 22 Kitchener-Cambridge-Waterloo, Ontario [35541]
#   Violations: 1 Total, all violations; 4 Total violent Criminal Code violations
#   Statistics: 2 Rate per 100,000 population
# "All violations" is the same coverage as the Crime Severity Index (every
# Criminal Code violation, traffic included, plus the federal statutes), so
# the index and the rate describe the same crimes. Non-violent is worked out
# in 02_clean_data.R as all violations less violent ones.
wds_table("35-10-0177-01", list(c(1, 16, 22), c(1, 4), 2), periods = 5) |>
  save_table("table_35100177")

# ---- CMHC ------------------------------------------------------------------------
# CMHC's Housing Market Information Portal, through the cmhc package. Its
# place codes: 35541 is the Kitchener - Cambridge - Waterloo CMA (Statistics
# Canada's CMA code with the province in front), 35 Ontario, 01 Canada.
# The survey, series and dimension names were checked with
# list_cmhc_tables() on 2026-09-25; CMHC renames them now and then.
cmhc_places <- c("35541", "35", "01")

# Fetches one CMHC series over time for the three places, one row per figure
cmhc_series <- function(survey, series, dimension, frequency = NULL) {
  cmhc_places |>
    map(\(place) {
      get_cmhc(
        survey = survey, series = series, dimension = dimension,
        breakdown = "Historical Time Periods", geo_uid = place,
        frequency = frequency
      )
    }) |>
    list_rbind() |>
    mutate(across(where(is.factor), as.character))
}

# Starts and Completions Survey, monthly, actual counts (not annual rates).
# For Ontario and Canada these cover centres of 10,000 people or more, which
# is what CMHC surveys every month (its "Default" geography filter).
cmhc_series("Scss", "Starts", "Dwelling Type", frequency = "Monthly") |>
  write_csv(file.path(raw_dir, "cmhc_starts_dwelling_type.csv"))
cmhc_series("Scss", "Starts", "Intended Market", frequency = "Monthly") |>
  write_csv(file.path(raw_dir, "cmhc_starts_intended_market.csv"))

# Rental Market Survey, every October, purpose-built rental apartments and
# rows. Each figure carries CMHC's reliability rating in the Quality column.
cmhc_series("Rms", "Vacancy Rate", "Bedroom Type") |>
  write_csv(file.path(raw_dir, "cmhc_vacancy_rate.csv"))
cmhc_series("Rms", "Average Rent", "Bedroom Type") |>
  write_csv(file.path(raw_dir, "cmhc_average_rent.csv"))
cmhc_series("Rms", "Average Rent Change", "Bedroom Type") |>
  write_csv(file.path(raw_dir, "cmhc_average_rent_change.csv"))

message("Done. Raw files are in ", raw_dir)
