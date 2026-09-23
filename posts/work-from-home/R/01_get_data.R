# 01_get_data.R
# Fetches every raw input for the "Working from home in Waterloo Region" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/work-from-home/R/01_get_data.R")

here::i_am("posts/work-from-home/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "work-from-home", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# Big downloads that are only needed while this script runs go to a folder in
# the computer's temporary directory rather than to data-raw/, which sits in
# OneDrive and would otherwise sync them. tempdir() is a fresh folder every
# time R starts, so its parent is used instead: that way a second run finds the
# files already there.
download_dir <- file.path(dirname(tempdir()), "cwr_downloads")
dir.create(download_dir, showWarnings = FALSE)

# ---- Fetching single cells of a census table -------------------------------
# Table 98-10-0456 below has 42 million cells, so rather than download the
# whole thing only the cells the charts need are fetched, one by one, from
# Statistics Canada's web data service. A cell is named by its "coordinate",
# the member id of each dimension in order, joined by dots, with zeros for the
# dimensions the table does not have. Member 1 is each dimension's total. The
# ids are from the table's metadata (getCubeMetadata), where they can be
# checked.
#
# The service is called directly with httr2 rather than through cansim, whose
# cell function drops each cell's status ("..." not applicable, E, F), which
# the data-quality check needs. A cell the service does not return at all is a
# zero - census tables leave zeros out - and is written with no value and no
# status for 02_clean_data.R to fill in.

# What each status code means, from Statistics Canada's own code list. Code 0
# is "normal", which has no symbol.
status_codes <- get_cansim_code_set("status") |>
  select(statusCode, STATUS = statusRepresentationEn)

# Fetches the cells named by `coordinates` from table `product_id` (written as
# a number, 98100456) in one request, and returns one row per cell: its
# coordinate, value and status. The answers do not come back in the order
# asked, so each is matched to its cell by coordinate.
wds_cells <- function(product_id, coordinates) {
  httr2::request(
    "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  ) |>
    httr2::req_body_json(map(coordinates, \(coordinate) {
      list(productId = product_id, coordinate = coordinate, latestN = 1)
    })) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    map(\(cell) {
      point <- pluck(cell, "object", "vectorDataPoint", 1)
      tibble(
        COORDINATE = pluck(cell, "object", "coordinate"),
        VALUE = pluck(point, "value", .default = NA_real_),
        statusCode = as.character(pluck(point, "statusCode", .default = NA))
      )
    }) |>
    list_rbind() |>
    left_join(status_codes, join_by(statusCode)) |>
    select(-statusCode)
}

# The Region and its seven districts, with their member ids in the Geography
# dimension. The ids are the same in every 2021 census table with this
# geography (checked in 98-10-0456, and in 98-10-0464 in the commuting post).
census_places <- tribble(
  ~GeoUID,    ~GEO,              ~place_member,
  "3530",     "Waterloo Region", 2439,
  "3530004",  "North Dumfries",  2440,
  "3530010",  "Cambridge",       2441,
  "3530013",  "Kitchener",       2442,
  "3530016",  "Waterloo",        2443,
  "3530020",  "Wilmot",          2444,
  "3530027",  "Wellesley",       2445,
  "3530035",  "Woolwich",        2446
)
census_statistics <- tribble(
  ~`Statistics (3)`,                              ~statistic_member,
  "Count",                                        1,
  "95% confidence interval lower bound, Count",   2,
  "95% confidence interval upper bound, Count",   3
)
# ---- Place of work status, 2021 census --------------------------------------
# Table 98-10-0456, "Place of work status by industry sectors, occupation broad
# category and gender", for the post's 2021 figures: the charts of working at
# home by district and by industry, and the chart of who works in agriculture. Place of work status sorts
# every employed person into one of four groups - worked at home, worked
# outside Canada, no fixed workplace address, and usual place of work - and its
# total is everyone employed in the census week.
#
# It is also the 2021 counterpart of the 2016 data table below, which crosses
# the same place of work status with the same 20 NAICS sectors, so the two
# years can be compared by district and ranked by industry from tables built
# the same way. (98-10-0467 crosses place of work status with education, age
# and gender instead and was used here at first; it gives the same figures for
# the districts, give or take Statistics Canada's random rounding - Wilmot's
# share differs by a tenth of a point - and none of its three variables is used
# in the post, so one table now serves both charts.)
#
# The table has 42 million cells, so only the 189 needed are fetched by
# coordinate: the Region and its seven districts at the table's own
# industry total, for the chart by district, and the Region at each of the 20
# sectors, for the industry chart; each of those 28 combinations as 2 statuses
# (the total and "Worked at home") x 3 statistics (the count and its 95%
# confidence interval bounds), with occupation and gender at their totals. The
# last 21 are for the agriculture chart: the seven districts at the first NAICS
# sector, in the place-of-work total only (the chart is about who works in
# agriculture, not where they work from), x the same 3 statistics. Their
# denominator is each district's industry total, already fetched above.
# Coordinate: place . occupation . gender . statistic . industry . place of
# work status, then four zeros for the dimensions the table does not have. Member ids from the table's metadata
# (getCubeMetadata); 2439 is the Region.
home_statuses <- tribble(
  ~`Place of work status (5)`,       ~status_member,
  "Total - Place of work status",    1,
  "Worked at home",                  2
)

home_cells <- bind_rows(
  # Every place, at the industry total, in both place-of-work groups
  census_places |> mutate(industry_member = 1) |> cross_join(home_statuses),
  # The Region, at each of the 20 sectors, in both groups
  census_places |>
    filter(GeoUID == "3530") |>
    cross_join(tibble(industry_member = 2:21)) |>
    cross_join(home_statuses),
  # The seven districts at agriculture, forestry, fishing and hunting - member
  # 2, the first NAICS sector - at the place-of-work total only. 02_clean_data.R
  # checks that name against the member list rather than trusting the id.
  census_places |>
    filter(GeoUID != "3530") |>
    mutate(industry_member = 2) |>
    cross_join(home_statuses |> filter(status_member == 1))
) |>
  cross_join(census_statistics) |>
  mutate(COORDINATE = str_glue(
    "{place_member}.1.1.{statistic_member}.{industry_member}.{status_member}.0.0.0.0"
  ))

home_cells |>
  left_join(wds_cells(98100456, home_cells$COORDINATE), join_by(COORDINATE)) |>
  mutate(
    `Occupation - Broad category - National Occupational Classification (NOC) 2021 (11)` =
      "Total - Occupation - Broad category - National Occupational Classification (NOC) 2021",
    `Gender (3)` = "Total - Gender"
  ) |>
  select(GeoUID, GEO, starts_with("Occupation"), `Gender (3)`, `Statistics (3)`,
         industry_member, `Place of work status (5)`, VALUE, STATUS, COORDINATE) |>
  write_csv(file.path(raw_dir, "table_98100456.csv"))

# The industry dimension's member names, so 02_clean_data.R can put a name to
# each `industry_member` above (and match them to the 2016 table's sectors).
# They come from the table's metadata rather than being typed out here.
httr2::request("https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata") |>
  httr2::req_body_json(list(list(productId = 98100456))) |>
  httr2::req_perform() |>
  httr2::resp_body_json() |>
  pluck(1, "object", "dimension") |>
  keep(\(dimension) str_starts(dimension$dimensionNameEn, "Industry")) |>
  pluck(1, "member") |>
  map(\(member) tibble(
    industry_member = as.integer(member$memberId),
    industry = member$memberNameEn
  )) |>
  list_rbind() |>
  write_csv(file.path(raw_dir, "table_98100456_members.csv"))

# ---- Place of work status, 2016 census --------------------------------------
# The 2016 census equivalent is data table 98-400-X2016321, "Place of Work
# Status (5), Industry (21), Occupation (11) and Sex (3) for the Employed
# Labour Force Aged 15 Years and Over in Private Households", the only 2016
# table of place of work status for census subdivisions. (2016 tables were not
# put into Statistics Canada's table database, so it has no 98-10 number and
# cansim cannot fetch it.) Its total is the same group as 2021's - everyone
# employed in the census week, in all four place-of-work groups - so the two
# years compare directly. It publishes no confidence intervals.
#
# The file is one zip holding a 640 MB CSV for every census subdivision in
# Canada, so it goes to the temporary download folder and only the Region's
# rows are kept: its eight geographies at every one of the 21
# industry members (the table's own total, plus the 20 NAICS sectors), with
# occupation and sex at their totals. The total-industry rows are what the
# work-at-home chart compares with 2021; the sector rows are what the industry
# chart ranks. The file is wide: one column per place of work status. GNR is
# each area's long-form global non-response rate, which 02_clean_data.R checks
# like the 2021 rates further down.
pow_2016_zip <- file.path(download_dir, "98-400-X2016321.zip")
if (!file.exists(pow_2016_zip)) {
  options(timeout = 1200)
  download.file(
    "https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/dt-td/CompDataDownload.cfm?LANG=E&PID=110710&OFT=CSV",
    destfile = pow_2016_zip,
    mode = "wb"
  )
}

read_csv_chunked(
  unz(pow_2016_zip, "98-400-X2016321_English_CSV_data.csv"),
  callback = DataFrameCallback$new(\(chunk, pos) {
    filter(
      chunk,
      str_starts(`GEO_CODE (POR)`, "3530"),
      str_starts(`DIM: Occupation - National Occupational Classification (NOC) 2016 (11)`, "Total"),
      `DIM: Sex (3)` == "Total - Sex"
    )
  }),
  chunk_size = 1e6,
  col_types = cols(.default = col_character())
) |>
  write_csv(file.path(raw_dir, "table_2016321_region.csv"))

# The table's notes, in the same shape as the footnotes saved for 98-10-0456, so 02_clean_data.R can check them the same way. They are read
# from the metadata file in the zip; the two kept are the note on data quality
# (which applies to the whole table) and the footnote on the place of work
# status total.
pow_2016_meta <- read_lines(unz(pow_2016_zip, "98-400-X2016321_English_meta.txt"))
tibble(
  `Note ID` = c("quality", "3"),
  Note = c(
    pow_2016_meta[str_which(pow_2016_meta, "^For information on data quality")],
    pow_2016_meta[str_which(pow_2016_meta, "^Footnote 3$") + 1]
  ),
  `Dimension name` = c(NA, "Place of work status (5)"),
  `Member Name` = c(NA, "Total - Place of work status")
) |>
  write_csv(file.path(raw_dir, "table_2016321_notes.csv"))

# ---- Census response rates -------------------------------------------------
# How completely the long form was answered in each area (cwr-charts rule 9b;
# the helpers are in R/data_quality.R, which explains both measures).
source(here("R", "data_quality.R"))

# The total non-response rate for the Region and its seven municipalities, read
# from each area's Census Profile page. Waterloo Region is census division
# 3530; its municipalities are census subdivisions 3530004 to 3530035.
c("2021A00033530", paste0("2021A0005", c(
  "3530004", "3530010", "3530013", "3530016", "3530020", "3530027", "3530035"
))) |>
  cwr_census_tnr() |>
  write_csv(file.path(raw_dir, "census_tnr.csv"))

# Table 98-10-0572, the non-response and imputation rates for each commuting
# question. Only the place-of-work-status rows matter here - that is the
# question every chart in this post rests on - and 02_clean_data.R picks them
# out; the table is cut to the Region's eight geographies as it arrives.
get_cansim("98-10-0572") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100572.csv"))

# ---- Footnotes -------------------------------------------------------------
# Each Statistics Canada table carries footnotes, some of them about the
# quality or comparability of the figures. They are saved beside the tables so
# that 02_clean_data.R can report the ones that apply to the rows this post
# keeps (cwr_quality_flags() in R/data_quality.R) without going back online.
c("98-10-0456") |>
  walk(\(table_number) {
    get_cansim_table_notes(table_number) |>
      write_csv(file.path(
        raw_dir,
        str_c("table_", str_remove_all(table_number, "-"), "_notes.csv")
      ))
  })
