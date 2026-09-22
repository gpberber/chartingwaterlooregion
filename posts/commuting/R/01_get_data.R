# 01_get_data.R
# Fetches every raw input for the "Commuting in Waterloo Region" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/commuting/R/01_get_data.R")

here::i_am("posts/commuting/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "commuting", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# Big downloads that are only needed while this script runs go to a folder in
# the computer's temporary directory rather than to data-raw/, which sits in
# OneDrive and would otherwise sync them. tempdir() is a fresh folder every
# time R starts, so its parent is used instead: that way a second run finds the
# files already there.
download_dir <- file.path(dirname(tempdir()), "cwr_downloads")
dir.create(download_dir, showWarnings = FALSE)

# ---- Commuting, 2021 census -----------------------------------------------
# Table 98-10-0462, "Commuting destination by main mode of commuting, age and
# gender". The dimension that matters is commuting destination, which sorts
# every worker by how far they go: within their own municipality, to another
# municipality in the same census division, to another census division in the
# province, or to another province.
#
# The whole table covers every census subdivision in Canada and is 67 MB
# zipped, so it is cut down as it arrives to Waterloo Region's eight
# geographies: the Region itself (census division 3530) and its seven
# municipalities (census subdivisions 3530xxx), whose codes all start "3530".
# (Statistics Canada's coordinate service could fetch just the cells needed
# instead; a plain download is easier to check.)
get_cansim("98-10-0462") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100462.csv"))

# ---- Main mode of commuting, 2021 census -----------------------------------
# Table 98-10-0464, "Main mode of commuting by industry sectors, occupation
# broad category and gender", for the mode chart. 98-10-0462 above has mode
# too, but only for people with a usual place of work, because it is crossed
# with commuting destination. The mode question itself also covers people with
# no fixed workplace address (2021 Census Dictionary, "Main mode of
# commuting"), and so does this table: about 29,000 more commuters in the
# Region. It is the table Statistics Canada's own commuting release (The Daily,
# 2022-11-30) quotes. 98-10-0461 was checked too and has 0462's limit, since
# it is crossed with distance to a usual place of work.
#
# The table has 90 million cells, so only the 96 the chart needs are fetched,
# one by one, from Statistics Canada's web data service: 8 places x 4 modes x
# 3 statistics (the count and its 95% confidence interval bounds), with
# industry, occupation and gender at their totals. A cell is named by its
# "coordinate", the member id of each dimension in order, joined by dots:
# place . occupation . gender . statistic . industry . mode, then four zeros
# for dimensions the table does not have. Member 1 is each dimension's total.
# The ids are from the table's metadata (getCubeMetadata), where they can be
# checked.
#
# The service is called directly with httr2 rather than through cansim, whose
# cell function drops each cell's status ("..." not applicable, E, F), which
# the data-quality check needs. A cell the service does not return at all is a
# zero - census tables leave zeros out - and is written with no value and no
# status for 02_clean_data.R to fill in. wds_cells() below does the fetching,
# for this table and for 98-10-0467 further down.

# What each status code means, from Statistics Canada's own code list. Code 0
# is "normal", which has no symbol.
status_codes <- get_cansim_code_set("status") |>
  select(statusCode, STATUS = statusRepresentationEn)

# Fetches the cells named by `coordinates` from table `product_id` (written as
# a number, 98100464) in one request, and returns one row per cell: its
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
# geography (checked in 98-10-0464 and 98-10-0467).
mode_places <- tribble(
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
mode_statistics <- tribble(
  ~`Statistics (3)`,                              ~statistic_member,
  "Count",                                        1,
  "95% confidence interval lower bound, Count",   2,
  "95% confidence interval upper bound, Count",   3
)
mode_modes <- tribble(
  ~`Main mode of commuting (11A)`,   ~mode_member,
  "Total - Main mode of commuting",  1,
  "Car, truck or van",               2,
  "Public transit",                  9,
  "Active transportation",           10
)

mode_cells <- mode_places |>
  cross_join(mode_statistics) |>
  cross_join(mode_modes) |>
  mutate(COORDINATE = str_glue("{place_member}.1.1.{statistic_member}.1.{mode_member}.0.0.0.0"))

# The dimensions held at their totals are written out too, so the table's
# footnotes can be matched to them in 02_clean_data.R
mode_cells |>
  left_join(wds_cells(98100464, mode_cells$COORDINATE), join_by(COORDINATE)) |>
  mutate(
    `Occupation - Broad category - National Occupational Classification (NOC) 2021 (11)` =
      "Total - Occupation - Broad category - National Occupational Classification (NOC) 2021",
    `Gender (3)` = "Total - Gender",
    `Industry - Sectors - North American Industry Classification System (NAICS) 2017 (21)` =
      "Total - Industry - Sectors - North American Industry Classification System (NAICS) 2017"
  ) |>
  select(GeoUID, GEO, starts_with("Occupation"), `Gender (3)`, `Statistics (3)`,
         starts_with("Industry"), `Main mode of commuting (11A)`, VALUE, STATUS, COORDINATE) |>
  write_csv(file.path(raw_dir, "table_98100464.csv"))

# ---- Place of work status, 2021 census --------------------------------------
# Table 98-10-0467, "Place of work status by highest level of education, age
# and gender", for the share of workers who worked at home. Place of work
# status sorts every employed person into one of four groups: worked at home,
# worked outside Canada, no fixed workplace address, and usual place of work;
# its total is everyone employed in the census week. The table has 45 million
# cells, so like 98-10-0464 only the 48 needed are fetched: 8 places x 2
# statuses (the total and "Worked at home") x 3 statistics, with age, gender
# and education at their totals. Coordinate: place . age . gender . statistic
# . education . place of work status, then four zeros. Member ids from the
# table's metadata (getCubeMetadata).
home_statuses <- tribble(
  ~`Place of work status (5)`,       ~status_member,
  "Total - Place of work status",    1,
  "Worked at home",                  2
)

home_cells <- mode_places |>
  cross_join(mode_statistics) |>
  cross_join(home_statuses) |>
  mutate(COORDINATE = str_glue("{place_member}.1.1.{statistic_member}.1.{status_member}.0.0.0.0"))

home_cells |>
  left_join(wds_cells(98100467, home_cells$COORDINATE), join_by(COORDINATE)) |>
  mutate(
    `Age (15A)` = "Total - Age",
    `Gender (3)` = "Total - Gender",
    `Highest certificate, diploma or degree (16)` = "Total - Highest certificate, diploma or degree"
  ) |>
  select(GeoUID, GEO, `Age (15A)`, `Gender (3)`, `Statistics (3)`,
         `Highest certificate, diploma or degree (16)`, `Place of work status (5)`,
         VALUE, STATUS, COORDINATE) |>
  write_csv(file.path(raw_dir, "table_98100467.csv"))

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
# Canada, so like the flows it goes to the temporary download folder and only
# the Region's eight rows at the table's totals are kept. The file is wide: one
# column per place of work status. GNR is each area's long-form global
# non-response rate, which 02_clean_data.R checks like the 2021 rates below.
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
      `DIM: Sex (3)` == "Total - Sex",
      str_starts(`DIM: Industry - North American Industry Classification System (NAICS) 2012 (21)`, "Total")
    )
  }),
  chunk_size = 1e6,
  col_types = cols(.default = col_character())
) |>
  write_csv(file.path(raw_dir, "table_2016321_region.csv"))

# The table's notes, in the same shape as the footnotes saved for the 98-10
# tables below, so 02_clean_data.R can check them the same way. They are read
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

# ---- Commuting flows, 2021 census ------------------------------------------
# Table 98-10-0459, "Commuting flow from geography of residence to geography of
# work": for every municipality people live in, how many work in every
# municipality in Canada. It counts workers with a usual place of work, so
# people who work from home or have no fixed workplace are not in it.
#
# Every place is crossed with every other, so the table is 26 million rows -
# 292 MB zipped, 2.2 GB unzipped - almost all of them zero. It is downloaded to
# the temporary download folder (see the top of this script), and only the rows
# with one end of the commute in Waterloo Region are kept.
flows_zip <- file.path(download_dir, "98100459-eng.zip")
if (!file.exists(flows_zip)) {
  # The default 60-second timeout is too short for a file this size
  options(timeout = 1200)
  download.file(
    "https://www150.statcan.gc.ca/n1/tbl/csv/98100459-eng.zip",
    destfile = flows_zip,
    mode = "wb"
  )
}

# The metadata file, which travels in the same zip, lists every place in both
# dimensions with a member id and - for places of residence - the census
# subdivision code. The two dimensions list the same 5,161 places with the same
# member ids, so this is what turns a place-of-work member id into a code, and
# what says which member ids are the Region's seven municipalities. The file
# stacks several tables of different widths one after another, separated by
# blank lines, so the one wanted is cut out as lines before it is parsed.
metadata_lines <- read_lines(unz(flows_zip, "98100459_MetaData.csv"))
members_start <- str_which(metadata_lines, "^\"Dimension ID\",\"Member Name\"")
members_end <- members_start +
  str_which(metadata_lines[-seq_len(members_start)], "^$")[1] - 1

members <- metadata_lines[members_start:members_end] |>
  I() |>
  read_csv(col_types = cols(.default = col_character()))

write_csv(members, file.path(raw_dir, "table_98100459_members.csv"))

# The seven municipalities' member ids. Every census subdivision code begins
# with its census division code, and Waterloo Region is division 3530.
region_members <- members |>
  filter(`Dimension ID` == "1", str_starts(str_remove_all(`Classification Code`, "[^0-9]"), "3530")) |>
  pull(`Member ID`)

# read_csv_chunked() reads the file a million rows at a time and keeps only the
# rows the callback lets through, so the whole table never has to fit in
# memory. unz() reads the one file wanted straight out of the zip.
#
# Both directions are kept: the rows where somebody lives in the Region, and
# the rows where somebody works in it. The post needs both - where the Region's
# own commuters go, and where the people who work here come from - and one pass
# over 26 million rows is enough for both.
#
# Place of residence is in DGUID, the "dissemination geography unique
# identifier": a prefix saying which geography scheme it is ("2021A0005" means
# a 2021 census subdivision) followed by the census subdivision code.
#
# Place of work arrives as a name ("Guelph (CY), Ont.") rather than a code, so
# it is picked out by member id instead: `Coordinate` is "residence.work", and
# the number after the dot is the place of work's member id.
read_csv_chunked(
  unz(flows_zip, "98100459.csv"),
  callback = DataFrameCallback$new(\(chunk, pos) {
    filter(
      chunk,
      str_starts(DGUID, "2021A00053530") | str_extract(Coordinate, "[0-9]+$") %in% region_members
    )
  }),
  chunk_size = 1e6,
  col_types = cols(.default = col_character())
) |>
  write_csv(file.path(raw_dir, "table_98100459_region.csv"))

# ---- Municipal boundaries --------------------------------------------------
# Statistics Canada's 2021 census subdivision cartographic boundary file, the
# same one the welcome post uses. It covers every municipality in Canada
# (about 150 MB), so like the flows it is downloaded to the temporary folder and
# only the pieces this post draws are kept: the Region's seven municipalities,
# plus every place outside the Region that its residents commute to, so the
# map can mark where those places are.
boundary_zip <- file.path(download_dir, "lcsd000b21a_e.zip")
if (!file.exists(boundary_zip)) {
  options(timeout = 1200)
  download.file(
    "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip",
    destfile = boundary_zip,
    mode = "wb"
  )
}
boundary_dir <- file.path(download_dir, "csd_boundaries")
unzip(boundary_zip, exdir = boundary_dir)

# Every place of work the Region's own commuters travel to, read back from the
# file just written. Its member ids are matched to codes the same way
# 02_clean_data.R does it.
work_codes <- read_csv(
  file.path(raw_dir, "table_98100459_region.csv"),
  col_types = cols(.default = col_character())
) |>
  # Only the rows for people who live here, and only the places at least one of
  # them travels to: the table lists every place of work, including thousands
  # nobody goes to
  filter(str_starts(DGUID, "2021A00053530"), `Gender (3):Total - Gender[1]` != "0") |>
  mutate(member_id = str_extract(Coordinate, "[0-9]+$")) |>
  distinct(member_id) |>
  inner_join(members |> filter(`Dimension ID` == "1"), join_by(member_id == `Member ID`)) |>
  # The code is published in square brackets, "[3523008]"; keep the digits
  mutate(code = str_remove_all(`Classification Code`, "[^0-9]")) |>
  pull(code)

# st_read()'s `query` is passed to GDAL, which filters while it reads, so the
# other 5,000 polygons are never loaded. A geopackage is a single-file spatial
# format, which keeps data-raw/ tidy.
sf::st_read(
  file.path(boundary_dir, "lcsd000b21a_e.shp"),
  query = paste0(
    "SELECT * FROM lcsd000b21a_e WHERE CSDUID IN ('",
    paste(unique(work_codes), collapse = "','"),
    "') OR CSDUID LIKE '3530%'"
  ),
  quiet = TRUE
) |>
  sf::write_sf(file.path(raw_dir, "csd_boundaries.gpkg"), delete_dsn = TRUE)

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
# question, cut to the Region's eight geographies like 98-10-0462 above
get_cansim("98-10-0572") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100572.csv"))

# ---- Footnotes -------------------------------------------------------------
# Each Statistics Canada table carries footnotes, some of them about the
# quality or comparability of the figures. They are saved beside the tables so
# that 02_clean_data.R can report the ones that apply to the rows this post
# keeps (cwr_quality_flags() in R/data_quality.R) without going back online.
c("98-10-0462", "98-10-0464", "98-10-0467", "98-10-0459") |>
  walk(\(table_number) {
    get_cansim_table_notes(table_number) |>
      write_csv(file.path(
        raw_dir,
        str_c("table_", str_remove_all(table_number, "-"), "_notes.csv")
      ))
  })
