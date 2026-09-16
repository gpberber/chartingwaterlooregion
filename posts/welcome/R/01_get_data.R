# 01_get_data.R
# Fetches the raw inputs for the "Welcome to Charting Waterloo Region!" post
# into data-raw/. That folder is gitignored, so this script is how a reader
# (or future you) recreates it.
# Run from the project root: source("posts/welcome/R/01_get_data.R")

here::i_am("posts/welcome/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "welcome", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- 1. Municipal boundaries ----------------------------------------------
# Statistics Canada's 2021 census subdivision (CSD) cartographic boundary file.
# A "census subdivision" is StatCan's word for a municipality, so the seven
# pieces of Waterloo Region - three cities and four townships - are seven CSDs
# inside census division 3530.
#
# Two other sources were considered and rejected. The Region's own open-data
# portal publishes a municipal boundary layer, and it is a much smaller file,
# but its polygons are not guaranteed to match the geography the population
# estimates are counted on. StatCan's file is, because the estimates in step 2
# are published against these very CSD codes. Using one source for both means
# the areas and the populations describe the same shapes, which is what makes
# the density figure honest.
#
# The download is about 150 MB: it covers every municipality in Canada and
# there is no per-province version. It only has to be done once, and
# 02_clean_data.R immediately cuts it down to the seven polygons we keep.
#
# "b" in the file name is the cartographic version, whose coastlines and
# borders are simplified for mapping (the "a" version is the full-detail legal
# one, and is larger still).
boundary_zip <- file.path(raw_dir, "lcsd000b21a_e.zip")

if (!file.exists(boundary_zip)) {
  download.file(
    "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip",
    destfile = boundary_zip,
    mode = "wb"
  )
}

# The zip holds an ESRI shapefile: one logical layer spread over several files
# (.shp geometry, .dbf attributes, .prj projection, and so on), which is why
# the whole set is unpacked rather than a single file pulled out.
unzip(boundary_zip, exdir = file.path(raw_dir, "csd_boundaries"))

# ---- 2. Population estimates ----------------------------------------------
# Table 17-10-0155-01, "Population estimates, July 1, by census subdivision,
# 2021 boundaries". These are StatCan's yearly estimates, not census counts:
# the census is taken every five years, and in between StatCan updates the
# figure for births, deaths, and migration. 2025 is the most recent year.
#
# get_cansim() downloads the whole table (every municipality in Canada, 2001
# onward) and caches it, so re-running this is cheap.
get_cansim("17-10-0155-01") |>
  write_csv(file.path(raw_dir, "table_17100155.csv"))

# ---- 3. Income, 2021 census ------------------------------------------------
# Two census tables, both published for census subdivisions:
#
#   98-10-0057-01  median household total income
#   98-10-0070-01  median total income of people aged 15 and over
#
# Both report income earned in 2020, the calendar year before the census. That
# is as recent as census income gets: the census is taken every five years and
# asks about the year before it.
#
# Each table covers every census subdivision in Canada - 98-10-0057-01 alone is
# 2.5 million rows - and only Waterloo Region's eight geographies are ever
# used, so each is cut to them here rather than written out whole. The other
# raw files in this folder are untouched downloads; these two are the exception
# because writing them whole would leave 400 MB on disk to reach eight rows.
#
# Every census subdivision code begins with its census division code, and
# Waterloo Region is division 3530, so codes starting "3530" are the seven
# municipalities. The bare code "3530" is the division itself - the Region as a
# whole - which is the eighth row, and it comes free with the same filter.
walk2(
  c("98-10-0057-01", "98-10-0070-01"),
  c("table_98100057.csv", "table_98100070.csv"),
  \(table_number, file_name) {
    get_cansim(table_number) |>
      filter(str_starts(GeoUID, "3530")) |>
      write_csv(file.path(raw_dir, file_name))
  }
)

# ---- 4. Consumer Price Index ----------------------------------------------
# Table 18-10-0004-01, the monthly CPI, used to restate those 2020 incomes in
# today's dollars.
#
# All-items for Ontario. The CPI is not published for Kitchener-Cambridge-
# Waterloo - in Ontario only Toronto, Ottawa and Thunder Bay get their own
# index - so the province is the closest published basket. Canada-wide would
# also be defensible; Ontario is nearer the prices these households pay.
#
# The monthly table rather than the annual averages in 18-10-0005-01, because
# the current year has no annual average yet and 02_clean_data.R needs one.
# Averaging the months of a year is exactly how Statistics Canada computes it.
# Filtered on the way in for the same reason as above: over a million rows.
get_cansim("18-10-0004-01") |>
  filter(GEO == "Ontario", `Products and product groups` == "All-items") |>
  write_csv(file.path(raw_dir, "table_18100004.csv"))

# ---- 5. Households and dwellings, 2021 census ------------------------------
# Table 98-10-0041, "Structural type of dwelling and household size". One table
# answers two questions at once: what kind of dwelling people live in, and how
# many of them live in it. It publishes average household size directly, which
# is better than working one out here from the size categories - their top band,
# "5 or more persons", is open-ended and cannot be averaged honestly.
#
# Cut to Waterloo Region on the way in, for the reason given in section 3.
get_cansim("98-10-0041") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100041.csv"))

# ---- 6. Mother tongue, 2021 census ----------------------------------------
# Named languages, which only the detailed mother tongue table carries. The
# broad-category tables - English, French, non-official and nothing finer - are
# no use here, because the whole question is which non-official language.
#
# 98-10-0180 crosses every census subdivision in Canada with 538 languages and
# is 618 MB zipped; thirty-two numbers from it are wanted. So it is fetched a
# cell at a time through Statistics Canada's coordinate service instead of being
# downloaded at all.
#
# A coordinate names one cell of the cube: the member number of each dimension
# in order, which here is geography, age, gender, mother tongue, and single or
# multiple response. Member 1 is the "Total" of any dimension, so
# "2445.1.1.256.1" is Wellesley, all ages, all genders, Pennsylvania German,
# single and multiple responses counted together.
#
# The member numbers are the cube's own, read from its metadata (Statistics
# Canada's getCubeMetadata service, product 98100180) and fixed for the life of
# the table. They are written out here beside the names the metadata gives them,
# so that a reader can check them rather than trust them.
mother_tongue_places <- tribble(
  ~member, ~place,
  2439,    "Waterloo Region",   # the census division, not the city of the name
  2440,    "North Dumfries",
  2441,    "Cambridge",
  2442,    "Kitchener",
  2443,    "Waterloo",
  2444,    "Wilmot",
  2445,    "Wellesley",
  2446,    "Woolwich"
)

mother_tongue_languages <- tribble(
  ~member, ~language,
  1,       "Total - Mother tongue",
  5,       "Non-official languages",
  253,     "German",
  256,     "Pennsylvania German"
)

# Every place crossed with every language. A cell with nothing in it -
# Pennsylvania German in North Dumfries - does not come back at all, which
# 02_clean_data.R reads as a zero.
expand_grid(
  geography = mother_tongue_places$member,
  language = mother_tongue_languages$member
) |>
  mutate(
    cansimTableNumber = "98-10-0180",
    COORDINATE = paste(geography, 1, 1, language, 1, sep = ".")
  ) |>
  select(cansimTableNumber, COORDINATE) |>
  get_cansim_data_for_table_coord_periods(periods = 1) |>
  # What comes back names the geography only as "Waterloo (2)" and the like -
  # the service disambiguates duplicate names with a number rather than a code -
  # so the place is put back on from the coordinate that asked for it.
  mutate(geography = as.integer(str_split_i(COORDINATE, fixed("."), 1))) |>
  left_join(mother_tongue_places, join_by(geography == member)) |>
  write_csv(file.path(raw_dir, "table_98100180_coords.csv"))

# ---- 7. Commuting, 2021 census --------------------------------------------
# Table 98-10-0462, "Commuting destination by main mode of commuting, age and
# gender". The dimension that matters is commuting destination, which sorts
# every worker by how far they go: within their own municipality, to another
# municipality in the same census division, to another census division in the
# province, or to another province.
#
# Taken whole and cut down here rather than fetched cell by cell as in section
# 6: at 67 MB zipped it is large but not absurd, and a plain download is easier
# to check than a list of coordinate numbers. The cell-by-cell route is for the
# tables where no other option exists.
get_cansim("98-10-0462") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100462.csv"))
