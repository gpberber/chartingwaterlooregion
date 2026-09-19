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

# ---- 3. Cattle, 2021 Census of Agriculture --------------------------------
# Table 32-10-0370-01, "Cattle inventory on farms, Census of Agriculture, 2021":
# head of cattle on census day, 11 May 2021, by type of animal. The whole table
# is small (46,000 figures), so it is downloaded in full and cut to Waterloo
# Region: the census division (GeoUID 3530) and its places (3530xxx).
#
# Its smallest geography is the census consolidated subdivision (CCS), a group
# of neighbouring municipalities, not the municipality itself. Checked against
# the 2021 geographic attribute file (Statistics Canada 92-151): each of the
# four townships is a CCS on its own, under its own municipal code, while the
# City of Waterloo is folded into the Kitchener CCS. So the township rows are
# the townships exactly; the city rows would not be the cities.
get_cansim("32-10-0370") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_32100370.csv"))

# ---- 4. Population, 2021 census --------------------------------------------
# Table 98-10-0002-01, "Population and dwelling counts: Canada and census
# subdivisions (municipalities)": the 2021 census count, taken on the same day
# as the Census of Agriculture. A count rather than the yearly estimates in
# section 2, so that the people and the cattle are counted on the same day by
# the same census.
get_cansim("98-10-0002") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100002.csv"))

# ---- 5. Footnotes ----------------------------------------------------------
# Each Statistics Canada table carries footnotes, some of them about the
# quality or comparability of the figures. They are saved beside the tables so
# that 02_clean_data.R can report the ones that apply to the rows this post
# keeps (cwr_quality_flags() in R/data_quality.R) without going back online.
# The file names match the tables': table_17100155_notes.csv and so on.
c("17-10-0155-01", "32-10-0370-01", "98-10-0002-01") |>
  walk(\(table_number) {
    get_cansim_table_notes(table_number) |>
      write_csv(file.path(
        raw_dir,
        str_c("table_", str_sub(str_remove_all(table_number, "-"), 1, 8), "_notes.csv")
      ))
  })
