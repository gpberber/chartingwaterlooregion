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

# ---- 3. Employment by industry --------------------------------------------
# Table 14-10-0468-01, "Employment by industry, annual, census metropolitan
# areas". These are Labour Force Survey estimates - a monthly household survey,
# averaged over the year - reported in thousands of people.
#
# The geography is the Kitchener-Cambridge-Waterloo census metropolitan area,
# not Waterloo Region. A CMA is built by Statistics Canada out of whole
# municipalities around an urban core, so the two are close but not the same
# thing; a chart drawn from this table is about the CMA and should say so.
#
# The table stacks three different cuts of the same people - by industry, by
# occupation, and by class of worker - in one column, so 02_clean_data.R has
# to pick out the industries.
get_cansim("14-10-0468-01") |>
  write_csv(file.path(raw_dir, "table_14100468.csv"))
