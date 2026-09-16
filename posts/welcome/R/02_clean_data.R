# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads.
# Run from the project root: source("posts/welcome/R/02_clean_data.R")

here::i_am("posts/welcome/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)
library(sf)      # simple features: spatial data held as an ordinary data frame
                 # with one extra list column, `geometry`, holding the polygons

raw_dir  <- here("posts", "welcome", "data-raw")
data_dir <- here("posts", "welcome", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- 1. The seven municipal polygons ---------------------------------------
# The shapefile holds every census subdivision in Canada, about 5,000 polygons
# and 300 MB of geometry, so it is filtered while it is being read rather than
# after. st_read()'s `query` argument is passed to GDAL, the library sf uses to
# open spatial files, and GDAL applies it before anything is loaded into R.
#
# The filter is on the CSD identifier rather than on names. Every census
# subdivision code begins with its census division code, and Waterloo Region is
# division 3530, so the seven municipalities inside it are exactly the codes
# beginning "3530". Matching on names would be fragile: there is a Waterloo in
# Quebec and a Cambridge in New Brunswick.
csd_path <- file.path(raw_dir, "csd_boundaries", "lcsd000b21a_e.shp")

districts_raw <- st_read(
  csd_path,
  query = "SELECT * FROM lcsd000b21a_e WHERE CSDUID LIKE '3530%'",
  quiet = TRUE
) |>
  clean_names() |>
  # GDAL calls the geometry column `_ogr_geometry_` when a query is used, and
  # clean_names() leaves it alone. sf's rename() keeps track of which column
  # holds the geometry, so renaming it here is safe and the rest of the script
  # can refer to `geometry` like any other sf object.
  rename(geometry = "_ogr_geometry_")

# ---- 2. Project to latitude and longitude ----------------------------------
# The file arrives in "NAD83 / Statistics Canada Lambert", a projection that
# flattens Canada onto a plane. The map is drawn over web map tiles, which use
# their own projection, and area is measured most accurately on the globe
# itself, so everything is moved to plain latitude and longitude (EPSG:4326)
# here and reprojected once more at drawing time.
districts <- districts_raw |>
  st_transform(4326)

# ---- 3. Area ---------------------------------------------------------------
# `landarea` arrives with the boundary file: Statistics Canada's own 2021 census
# land area for each municipality, already in square kilometres.
#
# The alternative was to measure the polygons with st_area(), and that is what
# this script used to do. The published figure is better on both counts that
# matter. It is the number anyone else quoting a census area will have, so the
# post agrees with them rather than being a percent or two out for reasons only
# this script knows about. And it is land area: it excludes inland water -
# rivers, reservoirs, flooded gravel pits - where measuring the polygon counts
# everything inside the municipal border, water included.
districts <- districts |>
  rename(area_sq_km = landarea)

# ---- 4. Population --------------------------------------------------------
# Table 17-10-0155-01 covers every municipality in Canada for every year since
# 2001, so it is cut to 2025 and to the seven codes already in `districts`.
# `geo_uid` in the Statistics Canada table is the same seven-digit census
# subdivision code as `csduid` in the boundary file, which is what lets the two
# be joined without matching on names.
# Only three of the table's eighteen columns are wanted, and `col_select` drops
# the rest while the file is being read, which is quicker over 130,000 rows and
# sidesteps parsing complaints about footnote columns this post never touches.
#
# `col_types` forces the identifier to be read as text. Left to itself, readr
# would see seven digits and make it a number, which would not join to the
# character codes in the boundary file (and would drop any leading zero in a
# province whose codes have one).
population <- read_csv(file.path(raw_dir, "table_17100155.csv"),
                       col_select = c(REF_DATE, GeoUID, VALUE),
                       col_types = cols(GeoUID = col_character(),
                                        REF_DATE = col_character(),
                                        VALUE = col_double())) |>
  clean_names() |>
  filter(ref_date == "2025") |>
  select(csduid = geo_uid, population = value)

districts <- districts |>
  left_join(population, join_by(csduid)) |>
  mutate(
    # People per square kilometre, on the same polygon area reported above so
    # the three numbers in each map label are arithmetically consistent.
    density_per_sq_km = population / area_sq_km,

    # `csdtype` is a Statistics Canada code: CY is a city, TP a township. The
    # post labels each area as one or the other, so the code becomes a word.
    district_type = recode_values(csdtype, "CY" ~ "City", "TP" ~ "Township")
  ) |>
  select(csduid, district = csdname, district_type, population,
         area_sq_km, density_per_sq_km) |>
  arrange(desc(population))

# ---- 5. Label positions ----------------------------------------------------
# Each area gets a text label sitting inside it, so each needs an anchor point
# that is comfortably in the middle of the shape.
#
# A centroid is the obvious choice and the wrong one: for a crescent or an
# L-shaped municipality the centroid can fall outside the polygon altogether.
# st_point_on_surface() fixes that - it is guaranteed to land inside - but it
# only promises "inside", not "roomy", and it happily returns a point in a
# narrow arm of the shape where a three-line label would not fit.
#
# st_inscribed_circle() answers the question actually being asked: what is the
# largest circle that fits inside this polygon? Its centre is the point
# furthest from any edge, which is exactly where a label has the most room.
# GEOS, the geometry engine underneath sf, computes it by repeatedly
# subdividing the polygon; `dTolerance` is how precise to be, in metres, and
# 10 m is far finer than anything visible on a map of a whole region.
#
# Two details. The work is done in EPSG:3161, a projection made for Ontario,
# because "largest circle" is a question about a flat plane and means nothing
# on a globe. And the function is called one municipality at a time, because
# given several at once it returns all their circles in a single unlabelled
# bag with no way to tell which belongs to which.
label_points <- districts |>
  st_transform(3161) |>
  st_geometry() |>
  map(function(polygon) {
    circle <- st_inscribed_circle(st_sfc(polygon, crs = 3161), dTolerance = 10)
    circle <- circle[!st_is_empty(circle)]

    # The circle comes back as a polygon, so its centre is the middle of its
    # bounding box and its radius is half that box's width. The radius is kept
    # because it says how much room the label has: index.qmd uses it to decide
    # which labels have to be nudged off centre or shrunk.
    box <- st_bbox(circle)
    tibble(
      label_x = unname((box$xmin + box$xmax) / 2),
      label_y = unname((box$ymin + box$ymax) / 2),
      label_room_km = unname((box$xmax - box$xmin) / 2 / 1000)
    )
  }) |>
  list_rbind() |>
  st_as_sf(coords = c("label_x", "label_y"), crs = 3161) |>
  st_transform(4326)

districts <- districts |>
  bind_cols(
    label_room_km = label_points$label_room_km,
    st_coordinates(label_points) |>
      as_tibble() |>
      set_names(c("label_lon", "label_lat"))
  )

# ---- 6. Write -------------------------------------------------------------
# GeoJSON is a plain-text spatial format: readable, versionable, and readable
# by anything. These seven simplified polygons come to well under a megabyte,
# so the file is committed with the post.
write_sf(districts, file.path(data_dir, "districts.geojson"), delete_dsn = TRUE)

# The same table without the geometry, for the data download bundle and for
# anyone who wants the numbers and not the shapes.
districts |>
  st_drop_geometry() |>
  write_csv(file.path(data_dir, "districts.csv"))

message("Wrote ", nrow(districts), " districts to ", data_dir)

# ---- 7. Employment by industry ---------------------------------------------
# Table 14-10-0468-01 covers 38 census metropolitan areas and every year from
# 2011, so it is cut to one place and one year while being read.
employment_raw <- read_csv(
  file.path(raw_dir, "table_14100468.csv"),
  col_select = c(REF_DATE, GEO, `Employment characteristics`,
                 `Hierarchy for Employment characteristics`, VALUE, STATUS),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(geo == "Kitchener-Cambridge-Waterloo, Ontario", ref_date == "2025")

# Everyone working in the CMA, whatever their industry. Kept as the denominator
# for the share below rather than adding up the industries, because two of them
# are suppressed (see the filter's comment) and so would not sum to the truth.
total_employed <- employment_raw |>
  filter(employment_characteristics == "Total employed") |>
  pull(value)

# Statistics Canada shortens some of these names beyond what fits on a chart
# axis, and lengthens others past it. The short forms drop qualifiers that the
# category name carries anyway; nothing is merged or split, so every short name
# still means exactly one of the table's categories.
industry_names <- tribble(
  ~employment_characteristics,                           ~industry,
  "Forestry, fishing, mining, quarrying, oil and gas",    "Forestry, mining, oil and gas",
  "Finance, insurance, real estate, rental and leasing",  "Finance, insurance and real estate",
  "Professional, scientific and technical services",      "Professional and technical services",
  "Business, building and other support services",        "Business and building support",
  "Other services (except public administration)",        "Other services"
)

employment <- employment_raw |>
  # The `employment_characteristics` column stacks three separate breakdowns of
  # the same workers - industry, occupation, and class of worker - so taking it
  # whole would count everyone three times over. The hierarchy column is how
  # they are told apart: it numbers each row's place in the tree, and the
  # industries are the children of "Goods-producing sector" (1.2) and
  # "Services-producing sector" (1.8). Matching "1.2." or "1.8." therefore
  # takes the sixteen industries and leaves the sector subtotals, the
  # occupations, and the total behind.
  filter(str_detect(hierarchy_for_employment_characteristics, "^1[.](2|8)[.]")) |>
  left_join(industry_names, join_by(employment_characteristics)) |>
  mutate(
    industry = coalesce(industry, employment_characteristics),
    # STATUS "x" means Statistics Canada suppressed the figure to protect a
    # respondent's confidentiality - in a place this size that happens to the
    # smallest industries. Those rows are kept here with an empty value, so the
    # data file says plainly that the industry exists and its number does not.
    # (STATUS is empty for every industry that was published, and coalesce()
    # turns that empty into FALSE rather than leaving a missing value.)
    suppressed = coalesce(status == "x", FALSE),
    # The table is already in thousands of people; the share is a percentage of
    # everyone employed in the CMA, so the sixteen do not quite add to 100.
    share_percent = value / total_employed * 100
  ) |>
  select(industry, employed_thousands = value, share_percent, suppressed) |>
  arrange(desc(employed_thousands))

write_csv(employment, file.path(data_dir, "employment.csv"))

message("Wrote ", nrow(employment), " industries to ", data_dir)

# ---- 8. Income, restated in today's dollars --------------------------------
# The 2021 census reports income earned in 2020. Read in 2026 those figures
# understate what people have, so they are restated in 2026 dollars: the same
# income multiplied by what the same basket of goods now costs against what it
# cost then. That ratio is all a CPI adjustment is.
#
# Which years to compare is a judgement call, and this is the one made here.
# 2020 is a finished calendar year, so its index is the average of its twelve
# months - the arithmetic Statistics Canada itself uses to publish an annual
# average. 2026 is not finished, so its index is the average of the months
# published so far. That is the honest reading of "2026 dollars" in September:
# the year to date, not a guess at how it ends. Re-running this script after
# more months are published moves the figures slightly, which is correct.
income_year <- "2020"   # the year the census asked about
dollar_year <- "2026"   # the year the post is written for

cpi <- read_csv(
  file.path(raw_dir, "table_18100004.csv"),
  col_select = c(REF_DATE, VALUE),
  col_types = cols(REF_DATE = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  # REF_DATE is "2026-08", so the first four characters are the year
  mutate(year = str_sub(ref_date, 1, 4)) |>
  summarise(index = mean(value), months = n(), .by = year)

inflator <- (cpi |> filter(year == dollar_year) |> pull(index)) /
  (cpi |> filter(year == income_year) |> pull(index))

# Median household total income. The table crosses the income statistics with
# household size and with household type, so both of those are held at their
# "Total" line to get the figure for all households rather than for, say,
# four-person couple families. They are matched on the word "Total" rather than
# on the full member name because one of the two contains an en dash, a
# character that is easy to mistype and impossible to see in a script.
household_income <- read_csv(
  file.path(raw_dir, "table_98100057.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    str_starts(household_size_7, "Total"),
    str_starts(household_type_including_census_family_structure_11, "Total"),
    household_income_statistics_6 ==
      "Median household total income (2020) (2020 constant dollars)"
  ) |>
  select(geo_uid, household_income = value)

# Median total income of a person aged 15 or over who had any income. This
# table splits income into sources - wages, investments, pensions, transfers -
# so "Total income" is the line that adds them up, and "Median amount ($)" is
# the statistic rather than the count of people or the average.
individual_income <- read_csv(
  file.path(raw_dir, "table_98100070.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    income_sources_and_taxes_32 == "Total income",
    income_statistics_8 == "Median amount ($)"
  ) |>
  select(geo_uid, individual_income = value)

income <- household_income |>
  left_join(individual_income, join_by(geo_uid)) |>
  # Names and city/township come from `districts`, which has already tidied
  # both, so the two files cannot disagree about what a place is called. The
  # eighth row has no match there: it is the census division, and Statistics
  # Canada calls it "Waterloo", the same name as the city inside it. It is
  # named here so a reader of the chart is never in doubt which is which.
  left_join(
    districts |> st_drop_geometry() |> select(csduid, district, district_type),
    join_by(geo_uid == csduid)
  ) |>
  mutate(
    district = coalesce(district, "Waterloo Region"),
    district_type = coalesce(district_type, "Region"),
    # Both years are kept. The 2020 figures are what Statistics Canada
    # published and are what anyone checking this against the census will see;
    # the 2026 ones are this script's arithmetic on top of them.
    household_income_2026 = household_income * inflator,
    individual_income_2026 = individual_income * inflator
  ) |>
  select(
    district, district_type,
    household_income_2020 = household_income,
    individual_income_2020 = individual_income,
    household_income_2026, individual_income_2026
  ) |>
  arrange(desc(household_income_2026))

write_csv(income, file.path(data_dir, "income.csv"))

message("Wrote ", nrow(income), " income rows to ", data_dir,
        " (", income_year, " dollars x ", round(inflator, 4), " = ",
        dollar_year, " dollars)")
