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

# ---- 3. Area straight off the polygons -------------------------------------
# st_area() measures each polygon. Because the data is now in latitude and
# longitude, sf measures on the curved surface of the earth (through the s2
# library) instead of on a flat projection, which is the accurate way to do it.
# The answer comes back in square metres carrying a "units" attribute, so it is
# converted to square kilometres and then stripped back to a plain number, which
# is easier to format in a chart label.
#
# This is the area of the polygon as drawn, which is what the post says it is.
# It differs a little from the `landarea` column Statistics Canada ships in the
# same file, because that one excludes inland water - rivers, reservoirs, gravel
# pits - while the polygon includes everything inside the municipal border.
districts <- districts |>
  mutate(
    area_sq_km = st_area(geometry) |> units::set_units("km^2") |> as.numeric()
  )

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
