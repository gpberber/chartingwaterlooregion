# 01_get_data.R
# Fetches the raw inputs for the "Collisions near Kitchener schools" post into
# data-raw/. The files were downloaded by hand from the City of Kitchener open
# data portal and the two school boards' websites (see README.md), so they are
# attached to a GitHub Release rather than re-downloaded from the source.
# Run from the project root: source("posts/kitchener-school-collisions/R/01_get_data.R")

here::i_am("posts/kitchener-school-collisions/R/01_get_data.R")

source(here::here("R", "data_helpers.R"))

# Downloads kitchener_collisions.csv, traffic_collisions.geojson,
# public_schools.htm and catholic_schools.pdf into data-raw/
cwr_data_download("kitchener-school-collisions", kind = "data-raw", version = 1)
