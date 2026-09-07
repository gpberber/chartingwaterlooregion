# 01_get_data.R
# Fetches the raw inputs for the "Kitchener phone wait times" post into data-raw/.
# Run from the project root: source("posts/kitchener-phone-wait-times/R/01_get_data.R")

here::i_am("posts/kitchener-phone-wait-times/R/01_get_data.R")

library(here)
source(here("R", "data_helpers.R"))

raw_dir <- here("posts", "kitchener-phone-wait-times", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# Ontario statutory holidays (small, public, re-downloadable)
download.file(
  "https://raw.githubusercontent.com/uWaterloo/Datasets/master/Holidays/holidays.csv",
  destfile = file.path(raw_dir, "holidays.csv"),
  mode = "wb"
)

# The phone queue metrics export from the City of Kitchener open data portal,
# as downloaded in February 2025, attached to GitHub Release
# data-raw-kitchener-phone-wait-times-v1
cwr_data_download("kitchener-phone-wait-times", kind = "data-raw", version = 1)
