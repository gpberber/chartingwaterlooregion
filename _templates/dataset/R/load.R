# load.R
# Loads the cleaned "{{title}}" dataset into named objects for a post:
#
#   source(here::here("datasets", "{{slug}}", "R", "load.R"))
#
# Every post that uses this dataset gets the same objects with the same names.

library(here)
library(readr)
library(arrow)

data_dir_{{slug_snake}} <- here("datasets", "{{slug}}", "data")

read_{{slug_snake}} <- function(file) {
  path <- file.path(data_dir_{{slug_snake}}, file)
  if (!file.exists(path)) {
    stop("Missing ", path, ". Run datasets/{{slug}}/R/01_get_data.R then 02_clean_data.R.")
  }
  if (grepl("\\.parquet$", path)) read_parquet(path) else read_rds(path)
}

# ---- Objects for posts (one line each, with a comment saying what it is) -----
# example <- read_{{slug_snake}}("example.rds")   # one row per region x year

source(here("datasets", "{{slug}}", "R", "helpers.R"))
