# 02_clean_data.R
# Turns data-raw/ into the tidy, complete, general-purpose files in data/ that
# posts load through load.R. No story-specific filtering here: keep every year
# and every region so any post can use the result.
# Files under 25 MB are committed; bigger ones are written as Parquet and
# attached to a GitHub Release with /share-data.
# Run from the project root: source("datasets/{{slug}}/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(arrow)
library(here)

source(here("datasets", "{{slug}}", "R", "helpers.R"))

raw_dir <- here("datasets", "{{slug}}", "data-raw")
data_dir <- here("datasets", "{{slug}}", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Read raw files (clean_names() right after every read) ----------------
# raw <- read_csv(file.path(raw_dir, "file.csv")) |> clean_names()

# ---- Tidy ----------------------------------------------------------------
# clean <- raw |> ...

# ---- Write ---------------------------------------------------------------
# write_rds(clean, file.path(data_dir, "clean.rds"))            # small
# write_parquet(big, file.path(data_dir, "big.parquet"))        # large: goes to a release
