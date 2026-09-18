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
# Carry the quality flags through. A post filters the dataset to its own rows
# and runs cwr_quality_flags() on them; it can only do that if each figure's
# flag and each table's footnotes are in data/:
#   - a long table (one row per figure) keeps its status and symbol columns
#   - a table pivoted wide by statistic cannot, so write every flagged figure
#     to a separate long file instead - datasets/crime/R/03_quality_flags.R
#     is the worked example
# file.copy(file.path(raw_dir, "table_notes.csv"), data_dir, overwrite = TRUE)
# clean <- raw |> ...

# ---- Write ---------------------------------------------------------------
# write_rds(clean, file.path(data_dir, "clean.rds"))            # small
# write_parquet(big, file.path(data_dir, "big.parquet"))        # large: goes to a release
