# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads. Keep outputs under 25 MB each so they can be committed; if
# one must be bigger, /share-data moves it to a GitHub Release.
# Run from the project root: source("posts/{{slug}}/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

source(here("R", "data_quality.R"))   # cwr_quality_flags()

raw_dir <- here("posts", "{{slug}}", "data-raw")
data_dir <- here("posts", "{{slug}}", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Read raw files (clean_names() right after every read) ----------------
# raw <- read_csv(file.path(raw_dir, "file.csv")) |> clean_names()

# ---- Keep only what the post uses -----------------------------------------
# kept <- raw |> filter(geo %in% places, ref_date >= 2016)

# ---- Data-quality flags --------------------------------------------------
# Reports every quality symbol (E use with caution, F too unreliable, x
# suppressed, A-D grades, p preliminary...) and every quality footnote that
# applies to the rows kept, and records them in data/quality_flags.csv. Run it
# on `kept`, before the flag columns and missing values are dropped, once per
# source table. See R/data_quality.R for wide census tables and other sources.
#
# It also blanks every figure this site never uses - E, F, x and .. - to NA and
# hands the data back, so assign the result and carry on with it. Calling it
# without `kept <-` would report the E figures and then use them anyway.
# kept <- kept |>
#   cwr_quality_flags(
#     "35-10-0177-01",
#     notes = read_csv(file.path(raw_dir, "table_35100177_notes.csv")),
#     log   = file.path(data_dir, "quality_flags.csv")
#   )

# ---- Tidy ----------------------------------------------------------------
# clean <- kept |>
#   ...

# ---- Write ---------------------------------------------------------------
# write_csv(clean, file.path(data_dir, "clean.csv"))
