# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads. Keep outputs under 25 MB each so they can be committed; if
# one must be bigger, /share-data moves it to a GitHub Release.
# Run from the project root: source("posts/{{slug}}/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir <- here("posts", "{{slug}}", "data-raw")
data_dir <- here("posts", "{{slug}}", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Read raw files (clean_names() right after every read) ----------------
# raw <- read_csv(file.path(raw_dir, "file.csv")) |> clean_names()

# ---- Tidy ----------------------------------------------------------------
# clean <- raw |>
#   ...

# ---- Write ---------------------------------------------------------------
# write_csv(clean, file.path(data_dir, "clean.csv"))
