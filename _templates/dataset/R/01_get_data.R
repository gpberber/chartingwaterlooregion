# 01_get_data.R
# Fetches every raw input for the "{{title}}" dataset into data-raw/.
# data-raw/ is gitignored; this script is how anyone recreates it.
# Run from the project root: source("datasets/{{slug}}/R/01_get_data.R")

library(tidyverse)
library(here)

raw_dir <- here("datasets", "{{slug}}", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- Option A: download directly from the source --------------------------
# download.file("https://example.org/file.csv", file.path(raw_dir, "file.csv"), mode = "wb")

# ---- Option B: Statistics Canada tables via cansim ------------------------
# library(cansim)
# get_cansim("00-00-0000-01") |> write_csv(file.path(raw_dir, "table.csv"))

# ---- Option C: files that cannot be re-downloaded (attached to a release) --
# source(here("R", "data_helpers.R"))
# cwr_data_download("{{slug}}", kind = "data-raw", root = "datasets")
