# 01_get_data.R
# Fetches every raw input for the "{{title}}" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/{{slug}}/R/01_get_data.R")

library(tidyverse)
library(here)

raw_dir <- here("posts", "{{slug}}", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- Option A: download directly from the source --------------------------
# download.file(
#   "https://example.org/open-data/file.csv",
#   destfile = file.path(raw_dir, "file.csv"),
#   mode = "wb"
# )

# ---- Option B: Statistics Canada tables via cansim ------------------------
# library(cansim)
# get_cansim("35-10-0177-01") |> write_csv(file.path(raw_dir, "table_35100177.csv"))

# ---- Option C: files that cannot be re-downloaded (manual exports) --------
# They are attached to a GitHub Release by the /share-data skill:
# source(here("R", "data_helpers.R"))
# cwr_data_download("{{slug}}", kind = "data-raw")
