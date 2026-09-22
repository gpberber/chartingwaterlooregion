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
# Save the table's footnotes beside it. 02_clean_data.R reads them to report the
# ones about data quality that apply to the rows the post keeps.
# get_cansim_table_notes("35-10-0177-01") |> write_csv(file.path(raw_dir, "table_35100177_notes.csv"))

# Census long-form data only (cwr-charts rule 9b): each area's long-form total
# non-response rate, and the topic's per-question non-response and imputation
# rates ("Long-form data quality indicators for ...", 98-10-0572 for commuting,
# 98-10-0569 labour, 98-10-0566 mobility). DGUIDs: "2021A0003" + a census
# division code, "2021A0005" + a census subdivision code.
# source(here("R", "data_quality.R"))
# cwr_census_tnr(c("2021A00033530", "2021A00053530013")) |>
#   write_csv(file.path(raw_dir, "census_tnr.csv"))
# get_cansim("98-10-0572") |> filter(str_starts(GeoUID, "3530")) |>
#   write_csv(file.path(raw_dir, "table_98100572.csv"))

# ---- Option C: files that cannot be re-downloaded (manual exports) --------
# They are attached to a GitHub Release by the /share-data skill:
# source(here("R", "data_helpers.R"))
# cwr_data_download("{{slug}}", kind = "data-raw")
