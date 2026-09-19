# 01_get_data.R
# Fetches every raw input for the "Farm animals in the townships" post into
# data-raw/. data-raw/ is gitignored, so this script is how a reader (or future
# you) recreates it. Run from the project root:
# source("posts/agriculture/R/01_get_data.R")

here::i_am("posts/agriculture/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "agriculture", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- 1. Census of Agriculture livestock tables ---------------------------
# Six Statistics Canada tables: cattle, sheep and pigs on census day, once for
# 2021 and once for 2011 and 2016 together. The 2011-2016 tables are marked
# "inactive" - they will not be updated again - but are still published.
#
#   32-10-0370-01  Cattle inventory on farms, 2021
#   32-10-0371-01  Sheep inventory on farms, 2021
#   32-10-0372-01  Pig inventory on farms, 2021
#   32-10-0424-01  Cattle inventory on farms, 2011 and 2016
#   32-10-0425-01  Sheep inventory on farms, 2011 and 2016
#   32-10-0426-01  Pig inventory on farms, 2011 and 2016
#
# Each is small, so it is downloaded in full and cut to Waterloo Region: the
# census division (GeoUID 3530) and its places (3530xxx). The smallest
# geography is the census consolidated subdivision (CCS), a group of
# neighbouring municipalities. Each of the four townships is a CCS on its own,
# under its own municipal code, in all three censuses (the same seven codes
# appear in every table), so the township rows are the townships exactly. See
# the welcome post's 01_get_data.R, section 7, for how that was checked.
livestock_tables <- c(
  "32-10-0370-01", "32-10-0371-01", "32-10-0372-01",
  "32-10-0424-01", "32-10-0425-01", "32-10-0426-01"
)

# The file name is the table number without its dashes or the "-01" ending,
# the same pattern as the other posts: table_32100370.csv and so on
table_file <- \(table_number) {
  str_c("table_", str_remove_all(str_sub(table_number, 1, 10), "-"))
}

walk(livestock_tables, \(table_number) {
  get_cansim(table_number) |>
    filter(str_starts(GeoUID, "3530")) |>
    write_csv(file.path(raw_dir, str_c(table_file(table_number), ".csv")))
})

# ---- 2. Footnotes ----------------------------------------------------------
# Each table's footnotes, some about the quality or comparability of the
# figures, saved beside it so that 02_clean_data.R can report the ones that
# apply (cwr_quality_flags() in R/data_quality.R) without going back online.
walk(livestock_tables, \(table_number) {
  get_cansim_table_notes(table_number) |>
    write_csv(file.path(raw_dir, str_c(table_file(table_number), "_notes.csv")))
})
