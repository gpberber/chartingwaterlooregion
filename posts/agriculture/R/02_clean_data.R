# 02_clean_data.R
# Turns the raw Census of Agriculture tables in data-raw/ into the small tidy
# file in data/ that index.qmd reads: the number of cattle, sheep and pigs in
# each of the four townships in 2011, 2016 and 2021.
# Run from the project root: source("posts/agriculture/R/02_clean_data.R")

here::i_am("posts/agriculture/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

source(here("R", "data_quality.R"))   # cwr_quality_flags()

raw_dir <- here("posts", "agriculture", "data-raw")
data_dir <- here("posts", "agriculture", "data")
dir.create(data_dir, showWarnings = FALSE)

# Every run starts a fresh record of the quality flags
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# The four townships, by their census consolidated subdivision codes. Codes
# rather than names, because the table labels a place "Wilmot, Ontario
# [CCS350230020]" and there is a Wilmot in New Brunswick too.
townships <- tribble(
  ~geo_uid,  ~district,
  "3530004", "North Dumfries",
  "3530020", "Wilmot",
  "3530027", "Wellesley",
  "3530035", "Woolwich"
)

# ---- One livestock table -------------------------------------------------
# Reads one raw table, keeps the four townships' total head count, runs the
# quality check on those rows, and returns them in a common shape. The same
# steps for all six tables; only the table number, the animal and the name
# of the "total" row change (2011-2016 says "Total cattle and calves", 2021
# says "Total cattle").
read_livestock <- function(table_number, animal, total_row) {
  file_stem <- str_c("table_", str_remove_all(str_sub(table_number, 1, 10), "-"))

  kept <- read_csv(
    file.path(raw_dir, str_c(file_stem, ".csv")),
    col_types = cols(.default = col_character(), VALUE = col_double())
  ) |>
    clean_names() |>
    # The type-of-animal column is the table's sixth; its name changes from
    # table to table (cattle, cattle_and_calves, sheep ...), so it is renamed
    # by position. The quality check matches footnotes on the values in the
    # rows, not on column names, so the rename does not hide any.
    rename(animal_type = 6) |>
    filter(
      geo_uid %in% townships$geo_uid,
      animal_type == total_row,
      unit_of_measure == "Number of animals"
    )

  notes <- read_csv(
    file.path(raw_dir, str_c(file_stem, "_notes.csv")),
    col_types = cols(.default = col_character())
  )

  # Reports every flag on the kept rows (quality grades A to D, E, F, x) and
  # every quality footnote, records them in data/quality_flags.csv, and blanks
  # the figures this site never uses - E, F, x and .. - to NA. Assigned, so the
  # rest of the script carries on with the blanked data.
  kept <- kept |>
    cwr_quality_flags(table_number, notes = notes, log = quality_log)

  kept |>
    left_join(townships, join_by(geo_uid)) |>
    mutate(year = as.integer(ref_date), animal = animal) |>
    select(year, district, animal, animals = value, status)
}

# ---- All six tables ------------------------------------------------------
livestock <- tribble(
  ~table_number,   ~animal,  ~total_row,
  "32-10-0424-01", "Cattle", "Total cattle and calves",
  "32-10-0370-01", "Cattle", "Total cattle",
  "32-10-0426-01", "Pigs",   "Total pigs",
  "32-10-0372-01", "Pigs",   "Total pigs",
  "32-10-0425-01", "Sheep",  "Total sheep and lambs",
  "32-10-0371-01", "Sheep",  "Total sheep"
) |>
  pmap(read_livestock) |>
  list_rbind() |>
  arrange(animal, district, year)

write_csv(livestock, file.path(data_dir, "livestock.csv"))

message("Wrote ", nrow(livestock), " livestock rows to ", data_dir)
