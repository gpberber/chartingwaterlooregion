# 03_quality_flags.R
# Records Statistics Canada's data-quality flags and footnotes for every table
# in the crime dataset, so a post can check the rows it uses.
#
# Why a separate file rather than a column in each table: 02_clean_data.R
# pivots each table wide - one column per statistic (actual incidents, rate per
# 100,000, ...) - and a figure's flag belongs to one statistic, so a single
# flag column cannot survive the pivot. Instead every flagged figure is listed
# here in long form, with the keys posts filter the dataset on: year, geo_uid,
# ucr_code and statistic.
#
# Writes two files to data/:
#   quality_flags.parquet  one row per flagged figure in any crime table: the
#                          status (E, F, x, .., 0s, A-D), symbol (p, r) and
#                          terminated (t) columns as published, plus its keys
#   quality_notes.csv      every footnote of every table, saved by
#                          01_get_data.R, with the dimension and member each
#                          is attached to (none means the whole table)
#
# A post filters both to what it uses and passes them to cwr_quality_flags()
# in R/data_quality.R. The dataset itself keeps every row, so it cannot say
# which flags matter; only a post can.
#
# Run from the project root after 02_clean_data.R (which sources it at the end):
#   source("datasets/crime/R/03_quality_flags.R")

library(tidyverse)
library(arrow)
library(here)

raw_dir <- here("datasets", "crime", "data-raw")
data_dir <- here("datasets", "crime", "data")

# ---- Which raw file holds which tables -------------------------------------
# Some raw files combine two tables (01_get_data.R binds them together), so a
# flagged figure there is labelled with both table numbers.
sources <- tribble(
  ~file,                                  ~tables,
  "criminal_incidents_canada.csv",        "35-10-0177-01",
  "criminal_incidents_ontario.parquet",   "35-10-0180-01",
  "csi_canada_provs_ont_cmas.csv",        "35-10-0026-01",
  "csi_ontario_forces.csv",               "35-10-0188-01",
  "hate_crimes.csv",                      "35-10-0191-01",
  "cyber_crimes.csv",                     "35-10-0002-01",
  "homicide_victims.csv",                 "35-10-0071-01, 35-10-0068-01",
  "police_personnel_munic.csv",           "35-10-0077-01",
  "police_personnel_ont_can.csv",         "35-10-0076-01"
)

# Columns every Statistics Canada download has that are not a dimension of the
# table. Whatever is left over (Violations, Statistics, Age, Gender ...) is a
# dimension, and its members say which figure a flag belongs to.
machinery <- c(
  "REF_DATE", "Date", "GEO", "DGUID", "GeoUID", "VALUE", "val_norm", "UOM",
  "UOM_ID", "SCALAR_FACTOR", "SCALAR_ID", "VECTOR", "COORDINATE", "STATUS",
  "SYMBOL", "TERMINATED", "DECIMALS"
)

# ---- Read the flagged figures from one raw file ----------------------------
read_flagged <- function(file, tables) {
  path <- file.path(raw_dir, file)

  # Only rows carrying a flag are read. For the Ontario parquet (over 100 MB)
  # arrow applies the filter before anything reaches memory.
  raw <- if (str_ends(file, "parquet")) {
    open_dataset(path) |>
      filter(!is.na(STATUS) | !is.na(SYMBOL) | !is.na(TERMINATED)) |>
      collect() |>
      mutate(across(everything(), as.character))
  } else {
    read_csv(path, col_types = cols(.default = col_character())) |>
      filter(!is.na(STATUS) | !is.na(SYMBOL) | !is.na(TERMINATED))
  }

  dimensions <- setdiff(
    names(raw)[!str_detect(names(raw), "^(Hierarchy|Classification Code) for ")],
    c(machinery, "Statistics")
  )

  raw |>
    mutate(
      tables = tables,
      source_file = file,
      # The dataset calls a violation by its UCR code, "[1310]" in the raw file
      ucr_code = if ("Classification Code for Violations" %in% names(raw)) {
        str_extract(`Classification Code for Violations`, r"((?<=\[)\d+(?=\]))")
      } else {
        NA_character_
      },
      statistic = if ("Statistics" %in% names(raw)) Statistics else NA_character_,
      # The victims files were downloaded without GeoUID; GEO still names the place
      geo_uid = if ("GeoUID" %in% names(raw)) GeoUID else NA_character_,
      # Every other dimension's member, "Violations: Assault | Age: ...", so
      # a flag can be traced to its figure whatever the table. The CSI tables
      # have no dimension besides Statistics, so there is nothing to list.
      members = if (length(dimensions) == 0) {
        NA_character_
      } else {
        pmap_chr(pick(all_of(dimensions)), \(...) {
          m <- c(...)
          str_c(names(m)[!is.na(m)], ": ", m[!is.na(m)], collapse = " | ")
        })
      }
    ) |>
    select(
      tables, source_file,
      year = REF_DATE, geo = GEO, geo_uid, ucr_code, statistic,
      members, value = VALUE, status = STATUS, symbol = SYMBOL,
      terminated = TERMINATED
    )
}

quality_flags <- sources |>
  pmap(read_flagged) |>
  list_rbind()

write_parquet(quality_flags, file.path(data_dir, "quality_flags.parquet"))

# ---- Footnotes -------------------------------------------------------------
read_csv(file.path(raw_dir, "table_notes.csv"),
         col_types = cols(.default = col_character())) |>
  write_csv(file.path(data_dir, "quality_notes.csv"), na = "")

message(
  "Wrote ", nrow(quality_flags), " flagged figures from ", nrow(sources),
  " raw files to ", file.path(data_dir, "quality_flags.parquet"),
  "\n", str_c(
    count(quality_flags, source_file, status) |>
      pmap_chr(\(source_file, status, n) str_glue("  {source_file}: {coalesce(status, '(symbol or terminated only)')} x {n}")),
    collapse = "\n"
  )
)
