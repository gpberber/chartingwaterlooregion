# 01_get_data.R
# Fetches every raw input for the "Households and income" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/households-and-income/R/01_get_data.R")

here::i_am("posts/households-and-income/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "households-and-income", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- 1. Income, 2021 census ------------------------------------------------
# Two census tables, both published for census subdivisions:
#
#   98-10-0057-01  median household total income
#   98-10-0070-01  median total income of people aged 15 and over
#
# Both report income earned in 2020, the calendar year before the census. That
# is as recent as census income gets: the census is taken every five years and
# asks about the year before it.
#
# Each table covers every census subdivision in Canada - 98-10-0057-01 alone is
# 2.5 million rows - and only Waterloo Region's eight geographies are ever
# used, so each is cut to them here rather than written out whole. Writing them
# whole would leave 400 MB on disk to reach eight rows.
#
# Every census subdivision code begins with its census division code, and
# Waterloo Region is division 3530, so codes starting "3530" are the seven
# municipalities. The bare code "3530" is the division itself - the Region as a
# whole - which is the eighth row, and it comes free with the same filter.
walk2(
  c("98-10-0057-01", "98-10-0070-01"),
  c("table_98100057.csv", "table_98100070.csv"),
  \(table_number, file_name) {
    get_cansim(table_number) |>
      filter(str_starts(GeoUID, "3530")) |>
      write_csv(file.path(raw_dir, file_name))
  }
)

# ---- 2. Consumer Price Index ----------------------------------------------
# Table 18-10-0004-01, the monthly CPI, used to restate those 2020 incomes in
# today's dollars.
#
# All-items for Ontario. The CPI is not published for Kitchener-Cambridge-
# Waterloo - in Ontario only Toronto, Ottawa and Thunder Bay get their own
# index - so the province is the closest published basket. Canada-wide would
# also be defensible; Ontario is nearer the prices these households pay.
#
# The monthly table rather than the annual averages in 18-10-0005-01, because
# the current year has no annual average yet and 02_clean_data.R needs one.
# Averaging the months of a year is exactly how Statistics Canada computes it.
# Filtered on the way in for the same reason as above: over a million rows.
get_cansim("18-10-0004-01") |>
  filter(GEO == "Ontario", `Products and product groups` == "All-items") |>
  write_csv(file.path(raw_dir, "table_18100004.csv"))

# ---- 3. Households and dwellings, 2021 census ------------------------------
# Table 98-10-0041, "Structural type of dwelling and household size". One table
# answers two questions at once: what kind of dwelling people live in, and how
# many of them live in it. It publishes average household size directly, which
# is better than working one out here from the size categories - their top band,
# "5 or more persons", is open-ended and cannot be averaged honestly.
#
# Cut to Waterloo Region on the way in, for the reason given in section 1.
get_cansim("98-10-0041") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100041.csv"))

# ---- 4. Footnotes ----------------------------------------------------------
# Each Statistics Canada table carries footnotes, some of them about the
# quality or comparability of the figures. They are saved beside the tables so
# that 02_clean_data.R can report the ones that apply to the rows this post
# keeps (cwr_quality_flags() in R/data_quality.R) without going back online.
# The file names match the tables': table_98100057_notes.csv and so on.
c("98-10-0057-01", "98-10-0070-01", "18-10-0004-01", "98-10-0041") |>
  walk(\(table_number) {
    get_cansim_table_notes(table_number) |>
      write_csv(file.path(
        raw_dir,
        str_c("table_", str_sub(str_remove_all(table_number, "-"), 1, 8), "_notes.csv")
      ))
  })
