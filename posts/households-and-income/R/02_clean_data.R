# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads.
# Run from the project root: source("posts/households-and-income/R/02_clean_data.R")

here::i_am("posts/households-and-income/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir  <- here("posts", "households-and-income", "data-raw")
data_dir <- here("posts", "households-and-income", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Data-quality flags ----------------------------------------------------
# cwr_quality_flags() reports the quality symbols (E use with caution, F too
# unreliable, x suppressed...) and quality footnotes on the rows each section
# below keeps, and records them in data/quality_flags.csv. It is called once
# per table, on every row the post uses from that table. It also hands back
# those rows with every figure this site never uses (E, F, x, ..) blanked to
# NA, so each section carries on with what it returns. The log is started
# afresh so a table this script stops using does not linger in it.
source(here("R", "data_quality.R"))
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# The footnotes 01_get_data.R saved beside each table
table_notes <- function(file_stem) {
  read_csv(file.path(raw_dir, str_c(file_stem, "_notes.csv")),
           col_types = cols(.default = col_character()))
}

# ---- The seven municipalities ----------------------------------------------
# The census tables name each place but not what kind of place it is, and name
# two of them "Waterloo" - the city and the Region. So the seven are listed here
# by their census subdivision code, which is unambiguous, with the city or
# township type the charts colour by. (The welcome post reads the same names
# and types from the census boundary file, a 150 MB download this post does not
# otherwise need.)
municipalities <- tribble(
  ~csduid,    ~district,         ~district_type,
  "3530004",  "North Dumfries",  "Township",
  "3530010",  "Cambridge",       "City",
  "3530013",  "Kitchener",       "City",
  "3530016",  "Waterloo",        "City",
  "3530020",  "Wilmot",          "Township",
  "3530027",  "Wellesley",       "Township",
  "3530035",  "Woolwich",        "Township"
)

# ---- 1. Income, restated in today's dollars --------------------------------
# The 2021 census reports income earned in 2020. Read in 2026 those figures
# understate what people have, so they are restated in 2026 dollars: the same
# income multiplied by what the same basket of goods now costs against what it
# cost then. That ratio is all a CPI adjustment is.
#
# Which years to compare is a judgement call, and this is the one made here.
# 2020 is a finished calendar year, so its index is the average of its twelve
# months - the arithmetic Statistics Canada itself uses to publish an annual
# average. 2026 is not finished, so its index is the average of the months
# published so far. That is the honest reading of "2026 dollars" in September:
# the year to date, not a guess at how it ends. Re-running this script after
# more months are published moves the figures slightly, which is correct.
income_year <- "2020"   # the year the census asked about
dollar_year <- "2026"   # the year the post is written for

cpi_monthly <- read_csv(
  file.path(raw_dir, "table_18100004.csv"),
  col_select = c(REF_DATE, GEO, `Products and product groups`, VALUE, STATUS, SYMBOL),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  # REF_DATE is "2026-08", so the first four characters are the year
  mutate(year = str_sub(ref_date, 1, 4))

# Only the months of the two years compared feed the figures
cpi_monthly <- cpi_monthly |>
  filter(year %in% c(income_year, dollar_year)) |>
  cwr_quality_flags("18-10-0004-01", notes = table_notes("table_18100004"), log = quality_log)

cpi <- cpi_monthly |>
  summarise(index = mean(value), months = n(), .by = year)

inflator <- (cpi |> filter(year == dollar_year) |> pull(index)) /
  (cpi |> filter(year == income_year) |> pull(index))

# Median household total income. The table crosses the income statistics with
# household size and with household type, so both of those are held at their
# "Total" line to get the figure for all households rather than for, say,
# four-person couple families. They are matched on the word "Total" rather than
# on the full member name because one of the two contains an en dash, a
# character that is easy to mistype and impossible to see in a script.
household_income <- read_csv(
  file.path(raw_dir, "table_98100057.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    str_starts(household_size_7, "Total"),
    str_starts(household_type_including_census_family_structure_11, "Total"),
    household_income_statistics_6 ==
      "Median household total income (2020) (2020 constant dollars)"
  )

household_income <- cwr_quality_flags(household_income, "98-10-0057-01",
                                      notes = table_notes("table_98100057"), log = quality_log)

household_income <- household_income |>
  select(geo_uid, household_income = value)

# Median total income of a person aged 15 or over who had any income. This
# table splits income into sources - wages, investments, pensions, transfers -
# so "Total income" is the line that adds them up, and "Median amount ($)" is
# the statistic rather than the count of people or the average.
individual_income <- read_csv(
  file.path(raw_dir, "table_98100070.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    income_sources_and_taxes_32 == "Total income",
    income_statistics_8 == "Median amount ($)"
  )

individual_income <- cwr_quality_flags(individual_income, "98-10-0070-01",
                                       notes = table_notes("table_98100070"), log = quality_log)

individual_income <- individual_income |>
  select(geo_uid, individual_income = value)

income <- household_income |>
  left_join(individual_income, join_by(geo_uid)) |>
  # Names and city/township come from `municipalities` above, so the two files
  # cannot disagree about what a place is called. The eighth row has no match
  # there: it is the census division, and Statistics Canada calls it
  # "Waterloo", the same name as the city inside it. It is named "Region", the
  # house label for Waterloo Region in chart text (cwr_region in
  # R/theme_cwr.R, which this script does not load), so a reader of the chart
  # is never in doubt which is which.
  left_join(municipalities, join_by(geo_uid == csduid)) |>
  mutate(
    district = coalesce(district, "Region"),
    district_type = coalesce(district_type, "Region"),
    # Both years are kept. The 2020 figures are what Statistics Canada
    # published and are what anyone checking this against the census will see;
    # the 2026 ones are this script's arithmetic on top of them.
    household_income_2026 = household_income * inflator,
    individual_income_2026 = individual_income * inflator
  ) |>
  select(
    district, district_type,
    household_income_2020 = household_income,
    individual_income_2020 = individual_income,
    household_income_2026, individual_income_2026
  ) |>
  arrange(desc(household_income_2026))

write_csv(income, file.path(data_dir, "income.csv"))

message("Wrote ", nrow(income), " income rows to ", data_dir,
        " (", income_year, " dollars x ", round(inflator, 4), " = ",
        dollar_year, " dollars)")

# ---- 2. Households and dwellings -------------------------------------------
# Two figures about how people live, from one table. Cut to the rows the two
# figures below use, for the seven municipalities only (the Region-wide rows
# are left out), and checked as one table.
household_dwellings <- read_csv(
  file.path(raw_dir, "table_98100041.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    geo_uid %in% municipalities$csduid,
    (str_starts(structural_type_of_dwelling_9, "Total") &
       household_size_8 == "Average household size") |
      (str_starts(household_size_8, "Total") &
         (structural_type_of_dwelling_9 == "Single-detached house" |
            str_starts(structural_type_of_dwelling_9, "Total")))
  ) |>
  cwr_quality_flags("98-10-0041", notes = table_notes("table_98100041"), log = quality_log)

# Statistics Canada publishes the average itself, so it is read rather than
# worked out here: the size categories stop at "5 or more persons", and an
# open-ended top band cannot be averaged without inventing a number for it.
# Held at the total for dwelling type, so it covers every household.
household_size <- household_dwellings |>
  filter(
    str_starts(structural_type_of_dwelling_9, "Total"),
    household_size_8 == "Average household size"
  ) |>
  select(geo_uid, average_household_size = value)

# The dwelling mix, held at the total for household size. A share rather than a
# count, because the municipalities differ in size by a factor of thirty.
dwelling_type <- household_dwellings |>
  filter(str_starts(household_size_8, "Total")) |>
  summarise(
    single_detached_percent =
      value[structural_type_of_dwelling_9 == "Single-detached house"] /
      value[str_starts(structural_type_of_dwelling_9, "Total")] * 100,
    .by = geo_uid
  )

# Joining from `municipalities` names the seven places
households <- municipalities |>
  left_join(household_size, join_by(csduid == geo_uid)) |>
  left_join(dwelling_type, join_by(csduid == geo_uid)) |>
  select(-csduid) |>
  arrange(desc(average_household_size))

write_csv(households, file.path(data_dir, "households.csv"))

message("Wrote ", nrow(households), " household rows to ", data_dir)
