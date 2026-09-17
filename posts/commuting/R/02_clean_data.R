# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads. Keep outputs under 25 MB each so they can be committed; if
# one must be bigger, /share-data moves it to a GitHub Release.
# Run from the project root: source("posts/commuting/R/02_clean_data.R")

here::i_am("posts/commuting/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir <- here("posts", "commuting", "data-raw")
data_dir <- here("posts", "commuting", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- The seven municipalities ----------------------------------------------
# The census table names each place but not what kind of place it is, and
# names two of them "Waterloo" - the city and the Region. So the seven are
# listed here by their census subdivision code, which is unambiguous, with the
# city or township type the charts colour by.
municipalities <- tribble(
  ~geo_uid,   ~district,         ~district_type,
  "3530004",  "North Dumfries",  "Township",
  "3530010",  "Cambridge",       "City",
  "3530013",  "Kitchener",       "City",
  "3530016",  "Waterloo",        "City",
  "3530020",  "Wilmot",          "Township",
  "3530027",  "Wellesley",       "Township",
  "3530035",  "Woolwich",        "Township"
)

# ---- Read ------------------------------------------------------------------
# Where people who live in each municipality go to work. The table crosses this
# with age, gender and mode of travel, so all three are held at their totals,
# and only the count is wanted rather than the confidence bounds around it.
# Every column is read as text except the value, so codes keep their leading
# digits exactly as published.
commuting_raw <- read_csv(
  file.path(raw_dir, "table_98100462.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    str_starts(age_15a, "Total"),
    str_starts(gender_3, "Total"),
    str_starts(main_mode_of_commuting_11a, "Total"),
    statistics_3 == "Count"
  )

# ---- Tidy ------------------------------------------------------------------
# Statistics Canada measures distance in units of geography rather than in
# kilometres, so its four categories are re-cut into the three that matter to a
# reader here: stayed home municipality, crossed into another one inside the
# Region, or left the Region. Waterloo Region is a census division, which is
# what makes that middle line drawable at all.
destinations <- tribble(
  ~commuting_destination_5,                                                                                             ~destination,
  "Commute within census subdivision (CSD) of residence",                                                               "In their own municipality",
  "Commute to a different census subdivision (CSD) within census division (CD) of residence",                           "Elsewhere in the Region",
  "Commute to a different census subdivision (CSD) and census division (CD) within province or territory of residence",  "Outside the Region",
  "Commute to a different province or territory",                                                                       "Outside the Region"
)

commuting <- commuting_raw |>
  # An inner join drops the table's own "Total - Commuting destination" row,
  # which would otherwise be double-counted with the parts that make it up.
  inner_join(destinations, join_by(commuting_destination_5)) |>
  summarise(workers = sum(value), .by = c(geo_uid, destination)) |>
  mutate(percent = workers / sum(workers) * 100, .by = geo_uid) |>
  # And this one drops the Region's own rows, keeping the seven municipalities
  inner_join(municipalities, join_by(geo_uid)) |>
  select(district, district_type, destination, workers, percent) |>
  arrange(district, destination)

# ---- Main mode of commuting ------------------------------------------------
# The same table, cut the other way: destination held at its total and mode of
# travel let through. Only three of the modes are kept, the three the chart
# shows. They are each other's siblings or cousins in Statistics Canada's
# hierarchy ("Public transit" and "Active transportation" sit under
# "Sustainable transportation"), so none of the three double-counts another;
# the "Other method" group is left out, so they do not add to 100.
modes <- c("Car, truck or van", "Public transit", "Active transportation")

commuting_mode <- read_csv(
  file.path(raw_dir, "table_98100462.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names() |>
  filter(
    str_starts(age_15a, "Total"),
    str_starts(gender_3, "Total"),
    str_starts(commuting_destination_5, "Total"),
    statistics_3 == "Count"
  ) |>
  # Each mode's share of all commuters in the municipality. The total row is
  # picked out as the denominator before the other modes are dropped.
  mutate(
    percent = value / value[str_starts(main_mode_of_commuting_11a, "Total")] * 100,
    .by = geo_uid
  ) |>
  filter(main_mode_of_commuting_11a %in% modes) |>
  inner_join(municipalities, join_by(geo_uid)) |>
  select(district, district_type, mode = main_mode_of_commuting_11a, workers = value, percent) |>
  arrange(district, mode)

# ---- Write -----------------------------------------------------------------
write_csv(commuting, file.path(data_dir, "commuting.csv"))
write_csv(commuting_mode, file.path(data_dir, "commuting_mode.csv"))

message("Wrote ", nrow(commuting), " commuting rows and ", nrow(commuting_mode),
        " mode rows to ", data_dir)
