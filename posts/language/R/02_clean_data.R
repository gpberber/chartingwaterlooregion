# 02_clean_data.R
# Turns the raw inputs in data-raw/ into the small tidy files in data/ that
# index.qmd reads.
# Run from the project root: source("posts/language/R/02_clean_data.R")

here::i_am("posts/language/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir  <- here("posts", "language", "data-raw")
data_dir <- here("posts", "language", "data")
dir.create(data_dir, showWarnings = FALSE)

# ---- Data-quality flags ----------------------------------------------------
# cwr_quality_flags() reports the quality symbols (E use with caution, F too
# unreliable, x suppressed...) and quality footnotes on the rows the post
# keeps, and records them in data/quality_flags.csv. It also hands back those
# rows with every figure this site never uses (E, F, x, ..) blanked to NA, so
# the script carries on with what it returns. The log is started afresh so a
# table this script stops using does not linger in it.
source(here("R", "data_quality.R"))
quality_log <- file.path(data_dir, "quality_flags.csv")
unlink(quality_log)

# ---- The seven municipalities ----------------------------------------------
# The census names each place but not what kind of place it is, so the seven
# are listed here with the city or township type the charts colour by. The
# names are the ones 01_get_data.R puts on each cell it fetched. (The welcome
# post reads the same names and types from the census boundary file, a 150 MB
# download this post does not otherwise need.)
municipalities <- tribble(
  ~district,         ~district_type,
  "North Dumfries",  "Township",
  "Cambridge",       "City",
  "Kitchener",       "City",
  "Waterloo",        "City",
  "Wilmot",          "Township",
  "Wellesley",       "Township",
  "Woolwich",        "Township"
)

# ---- 1. Mother tongue ------------------------------------------------------
# Mother tongue is the language a person first learned at home in childhood and
# still understands: a question about origin rather than about daily use.
mother_tongue <- read_csv(
  file.path(raw_dir, "table_98100180_coords.csv"),
  col_types = cols(.default = col_character(), VALUE = col_double())
) |>
  clean_names()

# Every cell fetched is used, so the whole file is checked
mother_tongue <- cwr_quality_flags(
  mother_tongue, "98-10-0180",
  notes = read_csv(file.path(raw_dir, "table_98100180_notes.csv"),
                   col_types = cols(.default = col_character())),
  log = quality_log
)

mother_tongue <- mother_tongue |>
  select(district = place, language = mother_tongue_538, value)

# German and Pennsylvania German are added together: they are one community's
# languages here, the Old Order Mennonite settlements in the northern and
# western townships, and the census splits them by dialect.
#
# sum() over no rows is zero, which is what makes the missing cells harmless:
# Pennsylvania German in North Dumfries never came back from Statistics Canada
# because there was nobody to count, and zero is the right answer.
language_shares <- mother_tongue |>
  summarise(
    total = sum(value[language == "Total - Mother tongue"]),
    non_official = sum(value[language == "Non-official languages"]),
    german = sum(value[language %in% c("German", "Pennsylvania German")]),
    .by = district
  ) |>
  mutate(
    non_official_percent = non_official / total * 100,
    german_percent_of_non_official = german / non_official * 100
  )

# Joining from `municipalities` keeps the seven and drops the Region-wide cells
language <- municipalities |>
  left_join(language_shares, join_by(district)) |>
  select(
    district, district_type,
    non_official_percent, german_percent_of_non_official
  ) |>
  arrange(desc(non_official_percent))

write_csv(language, file.path(data_dir, "language.csv"))

message("Wrote ", nrow(language), " language rows to ", data_dir)
