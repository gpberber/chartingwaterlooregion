# 01_get_data.R
# Fetches every raw input for the "Language" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/language/R/01_get_data.R")

here::i_am("posts/language/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "language", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- 1. Mother tongue, 2021 census ----------------------------------------
# Named languages, which only the detailed mother tongue table carries. The
# broad-category tables - English, French, non-official and nothing finer - are
# no use here, because the whole question is which non-official language.
#
# 98-10-0180 crosses every census subdivision in Canada with 538 languages and
# is 618 MB zipped; thirty-two numbers from it are wanted. So it is fetched a
# cell at a time through Statistics Canada's coordinate service instead of being
# downloaded at all.
#
# A coordinate names one cell of the cube: the member number of each dimension
# in order, which here is geography, age, gender, mother tongue, and single or
# multiple response. Member 1 is the "Total" of any dimension, so
# "2445.1.1.256.1" is Wellesley, all ages, all genders, Pennsylvania German,
# single and multiple responses counted together.
#
# The member numbers are the cube's own, read from its metadata (Statistics
# Canada's getCubeMetadata service, product 98100180) and fixed for the life of
# the table. They are written out here beside the names the metadata gives them,
# so that a reader can check them rather than trust them.
mother_tongue_places <- tribble(
  ~member, ~place,
  2439,    "Waterloo Region",   # the census division, not the city of the name
  2440,    "North Dumfries",
  2441,    "Cambridge",
  2442,    "Kitchener",
  2443,    "Waterloo",
  2444,    "Wilmot",
  2445,    "Wellesley",
  2446,    "Woolwich"
)

mother_tongue_languages <- tribble(
  ~member, ~language,
  1,       "Total - Mother tongue",
  5,       "Non-official languages",
  253,     "German",
  256,     "Pennsylvania German"
)

# Every place crossed with every language. A cell with nothing in it -
# Pennsylvania German in North Dumfries - does not come back at all, which
# 02_clean_data.R reads as a zero.
expand_grid(
  geography = mother_tongue_places$member,
  language = mother_tongue_languages$member
) |>
  mutate(
    cansimTableNumber = "98-10-0180",
    COORDINATE = paste(geography, 1, 1, language, 1, sep = ".")
  ) |>
  select(cansimTableNumber, COORDINATE) |>
  get_cansim_data_for_table_coord_periods(periods = 1) |>
  # What comes back names the geography only as "Waterloo (2)" and the like -
  # the service disambiguates duplicate names with a number rather than a code -
  # so the place is put back on from the coordinate that asked for it.
  mutate(geography = as.integer(str_split_i(COORDINATE, fixed("."), 1))) |>
  left_join(mother_tongue_places, join_by(geography == member)) |>
  write_csv(file.path(raw_dir, "table_98100180_coords.csv"))

# ---- 2. Footnotes ----------------------------------------------------------
# The table's footnotes, some of them about the quality or comparability of the
# figures, saved beside it so that 02_clean_data.R can report the ones that
# apply to the cells this post keeps (cwr_quality_flags() in R/data_quality.R)
# without going back online.
get_cansim_table_notes("98-10-0180") |>
  write_csv(file.path(raw_dir, "table_98100180_notes.csv"))
