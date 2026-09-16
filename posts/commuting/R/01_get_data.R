# 01_get_data.R
# Fetches every raw input for the "Commuting in Waterloo Region" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/commuting/R/01_get_data.R")

here::i_am("posts/commuting/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "commuting", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- Commuting, 2021 census -----------------------------------------------
# Table 98-10-0462, "Commuting destination by main mode of commuting, age and
# gender". The dimension that matters is commuting destination, which sorts
# every worker by how far they go: within their own municipality, to another
# municipality in the same census division, to another census division in the
# province, or to another province.
#
# The whole table covers every census subdivision in Canada and is 67 MB
# zipped, so it is cut down as it arrives to Waterloo Region's eight
# geographies: the Region itself (census division 3530) and its seven
# municipalities (census subdivisions 3530xxx), whose codes all start "3530".
# (Statistics Canada's coordinate service could fetch just the cells needed
# instead; a plain download is easier to check.)
get_cansim("98-10-0462") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100462.csv"))
