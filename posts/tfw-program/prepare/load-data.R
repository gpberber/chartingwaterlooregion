here::i_am("posts/tfw-program/prepare/load-data.R")

library(tidyverse)
library(janitor)
library(openxlsx)
library(here)

# source: https://open.canada.ca/data/en/dataset/90fed587-1364-4f33-a9ee-208181dc0b97

data_urls <- read_csv(here("posts", "tfw-program", "data", "data-urls.csv"))

new_col_names <- c(occupation = "occupations_under_noc_2011",
                   occupation = "occupaitons_under_noc_2011",
                   program_stream = "stream",
                   num_positions_approved = "approved_positions",
                   num_positions_approved = "positions_approved",
                   employer = "employer_name"
                   )

data_csv <- data_urls |> 
  filter(grepl(".csv", url)) |> 
  pull(url) |> 
  map_df(~{
    . |> 
      read_csv (skip = 1, trim_ws = TRUE, locale = locale(encoding = "latin1")) |> 
      clean_names() |> 
      mutate(filename = str_remove(., ".+\\/"),
             year = str_extract(filename, "20\\d\\d"),
             quarter = toupper(str_extract(filename, "[qQ][1-4]"))
      ) |> 
      select(-filename) |> 
      rename(any_of(new_col_names))
  }) 

data_xlsx <- data_urls |> 
  filter(grepl(".xlsx", url)) |> 
  pull(url) |> 
  map_df(~{
    .  |> 
      read.xlsx (startRow = 2) |> 
      clean_names() |> 
      mutate(filename = str_remove(., ".+\\/"),
             year = str_extract(filename, "20\\d\\d"),
             quarter = toupper(str_extract(filename, "[qQ][1-4]"))
      ) |> 
      select(-filename) |> 
      rename(any_of(new_col_names))
  }) 

tfw_data <- bind_rows(data_csv, data_xlsx) |>
  rename(c(
    "organization_type" = "incorporate_status",
    "num_positions_requested" = "requested_positions",
    "num_lmias_approved" = "approved_lmi_as",
    "num_lmias_requested" = "requested_lmi_as"
  ))

write_csv(tfw_data, here("posts", "tfw-program", "data", "tfw_data_dirty.csv"))

# source_main: https://www.serviceobjects.com/blog/free-zip-code-and-postal-code-database-with-geocoordinates/ 
# source_supp: https://raw.githubusercontent.com/ccnixon/postalcodes/master/CanadianPostalCodes.csv

post_codes_main <- read_csv(here("posts", "tfw-program", "data", "postal_codes_main.csv"), trim_ws = TRUE) |>
  clean_names() |>
  mutate(version = "main")

post_codes_supp <- read_csv(here("posts", "tfw-program", "data", "postal_codes_supp.csv"), trim_ws = TRUE) |>
  clean_names() |>
  rename("city" = "place_name") |>
  mutate(version = "supp")
  

