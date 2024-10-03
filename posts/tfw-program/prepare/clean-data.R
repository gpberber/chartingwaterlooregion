here::i_am("posts/tfw-program/prepare/clean-data.R")

library(here)
library(fuzzyjoin)
library(stringi)
library(stringdist)
source(here("posts", "tfw-program", "prepare", "load-data.R"))
source(here("posts", "tfw-program", "prepare", "functions.R"))


clean_post_codes_main <- post_codes_main |>
  mutate(
    postal_code = str_remove_all(postal_code, "\\s"),
    city = str_to_lower(city),
    city = str_replace_all(city, c(
      "-\\s*" = " ",
      " st\\.* " = " saint ", 
      " ste\\.* " = " sainte ",
      "^st\\.* " = "saint ", 
      "^ste\\.* " = "sainte "
    )),
    city = str_remove_all(city, "#$|\\scb$|\\sbdv$"),
    province_abbr = str_replace_all(province_abbr, c(
      "NL" = "Newfoundland and Labrador",
      "PE" = "Prince Edward Island",
      "NS" = "Nova Scotia",
      "NB" = "New Brunswick",
      "QC" = "Quebec",
      "ON" = "Ontario",
      "MB" = "Manitoba",
      "SK" = "Saskatchewan",
      "AB" = "Alberta",
      "BC" = "British Columbia",
      "YT" = "Yukon",
      "NT" = "Northwest Territories",
      "NU" = "Nunavut"
    ))
  ) |>
  distinct(postal_code, city, .keep_all = TRUE) |>
  rename(province_territory = province_abbr) |> 
  select(-time_zone)

temp_post_codes_supp <- post_codes_supp |> 
  mutate(postal_code = str_remove_all(postal_code, "\\s"),
         fsa_province = as.character(fsa_province)
         ) |> 
  anti_join(clean_post_codes_main, by = "postal_code") |> 
  mutate(
    city = str_to_lower(city),
    city = str_replace_all(city, c("-\\s*" = " ",
                                   " st\\.* " = " saint ", 
                                   " ste\\.* " = " sainte ",
                                   "^st\\.* " = "saint ", 
                                   "^ste\\.* " = "sainte ",
                                   "&#39;" = "'"
    )),
    city = str_remove_all(city, "#$|\\scb$|\\sbdv$"),
    fsa_province = str_replace_all(fsa_province, c(
      "10" = "Newfoundland and Labrador",
      "11" = "Prince Edward Island",
      "12" = "Nova Scotia",
      "13" = "New Brunswick",
      "24" = "Quebec",
      "35" = "Ontario",
      "46" = "Manitoba",
      "47" = "Saskatchewan",
      "48" = "Alberta",
      "59" = "British Columbia",
      "60" = "Yukon",
      "61" = "Northwest Territories",
      "62" = "Nunavut"
    ))
  ) |>
  filter(city != "street") |>
  rename(province_territory = fsa_province) |> 
  distinct(postal_code, city, .keep_all = TRUE) |> 
  select(postal_code, city, province_territory, latitude, longitude, version) |> 
  add_count(postal_code) |> 
  filter(!(n == 2 & city != "toronto")) |> 
  select(-n)

clean_post_codes_supp_cities <- temp_post_codes_supp |> 
  select(city) |> 
  distinct() |> 
  filter(!grepl("\\?", city))

dirty_post_codes_supp_cities <- temp_post_codes_supp |> 
  select(city) |> 
  distinct() |> 
  filter(grepl("\\?", city))

dirty_clean_cities_match <- dirty_post_codes_supp_cities |> 
  stringdist_left_join(
    clean_post_codes_supp_cities, 
    by = "city", 
    max_dist = 2, 
    distance_col = "distance"
  ) |> 
  arrange(city.x, distance) |> 
  distinct(city.x, .keep_all = TRUE) |> 
  select(-distance) |> 
  mutate(city.y = if_else(is.na(city.y), city.x, city.y))

messy_cities <- c("beaus\\?jour" = "beausejour",
                  "c\\?te" = "cote",
                  "ch\\?teauguay" = "chateauguay",
                  "rivi\\?re" = "riviere",
                  "sup\\?rieur" = "superieur",
                  "c\\?teaux" = "coteaux",
                  "m\\?tabetchouan" = "metabetchouan",
                  "h\\?bertville" = "hebertville",
                  "l'\\?le" = "l'ile",
                  "\\?tienne" = "etienne",
                  "beno\\?t" = "benoit",
                  "g\\?rard" = "gerard",
                  "l\\?vy" = "levy",
                  "s\\?verin" = "severin",
                  "genevi\\?ve" = "genevieve",
                  "m\\?lanie" = "melanie",
                  "r\\?collet" = "recollet",
                  "fran\\?ois" = "francois"
                  )

clean_cities <- dirty_clean_cities_match |> 
  mutate(city.y = str_replace_all(city.y, messy_cities)) |> 
  rename(c("dirty_city" = "city.x", "clean_city" = "city.y")) |> 
  mutate(dirty_city = str_replace_all(dirty_city, "\\?", "\\\\\\?"))

clean_cities_vec <- setNames(as.character(clean_cities$clean_city), clean_cities$dirty_city)

clean_post_codes_supp <- temp_post_codes_supp |> 
  mutate(city = str_replace_all(city, clean_cities_vec))

clean_post_codes <- bind_rows(clean_post_codes_main, clean_post_codes_supp) |>
  mutate(across(where(is.character), str_squish)) |> 
  arrange(postal_code, city)
  
intermediate_tfw_data <- tfw_data |>
  filter(!grepl("^[0-9]|Notes:", province_territory)) |>
  fill(c(province_territory, program_stream, employer, address), .direction = "down") |>
  mutate(
    address = str_remove(address, "^\\W"),
    program_stream = str_remove(program_stream, " Stream"),
    province_territory = str_replace(province_territory, "Emp.*", "Outside Canada"),
    num_positions_approved = if_else(year == "2022" & quarter == "Q2", num_positions_requested, num_positions_approved),
    num_lmias_approved = if_else(year == "2022" & quarter == "Q2", num_lmias_requested, num_lmias_approved)
  ) |>
  select(-num_lmias_requested, -num_positions_requested) |> #Only 2022 Q2 had these columns, assumed "requested" should be "approved"
  separate_wider_delim(occupation, delim = "-", names = c("noc_code", "occupation"), too_few = "align_start", too_many = "merge") |>
  separate_wider_regex(address, c(city = "[\\D\\d,]+(?=,)", ".*", postal_code = "[A-Z]\\d[A-Z].*$"), too_few = "align_start") |>
  mutate(
    across(c(employer, city), str_to_title),
    employer = str_remove_all(employer, "\\."),
    employer = str_replace_all(employer, c(
      " Bc " = " BC ",
      " Ab " = " Alberta ",
      " Sask " = " Saskatchewan ",
      " Mb " = " Manitoba ",
      " Ont " = " Ontario ",
      " Qc " = " Quebec ",
      " Pei " = " PEI ",
       "Ns " = " Nova Scotia ",
      " Nb " = " New Brunswick ",
      " Nfld " = "Newfoundland ",
      "Aberta" = "Alberta"
    )),
    employer = stri_trans_general(str = employer, id = "Latin-ASCII"),
    city_original = city,     
    city = str_remove_all(city, ",[^,]*$| ?[Rr]{2}.*\\d ?|\\s*\\(.*"),
    city = str_replace_all(city, c(
      "\\s*#\\s*" = "",
      "-\\s*" = " ",
      "\\." = " ",
      "^Sta " = "Ste ",
      " St\\.* " = " Saint ",
      " Ste\\.* " = " Sainte ",
      "^St\\.* " = "Saint ",
      "^Ste\\.* " = "Sainte ",
      "Pocati..e" = "Pocatière",
      "Saint-Aim." = "Saint Aimé",
      "Saint-.Loi" = "Saint Eloi",
      "Notl" = "Niagara On The Lake",
      "Niagra On The Lake" = "Niagara On The Lake",
      "Saint-Tienne-Des-GrS" = "Saint Étienne Des Gres",
      "Saint-FLicien" = "Saint Félicien",
      "^Niagara$" = "Niagara On The Lake",
      "Ddo" = "Dollard Des Ormeaux",
      "Mtl" = "Montreal",
      "Rivires" = "Rivieres",
      "\\\u0082" = "e",
      "\\\u0087" = "c",
      "×" = "i",
      "\\\u0093" = "o",
      "\\\u0090" = "E"
    )),
    city = str_to_lower(city),
    city = stri_trans_general(str = city, id = "Latin-ASCII"),
    postal_code = str_remove_all(postal_code, "\\s"),
    across(where(is.character), str_squish)
  ) |>
  arrange(year, quarter, province_territory, employer, noc_code) |>
  mutate(id = row_number()) |>
  relocate(id, year, quarter, province_territory, employer, organization_type, city:occupation, program_stream, num_lmias_approved, num_positions_approved)

intermediate_tfw_data_clean_cities <- intermediate_tfw_data |> 
  inner_join(clean_post_codes, by = c("city", "province_territory"), keep = FALSE, multiple = "any") |> 
  select(-(postal_code.y:version)) |> 
  rename("postal_code" = "postal_code.x")

intermediate_tfw_data_dirty_cities <- intermediate_tfw_data |> 
  anti_join(clean_post_codes, by = c("city", "province_territory")) |> 
  filter(province_territory != "Outside Canada")

intermediate_tfw_data_cities_outside_canada <- intermediate_tfw_data |> 
  filter(province_territory == "Outside Canada")

reference_cities <- clean_post_codes |> 
  distinct(city, province_territory)

tfw_data_dirty_cities <- intermediate_tfw_data_dirty_cities |>
  select(postal_code, city, province_territory) |>
  distinct() |>
  drop_na() |>
  mutate(id = row_number()) |>
  left_join(select(clean_post_codes, c(postal_code, city, province_territory)), by = "postal_code") |>
  rename(c("city_tfw" = "city.x", "city_pc" = "city.y", "province_territory_tfw" = "province_territory.x", "province_territory_pc" = "province_territory.y")) |>
  add_count(id) |>
  rowwise() |>
  mutate(
    city_city = correct_city(city_tfw, province_territory_tfw, reference_cities)[1],
    city_city_dist = as.numeric(correct_city(city_tfw, province_territory_tfw, reference_cities)[2]),
    city_pc_dist = round(stringdist(city_tfw, city_pc, method = "jw"), 2),
    city_match = if_else(city_city == city_pc, 1, 0),
    province_match = province_territory_tfw == province_territory_pc
  ) |>
  group_by(id) |>
  mutate(
    min_dist = min(city_city_dist, city_pc_dist),
    city_match_max = max(city_match)
  ) |>
  ungroup() |> 
  filter(!(n > 1 & city_match == 0 & city_match_max == 1)) |>
  mutate(
    city_selected = case_when(
      city_match == 1 ~ city_city,
      province_match == FALSE ~ city_pc,
      city_pc_dist < 0.26 ~ city_pc,
      .default = city_tfw
    ),
    province_selected = case_when(province_match == FALSE ~ province_territory_pc, .default = province_territory_tfw)
  ) |>
  distinct(id, .keep_all = TRUE) |> 
  select(postal_code, city_tfw, city_selected, province_territory_tfw, province_selected)

intermediate_tfw_data_dirty_cities_cleaned <- intermediate_tfw_data_dirty_cities |> 
  left_join(tfw_data_dirty_cities, by = join_by(postal_code, city == city_tfw, province_territory == province_territory_tfw)) |> 
  mutate(
    city_selected = case_when(
      city == "plain city" ~ "plain city", 
      employer == "Te Boyle Farm & Forestry Ltd" ~ "tracadie",
      .default = city_selected
      ),
    province_selected = case_when(
      city == "plain city" ~ "Outside Canada", 
      employer == "Te Boyle Farm & Forestry Ltd" ~ "Nova Scotia",
      .default = province_selected
      ),
    city = city_selected,
    province_territory = province_selected
    ) |> 
  select(-city_selected, -province_selected)

tfw_data_clean <- rbind(intermediate_tfw_data_clean_cities, intermediate_tfw_data_dirty_cities_cleaned, intermediate_tfw_data_cities_outside_canada) |> 
  arrange(id) |> 
  mutate(city = str_to_title(city)) |> 
  rename("city_clean" = "city")

write_csv(tfw_data_clean, here("posts", "tfw-program", "data", "tfw_data_clean.csv"))