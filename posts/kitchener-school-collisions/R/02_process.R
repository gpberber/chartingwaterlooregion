here::i_am("posts/kitchener-school-collisions/R/02_process.R")

# Required packages
library(tidyverse)
library(here)

# load prepped data
collisions_with_proximity_to_schools <- read_csv(here("posts", "kitchener-school-collisions", "data", "collisions_with_proximity_to_schools.csv"))
kitchener_schools_geocoded <- read_csv(here("posts", "kitchener-school-collisions", "data", "kitchener_schools_geocoded.csv"))

# tibble containing only collisions during school-hour windows within 250m of school
collisions_near_schools_during_school_hours <- collisions_with_proximity_to_schools |> 
  mutate(
    near_school_btw_6am_6pm = if_else((near_school + on_school_day + btw_6am_6pm) == 3, TRUE, FALSE),
    near_school_btw_7am_5pm = if_else((near_school + on_school_day + btw_7am_5pm) == 3, TRUE, FALSE),
    near_school_btw_8am_4pm = if_else((near_school + on_school_day + btw_8am_4pm) == 3, TRUE, FALSE)
  ) |> 
  filter((near_school_btw_6am_6pm + near_school_btw_7am_5pm + near_school_btw_8am_4pm) > 0)

# Count maximum number of schools for any collision
max_schools <- collisions_near_schools_during_school_hours  |> 
  pull(nearby_schools) %>%
  str_count("; ") %>%
  max() %>%
  {. + 1}  # Add 1 since n delimiters = n+1 schools

# Create column names for the separated schools
school_cols <- paste0("nearby_school_", seq_len(max_schools))

collisions_near_schools_summary <- collisions_near_schools_during_school_hours |> 
  select(nearby_schools, ends_with("involved"), starts_with("btw"), school_day_type) |>
  pivot_longer(cols = starts_with("btw"), names_to = "time_window", values_to = "collision_occurred") |> 
  filter(collision_occurred) |> 
  select(-collision_occurred) |> 
  separate(
    col = nearby_schools,
    into = school_cols,
    sep = "; ",
    fill = "right"  # Fill empty values with NA
  ) |> 
  pivot_longer(cols = starts_with("nearby"), values_to = "school_name") |> 
  filter(!is.na(school_name)) |> 
  group_by(school_name, school_day_type, time_window) |> 
  summarize(
    pedestrian_collisions = sum(pedestrianinvolved),
    cyclist_collisions = sum(cyclistinvolved),
    other_collisions = sum(no_ped_cyc_involved)
  ) |> 
  ungroup() |> 
  mutate(total_collisions = pedestrian_collisions + cyclist_collisions + other_collisions)

all_schools_collision_summary <- kitchener_schools_geocoded |>
  left_join(collisions_near_schools_summary, by = join_by(school_name)) |>
  # remove schools that opened after 2022
  filter(!grepl(paste(c("Oak Creek", "Bakhita"), collapse = "|"), school_name)) |> # schools not open during study period
  replace_na(list(
    pedestrian_collisions = 0,
    cyclist_collisions = 0,
    other_collisions = 0,
    total_collisions = 0
  ))

write_csv(all_schools_collision_summary, "posts/kitchener-school-collisions/data/schools_collision_summary.csv")
