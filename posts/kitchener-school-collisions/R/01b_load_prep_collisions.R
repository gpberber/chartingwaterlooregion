here::i_am("posts/kitchener-school-collisions/R/01b_load_prep_collisions.R")

# Required packages
library(tidygeocoder)
library(janitor)
library(here)

source(here("posts", "kitchener-school-collisions", "R", "helpers.R"))

# load geocoded schools
kitchener_schools_geocoded <- read_csv(here("posts", "kitchener-school-collisions", "data", "kitchener_schools_geocoded.csv"))


# load collisions data
all_collisions <- read_csv("posts/kitchener-school-collisions/data-raw/kitchener_collisions.csv") |> 
  clean_names() |> 
  select(
    accidentnum:accident_month, 
    accident_hour, 
    accident_weekday, 
    latitude, 
    longitude, 
    xcoord,
    ycoord,
    accidentlocation,
    classificationofaccident,
    impactlocation,
    initialimpacttype,
    trafficcontrol,
    pedestrianinvolved,
    cyclistinvolved,
    environment_conditions = environmentcondition1,
    location_notes = xmlimportnotes
  ) 

# remove 178 duplicate collisions reported for 2019 
# most have a reported time of 12am in one row and a later time in the other row
# assumed the later time was the correct one
all_collisions <- all_collisions |> 
  mutate(accidentnum = str_remove_all(accidentnum, "-")) |> 
  arrange(accidentnum, desc(accident_hour)) |> 
  distinct(accidentnum, .keep_all = TRUE)          


# for collisions with lat-lon coords of 0, use x-y coords to obtain lat-lon
all_collisions <- add_latlon_to_tibble(
  data = all_collisions,
  x_col = "xcoord",
  y_col = "ycoord"
) |>
  mutate(
    latitude_use = if_else(latitude == 0, latitude_new, latitude),
    longitude_use = if_else(longitude == 0, longitude_new, longitude)
  ) |> 
  relocate(accidentnum:latitude, latitude_new, latitude_use, longitude, longitude_new, longitude_use)

# add distance to nearest school for each collision
all_collisions <- all_collisions |> 
  rowwise() |> 
  mutate(distance_to_nearest_school = min(distHaversine(
    p1 = matrix(c(longitude_use, latitude_use), ncol = 2),
    p2 = matrix(c(kitchener_schools_geocoded$longitude, kitchener_schools_geocoded$latitude), ncol = 2)
  ))) |>
  ungroup()

# add school day and school-hour window columns
all_collisions <- all_collisions |>
  mutate(
    on_school_day = if_else(
      !(accident_month %in% c(7, 8)) & !(accident_weekday %in% c("Saturday", "Sunday")),
      TRUE,
      FALSE
    ),
    btw_6am_6pm = (accident_hour >= 6) & (accident_hour < 18),
    btw_7am_5pm = (accident_hour >= 7) & (accident_hour < 17),
    btw_8am_4pm = (accident_hour >= 8) & (accident_hour < 16),
    no_ped_cyc_involved = if_else((pedestrianinvolved + cyclistinvolved) > 0, FALSE, TRUE)
  )

# add school proximity info to collisions data
collisions_with_proximity_to_schools <- analyze_collisions_near_schools(all_collisions, kitchener_schools_geocoded, radius_meters = 250)

write_csv(collisions_with_proximity_to_schools, here("posts", "kitchener-school-collisions", "data", "collisions_with_proximity_to_schools.csv"))