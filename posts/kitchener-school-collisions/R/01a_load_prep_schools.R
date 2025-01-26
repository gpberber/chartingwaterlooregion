here::i_am("posts/kitchener-school-collisions/R/01a_load_prep_schools.R")

# Required packages
library(tidygeocoder)
library(janitor)
library(here)

source(here("posts", "kitchener-school-collisions", "R", "helpers.R"))


# Read the public schools HTML content
public_schools_html_content <- read_html(here("posts", "kitchener-school-collisions", "data-raw", "public_schools.htm"))

# Extract school names and addresses
public_schools_data <- public_schools_html_content %>%
  html_nodes("tr") %>%
  map_df(extract_address_components) %>%
  filter(!is.na(school_name) & school_name != "")

# vector of address corrections identified when geocoding yielded null results
public_location_corrections <- c(
  "51 Ardelt Ave Kitchener ON N2R 2R5" = "51 Ardelt Ave Kitchener ON N2C 2R5",
  "55 McKay St Cambridge ON N1R 4G8" = "55 McKay St Cambridge ON N1R 4G6",
  "175 Main St E Cambridge ON N1R 1W5" = "175 Main St Cambridge ON N1R 1W5",
  "80 Tartan Ave Kitchener ON N2R 0N6" = "80 Tartan Ave Kitchener ON N2R 0C1",
  "2366 Spragues Rd Ayr ON N2V 2L3" = "2366 Spragues Rd North Dumfries ON N0B 1W0",
  "2001 Kressler Rd Waterloo ON N3R 4R8" = "2001 Kressler Rd Waterloo ON N2J 3Z4",
  "130 Woodbine Ave Kitchener ON N2R 1X9" = "130 Woodbine Ave Kitchener ON N2R 1X7",
  "5 First St W Elmira ON N3B 1G1" = "5 1st St W Elmira ON N3B 1G1",
  "82 Meadowcreek Lane, R.R. 33 Cambridge ON N2J 2A9" = "82 Meadowcreek Lane Cambridge ON N3H 4R8"
)

# Add column with full address, including corrections where needed
public_schools_clean <- public_schools_data |> 
  mutate(
    address = paste(street_address, location_info),
    address = str_replace_all(address, public_location_corrections)
  ) |> 
  select(-street_address, -location_info)

# Add geocoding
public_schools_geocoded <- public_schools_clean %>%
  geocode(
    address = address,
    method = 'osm',  # Using OpenStreetMap
    lat = latitude,
    long = longitude
  ) %>%
  arrange(school_name)

# Create vector of school names and addresses from the PDF data (these vectors were generated
# by having Claude AI scrape the PDF of Catholic schools as was unable to tidy it
# up using R code)
catholic_schools_data <- tibble(
  school_name = c(
    "Blessed Sacrament", "Canadian Martyrs", "Christ the King", "Holy Family",
    "Holy Rosary", "Holy Spirit", "John Sweeney", "Monsignor R.M. Haller",
    "Our Lady of Fatima", "Our Lady of Grace", "Our Lady of Lourdes", "Saint John Paul II",
    "Saint Teresa of Avila", "Sir Edgar Bauer", "St. Agnes", "St. Aloysius",
    "St. Anne Camb", "St. Anne Kit", "St. Augustine", "St. Bernadette",
    "St. Boniface", "St. Brigid", "St. Clement", "St. Daniel",
    "St. Dominic Savio", "St. Elizabeth", "St. Gabriel", "St. Gregory",
    "St. John", "St. Joseph", "St. Josephine Bakhita", "St. Kateri Tekakwitha",
    "St. Luke", "St. Margaret of Scotland", "St. Mark", "St. Matthew",
    "St. Michael", "St. Nicholas", "St. Paul", "St. Peter",
    "St. Teresa of Calcutta", "St. Teresa Kitchener", "St. Timothy", "St. Vincent de Paul",
    "St. Isidore (Virtual)",
    # Secondary Schools
    "Monsignor Doyle Catholic Secondary School", "Resurrection Catholic Secondary School",
    "St. Benedict Catholic Secondary School", "St. David Catholic Secondary School",
    "St. Mary's High School"
  ),
  address = c(
    "367 The Country Way Kitchener, ON N2E 2S3",
    "50 Confederation Drive Kitchener, ON N2B 2X5",
    "70 Acorn Way Cambridge, ON N1R 8M5",
    "313 Huron Street, New Hamburg, ON N3A 1K3",
    "485 Thorndale Drive Waterloo, ON N2T 1W5",
    "15 Gate House Dr., Cambridge, ON N1P 1C7",
    "185 Activa Ave., Kitchener, ON N2E 4A1",
    "118 Shea Crescent Kitchener, ON N2E 1E8",
    "55 Hammet Street Cambridge, ON N3C 2H5",
    "70 Gracefield Crescent Kitchener, ON N2E 1R9",
    "55 Roslin Ave., South Waterloo, ON N2L 6N5",
    "75 Pebblecreek Drive Kitchener, ON N2A 0E3",
    "69-75 First Street West Elmira, ON N3B 1G5",
    "660 Glen Forrest Blvd., Waterloo, ON N2L 4K2",
    "254 Neilson Avenue Waterloo, ON N2J 2M3",
    "504 Connaught Street Kitchener, ON N2C 1C2",
    "127 Elgin St. N., Cambridge, ON N1R 5H6",
    "250 East Avenue Kitchener, ON N2H 1Z4",
    "177 Bismark Drive Cambridge, ON N1S 4Y2",
    "245 Lorne Ave., Kitchener, ON N2M 3Y9",
    "225 Starlight Ave, Breslau, ON N0B 1M0",
    "50 Broom St., Ayr, ON N0B 1E0",
    "3639 Lobsinger Line St. Clements, ON N0B 2M0",
    "39 Midland Drive Kitchener, ON N2A 2A9",
    "3 Westforest Trail Kitchener, ON N2N 3A6",
    "50 Adler Drive Cambridge, ON N3C 4B7",
    "15 Baldwin Drive Cambridge, ON N3C 0B4",
    "34 Osborne Avenue Cambridge, ON N1S 3H1",
    "99 Strange Street Kitchener, ON N2G 1R4",
    "980 Westminster Dr. S, Cambridge, ON N3H 1V2",
    "25 Beckview Dr Kitchener, ON N2R 1R7",
    "560 Pioneer Drive Kitchener, ON N2P 1P2",
    "550 Chesapeake Dr., Waterloo, ON N2K 4G5",
    "210 Cowan Boulevard Cambridge, ON N1T 1V4",
    "240 Autumn Hill Cres Kitchener, ON N2N 1K8",
    "405 Pastern Trail Waterloo, ON N2K 3V6",
    "1150 Concession Road Cambridge, ON N3H 4L6",
    "525 Laurelwood Dr., Waterloo, ON N2V 2N1",
    "45 Birchcliffe Avenue Kitchener, ON N2M 4V7",
    "92 Avenue Road Cambridge, ON N1R 1C1",
    "520 Saginaw Parkway Cambridge, ON N1T 1W9",
    "270 Edwin Street Kitchener, ON N2H 4P4",
    "15 Bechtel Drive Kitchener, ON N2P 1T4",
    "30 Faial Rd. Cambridge, ON N1R 7C3",
    "Based out of St. Boniface",
    # Secondary Schools
    "185 Myers Road Cambridge, ON N1R 7H2",
    "455 University Ave. W, Kitchener, ON N2N 3B9",
    "50 Saginaw Parkway P.O. Box 578 Cambridge, ON N1R 5W1",
    "4 High Street Waterloo, ON N2L 3X5",
    "1500 Block Line Road Kitchener, ON N2C 2S2"
  )
)

# vector of erroneous addresses to be cleaned up so they can be geocoded
catholic_location_corrections <- c(
  "75 Pebblecreek Drive Kitchener, ON N2A 0E3" = "75 Pebblecreek Drive Kitchener, ON N2A 4K1",
  "50 Saginaw Parkway P.O. Box 578 Cambridge, ON N1R 5W1" = "50 Saginaw Parkway Cambridge, ON N1R 5W1",
  "25 Beckview Dr Kitchener, ON N2R 1R7" = "25 Beckview Dr Kitchener, ON N2R 1R8",
  "45 Birchcliffe Avenue Kitchener, ON N2M 4V7" = "45 Birchcliff Avenue Kitchener, ON N2M 4V7",
  "55 Roslin Ave., South Waterloo, ON N2L 6N5" = "55 Roslin Ave S Waterloo, ON N2L 6N5",
  "34 Osborne Avenue Cambridge, ON N1S 3H1" = "34 Osborne Street Cambridge, ON N1S 3H1",
  "69-75 First Street West Elmira, ON N3B 1G5" = "69 1st Street West Elmira, ON N3B 1G5",
  "15 Gate House Dr., Cambridge, ON N1P 1C7" = "15 Gatehouse Dr, Cambridge, ON N1P 1C7"
)

# Clean and display the data
clean_catholic_schools <- clean_catholic_school_data(catholic_schools_data) |> 
  mutate(address = str_replace_all(address, catholic_location_corrections))

# Add geocoding
catholic_schools_geocoded <- clean_catholic_schools %>%
  geocode(
    address = address,
    method = 'osm',  # Using OpenStreetMap
    lat = latitude,
    long = longitude
  ) %>%
  arrange(school_name)

# append public and catholic schools
schools_clean <- rbind(public_schools_geocoded, catholic_schools_geocoded) |> 
  arrange(school_name)

# filter for bricks-and-mortar Kitchener schools
kitchener_schools_geocoded <- schools_clean |> 
  filter(
    grepl("Kitchener", address),
    !grepl("Remote", school_name) #remove online schools
  )

write_csv(kitchener_schools_geocoded, here("posts", "kitchener-school-collisions", "data", "kitchener_schools_geocoded.csv"))