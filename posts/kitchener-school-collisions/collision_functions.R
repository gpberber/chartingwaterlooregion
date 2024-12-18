# Required packages
library(rvest)
library(tidyverse)
library(sf)
library(geosphere)
library(leaflet)
library(scales)

# Function to extract address components from public schools html file
extract_address_components <- function(row) {
  # Extract school name from the strong>a tag
  name <- row %>%
    html_node("strong a") %>%
    html_text() %>%
    str_squish()
  
  # Get the full text content
  full_text <- row %>%
    html_text() %>%
    str_squish()
  
  # Remove the school name and extra spaces
  addr_text <- str_replace(full_text, fixed(name), "") %>%
    str_squish()
  
  # Common street types with optional directional
  street_types <- c("St", "Rd", "Ave", "Dr", "Blvd", "Cres", "Pky", "Way", "Road", "Avenue", "R.R. 33", "Drive")
  directions <- c("N", "S", "E", "W")
  
  # Modified street pattern that ignores street types followed by lowercase letters
  street_pattern <- paste0(
    "(", 
    paste(street_types, collapse = "|"),
    ")",
    "(?![a-z])",  # Negative lookahead for lowercase letters
    "(?:\\s+(?:", paste(directions, collapse = "|"), "))?"
  )
  
  # Extract up to and including street type and direction for street address
  street_address <- str_extract(addr_text, paste0("^.*?\\s*", street_pattern)) %>%
    str_squish()
  
  # Extract location info with complete postal code
  location_info <- str_extract(addr_text, "[A-Z][a-z]+\\s+ON\\s+[A-Z][0-9][A-Z]\\s+[0-9][A-Z][0-9]") %>%
    str_squish()
  
  # Return as a one-row dataframe
  tibble(
    school_name = name,
    street_address = street_address,
    location_info = location_info
  )
}

# Create a function to clean the Catholic data
clean_catholic_school_data <- function(data) {
  data %>%
    # Remove any leading/trailing whitespace
    mutate(across(everything(), str_trim)) %>%
    # Sort alphabetically by school name
    arrange(school_name)
}

# function to convert x-y coords to lat-long
add_latlon_to_tibble <- function(data, x_col, y_col) {
  
  # Create an sf object with UTM Zone 17N (EPSG:26917)
  points_sf <- st_as_sf(data, 
                        coords = c(x_col, y_col), 
                        crs = 26917)
  
  # Transform to WGS84 (latitude/longitude)
  points_transformed <- st_transform(points_sf, 4326)
  
  # Extract coordinates
  coords <- st_coordinates(points_transformed)
  
  # Add the new columns to the original data
  data %>%
    mutate(
      longitude_new = coords[,1],
      latitude_new = coords[,2]
    )
}

# function to identify collisions within 250m of a school
analyze_collisions_near_schools <- function(collisions, schools, radius_meters = 250) {
  # Function to get names of nearby schools for a single collision
  get_nearby_schools <- function(collision_lat, collision_lon) {
    # Calculate distances from this collision to all schools
    distances <- distHaversine(
      p1 = matrix(c(collision_lon, collision_lat), ncol = 2),
      p2 = matrix(c(schools$longitude, schools$latitude), ncol = 2)
    )
    
    # Get names of schools within radius
    nearby_school_names <- schools$school_name[distances <= radius_meters]
    
    # Return comma-separated list of school names, or NA if none are nearby
    if (length(nearby_school_names) > 0) {
      paste(nearby_school_names, collapse = "; ")
    } else {
      NA_character_
    }
  }
  
  # Add columns for proximity and school names
  collisions_with_proximity <- collisions %>%
    rowwise() %>%
    mutate(
      nearby_schools = get_nearby_schools(latitude, longitude),
      near_school = !is.na(nearby_schools),
      distance_to_nearest_school = min(distHaversine(
        p1 = matrix(c(longitude_use, latitude_use), ncol = 2),
        p2 = matrix(c(schools$longitude, schools$latitude), ncol = 2)
      ))
    ) %>%
    ungroup()
  
  return(collisions_with_proximity)
}

# function to create inetractive map of collsion data near schools
create_school_collision_map <- function(data) {
  # Create popup content for each school
  popup_content <- sprintf(
    "<strong>%s</strong><br/>
     Pedestrian collisions: %d<br/>
     Cyclist collisions: %d<br/>
     Other collisions: %d",
    data$school_name,
    data$pedestrian_collisions,
    data$cyclist_collisions,
    data$other_collisions
  )
  
  # Calculate total collisions for sizing the markers
  data <- data %>%
    mutate(
      # Scale marker size between 8 and 20 based on total collisions
      marker_size = scales::rescale(total_collisions, to = c(8, 20))
    )
  
  # Create custom cluster icons
  cluster_icons <- awesomeIconList(
    "cluster" = makeAwesomeIcon(
      icon = "info-sign",
      markerColor = "blue",
      iconColor = "white",
      library = "glyphicon"
    )
  )
  
  # Create the map
  leaflet(data) %>%
    # Add default OpenStreetMap tiles
    addTiles() %>%
    
    # Add clustered school markers
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~marker_size,
      color = "#2B83BA",         # Blue border
      weight = 2,                # Border width
      fillColor = "#2B83BA",     # Blue fill
      fillOpacity = 0.6,         # Semi-transparent
      popup = ~popup_content,    # Show popup on click
      label = ~school_name,      # Show school name on hover
      
      # Clustering options
      clusterOptions = markerClusterOptions(
        spiderfyOnMaxZoom = TRUE,        # Spread out clusters when fully zoomed
        showCoverageOnHover = TRUE,      # Show cluster bounds on hover
        zoomToBoundsOnClick = TRUE,      # Zoom to cluster bounds on click
        maxClusterRadius = 25,           # Maximum radius to cluster points
        iconCreateFunction = JS("
          function(cluster) {
            var count = cluster.getChildCount();
            var size = Math.min(count * 5 + 20, 50);  // Scale size with count
            return L.divIcon({
              html: '<div style=\"background-color: #2B83BA; color: white; border-radius: 50%; width: ' + size + 'px; height: ' + size + 'px; line-height: ' + size + 'px; text-align: center; font-weight: bold;\">' + count + '</div>',
              className: 'marker-cluster',
              iconSize: L.point(size, size)
            });
          }
        ")
      ),
      
      # Label options
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) 
}