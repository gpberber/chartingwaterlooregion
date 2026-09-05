here::i_am("datasets/crime/R/helpers.R")

source(here::here("datasets", "crime", "R", "00_setup.R"))

# Function to convert date columns in occurrences data
convert_dates <- function(date_col) {
  # Initialize result with NAs
  result <- rep(as.POSIXct(NA), length(date_col))
  
  # Process already formatted dates (YYYY-MM-DD format)
  formatted_pattern <- grepl("^\\d{4}-\\d{2}-\\d{2}", date_col)
  if(any(formatted_pattern)) {
    # For dates already in YYYY-MM-DD format, just convert directly
    formatted_indices <- which(formatted_pattern)
    result[formatted_indices] <- as.POSIXct(date_col[formatted_indices], format="%Y-%m-%d %H:%M:%S")
  }
  
  # Process Excel numeric dates
  numeric_pattern <- grepl("^\\d+\\.\\d+$", date_col)
  if(any(numeric_pattern)) {
    numeric_indices <- which(numeric_pattern)
    numeric_dates <- as.numeric(date_col[numeric_indices])
    result[numeric_indices] <- convertToDateTime(numeric_dates, origin = "1899-12-30")
  }
  
  # Process integer dates (without decimal part)
  integer_pattern <- grepl("^\\d+$", date_col) & !grepl("^\\d{4}-\\d{2}-\\d{2}", date_col)
  if(any(integer_pattern)) {
    integer_indices <- which(integer_pattern)
    integer_dates <- as.numeric(date_col[integer_indices])
    result[integer_indices] <- convertToDateTime(integer_dates, origin = "1899-12-30")
  }
  
  return(result)
}

# Function to retrive Waterloo Region neighbourhood shapefiles from geoJSON server
get_waterloo_neighbourhoods <- function(
    url = "https://utility.arcgis.com/usrsvcs/servers/3aa08470e3614f228b3953b4a32af081/rest/services/OpenData/OpenData/MapServer/26/query?outFields=*&where=1%3D1&f=geojson",
    save_path = "data/waterloo_region_neighbourhoods.rds"
) {
  # Retrieve the GeoJSON data
  waterloo_data <- sf::st_read(url)
  
  # Clean column names
  waterloo_data <- janitor::clean_names(waterloo_data)
  
  # Calculate area in square kilometers
  waterloo_data$area_km2 <- sf::st_area(waterloo_data) |>
    units::set_units("km^2")
  
  # Create simplified version
  waterloo_simplified <- waterloo_data |>
    dplyr::select(newcdtxt, municipality, neighbourhood_number, area_km2) |>
    dplyr::rename(
      neighbourhood = newcdtxt,
      code = neighbourhood_number
    )
  
  # Save the simplified data to the specified path
  saveRDS(waterloo_simplified, file = save_path)
  
  # Return the simplified data invisibly
  return(invisible(waterloo_simplified))
}

# Function r=to retrieve city/municipality shapefiles from geoJSON server
get_waterloo_cities <- function(
    url = "https://utility.arcgis.com/usrsvcs/servers/369f39237bf844e3abcf39d71947ee49/rest/services/OpenData/OpenData/MapServer/15/query?outFields=*&where=1%3D1&f=geojson",
    save_path = "data/waterloo_region_cities.rds"
) {
  # Retrieve the GeoJSON data
  cities_data <- sf::st_read(url, quiet = TRUE)
  
  # Clean column names
  cities_data <- janitor::clean_names(cities_data)
  
  # Rename columns as requested
  cities_data <- cities_data |>
    dplyr::rename(
      city = place_name
      # Municipality is already correctly named after clean_names()
    )
  
  # Check and fix invalid geometries
  is_valid <- sf::st_is_valid(cities_data)
  
  if (!all(is_valid)) {
    message("Some geometries are invalid. Attempting to repair...")
    # Make a valid geometry using st_make_valid
    cities_data <- sf::st_make_valid(cities_data)
    
    # Check again after repair
    is_valid_after <- sf::st_is_valid(cities_data)
    if (!all(is_valid_after)) {
      warning("Some geometries are still invalid after repair. Results may be inaccurate.")
    }
  }
  
  # Calculate area in square kilometers
  # Use try-catch to handle potential errors in area calculation
  cities_data$area_km2 <- tryCatch({
    areas <- sf::st_area(cities_data)
    units::set_units(areas, "km^2")
  }, error = function(e) {
    warning("Error calculating areas: ", e$message, 
            "\nUsing an alternative method to calculate areas.")
    
    # Alternative calculation using planar geometry if s2 spherical geometry fails
    sf::sf_use_s2(FALSE)  # Temporarily disable s2
    areas <- sf::st_area(cities_data)
    sf::sf_use_s2(TRUE)   # Re-enable s2
    units::set_units(areas, "km^2")
  })
  
  # Save the data to the specified path
  saveRDS(cities_data, file = save_path)
  
  # Return the data invisibly
  return(invisible(cities_data))
}
# ---------------------------------------------------------------------------
# WRPS occurrence data disclaimer
# ---------------------------------------------------------------------------
# WRPS publishes its occurrence dataset for public analysis with this condition
# attached. Every post that uses wat_region_occurrences must show it, for
# example in a callout in the Data and methods section:
#   ::: {.callout-note}
#   `r wrps_disclaimer`
#   :::
wrps_disclaimer <- paste(
  "This analysis uses the Waterloo Regional Police Service (WRPS) occurrence dataset.",
  "Any statements, conclusions, or publications based upon this WRPS occurrence data",
  "made by non-WRPS employees are made without the authorization of WRPS and are not",
  "the opinion of the WRPS."
)
