# Function to get the closest match for a city based on fuzzy string distance
correct_city <- function(misspelled_city, province, reference_data) {
  # Filter reference data to match the province/territory
  ref_subset <- reference_cities %>%
    filter(province_territory == province)
  
  # Compute string distances between the misspelled city and reference cities
  distances <- stringdist(misspelled_city, ref_subset$city, method = "jw")
  
  # Return the closest match and its distance from teh original
  closest <- ref_subset$city[which.min(distances)]
  distance <- round(min(distances), 2)
  return(c(closest, distance))
}

