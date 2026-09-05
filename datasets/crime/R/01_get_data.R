source(here::here("datasets", "crime", "R", "00_setup.R"))

# Get data (Run this code once a year after latest annual data added to StasCan and WRPS, usually in summer)

# Stats Canada Incidents data for Canada, provs, cmas
incidents_data <- get_cansim("35-10-0177-01")

# Filter for Canada data
incidents_can <- incidents_data %>%
  filter(GEO == "Canada")

# Incidents for Ontario police forces
incidents_ont_connection <- get_cansim_connection("35-10-0180-01")

incidents_ont <- incidents_ont_connection |>
  collect_and_normalize()

# Manual download from Statistics Canada if above times out or hits issues
# url <- "https://www150.statcan.gc.ca/n1/tbl/csv/35100180-eng.zip"
# zip_filename <- here("datasets", "crime", "data-raw", "35100180-eng.zip")
# 
# download.file(url, destfile = zip_filename, mode = "wb")
# unzip(zip_filename, exdir = here("datasets", "crime", "data-raw"))
# file.remove(zip_filename)

# Read the CSV
# incidents_ont <- read_csv(here("datasets", "crime", "data-raw", "35100180.csv"))
# file.remove(here("datasets", "crime", "data-raw", "35100180.csv"))

# Create list of top 50 forces based on population + Ontario overall
top_100_forces <- incidents_ont |> 
  filter(
    REF_DATE == max(REF_DATE),
    #str_detect(GEO, "Provincial", negate = TRUE),     uncomment if want to exclude OPP-enforced regions
    Violations == "Total, all violations",
    Statistics %in% c("Actual incidents", "Rate per 100,000 population")
  ) |> 
  select(GEO, Statistics, VALUE) |> 
  pivot_wider(names_from = Statistics, values_from = VALUE) |> 
  clean_names() |> 
  mutate(population = actual_incidents / rate_per_100_000_population * 100000) |> 
  arrange(desc(population)) |> 
  slice_head(n = 101) |> 
  select(geo)

incidents_ont_top_100 <- incidents_ont |> 
  inner_join(top_100_forces, join_by(GEO == geo))
  
# save to file
write_csv(incidents_can, here("datasets", "crime", "data-raw", "criminal_incidents_canada.csv"))
write_parquet(incidents_ont_top_100, here("datasets", "crime", "data-raw", "criminal_incidents_ontario.parquet"))

# Stats Canada CSI data for CMAs and above
csi_all_cma_higher <- get_cansim("35-10-0026-01")

# Filter for desired data (Canada, provinces, and Ontario CMAs)
csi_can_prov_ont_cma <- csi_all_cma_higher |>
  filter(
    GEO == "Canada" | str_detect(GEO, "Ontario") | nchar(GeoUID) == 2,
    str_detect(GEO, "Quebec", negate = TRUE)
  )

# Stats Canada CSI data for Ontario police forces
csi_ont <- get_cansim("35-10-0188-01")

# save to file
write_csv(csi_can_prov_ont_cma, here("datasets", "crime", "data-raw", "csi_canada_provs_ont_cmas.csv"))
write_csv(csi_ont, here("datasets", "crime", "data-raw", "csi_ontario_forces.csv"))

# get Stats Can hate crimes data
hate_crimes <- get_cansim("35-10-0191-01")

write_csv(hate_crimes, here("datasets", "crime", "data-raw", "hate_crimes.csv"))

# get Stats Can cybercrime data
cyber_crimes <- get_cansim("35-10-0002-01")

write_csv(cyber_crimes, here("datasets", "crime", "data-raw", "cyber_crimes.csv"))

# get crime victim data

homicide_victims_cmas <- get_cansim("35-10-0071-01") |> 
  filter(GEO != "Canada") # remove Canada data also included in next table

homicide_victims_provinces <- get_cansim("35-10-0068-01")

homicide_victims <- bind_rows(homicide_victims_cmas, homicide_victims_provinces)

write_csv(homicide_victims, here("datasets", "crime", "data-raw", "homicide_victims.csv"))

# age of victims of violent crimes
violent_crime_victims_age <- get_cansim("35-10-0049-01")

# gender of victims of violent crimes
violent_crime_victims_gender <- get_cansim("35-10-0050-01")

# combine and save
violent_crime_victims <- bind_rows(violent_crime_victims_age, violent_crime_victims_gender)

write_csv(violent_crime_victims, here("datasets", "crime", "data-raw", "violent_crime_victims.csv"))

# family and IPV victims
family_victims <- get_cansim("35-10-0200-01")
ipv_victims <- get_cansim("35-10-0202-01")

# combine and save
family_ipv_victims <- bind_rows(family_victims, ipv_victims)

write_csv(family_ipv_victims, here("datasets", "crime", "data-raw", "family_ipv_victims.csv"))


# get Stats Can police personnel data for Ontario municipalities
# No 2020 due to COVID
# As of 2023, the Police Administration Survey is conducted biennially. As such, data were not collected for 2024. 
police_personnel_munic <- get_cansim("35-10-0077-01") |> 
  filter(str_detect(GEO, "Ontario"))

# get personnel data for Ontario and Canada
police_personnel_ont_can <- get_cansim("35-10-0076-01") |> 
  filter(str_detect(GEO, "Ontario|Canada"))

write_csv(police_personnel_munic, here("datasets", "crime", "data-raw", "police_personnel_munic.csv"))
write_csv(police_personnel_ont_can, here("datasets", "crime", "data-raw", "police_personnel_ont_can.csv"))

# WRPS occurrence data
# Define new column names based on existing names that vary in format across files
new_col_names <- c("occurrence_number", "geographic_location", 
                   "nearest_intersection_location", "patrol_division", 
                   "patrol_zone", "municipality", "reported_date_and_time", 
                   "initial_call_type", "initial_call_type_description", 
                   "final_call_type", "final_call_type_description", 
                   "initial_priority", "final_priority", "disposition", 
                   "dispatch_date_and_time", "arrival_date_and_time", 
                   "cleared_date_and_time", "call_dispatch_delay", 
                   "call_travel_time", "call_on_scene_time", 
                   "call_response_time", "call_service_time", 
                   "total_unit_service_time"
) 

# Get a list of all Excel files in the raw_occurrence_data folder
excel_files <- list.files(
  path = here("datasets", "crime", "data-raw", "raw_occurrence_data_files"),
  full.names = TRUE
)

# Read and combine all Excel files
raw_occurrence_data <- excel_files %>%
  map_df(~{
    # Read the Excel file with all columns as text
    read_excel(
      .x, 
      col_types = "text"  # Force all columns to be read as text
    ) %>%
      # Assign new column names
      set_names(new_col_names)
  })

# Save to local file
saveRDS(raw_occurrence_data, here("datasets", "crime", "data-raw", "occurrence_data.rds"))

# download and combine municipal financial returns

# Function to download, extract, and process FIR data
download_and_process_fir_data <- function() {
  
  # Years to download (2000-2025)
  years <- 2000:2025
  base_url <- "https://efis.fma.csc.gov.on.ca/fir/MultiYearReport/fir_data_"
  
  # Create a list to store all dataframes
  all_fir_data <- list()
  successful_downloads <- 0
  failed_downloads <- 0
  
  cat("Starting download and processing of FIR data for", length(years), "years...\n\n")
  
  for (year in years) {
    cat("Processing year:", year, "\n")
    
    # Construct URL and file paths
    zip_url <- paste0(base_url, year, ".zip")
    zip_file <- here("datasets", "crime", "data-raw", "municipal_fir_raw", paste0("fir_data_", year, ".zip"))
    
    tryCatch({
      # Download zip file
      cat("  Downloading zip file...")
      download.file(zip_url, zip_file, mode = "wb", quiet = TRUE)
      
      # Check if file was actually downloaded
      if (!file.exists(zip_file) || file.info(zip_file)$size == 0) {
        cat(" Failed (file not created or empty)\n")
        failed_downloads <- failed_downloads + 1
        next
      }
      cat(" ✓\n")
      
      # List contents of zip to find CSV file
      cat("  Checking zip contents...")
      zip_contents <- unzip(zip_file, list = TRUE)
      csv_files <- zip_contents$Name[grepl("\\.csv$", zip_contents$Name, ignore.case = TRUE)]
      
      if (length(csv_files) == 0) {
        cat(" No CSV files found\n")
        file.remove(zip_file)
        failed_downloads <- failed_downloads + 1
        next
      }
      cat(" ✓\n")
      
      # Extract CSV file(s)
      cat("  Extracting CSV file(s)...")
      unzip(zip_file, files = csv_files, exdir = here("datasets", "crime", "data-raw", "municipal_fir_raw"))
      cat(" ✓\n")
      
      # Read the CSV file (use the first CSV if multiple)
      csv_file_path <- here("datasets", "crime", "data-raw", "municipal_fir_raw", csv_files[1])
      cat("  Reading CSV data...")
      
      year_data <- read_csv(csv_file_path, 
                            show_col_types = FALSE,
                            locale = locale(encoding = "UTF-8")) |>
        mutate(data_year = year) |>  # Add year column to track source
        clean_names()
      
      cat(" ✓\n")
      cat("    Rows:", nrow(year_data), "| Columns:", ncol(year_data), "\n")
      
      # Store in list
      all_fir_data[[as.character(year)]] <- year_data
      successful_downloads <- successful_downloads + 1
      
      # Clean up: delete only the zip file, keep the CSV
      file.remove(zip_file)
      cat("  Cleaned up zip file, kept CSV ✓\n")
      
    }, error = function(e) {
      cat("  Error:", e$message, "\n")
      failed_downloads <- failed_downloads + 1
      
      # Clean up partial files if they exist (keep CSVs, remove zips)
      if (file.exists(zip_file)) file.remove(zip_file)
    })
    
    cat("\n")
  }
  
  cat("Download Summary:\n")
  cat("  Successful:", successful_downloads, "\n")
  cat("  Failed:", failed_downloads, "\n\n")
  
  return(all_fir_data)
}

# Function to combine all data and save
combine_and_save_fir_data <- function(data_list) {
  
  if (length(data_list) == 0) {
    cat("No data to combine.\n")
    return(NULL)
  }
  
  cat("Combining", length(data_list), "years of FIR data...\n")
  
  # Get all unique column names across all years
  all_columns <- data_list |>
    map(names) |>
    unlist() |>
    unique()
  
  cat("Total unique columns across all years:", length(all_columns), "\n")
  
  # Ensure all dataframes have the same columns (fill missing with NA)
  standardized_data <- map(data_list, function(df) {
    missing_cols <- setdiff(all_columns, names(df))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        df[[col]] <- NA
      }
    }
    return(df |> select(all_of(all_columns)))
  })
  
  # Combine all data then filter for Big 12 regions
  # create vector of Big 12 municipalities using FIR names
  big_12_regions <- c("Waterloo R", "Peel R", "London C", "Windsor C", "Toronto C", 
                      "Halton R", "Durham R", "York R", "Niagara R", "Hamilton C", 
                      "Hamilton - Wentworth R", "Sudbury R", "Greater Sudbury C",
                      "Ottawa - Carleton R", "Ottawa C")
  
  combined_data <- bind_rows(standardized_data) |> 
    clean_names() |> 
    filter(municipality_desc %in% big_12_regions)
  
  cat("Combined data dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
  
  # Save to CSV
  output_file <- here("datasets", "crime", "data-raw", "municipal_fir.csv")
  write_csv(combined_data, output_file)
  
  cat("Data saved to:", output_file, "\n")
  cat("File size:", round(file.info(output_file)$size / (1024^2), 2), "MB\n")
  
  return(combined_data)
}

# Execute the full process
cat("=== Starting FIR Data Download and Processing ===\n\n")

# Download and process all years
fir_data_list <- download_and_process_fir_data()

# Combine and save
if (length(fir_data_list) > 0) {
  combined_fir_data <- combine_and_save_fir_data(fir_data_list)
  
  cat("\n=== Process Complete ===\n")
  cat("You can now access the combined FIR data using:\n")
  cat('fir_data <- read_csv(here("datasets", "crime", "data-raw", "municipal_fir.csv"))\n')
  
  # Show a preview of the combined data
  if (!is.null(combined_fir_data)) {
    cat("\nPreview of combined data:\n")
    print(combined_fir_data |> select(1:6) |> head())
  }
  
} else {
  cat("\n=== Process Failed ===\n")
  cat("No data was successfully downloaded. Please check:\n")
  cat("1. Internet connection\n")
  cat("2. URL accessibility\n") 
  cat("3. File permissions in the data directory\n")
}