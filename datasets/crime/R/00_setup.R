# setup.R - Package management with pacman

library(pacman)

# Define all packages required by the project
# Group them by purpose for clarity
p_load(
  # Core data manipulation
  tidyverse,  # Includes dplyr, ggplot2, tidyr, etc.
  janitor,    # Cleaningcomparing column names, examining dupes, etc.
  
  # data sources
  cansim,     # Stats Canada tables
  
  # Other utilities
  pdftools,    # reading PDFs
  readxl,      # loading Excel files
  here,        # relative file paths
  openxlsx,    # for handling Excel date-time numbers
  sf,          # for mapping
  units,       # for handling units of measurement
  arrow        # for handling parquet files
)

# Optional: Set global options for your project
options(
  stringsAsFactors = FALSE,     # Prevent strings from converting to factors
  scipen = 999,                 # Avoid scientific notation
  dplyr.summarise.inform = FALSE  # Suppress dplyr join messages
)