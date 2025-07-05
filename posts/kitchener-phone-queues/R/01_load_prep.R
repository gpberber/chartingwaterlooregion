here::i_am("posts/kitchener-phone-queues/R/01_load_prep.R")

# Required packages
library(tidyverse)
library(janitor)
library(here)

# load Ontario stat holidays, source: https://github.com/uWaterloo/Datasets/blob/master/Holidays/holidays.csv
holidays <- read_csv(here("posts", "kitchener-phone-queues", "data", "holidays.csv")) |> 
  filter(holiday != "Additional Day") 

# covid lockdown dates
lockdown_dates <- c(
  seq.Date(ymd("2020-03-17"), ymd("2020-07-16"), "days"),
  seq.Date(ymd("2020-12-26"), ymd("2021-02-09"), "days"),
  seq.Date(ymd("2021-04-08"), ymd("2021-07-15"), "days"),
  seq.Date(ymd("2022-01-05"), ymd("2022-03-20"), "days")
)

# Read the raw file
raw_text <- readLines(here("posts", "kitchener-phone-queues", "data", "kitchener_phone_queue_metrics.csv"))

# Skip the header line (since it's formatted correctly)
header <- raw_text[1]
data_lines <- raw_text[-1]

# First add newlines between records
fixed_text <- str_replace_all(data_lines, '(\\d+)"(\\d{2}-[A-Z]{3}-\\d{2})', '\\1\n\\2')

# Remove ALL quotes around dates (both before and after)
fixed_text <- str_replace_all(fixed_text, '(\\d{2}-[A-Z]{3}-\\d{2})"', '\\1')
fixed_text <- str_replace_all(fixed_text, '"(\\d{2}-[A-Z]{3}-\\d{2})', '\\1')

# Now split into individual records and add line breaks
records <- str_split(fixed_text, '(?=\\d{2}-[A-Z]{3}-\\d{2})')[[1]]

# Combine header and fixed data into tibble
queue_data_fixed <- read_csv(paste(c(header, records), collapse = "\n"))

# Clean column names and types, add date columns
queue_data_clean <- queue_data_fixed |> 
  clean_names() |> 
  rename_with(~ str_remove(., "phone_")) |> 
  mutate(date = dmy(date)) |> 
  select(-queue) |> # queue has same value for every record
  arrange(date, metric) |> 
  left_join(holidays) |> 
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    year_month = make_date(year = year, month = month_num),
    day_of_week = factor(
      weekdays(date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    type_of_day = if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    holiday = !is.na(holiday),
    weekend_holiday = if_else(type_of_day == "Weekend" | holiday, TRUE, FALSE),
    covid_lockdown = date %in% lockdown_dates
  ) |> 
  relocate(date) |> 
  distinct()

write_csv(queue_data_clean, here("posts", "kitchener-phone-queues", "data", "queue_data_clean.csv"))
