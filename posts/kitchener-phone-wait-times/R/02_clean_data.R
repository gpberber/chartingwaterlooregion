# 02_clean_data.R
# Turns the raw City of Kitchener phone-queue export into one tidy table,
# data/queue_daily.csv: one row per day, one column per metric.
# Run from the project root: source("posts/kitchener-phone-wait-times/R/02_clean_data.R")

here::i_am("posts/kitchener-phone-wait-times/R/02_clean_data.R")

library(tidyverse)
library(janitor)
library(here)

raw_dir <- here("posts", "kitchener-phone-wait-times", "data-raw")

# ---- Ontario statutory holidays ---------------------------------------------
# "Additional Day" rows mark the observed day when a holiday falls on a weekend;
# the contact centre is closed on the holiday itself, so keep only real holidays.
holidays <- read_csv(file.path(raw_dir, "holidays.csv"), show_col_types = FALSE) |>
  clean_names() |>
  filter(holiday != "Additional Day")

# ---- Repair the raw export ---------------------------------------------------
# The portal export has broken line endings: records run together on one line
# and dates are wrapped in stray quotes. Read it as text, split it back into
# one record per line, then parse it as a normal CSV.
raw_text <- read_lines(file.path(raw_dir, "kitchener_phone_queue_metrics.csv"))
header <- raw_text[1]
data_lines <- raw_text[-1]

fixed_text <- data_lines |>
  # a value glued to the next record's date: put a newline between them
  str_replace_all('(\\d+)"(\\d{2}-[A-Z]{3}-\\d{2})', "\\1\n\\2") |>
  # quotes on either side of a date
  str_replace_all('(\\d{2}-[A-Z]{3}-\\d{2})"', "\\1") |>
  str_replace_all('"(\\d{2}-[A-Z]{3}-\\d{2})', "\\1")

records <- str_split(fixed_text, "(?=\\d{2}-[A-Z]{3}-\\d{2})")[[1]]
queue_long <- read_csv(I(paste(c(header, records), collapse = "\n")), show_col_types = FALSE) |>
  clean_names() |>
  rename_with(~ str_remove(.x, "phone_")) |>
  mutate(date = dmy(date)) |>
  select(-queue) |>                 # same value on every row
  distinct()

# ---- Keep only complete days -------------------------------------------------
# Every normal day reports the same 17 metrics. A handful of days have fewer
# (incomplete) or more (two conflicting values for the same metric); both kinds
# are dropped. The export starts on 26 February 2019, so the first partial
# month is dropped too.
queue_daily <- queue_long |>
  add_count(date, name = "n_metrics") |>
  filter(n_metrics == 17, date >= ymd("2019-03-01")) |>
  select(-n_metrics) |>
  pivot_wider(names_from = metric, values_from = value) |>
  clean_names() |>
  # shorter names for the columns the post uses most
  rename(
    answered_within_target = calls_handled_service_level,
    abandoned_within_target = calls_abandoned_service_level,
    pct_answered_within_target = percentage_of_service_level_met_with_abandoned_calls_counted_negatively,
    pct_handled_within_target = percentage_of_service_level_met_handled,
    pct_within_target_abandoned_positive = percentage_of_service_level_met_with_abandoned_calls_counted_positively,
    pct_within_target_excl_abandoned = percentage_of_service_level_met_without_abandoned_calls,
    target_seconds = service_level_sec
  ) |>
  # calendar columns
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    year_month = floor_date(date, "month"),
    day_of_week = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    holiday = date %in% holidays$date,
    weekend_or_holiday = day_of_week %in% c("Sat", "Sun") | holiday
  ) |>
  relocate(date, year, month, year_month, day_of_week, holiday, weekend_or_holiday) |>
  arrange(date)

write_csv(queue_daily, here("posts", "kitchener-phone-wait-times", "data", "queue_daily.csv"))
