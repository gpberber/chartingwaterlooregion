# Run this once a year after raw data has been updated via 02_update_data.R

# Load crime data from local files
source(here::here("posts", "crime", "R", "01_setup.R"))
source(here::here("posts", "crime", "R", "03a_prep_functions.R"))

# Run this once a year after raw data has been updated via 02_update_data.R

# ---------------------------------------------------------------------------
# Load crime data from local files
# ---------------------------------------------------------------------------

incidents_can_raw <- read_csv(here("posts", "crime", "data", "criminal_incidents_canada.csv")) |> 
  clean_names()

# NOTE: the Ontario parquet is read further below, after the needed columns are
# identified. Reading all 24 columns produced a 1.9 GB object.

csi_can_provs_ont_raw <- read_csv(here("posts", "crime", "data", "csi_canada_provs_ont_cmas.csv")) |> 
  clean_names()

csi_ont_forces_raw <- read_csv(here("posts", "crime", "data", "csi_ontario_forces.csv")) |> 
  clean_names()

homicide_victims_raw <- read_csv(here("posts", "crime", "data", "homicide_victims.csv")) |> 
  clean_names()

hate_crimes_raw <- read_csv(here("posts", "crime", "data", "hate_crimes.csv")) |> 
  clean_names()

cyber_crimes_raw <- read_csv(here("posts", "crime", "data", "cyber_crimes.csv")) |> 
  clean_names()

violent_crime_victims_raw <- read_csv(here("posts", "crime", "data", "violent_crime_victims.csv")) |> 
  clean_names()

family_ipv_victims_raw <- read_csv(here("posts", "crime", "data", "family_ipv_victims.csv")) |> 
  clean_names()

personnel_munic_raw <- read_csv(here("posts", "crime", "data", "police_personnel_munic.csv")) |> 
  clean_names()

personnel_ont_can_raw <- read_csv(here("posts", "crime", "data", "police_personnel_ont_can.csv")) |> 
  clean_names()

ucr_categories_raw <- read_csv(here("posts", "crime", "data", "ucr_categories.csv")) |> 
  mutate(ucr_code = as.character(ucr_code))

fir_raw <- read_csv(here("posts", "crime", "data", "municipal_fir.csv")) |> 
  clean_names()

# --- Occurrence data ---------------------------------------------------------
# Only used by the occurrence-cleaning section at the end of the script, and
# among the largest objects in the session. Leave commented out unless you are
# actually re-running that section.

# occurrence_raw <- read_rds(here("posts", "crime", "data", "occurrence_data.rds"))
# occurrence_old <- read_rds(here("posts", "crime", "data", "clean", "wat_region_occurrences.rds"))
# occurrence_new <- anti_join(occurrence_raw, occurrence_old, join_by(occurrence_number))


# ---------------------------------------------------------------------------
# Prep incident data for cleaning
# ---------------------------------------------------------------------------

# Note: the per-100k figures in the raw StatsCan data are based on populations
# for corresponding year from Table 17-10-0155-01 (verified by reverse-engineering
# the per-100k figures to get population and then matching against figures in that table)

# note there are no entries for ucr codes 1633 to 1641, which came into effect in 2022. This is the likely cause
# of the post-2021 differences between the Total Other Violent Violations and the sum of the 4-digit ucr categories
# that make up this total. Stats Can table 35-10-0177-01 must be incorrectly excluding these codes. 

# Some "Total" lines do not have children among the 4-digit ucr codes in the incidents data,
# so these lines need to be included in the incidents table despite Totals otherwise being excluded from this table
# The code below identifies all Totals without 4-digit ucr codes further down the violation hierarchy

violations <- incidents_can_raw |> 
  select(contains("violations")) |> 
  distinct() |> 
  mutate(ucr_code = str_remove_all(classification_code_for_violations, "[\\[\\]]"))

number_of_children <- violations |> 
  mutate(
    parent_violation = str_remove(hierarchy_for_violations, "\\.[^.]*$")
  ) |> 
  summarise(num_children = n(), .by = c(parent_violation))

totals_missing_children <- violations |> 
  left_join(number_of_children, join_by(hierarchy_for_violations == parent_violation)) |> 
  filter(
    str_starts(violations, "Total"),
    is.na(num_children)
  ) |> 
  arrange(ucr_code) 

totals_to_include <- totals_missing_children |> 
  pull(ucr_code)

# The Totals included represent the following four-digit ucr codes
# Determined by matching nature of violation with similarly worded violations that have 4-digit codes
# not appearing in the raw incidents listing
# 135 = 1460-1462
# 220 = 2135
# 211 = 2153, 2156
# 212 = 2152, 2155
# 335 = 3812-3816
# 620 = 6500-6530
# 930 = 9310-9313

# create rows to add to ucr categories
extra_ucr_categories <- tribble(
  ~ucr_code,  ~category,                        ~subcategory,                            ~violation_master,
  "135",  "Crimes Against the Person",      "Assaults/Firearm related offences",     "Assault Against a Peace Officer",
  "211",  "Crimes Against Property",        "Possession / Trafficking Stolen Goods", "Possession of Stolen Goods",
  "212",  "Crimes Against Property",        "Possession / Trafficking Stolen Goods", "Trafficking of Stolen Property",
  "220",  "Crimes Against Property",        "Theft",                                 "Motor Vehicle Theft",
  "335",  "Other Criminal Code Violations", "Other Criminal Code",                   "Violations Related to Animal Cruelty",
  "620",  "Other Criminal Code Violations", "Other Criminal Code",                   "Immigration And Refugee Protection Act",
  "930",  "Traffic Violations",             "Failure to Stop after Accident",        "Failure to Stop or Remain"
)

ucr_categories <- ucr_categories_raw |> 
  bind_rows(extra_ucr_categories) |> 
  arrange(category, subcategory, ucr_code)

# --- Read only the Ontario columns that survive into incidents_prepped -------
#
# The parquet is 10,744,024 rows x 24 columns (106 MB on disk, 1.9 GB in memory).
# Only 8 of those columns are used downstream; the other 16 (Date, DGUID,
# val_norm, UOM, UOM_ID, SCALAR_FACTOR, SCALAR_ID, VECTOR, COORDINATE, STATUS,
# SYMBOL, TERMINATED, DECIMALS, Hierarchy for GEO, and the two Statistics
# classification columns) were read, type-converted, and then discarded.

ont_parquet <- here("posts", "crime", "data", "criminal_incidents_ontario.parquet")

# clean_names() versions of the columns actually needed
ont_cols_needed <- c(
  "ref_date", "geo", "geo_uid", "violations", "statistics", "value",
  "classification_code_for_violations", "hierarchy_for_violations"
)

# col_select applies to the ORIGINAL parquet names (REF_DATE, GEO, GeoUID,
# Violations, Statistics, VALUE, "Classification Code for Violations",
# "Hierarchy for Violations"), so match on the cleaned version rather than
# hard-coding cansim's capitalisation. open_dataset() reads metadata only.
ont_cols_to_read <- tibble(original = names(open_dataset(ont_parquet))) |> 
  filter(make_clean_names(original) %in% ont_cols_needed) |> 
  pull(original)

# fails loudly if the schema ever changes
stopifnot(length(ont_cols_to_read) == length(ont_cols_needed))

# arrow reads column-wise, so unselected columns are never decompressed
incidents_ont_raw <- read_parquet(ont_parquet, col_select = all_of(ont_cols_to_read)) |> 
  clean_names()


# extract/remove geo_uid and ucr codes from columns in Ontario incidents
#
# The conversions for uom_id, scalar_id, terminated, decimals, hierarchy_for_geo,
# classification_code_for_statistics and hierarchy_for_statistics are gone: those
# columns are no longer read, and were dropped by the select() below anyway.
# VALUE is already double in the parquet, so it never needed converting.
#
# The year filter is pulled forward from incidents_prepped so pre-2000 rows are
# dropped before bind_rows() and pivot_wider(), the two peak-memory steps.
incidents_ont_clean <- incidents_ont_raw |> 
  mutate(
    ref_date = as.numeric(ref_date),
    geo_uid  = as.numeric(geo_uid),
    geo      = str_remove(geo, " \\[.*\\]")
  ) |> 
  filter(ref_date > 1999)   # to match personnel data's start year

rm(incidents_ont_raw)


incidents_prepped <- incidents_can_raw |> 
  # take the same 8 columns from the Canada file before binding, so bind_rows()
  # is not reconciling 16 Canada-only columns against NA-filled Ontario ones
  select(all_of(ont_cols_needed)) |> 
  filter(ref_date > 1999) |> 
  bind_rows(incidents_ont_clean) |> 
  select(
    year = ref_date,
    region = geo,
    geo_uid,
    violation = violations,
    statistics,
    value,
    ucr_code = classification_code_for_violations,
    violation_hierarchy = hierarchy_for_violations
  ) |> 
  mutate(
    opp = if_else(str_detect(region, "Ontario Provincial"), "OPP", "Non-OPP"),
    ucr_code = as.character(str_extract(ucr_code, "(?<=\\[)\\d+(?=\\])")),
    region = str_remove_all(region, " Region.*|, Ontario.*|^Greater "),
    region = str_replace(region, "Waterloo", "WRPS")
  ) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(
    incidents = actual_incidents,
    incidents_per_100k = rate_per_100_000_population,
    yoy_change_incidents_per_100k = percentage_change_in_rate,
    perc_contribution_to_csi = percentage_contribution_to_the_crime_severity_index_csi,
    persons_charged = total_persons_charged,
    persons_charged_per_100k_12_over = rate_total_persons_charged_per_100_000_population_aged_12_years_and_over,
    adults_charged = total_adult_charged,
    adults_charged_per_100k_18_over = rate_adult_charged_per_100_000_population_aged_18_years_and_over,
    youth_charged = total_youth_charged,
    youth_charged_per_100k_12_to_17 = rate_youth_charged_per_100_000_population_aged_12_to_17_years,
    youth_diverted = total_youth_not_charged,
    youth_diverted_per_100k_12_to_17 = rate_youth_not_charged_per_100_000_population_aged_12_to_17_years
  ) |> 
  filter(!is.na(incidents)) |>   # year filter now applied upstream
  mutate(
    violation = str_replace(violation, "Homicide", "Total homicides"),
    violation = str_replace(violation, "Possession, cannabis", "Cannabis, possession"),
    violation = str_to_title(violation),
    violation = if_else(ucr_code %in% totals_to_include, str_replace(violation, "Total ", ""), violation),
    type = case_when(
      str_starts(ucr_code, "1") == 1 ~ "Violent", 
      ucr_code %in% c("0", "25", "50") ~ "All",
      TRUE ~ "Non-Violent"
    ),
    clearance_rate = if_else(incidents == 0, NA_integer_, round(total_cleared / incidents * 100, 2)),
    youth_charged_per_100k_12_to_17 = if_else(year < 2004, NA_integer_, youth_charged_per_100k_12_to_17), # all original values were zero for these years, even when numerator was sizable, so assumed not calculated for these years
    youth_diverted_per_100k_12_to_17 = if_else(year < 2004, NA_integer_, youth_diverted_per_100k_12_to_17),
    adults_charged_per_100k_18_over = if_else(year < 2004, NA_integer_, adults_charged_per_100k_18_over),
    persons_charged_per_100k_12_over = if_else(year < 2004, NA_integer_, persons_charged_per_100k_12_over),
    # was nested inside a second mutate() call, relying on dplyr's auto-splicing
    # of unnamed data frame arguments - same result, less surprising
    across(c(region, violation), as.factor)
  ) |> 
  arrange(year, region, ucr_code) |> 
  relocate(year, region, geo_uid, ucr_code, violation_hierarchy, type)

# drop the large inputs now that incidents_prepped exists
rm(incidents_can_raw, incidents_ont_clean)
gc()

# Arrow allocates through its own memory pool, so the Environment pane figure can
# lag. These two together give the real picture:
#   lobstr::mem_used()
#   arrow::default_memory_pool()$bytes_allocated
  

# Create tibble of incident totals and subtotals by ucr categories
incident_totals <- incidents_prepped |> 
  filter(str_detect(violation, "^Total")) |> 
  mutate(violation = str_remove(violation, "Total,* ")) |> 
  arrange(region, year)

write_rds(incident_totals, here("posts", "crime", "data", "clean", "criminal_incident_totals.rds"))

# create tibble of violation categories for joining below
violation_categories <- incident_totals |> 
  select(
    violation_category_num = violation_hierarchy,
    class = violation  
  ) |> 
  distinct()

# Create tibble of detailed incident data by ucr code

incidents <- incidents_prepped |> 
  filter(nchar(ucr_code) == 4 | ucr_code %in% totals_to_include) |> 
  left_join(ucr_categories, by = "ucr_code") |> 
  relocate(year, region, geo_uid, ucr_code, violation_hierarchy, type, category:violation_master, violation, effective_date:expiry_date) |> 
  filter(
    is.na(effective_date) | year(effective_date) <= year | incidents > 0,
    is.na(expiry_date) | year(expiry_date) >= year  | incidents > 0
  ) |>
  mutate(violation_category_num = if_else(
    ucr_code %in% totals_to_include,
    violation_hierarchy,
    str_remove(violation_hierarchy, "\\.[^.]*$")
    )) |> 
  left_join(violation_categories) |> 
  select(-violation_category_num) |> 
  relocate(class, .after = "subcategory") |> 
  mutate(across(category:violation_master, as.factor))
  
write_rds(incidents, here("posts", "crime", "data", "clean", "criminal_incidents.rds"))

# add nonviolent category to summary stats and create other relevant columns
summary_totals <- incident_totals |> 
  filter(violation %in% c("All Violations", "Violent Criminal Code Violations")) |> 
  select(year, region, geo_uid, opp, type, incidents:total_cleared, clearance_rate) |> 
  arrange(region, type, year) |> 
  pivot_wider(
    names_from = type,
    names_glue = "{.value}_{type}",
    values_from = c(incidents:clearance_rate)
  ) |> 
  clean_names() |> 
  mutate(
    incidents_nonviolent = incidents_all - incidents_violent,
    incidents_per_100k_nonviolent = incidents_per_100k_all - incidents_per_100k_violent,
    perc_contribution_to_csi_nonviolent = perc_contribution_to_csi_all - perc_contribution_to_csi_violent,
    unfounded_incidents_nonviolent = unfounded_incidents_all - unfounded_incidents_violent,
    total_cleared_nonviolent = total_cleared_all - total_cleared_violent,
    clearance_rate_nonviolent = round(total_cleared_nonviolent / incidents_nonviolent * 100, 2),
    percent_unfounded_nonviolent = round(unfounded_incidents_nonviolent / incidents_nonviolent * 100, 2)
  ) |> 
  arrange(region, year) |> 
  group_by(region) |> 
  mutate(
    base_incidents_per_100k_all = round(incidents_per_100k_all / first(incidents_per_100k_all) * 100, 1),
    base_incidents_per_100k_violent = round(incidents_per_100k_violent / first(incidents_per_100k_violent) * 100, 1),
    base_incidents_per_100k_nonviolent = round(incidents_per_100k_nonviolent / first(incidents_per_100k_nonviolent) * 100, 1),
    yoy_change_incidents_per_100k_nonviolent = round((incidents_per_100k_nonviolent - lag(incidents_per_100k_nonviolent)) / lag(incidents_per_100k_nonviolent) * 100, 2)
  ) |> 
  ungroup()

write_rds(summary_totals, here("posts", "crime", "data", "clean", "criminal_incident_summary.rds"))

# clean csi data 
# reduce Ontario forces to top 50
csi_ont_forces_top_100 <- csi_ont_forces_raw |> 
  inner_join(incidents_prepped |> filter(geo_uid != 11124) |> select(geo_uid) |> distinct(), join_by(geo_uid)) |> 
  filter(
    str_detect(statistics, "[Yy]outh", negate = TRUE),
    geo != "Ontario [35]"
  ) |> 
  select(
    year = ref_date,
    region = geo,
    geo_uid,
    statistics,
    value
  ) |> 
  mutate(
    opp = if_else(str_detect(region, "Ontario Provincial"), "OPP", "Non-OPP"),
    region = str_remove_all(region, " Region.*|, Ontario.*| \\[.*\\]|^Greater "),
    region = str_replace(region, "Waterloo", "WRPS"),
    statistics = str_replace(statistics, "[Cc]rime severity index", "csi"),
    region_level = "Municipal"
  ) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names()

# Clean CSI data for Canada and provines
csi <- csi_can_provs_ont_raw |> 
  filter(
    str_detect(statistics, "[Yy]outh", negate = TRUE),
    str_detect(geo, ", Ontario", negate = TRUE)
  ) |> 
  select(
    year = ref_date,
    region = geo,
    geo_uid,
    statistics,
    value
  ) |> 
  mutate(
    region_level = if_else(region == "Canada", "National", "Provincial/Territorial"),
    statistics = str_replace(statistics, "[Cc]rime severity index", "csi"),
    region = str_remove_all(region, " \\[.*\\]")
  ) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  bind_rows(csi_ont_forces_top_100) |> 
  relocate(year, region, region_level) |> 
  arrange(region_level, region, year) |> 
  rename(
    yoy_perc_change_csi = percent_change_in_csi,
    csi_violent = violent_csi,
    yoy_perc_change_csi_violent = percent_change_in_violent_csi,
    csi_nonviolent = non_violent_csi,
    yoy_perc_change_csi_nonviolent = percent_change_in_non_violent_csi,
    yoy_perc_change_weighted_clearance_rate = percent_change_in_weighted_clearance_rate,
    weighted_clearance_rate_violent = violent_weighted_clearance_rate,
    yoy_perc_change_weighted_clearance_rate_violent = percent_change_in_violent_weighted_clearance_rate,
    weighted_clearance_rate_nonviolent = non_violent_weighted_clearance_rate,
    yoy_perc_change_weighted_clearance_rate_nonviolent = percent_change_in_non_violent_weighted_clearance_rate,
  ) |> 
  mutate(region = as.factor(region)) |> 
  filter(year > 1999) |>    # to match start year for personnel data
  group_by(region) |> 
  mutate(
    base_csi = round(csi / first(csi) * 100, 1),
    base_clearance_rate = round(weighted_clearance_rate / first(weighted_clearance_rate) * 100, 1),
    base_csi_violent = round(csi_violent / first(csi_violent) * 100, 1),
    base_clearance_rate_violent = round(weighted_clearance_rate_violent / first(weighted_clearance_rate_violent) * 100, 1),
    base_csi_nonviolent = round(csi_nonviolent / first(csi_nonviolent) * 100, 1),
    base_clearance_rate_nonviolent = round(weighted_clearance_rate_nonviolent / first(weighted_clearance_rate_nonviolent) * 100, 1)
  ) |> 
  ungroup()

write_rds(csi, here("posts", "crime", "data", "clean", "crime_severity_index.rds"))

# prep financial data for big 12 police force regions/cities

# create vector of Big 12 municipalities using FIR names
big_12_regions <- c("Waterloo R", "Peel R", "London C", "Windsor C", "Toronto C", 
                    "Halton R", "Durham R", "York R", "Niagara R", "Hamilton C", 
                    "Hamilton - Wentworth R", "Sudbury R", "Greater Sudbury C",
                    "Ottawa - Carleton R", "Ottawa C")

# Get Canadian CPI data from Statistics Canada for inflation adjustment purposes
# Table 18-10-0005-01: Consumer Price Index, annual average, not seasonally adjusted
cpi_data <- get_cansim("18-10-0005-01") |>
  clean_names()

# Filter for All-items CPI (the main inflation measure) and keep only needed columns
inflation_factors <- cpi_data |>
  filter(
    geo == "Canada",
    products_and_product_groups == "All-items"
  ) |>
  select(ref_date, value) |>
  rename(
    year = ref_date,
    cpi = value
  ) |>
  # Convert year to numeric
  mutate(year = as.numeric(year)) |>
  # Calculate inflation adjustment factors (latest year = base year)
  mutate(
    cpi_latest = cpi[year == max(year)],  # Get latest CPI value
    inflation_factor = cpi_latest / cpi  # Factor to convert past $ to current $
  ) |>
  select(year, inflation_factor) |>
  arrange(year)

# Filter to Police data and transform
police_fir_clean <- fir_raw |> 
  filter(
    schedule_line_desc == "Police",
    datatype_desc != "percentage"
  ) |> 
  select(
    year = marsyear,
    region = municipality_desc,
    schedule = schedule_desc,
    item = schedule_column_desc,
    datatype = datatype_desc,
    amount
  ) |> 
  left_join(inflation_factors) |> 
  mutate(
    region = str_replace_all(region, c("Greater " = "", "Waterloo" = "WRPS", " .*$" = "")),
    schedule = str_to_title(schedule),
    amount = round(amount, 0),
    item = str_remove_all(item, "^\\d{4} | in \\d{4}$"),
    infl_adj_amount = if_else(
      datatype == "currency", 
      round(amount * inflation_factor, 0),
      amount
    )
  ) |> 
  select(-inflation_factor) |> 
  distinct() |> 
  group_by(year, region, schedule, item, datatype) |> 
  summarise(
    amount = sum(amount), 
    infl_adj_amount = sum(infl_adj_amount),
    .groups = "drop"
    )

write_rds(police_fir_clean, here("posts", "crime", "data", "clean", "police_fir.rds"))

# create tibble of select data for joining to personnel tibble
police_fir_subset <- police_fir_clean |> 
  filter(str_detect(item, "Before|Salaries|Positions|Seasonal")) |> 
  mutate(item = str_replace_all(
    item,
    c(
      "Salaries, Wages and Employee Benefits" = "salaries_benefits_infl_adj",
      "Total Expenses Before Adjustments" = "total_expenses_infl_adj",
      "Full-Time Funded Positions" = "ft_positions",
      "Part-Time Funded Positions" = "pt_positions",
      "Seasonal Employees" = "seasonal_employees"
    )
  )) |> 
  select(-schedule, -datatype, -amount) |> 
  pivot_wider(names_from = "item", values_from = "infl_adj_amount") |> 
  mutate(salaries_perc_total_exp = round(salaries_benefits_infl_adj / total_expenses_infl_adj * 100, 1))

# clean police personnel data

# Note: Per footnote 5 to Table 35-10-0177-01, the populations in this table
# are based on the respondent populations for the previous year. 
# Respondent populations for the current year are not yet available when 
# Police Administration Survey data are released. To get per-capita personnel
# data based on the current year's population, populations for each region and year
# will be calculated using the incidents table above (see note at beginning
# of that section), then joined with the personnel table, with all per-capita
# figures recalculated for each year using the population for that year

# create population table based on incidents data
population <- summary_totals |> 
  select(year, region, geo_uid, incidents_all, incidents_per_100k_all) |> 
  mutate(population = round(incidents_all / incidents_per_100k_all * 100000, 0)) |> 
  select(-contains("incident"))
  

personnel_munic_prepped <- personnel_munic_raw |> 
  filter(geo != "Ottawa, Ontario, municipal [716]") |> 
  select(
    year = ref_date,
    region = geo,
    geo_uid,
    statistics,
    value
  ) |> 
  mutate(
    region = str_remove_all(region, " Region.*|, Ontario.*| \\[.*|Greater "),
    region = str_replace(region, "Waterloo", "WRPS")
  ) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(
    officers = total_number_of_police_officers,                          
    male_officers = men_police_officers,                                     
    female_officers = women_police_officers,                                   
    civilians = total_number_of_civilian_and_other_personnel,            
    officers_per_100k = police_officers_per_100_000_population,                   
    auth_strength = authorized_police_officer_strength,                       
    auth_strength_per_100k =  authorized_police_officer_strength_per_100_000_population,
    officers_eligible_to_retire = police_officers_eligible_to_retire 
  ) |> 
  select(-crime_severity_index, -weighted_clearance_rate, -population)

# create vector of statistics matching those in municipal table
munic_stats <- c(
  "Total number of police officers",
  "Men police officers",
  "Women police officers",
  "Total number of civilian and other personnel",
  "Police officers per 100,000 population",
  "Authorized police officer strength",
  "Authorized police officer strength per 100,000 population"
)

personnel_ont_can_prepped <- personnel_ont_can_raw |> 
  filter(
    statistics %in% munic_stats,
    ref_date > 1999
    ) |> 
  select(
    year = ref_date,
    region = geo,
    geo_uid,
    statistics,
    val_norm
  ) |> 
  pivot_wider(names_from = statistics, values_from = val_norm) |> 
  clean_names() |> 
  rename(
    officers = total_number_of_police_officers,                          
    male_officers = men_police_officers,                                     
    female_officers = women_police_officers,                                   
    civilians = total_number_of_civilian_and_other_personnel,            
    officers_per_100k = police_officers_per_100_000_population,                   
    auth_strength = authorized_police_officer_strength,                       
    auth_strength_per_100k =  authorized_police_officer_strength_per_100_000_population
  ) 


# set Big 12 info
big_12_info <- population |> 
  filter(
    year == max(year),
    !(geo_uid %in% c(35, 11124))    # remove Ontario and canada
    ) |> 
  mutate(
    size_rank = min_rank(desc(population)),
    in_big_12 = size_rank < 13) |> 
  select(geo_uid, size_rank, in_big_12)

# add csi, incident, and financial details to personnel data and finish transforming
personnel_clean <- personnel_munic_prepped |> 
  left_join(big_12_info, join_by(geo_uid)) |> 
  bind_rows(personnel_ont_can_prepped) |> 
  left_join(police_fir_subset, join_by(year, region)) |> 
  left_join(population |> select(-region), join_by(year, geo_uid)) |> 
  mutate(
    officers_per_100k = round(officers / population * 100000, 0),
    auth_strength_per_100k = round(auth_strength / population * 100000, 0),
    salaries_per_capita = round(salaries_benefits_infl_adj / population, 0),
    total_exp_per_capita = round(total_expenses_infl_adj / population, 0),
    prop_male = round(male_officers / officers * 100, 1),
    prop_female = 100 - prop_male,
    total_personnel = officers + civilians,
    prop_officers = round(officers / total_personnel * 100, 1),
    prop_civilians = 100 - prop_officers,
    civilians_per_100k = round(civilians / population * 100000, 1),
    actual_vs_auth = round(officers / auth_strength * 100, 1),
    officer_civilian_ratio = round(officers / civilians, 3),
    region = relevel(factor(region), ref = "WRPS"),
    year_factor = relevel(factor(year), ref = "2000"),
    total_personnel_per_100k = officers_per_100k + civilians_per_100k
  ) |> 
  left_join(csi |> select(-starts_with("region")), join_by(year, geo_uid)) |> 
  left_join(summary_totals |> 
              select(
                -opp,
                -region,
                -contains("clear"),
                -contains("contribution")
              ),
            join_by(year, geo_uid)
            ) |> 
  rename_with(~ str_remove(.x, "_all")) |> 
  rename_with(~ str_replace(.x, "yoy_change", "yoy_perc_change")) |> 
  arrange(region, year) |> 
  group_by(region) |> 
  mutate(
    incidents_per_officer = round(incidents_per_100k / officers_per_100k, 1),
    incidents_per_officer_violent = round(incidents_per_100k_violent / officers_per_100k, 1),
    incidents_per_officer_nonviolent = round(incidents_per_100k_nonviolent / officers_per_100k, 1),
    base_population = round(population / first(population) * 100, 1),
    base_officers_per_100k = round(officers_per_100k / first(officers_per_100k) * 100, 1),
    base_civilians_per_100k = round(civilians_per_100k / first(civilians_per_100k) * 100, 1),
    base_auth_strength_per_100k = round(auth_strength_per_100k / first(auth_strength_per_100k) * 100, 1),
    base_officer_civilian_ratio = round(officer_civilian_ratio / first(officer_civilian_ratio) * 100, 1),
    base_salaries_per_capita = round(salaries_per_capita / first(salaries_per_capita) * 100, 1),
    base_total_exp_per_capita = round(total_exp_per_capita / first(total_exp_per_capita) * 100, 1),
    base_incidents_per_officer = round(incidents_per_officer / first(incidents_per_officer) * 100, 1),
    base_incidents_per_officer_violent = round(incidents_per_officer_violent / first(incidents_per_officer_violent) * 100, 1),
    base_incidents_per_officer_nonviolent = round(incidents_per_officer_nonviolent / first(incidents_per_officer_nonviolent) * 100, 1),
    yoy_perc_change_population = round((population - lag(population)) / lag(population) * 100, 1),
    yoy_perc_change_officers_per_100k = round((officers_per_100k - lag(officers_per_100k)) / lag(officers_per_100k) * 100, 1),
    yoy_perc_change_civilians_per_100k = round((civilians_per_100k - lag(civilians_per_100k)) / lag(civilians_per_100k) * 100, 1),
    yoy_perc_change_auth_strength_per_100k = round((auth_strength_per_100k - lag(auth_strength_per_100k)) / lag(auth_strength_per_100k) * 100, 1),
    yoy_perc_change_salaries_per_capita = round((salaries_per_capita - lag(salaries_per_capita)) / lag(salaries_per_capita) * 100, 1),
    yoy_perc_change_total_exp_per_capita = round((total_exp_per_capita - lag(total_exp_per_capita)) / lag(total_exp_per_capita) * 100, 1),
    yoy_perc_change_incidents_per_officer = round((incidents_per_officer - lag(incidents_per_officer)) / lag(incidents_per_officer) * 100, 1),
    yoy_perc_change_incidents_per_officer_violent = round((incidents_per_officer_violent - lag(incidents_per_officer_violent)) / lag(incidents_per_officer_violent) * 100, 1),
    yoy_perc_change_incidents_per_officer_nonviolent = round((incidents_per_officer_nonviolent - lag(incidents_per_officer_nonviolent)) / lag(incidents_per_officer_nonviolent) * 100, 1),
    lagged_total_personnel_per_100k = lag(total_personnel_per_100k, 1),
    lagged_prop_civilians = lag(prop_civilians, 1),
    lagged_prop_male = lag(prop_male, 1),
    lagged_csi = lag(csi, 1),
    lagged_csi_violent = lag(csi_violent, 1),
    lagged_csi_nonviolent = lag(csi_nonviolent, 1),
    lagged_clearance_rate = lag(weighted_clearance_rate, 1),
    lagged_clearance_rate_violent = lag(weighted_clearance_rate_violent, 1),
    lagged_clearance_rate_nonviolent = lag(weighted_clearance_rate_nonviolent, 1),
    lagged_incidents_per_100k = lag(incidents_per_100k, 1),
    lagged_incidents_per_100k_violent = lag(incidents_per_100k_violent, 1),
    lagged_incidents_per_100k_nonviolent = lag(incidents_per_100k_nonviolent, 1)
  ) |> 
  ungroup() |> 
  relocate(population, .after = geo_uid)

write_rds(personnel_clean, here("posts", "crime", "data", "clean", "personnel.rds"))

# create summary financial dataset for big 12
big_12_financial_summary <- police_fir_subset |> 
  left_join(population, join_by(year, region)) |> 
  mutate(
    salaries_per_capita = round(salaries_benefits_infl_adj / population, 0),
    total_exp_per_capita = round(total_expenses_infl_adj / population, 0)
  )

write_rds(big_12_financial_summary, here("posts", "crime", "data", "clean", "big_12_financial_summary.rds"))

# clean homicide victims data
homicide_victims_transform <- homicide_victims_raw |> 
  select(
    year = ref_date,
    region = geo,
    homicides,
    value
  ) |> 
  filter(
    str_detect(homicides, "change", negate = TRUE),
    !is.na(value)  # missing values are for CMAs before they reached CMA status
  ) |> 
  pivot_wider(names_from = homicides, values_from = value) |> 
  clean_names() |> 
  rename(
    victims = number_of_homicide_victims,
    victims_per_100k = homicide_rates_per_100_000_population
  ) |> 
  mutate(
    region = str_replace_all(region, " \\[.*| part", ""),
    region = str_replace(region, "Non-Census metropolitan area", "Total Non-CMA, Canada"),
    region = str_replace(region, "Kitchener-Cambridge-Waterloo", "KWC")
  ) |> 
  separate_wider_delim(
    region, 
    delim = ", ", 
    names = c("city", "province"), 
    too_few = "align_end",
    too_many = "merge",
    cols_remove = TRUE
  ) |> 
  mutate(
    city = if_else(is.na(city), "Total All Regions", city),
    est_popul = if_else(victims == 0, NA_integer_, victims * 100000 / victims_per_100k)
  )

# create tibbles of provincial CMA and non-CMA totals for each year
prov_cma_victims <- homicide_victims_transform |> 
  filter(
    province != "Canada",
    year > 1980  # no cma data before 1981
  ) |> 
  group_by(year, province) |> 
  mutate(prov_popul = max(est_popul, na.rm = TRUE)) |> 
  filter(city != "Total All Regions") |> 
  summarise(
    cma_victims = sum(victims, na.rm = TRUE),
    cma_popul = sum(est_popul, na.rm = TRUE),
    prov_popul = min(prov_popul),
    .groups = "drop") |> 
  mutate(
    cma_victims_per_100k = if_else(cma_victims == 0, 0, round(cma_victims / cma_popul * 100000, 2)),
    non_cma_popul = prov_popul - cma_popul
  ) |> 
  select(-prov_popul, -cma_popul)

prov_non_cma_victims <- homicide_victims_transform |> 
  filter(
    province != "Canada",
    city == "Total All Regions",
    year > 1980   # no cma data before 1981
  ) |> 
  rename(
    total_victims = victims,
    total_victims_per_100k = victims_per_100k
  ) |> 
  left_join(prov_cma_victims, join_by(year, province)) |> 
  mutate(
    city = "Total Non-CMA",
    noncma_victims = if_else(is.na(cma_victims), total_victims, total_victims - cma_victims),
    noncma_victims_per_100k = if_else(is.na(cma_victims), total_victims_per_100k, round(noncma_victims / non_cma_popul *100000, 2))
  ) |> 
  select(-(total_victims:non_cma_popul)) |> 
  rename(
    victims = noncma_victims,
    victims_per_100k = noncma_victims_per_100k
  )

prov_cma_victims <- prov_cma_victims |> 
  select(-non_cma_popul) |> 
  mutate(city = "Total CMA", .after = "year") |> 
  rename(
    victims = cma_victims,
    victims_per_100k = cma_victims_per_100k
  )

# create tibble of Canada CMA totals by year
canada_cma_victims <- homicide_victims_transform |> 
  filter(
    year > 1980,
    province == "Canada"
  ) |> 
  pivot_wider(names_from = city, values_from = victims:est_popul) |> 
  clean_names() |> 
  mutate(
    victims = victims_total_all_regions - victims_total_non_cma,
    victims_per_100k = round(victims / (est_popul_total_all_regions - est_popul_total_non_cma) * 100000, 2),
    city = "Total CMA"
  ) |> 
  select(year, city, province, victims, victims_per_100k)


homicide_victims_clean <- homicide_victims_transform |> 
  select(-est_popul) |> 
  bind_rows(prov_cma_victims, prov_non_cma_victims, canada_cma_victims) |> 
  mutate(across(c(city, province), as.factor)) |> 
  arrange(province, city, year) |> 
  rename(
    incidents = victims,
    incidents_per_100k = victims_per_100k
  )

write_rds(homicide_victims_clean, here("posts", "crime", "data", "clean", "homicide_victims.rds"))
  

# clean hate crimes data
hate_crimes_transform <- hate_crimes_raw |> 
  select(
    year = ref_date,
    region = geo,
    statistics,
    value
  ) |> 
  filter(
    !is.na(value),  # missing values are for CMAs before they reached CMA status
    region != "Canadian Forces Military Police"  # not comparable with other data
    ) |> 
  mutate(
    region = str_replace_all(region, " \\[.*| part", ""),
    region = str_replace(region, "Kitchener–Cambridge–Waterloo", "KWC"),
    region = str_replace(region, "Total Non-Census metropolitan area", "Total Non-CMA, Canada"),
    region = str_replace(region, "Total Census metropolitan area", "Total CMA, Canada"),
    region = str_replace(region, "Total police-reported hate crime", "Total All Regions, Canada")
  ) |>
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(
    crimes = number_of_hate_crime_incidents,
    crimes_per_100k = rate_per_100_000_population
  ) |> 
  separate_wider_delim(
    region, 
    delim = ", ", 
    names = c("city", "province"), 
    too_few = "align_end",
    too_many = "merge",
    cols_remove = TRUE
  ) |> 
  mutate(
    city = if_else(is.na(city), "Total All Regions", city),
    est_popul = if_else(crimes == 0, NA_integer_, crimes * 100000 / crimes_per_100k)
  )

# create tibbles of provincial CMA and non-CMA totals for each year
prov_cma_crimes <- hate_crimes_transform |> 
  filter(province != "Canada") |> 
  group_by(year, province) |> 
  mutate(prov_popul = max(est_popul, na.rm = TRUE)) |> 
  filter(city != "Total All Regions") |> 
  summarise(
    cma_crimes = sum(crimes, na.rm = TRUE),
    cma_popul = sum(est_popul, na.rm = TRUE),
    prov_popul = min(prov_popul),
    .groups = "drop") |> 
  mutate(
    cma_crimes_per_100k = if_else(cma_crimes == 0, 0, round(cma_crimes / cma_popul * 100000, 2)),
    non_cma_popul = prov_popul - cma_popul
  ) |> 
  select(-prov_popul, -cma_popul)

prov_non_cma_crimes <- hate_crimes_transform |> 
  filter(
    province != "Canada",
    city == "Total All Regions"
  ) |> 
  rename(
    total_crimes = crimes,
    total_crimes_per_100k = crimes_per_100k
  ) |> 
  left_join(prov_cma_crimes, join_by(year, province)) |> 
  mutate(
    city = "Total Non-CMA",
    noncma_crimes = if_else(is.na(cma_crimes), total_crimes, total_crimes - cma_crimes),
    noncma_crimes_per_100k = if_else(is.na(cma_crimes), total_crimes_per_100k, round(noncma_crimes / non_cma_popul *100000, 2))
  ) |> 
  select(-(total_crimes:non_cma_popul)) |> 
  rename(
    crimes = noncma_crimes,
    crimes_per_100k = noncma_crimes_per_100k
  )

prov_cma_crimes <- prov_cma_crimes |> 
  select(-non_cma_popul) |> 
  mutate(city = "Total CMA", .after = "year") |> 
  rename(
    crimes = cma_crimes,
    crimes_per_100k = cma_crimes_per_100k
  )

hate_crimes_clean <- hate_crimes_transform |> 
  select(-est_popul) |> 
  bind_rows(prov_cma_crimes, prov_non_cma_crimes) |> 
  mutate(across(c(city, province), as.factor)) |>
  arrange(province, city, year) |> 
  rename(
    incidents = crimes,
    incidents_per_100k = crimes_per_100k
  )

write_rds(hate_crimes_clean, here("posts", "crime", "data", "clean", "hate_crimes.rds"))

# clean cyber crimes data
cyber_crimes_transform <- cyber_crimes_raw |> 
  select(
    year = ref_date,
    region = geo,
    statistics,
    value
  ) |> 
  filter(
    !is.na(value),  # missing values are for CMAs before they reached CMA status
    region != "Canadian Forces Military Police"  # not comparable with other data
  ) |> 
  mutate(
    region = str_replace_all(region, " \\[.*| part", ""),
    region = str_replace(region, "Kitchener–Cambridge–Waterloo", "KWC"),
    region = str_replace(region, "Total Non-Census metropolitan area", "Total Non-CMA, Canada"),
    region = str_replace(region, "Total Census metropolitan area", "Total CMA, Canada"),
    region = str_replace(region, "Total police-reported cyber crime", "Total All Regions, Canada")
  ) |>
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(
    crimes = number_of_cybercrime_incidents,
    crimes_per_100k = rate_per_100_000_population
  ) |> 
  separate_wider_delim(
    region, 
    delim = ", ", 
    names = c("city", "province"), 
    too_few = "align_end",
    too_many = "merge",
    cols_remove = TRUE
  ) |> 
  mutate(
    city = if_else(is.na(city), "Total All Regions", city),
    est_popul = if_else(crimes == 0, NA_integer_, crimes * 100000 / crimes_per_100k)
  )

# create tibbles of provincial CMA and non-CMA totals for each year
prov_cma_cyber_crimes <- cyber_crimes_transform |> 
  filter(province != "Canada") |> 
  group_by(year, province) |> 
  mutate(prov_popul = max(est_popul, na.rm = TRUE)) |> 
  filter(city != "Total All Regions") |> 
  summarise(
    cma_crimes = sum(crimes, na.rm = TRUE),
    cma_popul = sum(est_popul, na.rm = TRUE),
    prov_popul = min(prov_popul),
    .groups = "drop") |> 
  mutate(
    cma_crimes_per_100k = if_else(cma_crimes == 0, 0, round(cma_crimes / cma_popul * 100000, 2)),
    non_cma_popul = prov_popul - cma_popul
  ) |> 
  select(-prov_popul, -cma_popul)

prov_non_cma_cyber_crimes <- cyber_crimes_transform |> 
  filter(
    province != "Canada",
    city == "Total All Regions"
  ) |> 
  rename(
    total_crimes = crimes,
    total_crimes_per_100k = crimes_per_100k
  ) |> 
  left_join(prov_cma_cyber_crimes, join_by(year, province)) |> 
  mutate(
    city = "Total Non-CMA",
    noncma_crimes = if_else(is.na(cma_crimes), total_crimes, total_crimes - cma_crimes),
    noncma_crimes_per_100k = if_else(is.na(cma_crimes), total_crimes_per_100k, round(noncma_crimes / non_cma_popul *100000, 2))
  ) |> 
  select(-(total_crimes:non_cma_popul)) |> 
  rename(
    crimes = noncma_crimes,
    crimes_per_100k = noncma_crimes_per_100k
  )

prov_cma_cyber_crimes <- prov_cma_cyber_crimes |> 
  select(-non_cma_popul) |> 
  mutate(city = "Total CMA", .after = "year") |> 
  rename(
    crimes = cma_crimes,
    crimes_per_100k = cma_crimes_per_100k
  )

cyber_crimes_clean <- cyber_crimes_transform |> 
  select(-est_popul) |> 
  bind_rows(prov_cma_cyber_crimes, prov_non_cma_cyber_crimes) |> 
  mutate(across(c(city, province), as.factor)) |>
  arrange(province, city, year) |> 
  rename(
    incidents = crimes,
    incidents_per_100k = crimes_per_100k
  )

write_rds(cyber_crimes_clean, here("posts", "crime", "data", "clean", "cyber_crimes.rds"))

# clean victims data

# violent crime victims
# Create tibble with category and violation columns
crime_categories <- tibble(
  category = c(
    "Homicide, other violations causing death and attempted murder",
    "Homicide, other violations causing death and attempted murder", 
    "Sexual assaults",
    "Sexual assaults",
    "Sexual assaults",
    "Assaults",
    "Assaults", 
    "Assaults",
    "Assaults",
    "Criminal Code traffic violations causing death or bodily harm",
    "Criminal Code traffic violations causing death or bodily harm",
    "Other violent violations",
    "Other violent violations",
    "Other violent violations", 
    "Other violent violations",
    "Other violent violations",
    "Other violent violations",
    "Other violent violations",
    "Sexual violations against children"
  ),
  violation = c(
    "Homicide and other offences causing death",
    "Attempted murder",
    "Sexual assault, level 3, aggravated",
    "Sexual assault, level 2, weapon or bodily harm", 
    "Sexual assault, level 1",
    "Assault, level 3, aggravated",
    "Assault, level 2, weapon or bodily harm",
    "Assault, level 1", 
    "Other assaults",
    "Impaired driving and other Criminal Code traffic violations causing death",
    "Impaired driving and other Criminal Code traffic violations causing bodily harm",
    "Robbery",
    "Criminal harassment",
    "Indecent or harassing communications",
    "Uttering threats", 
    "Kidnapping, forcible confinement, abduction or hostage taking",
    "Trafficking in persons or prostitution",
    "Other violations",
    "Sexual violations against children"
  )
)

violent_victims_clean <- violent_crime_victims_raw |> 
  filter(str_detect(geo, "Ontario|Canada")) |> 
  select(
    year = ref_date,
    region = geo,
    violation = violations,
    age = age_of_victim,
    gender = gender_of_victim,
    statistics,
    value
  ) |> 
  mutate(
    region = str_remove_all(region, "-Gatineau|–Gatineau|,.*"),
    region = str_replace(region, "Kitchener.*", "Waterloo Region"),
    demographic = if_else(is.na(age), gender, age),
    demo_category = if_else(is.na(age), "Gender", "Age")
  ) |> 
  select(-age, -gender) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(num_victims = number_of_victims, victims_per_100k = rate_per_100_000_population) |> 
  filter(
    str_detect(demographic, "Total|12 to 17 years|17 years and younger|18 years and older|25 years and older|65 years and older", negate = TRUE),
    !(is.na(num_victims) & is.na(victims_per_100k))
  ) |>     
  inner_join(crime_categories, join_by(violation)) |> 
  relocate(category, violation, demo_category, .after = region) |> 
  group_by(year, region, violation, demo_category) |> 
  mutate(perc_victims_for_violation = round(num_victims / sum(num_victims) * 100, 1)) |> 
  ungroup() |> 
  group_by(region, violation, demo_category, demographic) |> 
  arrange(year) |> 
  mutate(
    base_rate_victims_per_100k = round(victims_per_100k / first(victims_per_100k, na_rm = TRUE) * 100, 1),
    perc_change_victims_per_100k = round(victims_per_100k / lag(victims_per_100k) * 100, 1)
  ) |> 
  ungroup()

write_rds(violent_victims_clean, here("posts", "crime", "data", "clean", "violent_victims.rds"))

# family and ipv victims
family_ipv_victims_clean <- family_ipv_victims_raw |> 
  filter(str_detect(geo, "Ontario|Canada")) |> 
  select(
    year = ref_date,
    region = geo,
    gender = gender_of_victim,
    age = age_of_victim,
    gender = gender_of_victim,
    family_relationship = relationship_of_accused_to_victim_family_non_family,
    ipv_relationship = relationship_of_accused_to_victim_intimate_partner_non_intimate_partner,
    statistics,
    value
  ) |> 
  mutate(
    region = str_remove_all(region, "-Gatineau|–Gatineau|,.*"),
    region = str_replace(region, "Kitchener.*", "Waterloo Region"),
    relationship = if_else(is.na(family_relationship), ipv_relationship, family_relationship),
    rel_category = if_else(is.na(family_relationship), "IPV", "Family"),
    relationship = str_remove(relationship, " .*relationship"),
    gender = str_replace_all(gender, c("Gender of victim unknown" = "Unknown", " victims" = "")),
    age = str_replace(age, "Age of victim unknown", "Unknown")
  ) |> 
  select(-family_relationship, -ipv_relationship) |> 
  pivot_wider(names_from = statistics, values_from = value) |> 
  clean_names() |> 
  rename(num_victims = number_of_victims, victims_per_100k = rate_per_100_000_population) |> 
  filter(
    # 75-79 and 80-84 missing for 2023, so kept 75-84 instead for all years
    str_detect(age, "Total|11 years and younger|12 to 17 years|17 years and younger|18 years and older|65 years and older|75 to 79 years|80 to 84 years", negate = TRUE),
    str_detect(gender, "Male|Female|Unknown"),
    str_detect(relationship, "Total", negate = TRUE),
    !(is.na(num_victims) & is.na(victims_per_100k))
  ) |>     
  relocate(rel_category, relationship, .after = age) |> 
  group_by(year, region, rel_category) |> 
  mutate(perc_of_victims_rel_category = round(num_victims / sum(num_victims) * 100, 2)) |> 
  ungroup() 

write_rds(family_ipv_victims_clean, here("posts", "crime", "data", "clean", "family_ipv_victims.rds"))
  
  
# Clean latest occurrence data

# Define the code descriptions for later use
priority_descriptions <- c(
  "0" = "Officer Needs Assistance",
  "1" = "Immediate",
  "2" = "Urgent", 
  "3" = "Routine",
  "4" = "Delay-When Zone Officer Becomes Available",
  "5" = "Officer not required to attend",
  "6" = "Collision Reporting Centre",
  "7" = "Officer Initiated",
  "8" = "Proactive",
  "9" = "Administrative"
)

disposition_codes <-  c(
  "CANCELEV" = "Cancelled", 
  "DUPNCAN" = "Duplicate", 
  "DPR" = "Handled by Phone", 
  "UNF" = "Unfounded",
  "ADMIN" = "Administrative"
)

# Perform the transformations
occurrence_transform <- occurrence_new |> 
  # remove fully duplicate records
  # partial dupes removed manually from raw Excel files (see Duplicate Occurrence Numbers.xlsx file in data folder)
  distinct() |> 
  mutate(
    # replace "NULL" with NA in all columns
    across(everything(), ~na_if(., "NULL")),
    # convert service time columns to numeric
    across(starts_with("call"), as.numeric),
    total_unit_service_time = as.numeric(total_unit_service_time),
    # Convert all date_and_time columns to datetime format
    reported_date_and_time = convert_dates(reported_date_and_time),
    dispatch_date_and_time = convert_dates(dispatch_date_and_time),
    arrival_date_and_time = convert_dates(arrival_date_and_time),
    cleared_date_and_time = convert_dates(cleared_date_and_time),
    # Create derived date/time columns for reported_date_and_time
    reported_date = date(reported_date_and_time),
    reported_year = year(reported_date_and_time),
    reported_month = month(reported_date_and_time, label = TRUE),
    reported_day = day(reported_date_and_time),
    reported_day_of_week = wday(reported_date_and_time, label = TRUE),
    reported_weekend = wday(reported_date_and_time) %in% c(1, 7), # 1=Sunday, 7=Saturday
    reported_hour = hour(reported_date_and_time),
    # remove call code from description columns
    initial_call_type_description = str_remove(initial_call_type_description, "^\\d{4}-") |> 
      str_trim(),
    final_call_type_description = str_remove(final_call_type_description, "^\\d{4}-") |> 
      str_trim(),
    # add priority descriptions
    initial_priority_desc = recode(initial_priority, !!!priority_descriptions, .default = NA_character_),
    final_priority_desc = recode(final_priority, !!!priority_descriptions, .default = NA_character_),
    # replace disposition codes with description
    disposition = str_replace_all(disposition, disposition_codes),
    disposition = str_replace_all(disposition, regex("(?i)rtf"), "Report to Follow"),
    disposition = str_replace_all(disposition, regex("(?i)nr"), "No Report")
  ) |> 
  select(
    # drop municipality column containing many NAs - to be replaced by geocoded column below
    -municipality
  )

# add geocoded city, municipality, and neighbourhood columns
# Step 1: Extract X and Y coordinates from the geographic_location column
occurrence_geocode <- occurrence_transform |>
  mutate(
    # Clean up the geographic_location column by removing any spaces around the comma
    geographic_location_clean = str_replace(geographic_location, " *, *", ","),
    
    # Extract X coordinate (before the comma)
    utm_x = as.numeric(str_extract(geographic_location_clean, "^[^,]+")),
    
    # Extract Y coordinate (after the comma)
    utm_y = as.numeric(str_extract(geographic_location_clean, "[^,]+$"))
  )

# Create an sf object with UTM coordinates
utm_points <- occurrence_geocode |>
  filter(!is.na(utm_x) & !is.na(utm_y)) |>  # Remove rows with missing coordinates
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 26917)  # NAD83 UTM Zone 17N EPSG code is 26917

# Transform to WGS84 (latitude/longitude)
wgs84_points <- st_transform(utm_points, 4326)  # WGS84 EPSG code is 4326

# Extract the transformed coordinates
transformed_coords <- st_coordinates(wgs84_points)

# Add the latitude and longitude back to the original data frame
occurrence_geocode <- occurrence_geocode |>
  mutate(
    longitude = NA_real_,
    latitude = NA_real_
  )

# Update only the rows that had valid coordinates
valid_indices <- which(!is.na(occurrence_geocode$utm_x) & !is.na(occurrence_geocode$utm_y))
occurrence_geocode$longitude[valid_indices] <- transformed_coords[, "X"]
occurrence_geocode$latitude[valid_indices] <- transformed_coords[, "Y"]

# Clean up temporary columns
occurrence_geocode <- occurrence_geocode |>
  select(-geographic_location_clean)

# Step 2: Convert the coordinates to sf points (only for rows with valid coordinates)
points_sf <- occurrence_geocode |>
  filter(!is.na(utm_x) & !is.na(utm_y)) |>
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 26917)  # NAD83 UTM Zone 17N

# Step 3: Get city and neighbourhood boundaries for Waterloo Region

# Get boundaries shapefiles
waterloo_region_cities <- get_waterloo_cities()
waterloo_region_hoods <- get_waterloo_neighbourhoods()

# Make sure the boundaries are in the same projection as our points
waterloo_region_cities <- st_transform(waterloo_region_cities, 26917)
waterloo_region_hoods <- st_transform(waterloo_region_hoods, 26917)

# Step 4: Perform spatial joins to determine which city/hood each point falls within
points_with_city <- st_join(points_sf, 
                            waterloo_region_cities |> 
                              select(city, municipality),  # Select city and munic columsn
                            join = st_within)

points_with_hood <- st_join(points_sf, 
                            waterloo_region_hoods |> 
                              select(neighbourhood),  # Select neighbourhood column
                            join = st_within)

# Step 5: Extract city/hood information and join back to original data
city_lookup <- points_with_city |>
  st_drop_geometry() |>  # Remove geometry to convert back to regular data frame
  select(occurrence_number, city, municipality)

hood_lookup <- points_with_hood |>
  st_drop_geometry() |>  # Remove geometry to convert back to regular data frame
  select(occurrence_number, neighbourhood)

# Step 6: Join the geocode information back to the original data frame
wat_region_occurrences <- occurrence_geocode |>
  left_join(city_lookup, join_by(occurrence_number)) |>
  left_join(hood_lookup, join_by(occurrence_number)) 

# Define columns to convert to factors
cols_to_factor <- c(
  "initial_priority_desc",
  "final_priority_desc",
  "disposition",
  "city",
  "municipality",
  "initial_priority",
  "final_priority",
  "final_call_type_description",
  "initial_call_type_description",
  "neighbourhood"
)

# convert specified columns to factors
wat_region_occurrences <- wat_region_occurrences |> 
  across(all_of(cols_to_factor), as_factor)

# Save clean file
saveRDS(wat_region_occurrences, here("posts", "crime", "data", "clean", "wat_region_occurrences.rds"))



