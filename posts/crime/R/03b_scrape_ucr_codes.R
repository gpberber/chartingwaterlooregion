# Build a lookup of Canadian UCR violation codes from the two CCJCSS reference
# PDFs in posts/crime/data/ucr_codes/:
#   Current_UCR_Codes.pdf  -> Violation Reference Chart (category, subcategory,
#                             violation, maximum penalty)
#   Expired_UCR_Codes.pdf  -> Expired Violation Codes (violation, expiry date)
# The two are combined into a single tibble, `ucr_categories`.

here::i_am("posts/crime/R/03b_scrape_ucr_codes.R")

source(here::here("posts", "crime", "R", "01_setup.R"))

ucr_dir <- here("posts", "crime", "data", "ucr_codes")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# The text layout of both PDFs is column-based and stable:
#   * the violation code sits in the left-most column (always ends before col 30)
#   * the maximum penalty / expiry date is right-aligned well past col 100
#   * wrapped descriptions get their own text lines, and in a multi-line cell the
#     code is vertically centred, so it lands on a line of its own
code_col_end <- 30
right_col_start <- 100

# Read a PDF into one row per non-blank line, tagged with its page and the
# blank-line-delimited block it belongs to. Blocks never straddle a page break.
read_pdf_lines <- function(file, drop_regex) {
  pdf_text(file) |>
    str_split("\n") |>
    imap(\(lines, page) tibble(page = page, line = lines)) |>
    list_rbind() |>
    filter(!str_detect(line, drop_regex)) |>
    mutate(blank = !str_detect(line, "\\S")) |>
    mutate(block = cumsum(blank & !lag(blank, default = TRUE)), .by = page) |>
    filter(!blank) |>
    mutate(block_id = consecutive_id(page, block)) |>
    select(page, block_id, line)
}

# Split each line into its three columns.
split_columns <- function(lines) {
  lines |>
    mutate(
      code_end = str_locate(line, "^\\s*\\d{4}(-\\d{4})?(?=\\s|$)")[, "end"],
      code_end = if_else(is.na(code_end) | code_end > code_col_end, 0L, code_end),
      right_at = str_locate(line, "\\s(\\d{4}-\\d{2}-\\d{2}|\\d+(\\.\\d+)?|N/A)\\s*$")[, "start"],
      right_at = if_else(is.na(right_at) | right_at < right_col_start,
                         nchar(line) + 1L, right_at),
      ucr_code = na_if(str_trim(str_sub(line, 1, code_end)), ""),
      right_col = na_if(str_trim(str_sub(line, right_at)), ""),
      text = str_squish(str_sub(line, code_end + 1, right_at - 1))
    ) |>
    select(page, block_id, ucr_code, text, right_col)
}

# Join wrapped description lines back together, keeping dates broken across a
# line ("(effective 2008-05-" + "01)") intact.
join_wrapped <- function(parts) {
  parts <- parts[parts != ""]
  if (length(parts) == 0) return(NA_character_)
  sep <- if_else(str_detect(head(parts, -1), "-$") & str_detect(tail(parts, -1), "^\\d"), "", " ")
  str_c(parts[1], str_c(sep, tail(parts, -1), collapse = ""))
}

# Group lines into table rows.
#
# A row is anchored by its code, but the description may begin on the line above
# (in a multi-line cell the code is centred, so it sits between description
# lines) and continue on the lines below. A run of code-less lines therefore
# opens the next row when the line following the run holds a code and nothing
# else, and it opens on the run's *last* line, the earlier ones still belonging
# to the row above. Rows are also opened at every block boundary within a page
# and after every heading; a block boundary at a page break is only an artefact
# of the page footer and does not close a row.
assemble_rows <- function(lines) {
  lines |>
    mutate(
      is_code = !is.na(ucr_code),
      code_only = is_code & text == "",
      block_break = block_id != lag(block_id, default = first(block_id)) &
        (page == lag(page, default = first(page)) | lag(is_heading, default = FALSE))
    ) |>
    filter(!is_heading) |>
    mutate(next_code_only = lead(code_only, default = FALSE), run = consecutive_id(is_code)) |>
    mutate(
      run_opens = !is_code & (first(block_break) | last(next_code_only)),
      opens_here = run_opens & row_number() == if_else(first(block_break), 1L, n()),
      .by = run
    ) |>
    mutate(
      row_id = cumsum(if_else(
        is_code,
        block_break | !(run != lag(run, default = 0L) & lag(run_opens, default = FALSE)),
        opens_here
      ))
    ) |>
    summarise(
      ucr_code = first(ucr_code[!is.na(ucr_code)], default = NA_character_),
      category = first(category),
      subcategory = first(subcategory),
      violation = join_wrapped(text),
      right_col = first(right_col[!is.na(right_col)], default = NA_character_),
      .by = row_id
    ) |>
    select(-row_id)
}

# "(effective yyyy-mm-dd)" and the bare "(yyyy-mm-dd)" used for code 3700; every
# other parenthetical is left in place.
effective_date_re <- regex("\\s*\\((?:effective\\s*)?(\\d{4}-\\d{2}-\\d{2})\\)", ignore_case = TRUE)

# ---------------------------------------------------------------------------
# Current codes: the Violation Reference Chart
# ---------------------------------------------------------------------------

current_junk <- str_c(
  c("^SECTION \\d+:",
    "^\\s*Violation Reference Chart\\s*$",
    "^\\s*Violation Code\\s+Description\\s*$",
    "^\\s*(Maximum|Penalty1)\\s*$",
    "^Canadian Centre for Justice",
    "^1\\s*$",
    "^\\s?The maximum penalty indicted",
    "^attached to the Criminal Code violation"),
  collapse = "|"
)

series_re <- "\\s*\\(\\d{4}(-\\d{4})? Series\\)$"

current_lines <- read_pdf_lines(here(ucr_dir, "Current_UCR_Codes.pdf"), current_junk) |>
  # the chart PDF reprints the expired table at the end; that is read separately
  filter(row_number() < which(str_detect(line, "Expired Violation Codes"))[1]) |>
  split_columns()

# A block with no violation code in it is a heading: category headings name their
# code series, everything else is a subcategory. A new category clears the running
# subcategory; where codes sit directly under a category heading with no subheading
# of their own, the category stands in as the subcategory.
current_headings <- current_lines |>
  summarise(heading = join_wrapped(text), is_heading = !any(!is.na(ucr_code)), .by = block_id) |>
  mutate(
    is_category = is_heading & str_detect(heading, series_re),
    category = if_else(is_category, str_remove(heading, series_re), NA),
    subcategory = case_when(is_category ~ "", is_heading ~ heading, .default = NA)
  ) |>
  fill(category, subcategory) |>
  #mutate(subcategory = if_else(subcategory == "", category, subcategory)) |>
  select(block_id, is_heading, category, subcategory)

current_ucr_codes <- current_lines |>
  left_join(current_headings, by = "block_id") |>
  assemble_rows() |>
  mutate(
    violation_master = str_squish(str_remove_all(violation, effective_date_re)),
    effective_date = ymd(str_match(violation, effective_date_re)[, 2]),
    maximum_penalty_in_years = parse_number(na_if(right_col, "N/A")),
    .keep = "unused"
  ) |> 
  mutate(
    subcategory = case_when(
      # replace missing subcategories with an appropriate name based on violation or category
      ucr_code %in% c("3140", "3141") ~ "Offences in Relation to Sexual Services", 
      ucr_code == "2110" ~ "Arson",
      subcategory == "" & str_ends(category, "Violations") ~ category,
      .default = subcategory
    ),
  )

# ---------------------------------------------------------------------------
# Expired codes
# ---------------------------------------------------------------------------

expired_junk <- str_c(
  c("^\\s*Expired Violation Codes\\s*$",
    "^\\s*Note: Although these violation codes",
    "^\\s*were previously in effect",
    "^\\s*reported to police but which occurred",
    "^\\s*Expiry Date\\s*$",
    "^Canadian Centre for Justice"),
  collapse = "|"
)

# The expired table carries no headings of its own. Category follows from the
# code series used in the chart above; the 5000 series predates the chart, which
# no longer lists Food and Drugs Act offences separately.
expired_category <- function(ucr_code) {
  case_when(
    str_starts(ucr_code, "49") ~ "Cannabis Act",
    str_starts(ucr_code, "1")  ~ "Crimes Against the Person",
    str_starts(ucr_code, "2")  ~ "Crimes Against Property",
    str_starts(ucr_code, "3")  ~ "Other Criminal Code Violations",
    str_starts(ucr_code, "4")  ~ "Controlled Drugs and Substances Act",
    str_starts(ucr_code, "5")  ~ "Food and Drugs Act",
    str_starts(ucr_code, "6")  ~ "Other Federal Statute Violations",
    str_starts(ucr_code, "7")  ~ "Provincial Statute Violations",
    str_starts(ucr_code, "9")  ~ "Traffic Violations"
  )
}

# Subcategory matched by hand to the nearest codes/violations in the chart above.
# NA where the chart lists the equivalent codes directly under a category
# heading (3110-3130 alongside 3140/3141), or has no comparable violation at all
# (the 5000 and 6000 series, provincial traffic, "other" catch-alls).
expired_subcategory <- tribble(
  ~ucr_code,   ~subcategory,
  "1300",      "Sexual Violations",
  "1340",      "Sexual Violations",
  "1375",      "Sexual Violations",
  "1510",      "Violations Resulting in the Deprivation of Freedom",
  "2131",      "Theft",
  "2141",      "Theft",
  "2150",      "Possession / Trafficking Stolen Goods",
  "2172",      "Mischief",
  "2174",      "Mischief",
  "3110",      "Offences in Relation to Sexual Services",
  "3115",      "Offences in Relation to Sexual Services",
  "3120",      "Offences in Relation to Sexual Services",
  "3125",      "Offences in Relation to Sexual Services",
  "3130",      "Offences in Relation to Sexual Services",
  "3240",      "Gaming and Betting",
  "3320",      "Offensive Weapons",
  "3330",      "Offensive Weapons",
  "3340",      "Offensive Weapons",
  "3350",      "Offensive Weapons",
  "3360",      "Offensive Weapons",
  "3385",      "Offensive Weapons",
  "3457",      "Other Criminal Code",
  "3461",      "Other Criminal Code",
  "3530",      "Other Criminal Code",
  "3716",      "Other Criminal Code",
  "3760",      "Other Criminal Code",
  "3770",      "Other Criminal Code",
  "3791",      "Other Criminal Code",
  "3810",      "Other Criminal Code",
  "4140",      "Possession",
  "4240",      "Trafficking",
  "4340",      "Importation and Exportation",
  "4440",      "Production",
  "4825",      "Other Drug Violations",
  "5120-5220", NA,
  "6450",      NA,
  "9131",      "Flight from Peace Officer",
  "9132",      "Flight from Peace Officer",
  "9240",      "Failure or Refusal to Comply with Demand",
  "9245",      "Failure or Refusal to Comply with Demand",
  "9250",      "Failure or Refusal to Comply with Demand",
  "9255",      "Failure or Refusal to Comply with Demand",
  "9310",      "Failure to Stop after Accident",
  "9330",      "Other Criminal Code Traffic",
  "9410",      "Dangerous Operation",
  "9420",      "Dangerous Operation",
  "9430",      "Dangerous Operation",
  "9440",      "Dangerous Operation",
  "9450",      "Dangerous Operation",
  "9510",      "Failure to Stop after Accident",
  "9520",      "Dangerous Operation",
  "9530",      "Operation while Prohibited"
)

expired_ucr_codes <- read_pdf_lines(here(ucr_dir, "Expired_UCR_Codes.pdf"), expired_junk) |>
  split_columns() |>
  mutate(is_heading = FALSE, category = NA_character_, subcategory = NA_character_) |>
  assemble_rows() |>
  mutate(
    category = expired_category(ucr_code),
    violation_master = str_squish(str_remove_all(violation, effective_date_re)),
    violation_master = str_remove(violation_master, "^.*?(?=Cannabis)"),
    effective_date = ymd(str_match(violation, effective_date_re)[, 2]),
    expiry_date = ymd(right_col)
  ) |>
  select(ucr_code, category, violation_master, effective_date, expiry_date) |>
  left_join(expired_subcategory, by = "ucr_code") |>
  relocate(subcategory, .after = category) |> 
  filter(!ucr_code %in% c("5120-5220", "6450"))     # 6450 YCJ Act updated, not expired, still exists post-2003; 5120-5220 predate the data

# ---------------------------------------------------------------------------
# Combined lookup
# ---------------------------------------------------------------------------

ucr_categories <- bind_rows(current_ucr_codes, expired_ucr_codes) |>
  relocate(expiry_date, .after = maximum_penalty_in_years) |> 
  arrange(ucr_code)

write_csv(ucr_categories, here("posts", "crime", "data", "ucr_categories.csv"))
