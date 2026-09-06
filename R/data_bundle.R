# data_bundle.R
# ---------------------------------------------------------------------------
# Builds the downloadable data bundle for a post: a zip holding every table the
# post uses (CSV and Parquet, plus one Excel workbook when the tables are small
# enough), a data dictionary, and a README with sources and licences. The zip
# is attached to the post's GitHub Release (tag data-<slug>-v<n>) so it has a
# stable URL that the post links to.
#
# Two small CSV files in posts/<slug>/data/ drive everything:
#
#   tables.csv      one row per table the post uses
#                   columns: table, file, description, source, licence, notes
#                   `file` is a path from the project root, so a table can live
#                   in the post's own data/ or in datasets/<name>/data/
#   dictionary.csv  one row per column of every table
#                   columns: table, column, description, units
#
# Column types and example values are read from the data itself, so the
# dictionary only needs the parts a machine cannot know: meaning and units.
#
# Usage (the /publish skill runs this):
#   source(here::here("R", "data_bundle.R"))
#   cwr_dictionary_check("kitchener-phone-queues")      # every column documented?
#   cwr_data_bundle("kitchener-phone-queues", version = 1)   # build zip and upload
#
# In a post's Reproducibility box:
#   cwr_bundle_url("kitchener-phone-queues", 1)         # link to the zip
#   cwr_dictionary_table("kitchener-phone-queues")      # dictionary as a gt table
# ---------------------------------------------------------------------------

library(here)
library(tidyverse)
library(arrow)

source(here("R", "data_helpers.R"))   # cwr_repo, cwr_data_tag(), cwr_release_upload()

# ---- Read the two description files ----------------------------------------
cwr_tables <- function(slug) {
  path <- here("posts", slug, "data", "tables.csv")
  if (!file.exists(path)) stop("No tables.csv in posts/", slug, "/data/. See R/data_bundle.R for the format.")
  read_csv(path, col_types = cols(.default = col_character())) |>
    mutate(across(everything(), \(x) replace_na(x, "")))
}

cwr_dictionary_entries <- function(slug) {
  path <- here("posts", slug, "data", "dictionary.csv")
  if (!file.exists(path)) stop("No dictionary.csv in posts/", slug, "/data/. See R/data_bundle.R for the format.")
  read_csv(path, col_types = cols(.default = col_character())) |>
    mutate(across(everything(), \(x) replace_na(x, "")))
}

# Read a table by its path from the project root, whatever its format
cwr_read_table <- function(file) {
  path <- here(file)
  if (!file.exists(path)) stop("Table file not found: ", file)
  ext <- str_to_lower(tools::file_ext(path))
  switch(
    ext,
    csv = read_csv(path, show_col_types = FALSE),
    parquet = read_parquet(path),
    rds = read_rds(path),
    stop("Unsupported table format: .", ext, " (use csv, parquet, or rds)")
  )
}

# ---- Describe columns from the data itself -----------------------------------
cwr_column_type <- function(x) {
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXct")) return("date-time")
  if (is.factor(x)) return("category")
  if (is.logical(x)) return("true/false")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("number")
  "text"
}

cwr_column_example <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("(all missing)")
  if (inherits(x, "Date") || inherits(x, "POSIXct") || is.numeric(x)) {
    return(paste(format(min(x)), "to", format(max(x))))
  }
  if (is.logical(x)) return("TRUE / FALSE")
  values <- unique(as.character(x))
  shown <- paste(head(values, 3), collapse = "; ")
  if (length(values) > 3) shown <- paste0(shown, "; ... (", length(values), " distinct values)")
  shown
}

# Columns of every table in tables.csv, with type and example generated from the data
cwr_dictionary_generated <- function(slug) {
  cwr_tables(slug) |>
    select(table, file) |>
    pmap(\(table, file) {
      x <- cwr_read_table(file)
      tibble(
        table = table,
        column = names(x),
        type = map_chr(x, cwr_column_type),
        example = map_chr(x, cwr_column_example),
        rows = nrow(x)
      )
    }) |>
    list_rbind()
}

# ---- Check and assemble the dictionary --------------------------------------
# Stops if any column in the data has no row in dictionary.csv; warns about
# rows in dictionary.csv that match no column (a renamed or dropped column).
cwr_dictionary_check <- function(slug) {
  generated <- cwr_dictionary_generated(slug)
  entries <- cwr_dictionary_entries(slug)
  missing <- anti_join(generated, entries, by = c("table", "column"))
  extra <- anti_join(entries, generated, by = c("table", "column"))
  if (nrow(extra) > 0) {
    warning(
      "dictionary.csv has rows for columns that do not exist: ",
      paste(extra$table, extra$column, sep = ".", collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(missing) > 0) {
    stop(
      "dictionary.csv is missing these columns: ",
      paste(missing$table, missing$column, sep = ".", collapse = ", "),
      call. = FALSE
    )
  }
  undescribed <- entries |> filter(description == "")
  if (nrow(undescribed) > 0) {
    stop(
      "dictionary.csv has empty descriptions for: ",
      paste(undescribed$table, undescribed$column, sep = ".", collapse = ", "),
      call. = FALSE
    )
  }
  message("Dictionary complete: ", nrow(generated), " columns across ", n_distinct(generated$table), " table(s).")
  invisible(TRUE)
}

cwr_dictionary <- function(slug) {
  cwr_dictionary_check(slug)
  cwr_dictionary_generated(slug) |>
    left_join(cwr_dictionary_entries(slug), by = c("table", "column")) |>
    select(table, column, type, description, units, example)
}

# The dictionary as a gt table for the post's Reproducibility box
cwr_dictionary_table <- function(slug) {
  library(gt)
  cwr_dictionary(slug) |>
    group_by(table) |>
    gt() |>
    cols_label(
      column = "Column", type = "Type", description = "Description",
      units = "Units", example = "Values"
    ) |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |>
    tab_options(table.font.size = px(13), data_row.padding = px(3)) |>
    tab_source_note("Generated from the data with R/data_bundle.R; descriptions from data/dictionary.csv")
}

# ---- Bundle name and URL -------------------------------------------------------
cwr_bundle_name <- function(slug, version) paste0(slug, "-data-v", version, ".zip")

# The download URL is deterministic, so a post can link to it before the upload
cwr_bundle_url <- function(slug, version) {
  paste0(
    "https://github.com/", cwr_repo, "/releases/download/",
    cwr_data_tag(slug, "data", version), "/", cwr_bundle_name(slug, version)
  )
}

# ---- Build (and upload) the bundle -----------------------------------------
# excel: also write one .xlsx workbook (a sheet per table plus the dictionary)
#        when every table fits Excel's row limit.
# upload: attach the zip to the post's release; FALSE just builds it locally.
cwr_data_bundle <- function(slug, version = 1, excel = TRUE, upload = TRUE) {
  tables <- cwr_tables(slug)
  dictionary <- cwr_dictionary(slug)
  meta <- rmarkdown::yaml_front_matter(here("posts", slug, "index.qmd"))
  title <- meta$title
  post_url <- paste0("https://gpberber.github.io/chartingwaterlooregion/posts/", slug, "/")

  bundle_dir <- file.path(tempdir(), paste0(slug, "-data-v", version))
  unlink(bundle_dir, recursive = TRUE)
  dir.create(bundle_dir)

  data_list <- set_names(map(tables$file, cwr_read_table), tables$table)

  # Tables in CSV (universal) and Parquet (typed, compact)
  iwalk(data_list, \(x, name) {
    write_csv(x, file.path(bundle_dir, paste0(name, ".csv")), na = "")
    write_parquet(x, file.path(bundle_dir, paste0(name, ".parquet")))
  })
  write_csv(dictionary, file.path(bundle_dir, "data_dictionary.csv"), na = "")

  # Excel workbook when every table fits (Excel allows 1,048,576 rows)
  excel_written <- FALSE
  if (excel && all(map_int(data_list, nrow) <= 1e6)) {
    writexl::write_xlsx(
      c(data_list, list(data_dictionary = dictionary)),
      file.path(bundle_dir, paste0(slug, "-data.xlsx"))
    )
    excel_written <- TRUE
  }

  # README
  file_lines <- tables |>
    pmap_chr(\(table, description, ...) {
      x <- data_list[[table]]
      paste0(
        table, ".csv / ", table, ".parquet\n",
        "    ", description, "\n",
        "    ", format(nrow(x), big.mark = ","), " rows, ", ncol(x), " columns"
      )
    })
  source_lines <- tables |>
    pmap_chr(\(table, source, licence, notes, ...) {
      line <- paste0(table, ": ", source, "\n    Licence: ", licence)
      if (notes != "") line <- paste0(line, "\n    ", notes)
      line
    })
  readme <- c(
    title,
    strrep("=", nchar(title)),
    "",
    paste0("Data used in the post at ", post_url),
    paste0("Bundle version ", version, ", generated ", format(Sys.Date(), "%Y-%m-%d"), "."),
    "",
    "Files",
    "-----",
    file_lines,
    "",
    "data_dictionary.csv",
    "    Every column of every table: type, description, units, and the range or",
    "    example values found in the data.",
    if (excel_written) c(
      "",
      paste0(slug, "-data.xlsx"),
      "    The same tables and dictionary as an Excel workbook, one sheet each."
    ),
    "",
    "Sources and licences",
    "--------------------",
    "Each table keeps the licence of its original source:",
    "",
    source_lines,
    "",
    "Reuse",
    "-----",
    "The text and charts of the post are CC BY 4.0 (credit Charting Waterloo Region",
    "and link to the post). The code that produced these tables is MIT licensed.",
    "",
    "Reproduce",
    "---------",
    paste0("Code: https://github.com/", cwr_repo, "/tree/master/posts/", slug),
    "Steps: https://gpberber.github.io/chartingwaterlooregion/reproduce.html"
  )
  write_lines(readme, file.path(bundle_dir, "README.txt"))

  # Zip it
  zip_path <- file.path(tempdir(), cwr_bundle_name(slug, version))
  if (file.exists(zip_path)) file.remove(zip_path)
  zip::zip(zip_path, files = list.files(bundle_dir), root = bundle_dir)
  message("Built ", basename(zip_path), " (", round(file.size(zip_path) / 1024^2, 1), " MB)")

  if (upload) {
    cwr_release_upload(
      tag = cwr_data_tag(slug, "data", version),
      paths = zip_path,
      title = paste0("Data for post '", slug, "' (v", version, ")"),
      body = paste0(
        "Data used in ", post_url, " as CSV, Parquet",
        if (excel_written) ", Excel" else "",
        ", with a data dictionary and README. Download ", cwr_bundle_name(slug, version), "."
      )
    )
    message("Download URL: ", cwr_bundle_url(slug, version))
  }
  invisible(zip_path)
}
