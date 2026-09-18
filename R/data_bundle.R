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
#                   columns: table, file, description, source, licence, sample, notes
#                   `file` is a path from the project root, so a table can live
#                   in the post's own data/ or in datasets/<name>/data/
#                   `sample` says whether the figures are estimates from a
#                   sample - "Census long form, 25% sample of households", a
#                   survey and its size - or "None (full count)"; it goes into
#                   the bundle's README beside the source, so somebody who only
#                   downloads the numbers is told too. Older files without the
#                   column still work.
#   dictionary.csv  one row per column of every table
#                   columns: table, column, description, units
#                   A confidence bound is described as one: "Lower bound of the
#                   95% confidence interval for `workers`", in the same units.
#
# Column types and example values are read from the data itself, so the
# dictionary only needs the parts a machine cannot know: meaning and units.
#
# The finished dictionary appears in exactly two places, and never in the post
# itself: the "## Data dictionary" section of posts/<slug>/README.md, which is
# what somebody browsing the repository sees, and data_dictionary.csv inside the
# zip, which is what somebody who downloads the data gets. Both are generated
# from the data, so neither can drift from it.
#
# Usage (the /publish skill runs this):
#   source(here::here("R", "data_bundle.R"))
#   cwr_dictionary_check("kitchener-phone-wait-times")      # every column documented?
#   cwr_data_bundle("kitchener-phone-wait-times", version = 1)   # build zip and upload
#
# cwr_data_bundle() rewrites the README section itself. To refresh it on its own,
# after changing a cleaning script or a description:
#   cwr_dictionary_readme("kitchener-phone-wait-times")
#
# In a post's Reproducibility box:
#   cwr_bundle_link("kitchener-phone-wait-times", 1)        # markdown link to the zip
# ---------------------------------------------------------------------------

library(here)
library(tidyverse)
library(arrow)

source(here("R", "data_helpers.R"))   # cwr_repo, cwr_data_tag(), cwr_release_upload()

# ---- Read the two description files ----------------------------------------
cwr_tables <- function(slug) {
  path <- here("posts", slug, "data", "tables.csv")
  if (!file.exists(path)) stop("No tables.csv in posts/", slug, "/data/. See R/data_bundle.R for the format.")
  tables <- read_csv(path, col_types = cols(.default = col_character()))
  # A tables.csv written before the `sample` column existed gets an empty one,
  # so the README below can always ask for it
  bind_rows(tibble(sample = character()), tables) |>
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

# ---- The dictionary in the post's README ------------------------------------
# The dictionary used to be printed in the post as well, in the Reproducibility
# box. It was dropped from there: six columns describing every field of every
# table is reference material for somebody about to use the data, and putting it
# in the middle of a reading page served neither that person (who wants it next
# to the download) nor the reader (who was not going to read it).
#
# So it goes in the README instead, generated rather than hand-written, because
# the column list has to match the data exactly and a hand-kept copy would drift
# the first time a cleaning script renamed something.

# The dictionary as markdown table rows
cwr_dictionary_markdown <- function(dictionary) {
  # A pipe inside a cell would end the cell early, so escape any that appear
  escape <- function(x) str_replace_all(replace_na(as.character(x), ""), fixed("|"), "\\|")

  rows <- dictionary |>
    pmap_chr(\(table, column, type, description, units, example) {
      cells <- escape(c(table, column, type, description, units, example))
      paste0("| ", paste(cells, collapse = " | "), " |")
    })

  c(
    "| Table | Column | Type | Description | Units | Values |",
    "|---|---|---|---|---|---|",
    rows
  )
}

# Write (or rewrite) the "## Data dictionary" section of the post's README.md.
# Everything from that heading to the next `## ` heading is replaced, so the
# section can sit anywhere in the file and the rest of the README is untouched.
# If the heading is not there yet, the section is added at the end.
cwr_dictionary_readme <- function(slug) {
  path <- here("posts", slug, "README.md")
  if (!file.exists(path)) {
    stop("No README.md for post '", slug, "' at ", path, call. = FALSE)
  }

  dictionary <- cwr_dictionary(slug)
  section <- c(
    "## Data dictionary",
    "",
    paste0(
      "Every column of every table in `data/tables.csv`. Types and example values are read from ",
      "the data itself; descriptions and units come from `data/dictionary.csv`. **This section is ",
      "generated - edit `data/dictionary.csv`, not the table below**, then run ",
      "`cwr_dictionary_readme(\"", slug, "\")` from `R/data_bundle.R` (building the download ",
      "bundle does it too). The same dictionary ships as `data_dictionary.csv` inside the bundle."
    ),
    "",
    cwr_dictionary_markdown(dictionary)
  )

  lines <- read_lines(path)
  start <- which(str_trim(lines) == "## Data dictionary")

  if (length(start) == 0) {
    lines <- c(lines, "", section)
  } else {
    start <- start[1]
    # Where the section ends: the line before the next `## ` heading, or the end
    # of the file if this is the last section
    after <- lines[(start + 1):length(lines)]
    next_heading <- which(str_starts(str_trim(after), "## "))
    end <- if (length(next_heading) > 0) start + next_heading[1] - 1 else length(lines)
    tail_lines <- if (end < length(lines)) lines[(end + 1):length(lines)] else NULL
    lines <- c(lines[seq_len(start - 1)], section, tail_lines)
  }

  write_lines(lines, path)
  message("Wrote the data dictionary into posts/", slug, "/README.md (",
          nrow(dictionary), " columns across ", n_distinct(dictionary$table), " table(s)).")
  invisible(path)
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

# The whole markdown link, name and URL together.
#
# A post used to write the link by hand as [`r cwr_bundle_name(...)`](`r cwr_bundle_url(...)`).
# That works, but it puts R code in a link *target*, and RStudio's visual editor
# rewrites the document through pandoc every time it saves: a target that is not a
# valid URL comes back percent-encoded, so the code turns into the literal text
# %60r%20cwr_bundle_url(...)%60 and the download link goes nowhere. It happened
# once, silently, and rendered without an error because a broken link is still a
# link. Returning the finished markdown from R keeps the code in an inline span,
# which the visual editor leaves alone.
cwr_bundle_link <- function(slug, version) {
  paste0("[", cwr_bundle_name(slug, version), "](", cwr_bundle_url(slug, version), ")")
}

# ---- Build (and upload) the bundle -----------------------------------------
# excel: also write one .xlsx workbook (a sheet per table plus the dictionary)
#        when every table fits Excel's row limit.
# upload: attach the zip to the post's release; FALSE just builds it locally.
cwr_data_bundle <- function(slug, version = 1, excel = TRUE, upload = TRUE) {
  tables <- cwr_tables(slug)
  dictionary <- cwr_dictionary(slug)

  # Refresh the README's dictionary section from the same data that is about to
  # go into the zip, so the two copies are built from one source in one step and
  # cannot describe different columns.
  cwr_dictionary_readme(slug)
  meta <- rmarkdown::yaml_front_matter(here("posts", slug, "index.qmd"))
  title <- meta$title
  post_url <- paste0("https://chartingwaterlooregion.ca/posts/", slug, "/")

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
    pmap_chr(\(table, source, licence, sample, notes, ...) {
      line <- paste0(table, ": ", source, "\n    Licence: ", licence)
      if (sample != "") line <- paste0(line, "\n    Sample: ", sample)
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
    paste0("Code: https://github.com/", cwr_repo, "/tree/master/posts/", slug)
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
