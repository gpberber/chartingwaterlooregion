# data_quality.R
# ---------------------------------------------------------------------------
# Finds the data-quality flags and footnotes that apply to the data a post
# actually uses, prints them, and records them in the post's
# data/quality_flags.csv so they are still on record when the post is reviewed.
#
# Statistics Canada marks individual figures with symbols: E "use with caution",
# F "too unreliable to be published", x "suppressed", A to D quality grades, p
# "preliminary" and so on. A full table carries thousands of them, almost all on
# rows a post throws away, so the check runs on the rows the cleaning script
# KEEPS: filter to your places, years and categories first, then call
# cwr_quality_flags() before dropping the flag columns or the missing values.
#
# It also REMOVES what this site never uses. Every figure whose flag is level
# "unusable" below - including E, "use with caution" - comes back as missing
# (NA), so it cannot reach a chart. Greg never uses figures flagged E
# (2026-09-18), and F, x and .. have no figure to use anyway. The function
# returns the data with those figures blanked, so the script must carry on
# with what it returns:
#   kept <- kept |> cwr_quality_flags(...)
# Blanking rather than dropping rows is deliberate: a total built from a
# blanked part comes out NA, which is loud, where a dropped row would quietly
# make the total too small.
#
# Where the flags are in a Statistics Canada download:
#   - Regular tables (get_cansim()): a STATUS column holds "..", "x", "E", "F",
#     "A" to "D"; a SYMBOL column holds "p", "r"; TERMINATED holds "t". cansim
#     turns "F" into a missing status; the function puts it back (see below).
#   - Census tables (98-10-xxxx): one Symbol column holds all of them. A wide
#     census table has one Symbol column after each value column (Symbol,
#     Symbol_1, Symbol_2 ...), each belonging to the column just before it.
#   - Footnotes: cansim::get_cansim_table_notes() lists the table's notes, each
#     tied either to the whole table or to one member of one dimension
#     ("Men+", "Kitchener"). R/01_get_data.R saves them to data-raw/ next to the
#     table so the cleaning script can read them without going online.
#
# Usage, in a post's R/02_clean_data.R:
#
#   source(here("R", "data_quality.R"))
#   kept <- raw |>
#     filter(geo %in% places, ref_date >= 2016) |>
#     cwr_quality_flags(
#       "35-10-0177-01",
#       notes = read_csv(file.path(raw_dir, "table_35100177_notes.csv")),
#       log   = file.path(data_dir, "quality_flags.csv")
#     )
#
# Other sources: pass `flag_cols` (the column holding the flag) and `legend`
# (a tibble of flag, meaning, level) built from that source's documentation.
# A source with caveats but no flag column gets a row in the README's
# Reliability table instead; there is nothing here to run.
# ---------------------------------------------------------------------------

library(tidyverse)
library(janitor)

# Statistics Canada's standard symbol legend. `level` sorts what needs a
# decision from what is only worth knowing:
#   unusable - no publishable figure (F, x, ..), or one this site never uses
#              (E); cwr_quality_flags() blanks every one of them
#   caution  - a figure exists but is weak; the reader should be told
#   note     - worth knowing, rarely changes a chart
# A symbol not listed here is reported as "caution" with a prompt to look it up
# in that table's own legend, which is printed under the table on the web page.
cwr_quality_legend <- tribble(
  ~flag, ~meaning,                                                        ~level,
  "F",   "too unreliable to be published",                                "unusable",
  "x",   "suppressed to meet the confidentiality requirements of the Statistics Act", "unusable",
  ".",   "not available for any reference period",                        "unusable",
  "..",  "not available for a specific reference period",                 "unusable",
  "E",   "use with caution (never used on this site)",                    "unusable",
  "D",   "data quality: acceptable",                                      "caution",
  "C",   "data quality: good",                                            "note",
  "B",   "data quality: very good",                                       "note",
  "A",   "data quality: excellent",                                       "note",
  "p",   "preliminary (will be revised)",                                 "note",
  "r",   "revised",                                                       "note",
  "0s",  "rounded to zero (the true value is not zero)",                  "note",
  "*",   "significantly different from the reference category (p < 0.05)", "note",
  "...", "not applicable",                                                "note",
  "t",   "terminated (the series is no longer updated)",                  "note"
)

# Footnotes are mostly definitions ("Age refers to..."). These words mark the
# ones about the quality or comparability of the figures, which are the ones
# reported. The rest are counted, so it is clear something was skipped. The
# list is deliberately narrow: broad words such as "estimate", "exclude" or
# "imputed" matched definitions ("imputed rent" is an income concept) and
# buried the notes that matter.
cwr_quality_words <- regex(
  str_c(
    "caution", "reliab", "data quality", "suppress", "confidential",
    "rounding", "revis", "preliminar", "break in", "not comparable",
    "comparability", "sampl", "coefficient of variation", "standard error",
    "confidence interval", "imputed using", "imputation", "undercoverage",
    "incomplete",
    sep = "|"
  ),
  ignore_case = TRUE
)

cwr_quality_flags <- function(data, table, notes = NULL, log = NULL,
                              values = NULL, flag_cols = NULL, value_cols = NULL,
                              legend = cwr_quality_legend) {
  # The data is returned with its own column names, so the caller's code works
  # unchanged; the checks below work on a clean_names() copy. clean_names()
  # keeps the columns in order, so a column's position is the same in both.
  original <- data
  data <- clean_names(data)
  cols <- names(data)

  # ---- Which columns hold flags --------------------------------------------
  # clean_names() has turned STATUS / Symbol / Symbol_1 into status / symbol /
  # symbol_1, so one pattern finds them in either kind of table.
  if (is.null(flag_cols)) {
    flag_cols <- cols[str_detect(cols, "^(status|symbol|terminated)(_\\d+)?$")]
  } else {
    flag_cols <- make_clean_names(flag_cols)
  }

  # A wide census table: each symbol column belongs to the value column just
  # before it. Given `values` (the value columns the post keeps), only their
  # symbol columns are checked, so flags on columns the post drops are ignored.
  if (!is.null(values)) {
    values <- make_clean_names(values)
    owner <- set_names(cols[match(flag_cols, cols) - 1], flag_cols)
    flag_cols <- flag_cols[owner %in% values | str_detect(flag_cols, "^(status|terminated)")]
  }

  # ---- Put back the F that cansim drops --------------------------------------
  # cansim (0.5.0) reads Statistics Canada's CSVs with "F" in its list of
  # missing-value strings, so a figure flagged F ("too unreliable to be
  # published") arrives with no value AND no status - the flag is lost before
  # this function sees it. In a regular table (STATUS and VALUE columns), a
  # missing figure with no status is what an F becomes and nothing else: "..",
  # "x" and "E" all survive the download. So the F is put back, in the data
  # checked here and in the data handed back, where a post may read the status
  # to say why a figure is missing. Confirmed 2026-09-18 against Statistics
  # Canada's own CSV for table 32-10-0372-01 (Wilmot, Ontario, Total pigs,
  # 2021), which shows "F" on a cell cansim returns empty.
  #
  # Census tables (98-10-xxxx) are left alone: they carry a Symbol column, not
  # STATUS, and in every census table the site has downloaded no blank figure
  # lacks a symbol, so there is no lost F to restore and no evidence of what a
  # bare blank would mean there.
  #
  # The rows must also still be straight from get_cansim(), which the
  # COORDINATE column marks. A table that has been pivoted and had its flags
  # joined back on (the crime dataset, in the globe-csi post) has blanks for
  # combinations that were never published at all, and calling those F would be
  # wrong. The crime dataset's raw files had no lost F when checked (2026-09-18).
  if (all(c("status", "value", "coordinate") %in% cols) && !str_starts(table, "98-10")) {
    lost_f <- is.na(data[["value"]]) & is.na(data[["status"]])
    data[["status"]][lost_f] <- "F"
    original[[match("status", cols)]][lost_f] <- "F"
  }

  # A few readable columns to say where a flag sits: the period, the place and
  # the category columns, leaving out codes and machinery.
  id_cols <- cols[
    map_lgl(data, is.character) &
      !str_detect(cols, "hierarchy|classification|dguid|geo_uid|coordinate|uom|scalar|vector|decimals|^value$|val_norm") &
      !cols %in% flag_cols
  ] |> head(4)

  # ---- Flags on the kept rows ----------------------------------------------
  symbols <- flag_cols |>
    map(\(col) {
      data |>
        # "0" is not a symbol: Statistics Canada's cell-by-cell service
        # (get_cansim_data_for_table_coord_periods()) returns 0 for "no symbol"
        filter(!is.na(.data[[col]]), !str_trim(.data[[col]]) %in% c("", "0")) |>
        mutate(flag = str_trim(.data[[col]]), column = col) |>
        select(flag, column, all_of(id_cols))
    }) |>
    list_rbind()

  found <- if (nrow(symbols) == 0) {
    tibble()
  } else {
    symbols |>
      unite("location", all_of(id_cols), sep = " / ", na.rm = TRUE) |>
      group_by(flag) |>
      summarise(
        rows = n(),
        columns = str_c(unique(column), collapse = ", "),
        examples = str_c(head(unique(location), 3), collapse = "; "),
        .groups = "drop"
      ) |>
      left_join(legend, by = "flag") |>
      mutate(
        kind = "symbol",
        meaning = coalesce(meaning, "not in the standard legend - look it up in this table's symbol legend"),
        level = coalesce(level, "caution")
      )
  }

  # ---- Footnotes that apply --------------------------------------------------
  # A note applies when it covers the whole table (no dimension named) or when
  # its member appears somewhere in the kept rows. Members that exist only as
  # column names in a wide table are not matched; their notes are caught only
  # if they are table-wide, so read the table's footnotes too when it is wide.
  skipped_notes <- 0
  if (!is.null(notes)) {
    notes <- clean_names(notes)
    kept_values <- data |> select(where(is.character)) |> unlist(use.names = FALSE) |> unique()
    applicable <- notes |>
      filter(is.na(member_name) | member_name %in% kept_values)
    # One note can be tied to several kept members ("Ontario", "All-items"),
    # so it is kept once
    applicable <- applicable |> distinct(note_id, .keep_all = TRUE)
    quality <- applicable |> filter(str_detect(note, cwr_quality_words))
    skipped_notes <- nrow(applicable) - nrow(quality)
    if (nrow(quality) > 0) {
      found <- bind_rows(
        found,
        quality |>
          cwr_note_rows()
      )
    }
  }

  # ---- Report ----------------------------------------------------------------
  level_order <- c("unusable", "caution", "note")
  if (nrow(found) > 0) {
    found <- found |>
      mutate(table = table, .before = 1) |>
      arrange(factor(level, level_order), kind, flag) |>
      select(table, kind, level, flag, meaning, rows, columns, examples)
  }

  if (nrow(found) == 0) {
    message("Data quality, ", table, ": no flags or quality footnotes on the rows kept.")
  } else {
    lines <- found |>
      pmap_chr(\(kind, level, flag, meaning, rows, examples, ...) {
        if (kind == "symbol") {
          str_glue("  [{level}] {flag}: {meaning} - {rows} row(s), e.g. {examples}")
        } else {
          str_glue("  [{level}] footnote {flag}: {meaning}")
        }
      })
    message(
      "Data quality, ", table, ": ", nrow(found), " flag(s) apply to the rows kept.\n",
      str_c(lines, collapse = "\n"),
      "\nDecide how each is handled before charting it (WORKFLOW.md section 3.2)."
    )
  }
  if (skipped_notes > 0) {
    message("  (", skipped_notes, " other footnote(s) apply but are definitions, not quality notes.)")
  }

  # ---- Record ----------------------------------------------------------------
  # One file per post; each table's rows are replaced every time the script
  # runs, so it always matches the current data and never piles up duplicates.
  if (!is.null(log)) {
    old <- if (file.exists(log)) {
      read_csv(log, col_types = cols(.default = col_character())) |> filter(table != !!table)
    } else {
      tibble()
    }
    new <- if (nrow(found) > 0) mutate(found, across(everything(), as.character)) else tibble()
    bind_rows(old, new) |>
      arrange(table) |>
      write_csv(log, na = "")
  }

  # ---- Remove what is never used ---------------------------------------------
  # Which column holds the figure a flag belongs to. In a wide census table it
  # is the column just before the symbol column; in a long table it is the
  # value column (and cansim's val_norm copy of it). `value_cols` names it for
  # a source that calls it something else.
  never_use <- legend |> filter(level == "unusable") |> pull(flag)
  # val_norm only ever comes as a copy of value: without a value column there
  # is nothing reliable to blank, and the function stops rather than guess.
  long_values <- if (!is.null(value_cols)) {
    make_clean_names(value_cols)
  } else if ("value" %in% cols) {
    intersect(c("value", "val_norm"), cols)
  } else {
    character()
  }
  removed <- 0

  for (col in flag_cols) {
    hit <- !is.na(data[[col]]) & str_trim(data[[col]]) %in% never_use
    if (!any(hit)) next

    targets <- if (!is.null(values) && str_starts(col, "symbol")) cols[match(col, cols) - 1] else long_values
    if (length(targets) == 0) {
      stop("Data quality, ", table, ": figures flagged ", str_c(unique(str_trim(data[[col]][hit])), collapse = ", "),
           " in column '", col, "' must be removed, but there is no value column to blank. ",
           "Pass value_cols = (long table) or values = (wide census table).", call. = FALSE)
    }
    # Counted on the first target only: val_norm is a copy of the same figure
    first <- match(targets[1], cols)
    removed <- removed + sum(hit & !is.na(original[[first]]) & original[[first]] != "")
    for (target in targets) {
      original[[match(target, cols)]][hit] <- NA
    }
  }

  if (removed > 0) {
    message("  Removed ", removed, " figure(s) flagged ", str_c(intersect(never_use, found$flag), collapse = ", "),
            ": they are now missing (NA) and cannot reach a chart. Give them a row in the README's Reliability table.")
  }

  invisible(original)
}

# Footnote rows in the same shape as symbol rows. `flag` is the note number,
# `meaning` the note text, and `columns` says what the note is attached to.
cwr_note_rows <- function(notes) {
  notes |>
    mutate(
      kind = "footnote",
      level = if_else(
        str_detect(note, regex("caution|unreliable|suppress|not comparable|break in", ignore_case = TRUE)),
        "caution", "note"
      ),
      flag = as.character(note_id),
      # Notes arrive with HTML links in them; keep the words
      meaning = str_squish(str_remove_all(note, "<[^>]+>")),
      rows = NA_integer_,
      columns = if_else(is.na(member_name), "whole table", str_c(dimension_name, ": ", member_name)),
      examples = NA_character_
    ) |>
    select(kind, level, flag, meaning, rows, columns, examples)
}
