# post_sections.R
# ---------------------------------------------------------------------------
# What a post prints after its charts: its key terms, its data sources, its
# reliability table, and the session information in the Reproducibility box.
# The first three are read out of the post's own README.md, so the post and the
# repository cannot drift apart - see cwr_readme_table() below.
#
# R/seo_post_render.R reads the same tables when it describes a post to search
# engines, and sources this file rather than keeping its own reader.
#
# Sourced by R/theme_cwr.R, which every post sources in turn - so a post never
# names this file. Split out of theme_cwr.R on 2026-09-26; the only change is
# that cwr_session_info() has its own comment back (it had drifted 200 lines up
# the file, above a section it had nothing to do with).
# ---------------------------------------------------------------------------

# ---- The post's data-source table ------------------------------------------
# Each post's README.md holds one table of its data sources: what each file is,
# where it came from, its licence, when it was downloaded. That table is what
# someone browsing the repository sees, and since 2026-09-14 it is also what the
# post's "Data sources" section shows. This function reads it out of the README
# rather than keeping a second copy in the post, so the two cannot drift apart.
#
# It prints the markdown straight through instead of building a gt table. The
# cells hold hand-written markdown links ([City of Kitchener](https://...)), and
# passing them along unchanged is both simpler and guarantees the post says
# exactly what the README says. Call it from a chunk with `#| output: asis`.
# The run of table lines under one "## " heading of a post's README. Shared by
# the two tables the post prints from its README: sources and reliability.
# Where a slug's README lives. Almost always `posts/<slug>/README.md`, but a
# standing page such as `vital-statistics/` sits at the top of the project
# instead, and used to have to say so at every call
# (`cwr_sources_table("vital-statistics", readme = here(...))`, three times a
# page). Looking in both places removes that. A `readme =` argument still wins,
# and a slug with no README anywhere returns the post path so the error names
# the file a post is expected to have.
cwr_page_readme <- function(slug) {
  candidates <- c(here::here("posts", slug, "README.md"), here::here(slug, "README.md"))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) candidates[[1]] else found[[1]]
}

cwr_readme_table <- function(slug, heading, readme = cwr_page_readme(slug)) {
  if (!file.exists(readme)) {
    stop("No README.md for post '", slug, "' at ", readme, call. = FALSE)
  }
  lines <- read_lines(readme)

  # Everything under the heading, stopping at the next heading
  start <- which(str_trim(lines) == str_c("## ", heading))
  if (length(start) == 0) {
    stop("README.md for '", slug, "' has no '## ", heading, "' heading", call. = FALSE)
  }
  after <- lines[(start[1] + 1):length(lines)]
  next_heading <- which(str_starts(str_trim(after), "## "))
  section <- if (length(next_heading) > 0) after[seq_len(next_heading[1] - 1)] else after

  # Lines inside an HTML comment are instructions for whoever edits the README,
  # never part of the table. The template's Reliability instructions hold
  # example rows that start with "|" once trimmed, and those used to print in
  # the post (found 2026-10-01 in seven drafts). So each line is given the
  # comment depth it starts at: accumulate() runs along the lines, adding the
  # "<!--" openings and taking away the "-->" closings it meets. Counting
  # rather than looking for the next "-->" matters because the instructions
  # contain a complete example comment on one line ("<!-- flags: ... -->"),
  # which opens and closes on the same line and leaves the depth unchanged.
  # Real table rows end with that same balanced comment and start at depth 0.
  depth_after <- accumulate(
    str_count(section, fixed("<!--")) - str_count(section, fixed("-->")),
    `+`
  )
  depth_before <- c(0, head(depth_after, -1))
  section <- section[depth_before == 0]

  # The table is the run of pipe-delimited lines in that section
  table_lines <- section[str_starts(str_trim(section), fixed("|"))]
  if (length(table_lines) == 0) {
    stop("No table under '## ", heading, "' in ", readme, call. = FALSE)
  }
  table_lines
}

cwr_sources_table <- function(slug, readme = cwr_page_readme(slug)) {
  table_lines <- cwr_readme_table(slug, "Data sources", readme)
  cat(table_lines, sep = "\n")
  cat("\n")
  invisible(table_lines)
}

# ---- Detailed table sources ---------------------------------------------------
# A chart names only its publisher ("Source: Statistics Canada"); the table
# numbers, report names and files behind it are listed here, figure by figure
# (Greg, 2026-10-14). Nothing is typed twice: each chart's cwr_caption(details =
# ) is logged when cwr_figure(), cwr_interactive() or cwr_leaflet() draws it,
# and each gt table's when its cwr_caption() runs in its "tbl-" chunk
# (cwr_log_caption(), chart_helpers.R); this prints the log. So it must come
# after the last chart - the top of the "Data sources and reliability" box is
# the place. Call it from a chunk with `#| output: asis`.
#
# Three columns: Figure (or "Figure or table" when gt tables are listed),
# Source, Source details. The first cell is a Quarto cross-reference
# (@fig-commuting, @tbl-watson-a2), which renders as "Figure 1" or "Table 1"
# and links to it, so the numbers follow the charts wherever they move. Table
# numbers are gathered and linked by cwr_join_source_items() below. Each figure
# gets one row per publisher, and that row lists every item the figure takes
# from the publisher - all its tables, reports and files, each once, separated
# by commas as the source line used to be (Greg, 2026-10-14). A tabset
# (cwr_figure(..., part_of = )) is one figure: its tabs' items are gathered
# into the tabset's row, under its number. The site's own row comes last in
# each figure, its descriptions each ending with a period and set apart by an
# empty line.
#
# A chart whose caption has no `details` is listed with its source line's own
# wording and an empty details cell, and the render warns, so a chart is never
# missed. Printed to stderr like the other render warnings, so a chunk's
# `warning: false` cannot hide it.
cwr_detailed_sources_table <- function() {
  charts <- cwr_source_log$charts
  if (length(charts) == 0) {
    message("cwr_detailed_sources_table(): no numbered charts have run yet, so ",
            "there is nothing to list. It has to come after the last chart.")
    return(invisible(NULL))
  }
  log <- bind_rows(charts)

  # One item per line: a publisher's items arrive as a vector in `details`
  log <- unnest_longer(log, details)

  missing_details <- log |> filter(is.na(details), !credit) |> distinct(id) |> pull(id)
  if (length(missing_details) > 0) {
    cat("cwr_detailed_sources_table(): no source details for ",
        str_flatten_comma(missing_details),
        " - add cwr_caption(details = ) to the chart's chunk\n", sep = "", file = stderr())
  }
  missing_credit <- log |> filter(is.na(details), credit) |> distinct(id) |> pull(id)
  if (length(missing_credit) > 0) {
    cat("cwr_detailed_sources_table(): ", str_flatten_comma(missing_credit),
        " credit Charting Waterloo Region without saying for what - give",
        " cwr_caption(credit = ) a description in place of TRUE\n", sep = "", file = stderr())
  }

  # A tab is listed under its tabset, any other chart under its own id. Then
  # one row per figure and publisher, in the order the charts appear (group_by()
  # would sort them, so the groups are kept in order of first appearance), with
  # each item once - the site's own row last in each figure. A publisher's
  # items are table numbers and file names, joined with commas as the source
  # line used to be. The site's are descriptions: each always ends with a
  # period, added here if the chunk left it off, and two or more in one figure
  # (the tabs of a tabset) are set apart by an empty line (Greg, 2026-10-14).
  # <br><br> is how a markdown table cell spells an empty line.
  full_stop <- \(x) if_else(str_ends(x, fixed(".")), x, str_c(x, "."))
  rows <- log |>
    mutate(figure = coalesce(part_of, id)) |>
    mutate(figure = fct_inorder(figure), source = fct_inorder(source)) |>
    group_by(figure, credit, source) |>
    summarise(
      details = if (first(credit)) {
        str_flatten(full_stop(unique(na.omit(details))), "<br><br>")
      } else {
        cwr_join_source_items(unique(na.omit(details)))
      },
      .groups = "drop"
    ) |>
    arrange(figure, credit, source) |>
    mutate(across(c(figure, source), as.character))

  # "Figure", or "Figure or table" in a post whose gt tables are listed too
  first_column <- if (any(str_starts(rows$figure, "tbl-"))) "Figure or table" else "Figure"

  # A "|" inside a cell would end it early
  escape <- \(x) str_replace_all(replace_na(x, ""), fixed("|"), "\\|")
  table_lines <- c(
    str_c("| ", first_column, " | Source | Source details |"),
    "|--------|--------|----------------|",
    str_c("| @", rows$figure, " | ", escape(rows$source), " | ", escape(rows$details), " |")
  )
  cat(table_lines, sep = "\n")
  cat("\n")
  invisible(rows)
}

# One publisher's items for one figure, as the Source details cell shows them.
# Every item that is only Statistics Canada table numbers ("Table
# 36-10-0468-01", "Tables 17-10-0152-01 and 17-10-0155-01") is gathered into
# one "Tables A, B and C", each number once, where the first of them stood; a
# tabset's tabs often share some tables and not others, and this lists the
# lot without repeating any. Each table number is then linked to its page on
# Statistics Canada's site, so a chunk writes the plain number. An item that
# already carries a markdown link is left as it is. Items are separated by
# commas, or by semicolons when an item has commas of its own.
cwr_join_source_items <- function(items) {
  number <- "\\d{2}-\\d{2}-\\d{4}-\\d{2}"
  only_tables <- str_c("^Tables? ", number, "((,|,? and) ", number, ")*$")
  is_table <- str_detect(items, only_tables)
  if (any(is_table)) {
    numbers <- unique(unlist(str_extract_all(items[is_table], number)))
    tables <- if (length(numbers) == 1) {
      str_c("Table ", numbers)
    } else {
      str_c("Tables ", str_flatten_comma(numbers, last = " and "))
    }
    # Everything before the first table item, the gathered tables, then the
    # other items after it
    first_table <- which(is_table)[1]
    later <- seq_along(items) > first_table & !is_table
    items <- c(head(items, first_table - 1), tables, items[later])
  }
  # Table 36-10-0468-01 lives at ...tv.action?pid=3610046801
  link <- \(item) {
    if (str_detect(item, fixed("]("))) return(item)
    str_replace_all(item, number, \(n) str_c(
      "[", n, "](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=",
      str_remove_all(n, "-"), ")"
    ))
  }
  # Commas between items, unless an item has commas of its own ("1976 Census,
  # Catalogue 92-804, Table 3"): then semicolons, so the items stay distinct
  separator <- if (any(str_detect(items, ","))) "; " else ", "
  str_flatten(map_chr(items, link), separator)
}

# ---- The post's key terms ------------------------------------------------------
# The README's "## Key terms" table defines the terms used in the main body of
# the post's charts - segment, legend, panel, axis and direct labels - and
# nothing else: no statistical terms, no census geography terms, nothing found
# only in a title, note or the prose (Greg, 2026-09-22; he asks for any term he
# wants added). It has two columns, Term and Definition. Definitions are
# the source's own, quoted from the table's metadata, the census dictionary or
# other official documentation, with the source named in brackets at the end;
# Greg adds his own where a source has none. Added 2026-09-22 after a chart note
# said people who work at home were counted in their own municipality, when
# the commuting tables leave them out altogether: a note, a label or a sentence
# of prose is checked against these definitions before it is written.
#
# Claude drafts the table and may list a term it thinks a reader needs but
# could find no official definition for, with the Definition cell left empty.
# Those rows are Greg's to fill or delete; the post leaves them out, and the
# render warns until each is settled, so none is forgotten.
#
# Printed as a markdown table, like the sources table, so the post says exactly
# what the README says, in alphabetical order by term (Greg, 2026-09-28) -
# sorted here as well as in the README, so a row added out of place still
# prints in its place. Call it from a chunk with `#| output: asis`.
cwr_key_terms_table <- function(slug, readme = cwr_page_readme(slug)) {
  table_lines <- cwr_readme_table(slug, "Key terms", readme)

  # Split each row into its cells; the first two lines are the header and the
  # |---|---| separator. A line "| a | b |" splits to "", " a ", " b ", "".
  rows <- table_lines[-(1:2)] |>
    map(\(line) {
      cells <- str_split_1(str_trim(line), fixed("|"))
      tibble(term = str_trim(cells[2]), definition = str_trim(cells[3]))
    }) |>
    list_rbind()

  undefined <- rows |> filter(term != "", definition == "")
  # Written to the console rather than raised as a warning: posts set
  # `warning: false`, which would swallow it
  if (nrow(undefined) > 0) {
    cat(
      "WARNING - post '", slug, "': these key terms have no definition yet and are left out of the post: ",
      str_c(pull(undefined, term), collapse = ", "),
      ". Add a definition in the README's Key terms table, or delete the row.\n",
      sep = "", file = stderr()
    )
  }

  # `.locale = "en"` sorts as a reader would, ignoring capitals ("Row" before
  # "rural"); dplyr's default sorts every capital before every small letter
  defined <- rows |>
    filter(term != "", definition != "") |>
    arrange(term, .locale = "en")
  if (nrow(defined) == 0) {
    cat("No key terms for this post.\n")
    return(invisible(defined))
  }
  printed <- defined |> mutate(line = str_c("| ", term, " | ", definition, " |")) |> pull(line)
  cat(c(table_lines[1:2], printed), sep = "\n")
  cat("\n")
  invisible(defined)
}

# ---- The post's reliability table --------------------------------------------
# The README's "## Reliability" table lists every data-quality issue found in
# the data the post uses - Statistics Canada's quality flags and footnotes
# (cwr_quality_flags(), R/data_quality.R), and any caveat another source's
# documentation gives. It has two columns: the issue, and "Left out of the post
# because". The post prints the issues whose second column is empty.
#
# That second column is how an issue comes out of the post. The row is never
# deleted: Greg decides an issue is not worth disclosing, writes why in that
# column, and the issue drops out of the post while the README keeps it and the
# reason. So the record of what was checked stays complete, and a later reader
# of the repository can see what was set aside and on what grounds.
#
# Printed as a one-column markdown table, like the sources table, so the post
# says exactly what the README says. Call it from a chunk with `#| output: asis`.
#
# Each row ends with a hidden code naming the flags it covers, which a reader
# of the post never sees:
#   ... (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 -->
# A symbol is "table:symbol" (17-10-0155-01:E); a footnote is "table:note" and
# its number (98-10-0459:note1); one row can list several, comma-separated.
# The render stops if any flag in the post's data/quality_flags.csv (written
# by cwr_quality_flags() in R/02_clean_data.R) has no row carrying its code,
# so a flag cannot reach the published post without being dealt with. A row
# left out of the post with a reason still counts: the flag was considered.
cwr_reliability_table <- function(slug, readme = cwr_page_readme(slug)) {
  table_lines <- cwr_readme_table(slug, "Reliability", readme)

  # ---- Every recorded flag has a row -----------------------------------------
  flags_file <- file.path(dirname(readme), "data", "quality_flags.csv")
  covered <- table_lines |>
    str_match_all(r"(<!--\s*flags:(.*?)-->)") |>
    map(\(m) m[, 2]) |>
    unlist() |>
    str_split(",") |>
    unlist() |>
    str_trim() |>
    purrr::discard(\(code) code == "")   # scales has a discard() too

  if (file.exists(flags_file)) {
    required <- read_csv(flags_file, col_types = cols(.default = col_character())) |>
      mutate(code = if_else(kind == "footnote", str_c(table, ":note", flag), str_c(table, ":", flag))) |>
      pull(code) |>
      unique()

    missing <- setdiff(required, covered)
    if (length(missing) > 0) {
      stop(
        "Post '", slug, "': these data-quality flags have no row in the README's Reliability table: ",
        str_c(missing, collapse = ", "), ". Give each a row (or add its code to the row that ",
        "covers it) ending <!-- flags: ", missing[1], " -->. A flag that should not be shown ",
        "still needs a row, with the reason in 'Left out of the post because'.",
        call. = FALSE
      )
    }

    # Codes for flags the data no longer has: the row may be out of date
    stale <- setdiff(covered, required)
    if (length(stale) > 0) {
      warning(
        "Post '", slug, "': the Reliability table covers flags that data/quality_flags.csv no ",
        "longer lists: ", str_c(stale, collapse = ", "), ". Check whether those rows still apply.",
        call. = FALSE
      )
    }
  }

  # Split each row into its cells. The first two lines are the header and the
  # |---|---| separator; every line after them is an issue.
  rows <- table_lines[-(1:2)] |>
    map(\(line) {
      cells <- str_split_1(str_trim(line), fixed("|"))
      # A line "| a | b |" splits to "", " a ", " b ", "": the cells are 2 and 3
      # The hidden flag codes are for the check above, not for readers
      tibble(
        issue = str_trim(str_remove_all(cells[2], r"(\s*<!--.*?-->)")),
        left_out = str_trim(coalesce(cells[3], ""))
      )
    }) |>
    list_rbind()

  shown <- if (nrow(rows) == 0) character() else rows |> filter(issue != "", left_out == "") |> pull(issue)

  if (length(shown) == 0) {
    cat("No data-reliability issues to note for this post.\n")
  } else {
    # "Items of note" rather than "Issues": most rows are a caveat worth knowing
    # about, not a fault in the data. The README's own column keeps the heading
    # "Issue", which only the writer sees.
    cat(c("| Items of note |", "|---|", str_c("| ", shown, " |")), sep = "\n")
    cat("\n")
  }
  invisible(shown)
}


# Session information for the Reproducibility box at the end of each post.
# sessioninfo::session_info() would also print the pandoc and quarto install
# paths, which expose the local user name and folder layout, so this prints
# only what a reader needs: R version, OS, date, and attached package versions.
cwr_session_info <- function() {
  platform <- sessioninfo::platform_info()
  cat(
    "R version: ", platform$version, "\n",
    "OS:        ", platform$os, "\n",
    "Rendered:  ", platform$date, "\n",
    "Quarto:    ", as.character(quarto::quarto_version()), "\n\n",
    sep = ""
  )
  # as.data.frame() drops sessioninfo's print method, which would add the
  # library path column back; keep only the informative columns
  pkgs <- as.data.frame(sessioninfo::package_info(pkgs = "attached"))
  pkgs <- pkgs[, c("package", "loadedversion", "date", "source")]
  names(pkgs) <- c("package", "version", "date", "source")
  print(pkgs, row.names = FALSE)
}
