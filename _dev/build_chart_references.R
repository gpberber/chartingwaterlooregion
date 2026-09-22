# build_chart_references.R
# ---------------------------------------------------------------------------
# Converts the RStudio snippet file (_dev/r.snippets) into three things:
#   .claude/skills/cwr-charts/references/*.md   the code templates Claude reads
#   CHARTS.md                                   the pick-a-chart table for the author
#   RStudio's own r.snippets                    what Shift+Tab fills in (section 4)
# All are generated, so none can drift from the snippets. Never edit the first two
# by hand, and edit snippets in _dev/r.snippets rather than in RStudio's copy.
#
# Re-run this after every edit to r.snippets:  Rscript _dev/build_chart_references.R
#
# Snippet syntax -> template syntax:
#   ${1:placeholder}  ->  <placeholder>
#   ${0} / ${1}       ->  (removed)
#   \$                ->  $
#   theme_tufte       ->  theme_cwr
# ---------------------------------------------------------------------------

library(tidyverse)
library(here)

snippet_lines <- read_lines(here("_dev", "r.snippets"))

# ---- 1. Parse the snippet file into a tibble of name + body -------------
starts <- which(str_detect(snippet_lines, "^snippet\\s+\\S+"))
ends <- c(starts[-1] - 1, length(snippet_lines))

snippets <- tibble(start = starts, end = ends) |>
  mutate(
    name = str_match(snippet_lines[start], "^snippet\\s+(\\S+)")[, 2],
    body = map2(start, end, \(s, e) snippet_lines[(s + 1):e])
  ) |>
  mutate(
    body = map(body, \(b) {
      b |>
        str_remove("^\t") |>                                   # drop the snippet indent
        str_replace_all("\\$\\{\\d+:([^}]*)\\}", "<\\1>") |>  # ${1:text} -> <text>
        str_replace_all("\\$\\{\\d+\\}", "") |>               # ${0} -> nothing
        str_replace_all("\\\\\\$", "$") |>                     # \$ -> $
        str_replace_all("theme_tufte", "theme_cwr")
    }),
    body = map(body, \(b) {
      # trim trailing blank lines
      while (length(b) > 0 && str_trim(b[length(b)]) == "") b <- b[-length(b)]
      b
    })
  ) |>
  select(name, body)

# ---- 2. Which snippets go in which reference file -----------------------
# description = one line shown above each template so Claude can pick quickly.
catalogue <- tribble(
  ~file,             ~name,                ~description,
  "bars",            "ggvertbar",          "Vertical bars, one value per category, right-hand y axis; optional direct labels replace the axis.",
  "bars",            "gghorbar",           "Horizontal bars (ranked categories), optional fill groups, gridlines run vertically, one label can be bolded.",
  "bars",            "gghorbarrect",       "Horizontal bars plus a boxed value column on the right (e.g. bar = count, box = rate).",
  "bars",            "ggcompbar_horiz",    "Two horizontal bars per category (wide focus bar + narrow comparison bar or tick).",
  "bars",            "ggcompbar_vert",     "Two vertical bars per category (wide focus bar + narrow comparison bar or tick).",
  "bars",            "ggshadedbars",       "Bars whose thickness encodes a second variable, with background reference bars and column headers.",
  "bars",            "ggvertbar_text",     "Building block: value labels inside stacked/vertical bars.",
  "lollipops",       "gglollipop",         "Lollipop (segment + dot) for ranked values with an x axis on top.",
  "lollipops",       "gglollipoplabel",    "Lollipop with direct value labels replacing the x axis entirely.",
  "lollipops",       "gglollipoprect",     "Lollipop plus a boxed value column on the right with a shaded header.",
  "lines",           "ggline",             "Multi-series line chart, colours by group, line labels placed automatically against each line by cwr_line_labels().",
  "lines",           "ggarea",             "Stacked area chart by group.",
  "lines",           "ggribbon",           "Two lines with a shaded band between them (min/max, range, confidence).",
  "lines",           "ggslope",            "Slope chart: two time points, one line per group, points masked at the ends.",
  "comparisons",     "ggdumbbell",         "Dumbbell: two dots per category joined by a segment, the two named on the top row.",
  "comparisons",     "ggdumbbellrect",     "Dumbbell plus a boxed value column on the right (e.g. the change).",
  "comparisons",     "ggdoterror",         "Dot per category with its confidence interval as a fading, flat-ended bar behind it, x axis on top.",
  "comparisons",     "ggnumcomp",          "Two figures written large with their names above and a ratio below, no axes.",
  "comparisons",     "ggarrow",            "Arrow from value 1 to value 2 per category; one annotation label.",
  "comparisons",     "ggarrowrect",        "Arrow chart plus a boxed value column on the right.",
  "points",          "ggscatter",          "Scatter with optional loess line and a y-axis title placed above the axis.",
  "points",          "ggbubble",           "Bubble chart (size = third variable) with a hand-built size legend.",
  "heatmaps",        "ggheatrank",         "Heatmap of ranks (rows ordered by average rank, best at top), stepped fill legend.",
  "heatmaps",        "ggheatraw",          "Heatmap of raw values (rows ordered by mean), stepped fill legend, optional cell labels.",
  "maps",            "ggmap",              "Outline map of the Region, one shape per district, labelled inside the shapes.",
  "maps",            "ggmapshaded",        "Choropleth: shapes shaded by a value, each labelled with its name and that value.",
  "multiples",       "ggmultiples",        "Small multiples via a plotting function + patchwork; insert any template inside.",
  "multiples",       "ggfacet",            "Building block: facet_wrap with free y scales.",
  "building-blocks", "ggtitles",           "labs() with the standard title / subtitle / Source caption.",
  "building-blocks", "ggscalex_cont",      "Continuous x scale with minor ticks.",
  "building-blocks", "ggscaley_cont",      "Continuous y scale on the right with explicit breaks.",
  "building-blocks", "ggscalex_date",      "Date x scale with automatic or anchored breaks.",
  "building-blocks", "ggscalex_disc",      "Discrete x scale with relabelled levels.",
  "building-blocks", "ggscaley_disc",      "Discrete y scale using pre-computed markdown labels (bold one row).",
  "building-blocks", "ggscale_color",      "Manual colour scale from manual_5_colours mapped to group names.",
  "building-blocks", "ggscale_fill",       "Manual fill scale from manual_5_colours mapped to group names.",
  "building-blocks", "ggviridis",          "Viridis colour/fill scale.",
  "building-blocks", "gglegend",           "Theme block for the standard inside-top-left horizontal legend.",
  "building-blocks", "ggannotate",         "annotate() text at a data position.",
  "building-blocks", "gggeom_text",        "geom_text() with every positioning argument spelled out.",
  "building-blocks", "gglabel",            "geom_label() (boxed text) with every positioning argument spelled out.",
  "building-blocks", "gggridx",            "Theme block that swaps gridlines to vertical for horizontal charts.",
  "building-blocks", "ggguides",           "Minor ticks on the x axis.",
  "building-blocks", "ggyaxis_break_label","Squiggle marker showing the y axis does not start at zero.",
  "building-blocks", "ggwraplabels",       "cwr_wrap(): break long category labels over two lines, narrower on the phone.",
  "building-blocks", "cwrfigure",          "cwr_figure(): the call that saves both PNGs and places the chart. Every chart ends with it; never call ggsave() in a post."
)

file_titles <- c(
  bars = "Bar charts",
  lollipops = "Lollipop charts",
  lines = "Line, area, ribbon and slope charts",
  comparisons = "Comparison charts: dumbbells, arrows and dots with intervals",
  points = "Scatter and bubble charts",
  heatmaps = "Heatmaps",
  maps = "Maps of the Region",
  multiples = "Small multiples and facets",
  `building-blocks` = "Building blocks: scales, labels, legends, annotations"
)

file_intros <- c(
  bars = "Use when each category has one value (or two to compare). Horizontal bars when category labels are long or there are more than ~6 categories. Bold the focus row with the y_label trick.",
  lollipops = "Use instead of bars when there are many categories or values are close together; the thin segment is easier to compare. Always ranked (reorder()).",
  lines = "Use for change over time. Colour by group with manual_n_colours, drop the legend and label the lines with cwr_line_labels(), which sets each label's height from its own line. The Region is always labelled cwr_region ('Region').",
  comparisons = "Use when the story is the gap or the change between two values per category (before/after, us/them).",
  points = "Use when the story is the relationship between two measures. Label points by hand with nudge values after the first render.",
  heatmaps = "Use for a category x time grid where the pattern matters more than exact values (ranks across years, rates across places).",
  maps = "Use when the story is about where, not how much - and only then, because a map spends a lot of space on shapes a reader already knows. R/maps.R holds the house map style: cwr_map_crs (the projection), cwr_label_point() (the roomiest point inside a shape, worked out in the cleaning script and stored as lon/lat), cwr_label_spot() (the roomiest place for a label of a given size, which is what a two-line label needs), cwr_map_nudges (the three district labels the welcome map moves by hand), cwr_map_theme() (no axes, no gridlines) and cwr_text_on_fill(). Labels go inside their shapes, level - never tilted, never shrunk to fit; a label that overhangs its border a little is normal. A map passes its own height, since it has no category axis.",
  multiples = "Use when one chart per group beats one crowded chart. make_multiples_plot() wraps any template; patchwork stacks the results.",
  `building-blocks` = "Fragments to add to any template. Most end with `+` so they can be pasted into a ggplot chain; `cwrfigure` is the separate call that follows the finished plot."
)

missing <- setdiff(catalogue$name, snippets$name)
if (length(missing) > 0) stop("Snippets not found: ", paste(missing, collapse = ", "))

# ---- 3. Write one markdown file per family --------------------------------
out_dir <- here(".claude", "skills", "cwr-charts", "references")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_family <- function(file_key, rows) {
  md <- c(
    paste0("# ", file_titles[[file_key]]),
    "",
    file_intros[[file_key]],
    "",
    "All templates assume `source(here::here(\"R\", \"theme_cwr.R\"))` has run: it provides the colours",
    "(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,",
    "`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.",
    ""
  )
  for (i in seq_len(nrow(rows))) {
    body <- snippets |> filter(name == rows$name[i]) |> pull(body) |> pluck(1)
    md <- c(
      md,
      paste0("## ", rows$name[i]),
      "",
      rows$description[i],
      "",
      "```r",
      body,
      "```",
      ""
    )
  }
  write_lines(md, file.path(out_dir, paste0(file_key, ".md")))
  message("wrote ", file_key, ".md (", nrow(rows), " templates)")
}

catalogue |>
  group_by(file) |>
  group_walk(\(rows, key) write_family(key$file, rows))

# ---- 4. Write CHARTS.md, the pick-a-chart reference -------------------------
# The same catalogue again, written for a person choosing a chart rather than
# for Claude writing one: what each template is called in plain words, and what
# it is good for. Generated from here so it cannot drift from the templates -
# **edit `picker` below, never CHARTS.md itself.**
picker <- tribble(
  ~name,                 ~kind,   ~label,                              ~use_for,
  "ggvertbar",           "chart", "Vertical bar chart",                "A handful of categories with one number each and short labels: spending by department, trips by mode, a count for each of a few years.",
  "gghorbar",            "chart", "Horizontal bar chart, ranked",      "A ranking - and the default whenever the category names are long or there are more than about six of them: districts, offence types, wards.",
  "gghorbarrect",        "chart", "Ranked bars with a value column",   "A ranking where a second number matters as much as the bar: ranked by count, with the rate printed beside it.",
  "ggcompbar_horiz",     "chart", "Paired horizontal bars",            "Us against them, one pair per category: Waterloo Region beside Ontario, this year beside last.",
  "ggcompbar_vert",      "chart", "Paired vertical bars",              "The same comparison when there are only a few categories and the labels are short.",
  "ggshadedbars",        "chart", "Bars weighted by a second variable","A rate or share where the size of the group behind it matters: a per-person figure with population as the bar's thickness.",
  "gglollipop",          "chart", "Lollipop chart, ranked",            "The same job as ranked bars, but easier to read when there are many categories or the values sit close together.",
  "gglollipoplabel",     "chart", "Lollipop with value labels",        "A ranking where the exact numbers matter as much as the order, so the axis can be dropped entirely.",
  "gglollipoprect",      "chart", "Lollipop with a value column",      "A ranking plus a second number per row: the value ranked, the change or rate boxed beside it.",
  "ggline",              "chart", "Line chart over time",              "A trend over months or years, for one group or a few compared.",
  "ggarea",              "chart", "Stacked area chart",                "How a total divides up over time, and whether the mix is shifting.",
  "ggribbon",            "chart", "Line with a shaded band",           "A range rather than a single figure over time: highest and lowest, a confidence interval, best and worst case.",
  "ggslope",             "chart", "Slope chart",                       "Exactly two dates: who rose, who fell, and whether the order changed between them.",
  "ggdumbbell",          "chart", "Dumbbell chart",                    "The gap between two values in each category: 2019 against 2024, one group against another.",
  "ggdumbbellrect",      "chart", "Dumbbell with a value column",      "The same gap, with its size spelled out in a column rather than left to the eye.",
  "ggdoterror",          "chart", "Dots with confidence intervals",    "Sample estimates side by side with their margins of error: poll results, survey shares, long-form census rates with published bounds.",
  "ggnumcomp",           "chart", "Two figures, side by side",         "Two totals whose comparison is the whole story - more cows than people - shown as the numbers themselves, with the ratio between them.",
  "ggarrow",             "chart", "Arrow chart",                       "Change per category when the direction is the point: which went up, which fell, and how far.",
  "ggarrowrect",         "chart", "Arrow chart with a value column",   "The same, with the size of each change printed beside its arrow.",
  "ggscatter",           "chart", "Scatter plot",                      "Whether two measures move together across places or units: density against transit use, income against distance.",
  "ggbubble",            "chart", "Bubble chart",                      "The same relationship, with a third variable sizing each point - usually population, so big places read as big.",
  "ggheatrank",          "chart", "Rank heatmap",                      "Where each place sat in a ranking in every period, and how positions moved.",
  "ggheatraw",           "chart", "Value heatmap",                     "A pattern across a category-by-time grid: calls by month and year, incidents by ward and season.",
  "ggmap",               "chart", "Map of the Region, labelled",       "Where something is rather than how much of it there is: which district, which corner of the Region. Labels sit inside the shapes.",
  "ggmapshaded",         "chart", "Shaded map (choropleth)",           "One value per district shown as shading, so the pattern reads before the numbers do: shares, rates, densities.",
  "ggmultiples",         "chart", "Small multiples",                   "One small chart per group instead of one crowded chart. Any template above can go inside it.",
  "ggvertbar_text",      "block", "Value labels inside bars",          "Printing each number on its bar, so the value axis, ticks and gridlines can come off.",
  "ggfacet",             "block", "Facets with free scales",           "facet_wrap() when each panel needs its own y scale.",
  "ggtitles",            "block", "Title, subtitle, source line",      "The standard labs() block. The source line comes from cwr_caption().",
  "ggscalex_cont",       "block", "Continuous x scale",                "A numeric x axis with minor ticks.",
  "ggscaley_cont",       "block", "Continuous y scale, on the right",  "A numeric value axis on the right, with breaks you choose.",
  "ggscalex_date",       "block", "Date x scale",                      "A time axis with automatic or anchored breaks.",
  "ggscalex_disc",       "block", "Discrete x scale",                  "A categorical x axis with relabelled levels.",
  "ggscaley_disc",       "block", "Discrete y scale",                  "A categorical y axis using markdown labels, which is how one row gets bolded.",
  "ggscale_color",       "block", "Manual colour scale",               "Mapping the house colours to named groups.",
  "ggscale_fill",        "block", "Manual fill scale",                 "The same, for fills.",
  "ggviridis",           "block", "Viridis scale",                     "A continuous colour or fill ramp; what the heatmaps use.",
  "gglegend",            "block", "Legend inside, top left",           "The house legend position, for the few charts that keep a legend instead of direct labels.",
  "ggannotate",          "block", "Annotation text",                   "A note placed at a data position.",
  "gggeom_text",         "block", "Direct text labels",                "Labelling line ends, bar ends or points, with every positioning argument spelled out.",
  "gglabel",             "block", "Boxed text labels",                 "The same with a box behind the text, for labels sitting over a busy background such as a map.",
  "gggridx",             "block", "Vertical gridlines",                "Swapping gridlines to vertical, which every horizontal chart needs.",
  "ggguides",            "block", "Minor x ticks",                     "Minor ticks on the x axis.",
  "ggyaxis_break_label", "block", "Axis-break marker",                 "A squiggle showing the value axis does not start at zero.",
  "ggwraplabels",        "block", "Wrapped category labels",           "Long category names on a horizontal chart - industries, offence types - broken over two lines so they stop crowding the bars, and broken harder on the phone.",
  "cwrfigure",           "block", "Save and place the chart",          "The call every chart ends with: writes the desktop and phone PNGs and places the figure."
)

missing_picker <- setdiff(catalogue$name, picker$name)
if (length(missing_picker) > 0) {
  stop("picker has no row for: ", paste(missing_picker, collapse = ", "))
}

# The question actually being asked when choosing a chart
start_here <- tribble(
  ~want,                                              ~templates,
  "Rank places or categories",                        "`gghorbar`, or `gglollipop` when there are many",
  "...and show a second number beside the ranking",   "`gghorbarrect`, `gglollipoprect`",
  "A few categories, one value each",                 "`ggvertbar`",
  "Compare two groups, category by category",         "`ggcompbar_horiz`, `ggdumbbell`",
  "Sample estimates with their margins of error",     "`ggdoterror`",
  "Two totals, and which is bigger",                  "`ggnumcomp`",
  "A trend over months or years",                     "`ggline`",
  "Change between exactly two dates",                 "`ggslope`, `ggarrow`, `ggdumbbell`",
  "How a total splits, and how the split moves",      "`ggarea`",
  "A range, not a single number, over time",          "`ggribbon`",
  "Whether two measures move together",               "`ggscatter`, or `ggbubble` to weight by size",
  "A pattern across categories and time",             "`ggheatraw`, or `ggheatrank` for positions",
  "Too many groups for one readable chart",           "`ggmultiples`"
)

write_chart_picker <- function() {
  meta <- catalogue |>
    left_join(picker, by = "name") |>
    mutate(file = factor(file, levels = names(file_titles)))

  md <- c(
    "# Choosing a chart",
    "",
    "Every chart on the site is built from one of these templates, which live in `_dev/r.snippets`.",
    "This page is for picking one. The code for each is in the `cwr-charts` skill",
    "(`.claude/skills/cwr-charts/references/`), and Claude reads it there - so ask for a chart in plain",
    "words and the template gets chosen for you. This is for when you want to know what is possible.",
    "",
    "**Never a pie or a donut.** Use `ggvertbar` or `gghorbar` for parts of a whole.",
    "",
    "## Start here: what do you want to show?",
    "",
    "| What you want to show | Template |",
    "|---|---|",
    pmap_chr(start_here, \(want, templates) paste0("| ", want, " | ", templates, " |")),
    "",
    "## The chart templates",
    ""
  )

  charts <- meta |> filter(kind == "chart") |> arrange(file)
  for (key in levels(droplevels(charts$file))) {
    rows <- charts |> filter(file == key)
    md <- c(
      md,
      paste0("### ", file_titles[[key]]),
      "",
      "| Snippet | Chart | Good for |",
      "|---|---|---|",
      pmap_chr(rows |> select(name, label, use_for),
               \(name, label, use_for) paste0("| `", name, "` | ", label, " | ", use_for, " |")),
      ""
    )
  }

  blocks <- meta |> filter(kind == "block")
  md <- c(
    md,
    "## Building blocks",
    "",
    "Not charts on their own - pieces added to one. Most end with `+` so they paste into a ggplot chain.",
    "",
    "| Snippet | What it is | Use it for |",
    "|---|---|---|",
    pmap_chr(blocks |> select(name, label, use_for),
             \(name, label, use_for) paste0("| `", name, "` | ", label, " | ", use_for, " |")),
    "",
    "---",
    "",
    paste0("Generated from `_dev/build_chart_references.R` on ", format(Sys.Date(), "%Y-%m-%d"),
           ". Do not edit this file by hand: add a snippet to `_dev/r.snippets`, describe it in that",
           " script's `catalogue` and `picker` tables, and run `Rscript _dev/build_chart_references.R`.")
  )

  write_lines(md, here("CHARTS.md"))
  message("wrote CHARTS.md (", sum(meta$kind == "chart"), " charts, ",
          sum(meta$kind == "block"), " building blocks)")
}

write_chart_picker()

# ---- 4. Install the snippets into RStudio -----------------------------------
# RStudio does not read _dev/r.snippets: it reads its own copy, in the user's
# RStudio settings folder, which is what fills in a snippet when you type its
# name and press Shift+Tab. Left alone, that copy drifts from the repo's (it
# was a month behind when this step was added), so every run of this script
# brings it up to date.
#
# It is a merge, not a copy. RStudio's file also holds snippets that are not in
# the repo - RStudio's own built-ins (lib, fun, if, for ...) and any written
# straight into RStudio - and a plain copy would delete them. So:
#   - every snippet in the repo replaces the one of the same name, or is added;
#   - every snippet only RStudio has is kept, after the repo's.
# The file it replaces is kept beside it as r.snippets.bak-<date>-<time>.
sync_rstudio_snippets <- function(repo_lines = snippet_lines) {
  # Windows keeps RStudio's settings under %APPDATA%; macOS and Linux under
  # ~/.config/rstudio
  settings_dir <- if (nzchar(Sys.getenv("APPDATA"))) {
    file.path(Sys.getenv("APPDATA"), "RStudio")
  } else {
    file.path("~", ".config", "rstudio")
  }
  rstudio_file <- file.path(settings_dir, "snippets", "r.snippets")
  dir.create(dirname(rstudio_file), recursive = TRUE, showWarnings = FALSE)

  # Split a snippet file into one block of lines per snippet, named by the
  # snippet. A block runs from its `snippet` line to the line before the next
  # one. The name may follow `snippet` after a space or a tab.
  snippet_blocks <- function(lines) {
    starts <- str_which(lines, "^snippet\\s+\\S+")
    if (length(starts) == 0) return(list())
    ends <- c(starts[-1] - 1, length(lines))
    map2(starts, ends, \(s, e) lines[s:e]) |>
      set_names(str_match(lines[starts], "^snippet\\s+(\\S+)")[, 2])
  }

  installed <- if (file.exists(rstudio_file)) read_lines(rstudio_file) else character()
  only_rstudio <- snippet_blocks(installed)
  only_rstudio <- only_rstudio[!names(only_rstudio) %in% names(snippet_blocks(repo_lines))]

  merged <- c(
    repo_lines,
    # A blank line before each kept block, so it cannot run on from the last
    # line of the snippet above it, and the block's own trailing blank lines
    # dropped, so running this again adds nothing
    flatten_chr(map(only_rstudio, \(block) {
      block <- str_trim(block, side = "right")
      c("", block[seq_len(max(which(block != "")))])
    }))
  )

  if (identical(merged, installed)) {
    message("RStudio snippets already match the repo (", rstudio_file, ")")
    return(invisible(rstudio_file))
  }
  # Dated, so a later run never overwrites an earlier backup. One appears only
  # when the file actually changes, which is whenever the repo's snippets do.
  if (file.exists(rstudio_file)) {
    file.copy(rstudio_file, paste0(rstudio_file, ".bak-", format(Sys.time(), "%Y%m%d-%H%M%S")))
  }
  write_lines(merged, rstudio_file)
  message("installed the repo's snippets into RStudio (", rstudio_file, "), keeping ",
          length(only_rstudio), " that only RStudio had: ",
          paste(names(only_rstudio), collapse = ", "))
  invisible(rstudio_file)
}

sync_rstudio_snippets()
