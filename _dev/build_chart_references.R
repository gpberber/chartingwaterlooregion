# build_chart_references.R
# ---------------------------------------------------------------------------
# Converts the RStudio snippet file (_dev/r.snippets) into the markdown
# reference files used by the cwr-charts skill
# (.claude/skills/cwr-charts/references/*.md).
#
# Re-run this after editing r.snippets:  Rscript _dev/build_chart_references.R
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
  "lines",           "ggline",             "Multi-series line chart, colours by group, labels placed manually once positions are known.",
  "lines",           "ggarea",             "Stacked area chart by group.",
  "lines",           "ggribbon",           "Two lines with a shaded band between them (min/max, range, confidence).",
  "lines",           "ggslope",            "Slope chart: two time points, one line per group, points masked at the ends.",
  "comparisons",     "ggdumbbell",         "Dumbbell: two dots per category joined by a segment, legend inside the panel.",
  "comparisons",     "ggdumbbellrect",     "Dumbbell plus a boxed value column on the right (e.g. the change).",
  "comparisons",     "ggarrow",            "Arrow from value 1 to value 2 per category; one annotation label.",
  "comparisons",     "ggarrowrect",        "Arrow chart plus a boxed value column on the right.",
  "points",          "ggscatter",          "Scatter with optional loess line and a y-axis title placed above the axis.",
  "points",          "ggbubble",           "Bubble chart (size = third variable) with a hand-built size legend.",
  "heatmaps",        "ggheatrank",         "Heatmap of ranks (rows ordered by average rank, best at top), stepped fill legend.",
  "heatmaps",        "ggheatraw",          "Heatmap of raw values (rows ordered by mean), stepped fill legend, optional cell labels.",
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
  "building-blocks", "ggsave",             "ggsave() with the standard size and dpi."
)

file_titles <- c(
  bars = "Bar charts",
  lollipops = "Lollipop charts",
  lines = "Line, area, ribbon and slope charts",
  comparisons = "Comparison charts: dumbbells and arrows",
  points = "Scatter and bubble charts",
  heatmaps = "Heatmaps",
  multiples = "Small multiples and facets",
  `building-blocks` = "Building blocks: scales, labels, legends, annotations"
)

file_intros <- c(
  bars = "Use when each category has one value (or two to compare). Horizontal bars when category labels are long or there are more than ~6 categories. Bold the focus row with the y_label trick.",
  lollipops = "Use instead of bars when there are many categories or values are close together; the thin segment is easier to compare. Always ranked (reorder()).",
  lines = "Use for change over time. Colour by group with manual_n_colours, drop the legend and place labels by hand at the line ends once positions are known.",
  comparisons = "Use when the story is the gap or the change between two values per category (before/after, us/them).",
  points = "Use when the story is the relationship between two measures. Label points by hand with nudge values after the first render.",
  heatmaps = "Use for a category x time grid where the pattern matters more than exact values (ranks across years, rates across places).",
  multiples = "Use when one chart per group beats one crowded chart. make_multiples_plot() wraps any template; patchwork stacks the results.",
  `building-blocks` = "Fragments to add to any template. Each ends with `+` so it can be pasted into a ggplot chain."
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
