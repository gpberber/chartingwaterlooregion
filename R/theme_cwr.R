# theme_cwr.R
# ---------------------------------------------------------------------------
# House chart style for Charting Waterloo Region.
#
# Every post sources this file once in its setup chunk:
#
#   source(here::here("R", "theme_cwr.R"))
#
# It does four things:
#   1. loads the packages every post needs,
#   2. defines the house colours (the same hex values as custom.scss),
#   3. defines and sets theme_cwr(), the Tufte-inspired ggplot2 theme,
#   4. defines small helpers (caption text, font lookup).
#
# The chart templates that build on this live in the cwr-charts skill
# (.claude/skills/cwr-charts/references/). The original RStudio snippets are
# kept for reference in _dev/r.snippets.
# ---------------------------------------------------------------------------

# ---- 1. Packages ----------------------------------------------------------
# Loaded here so a post's setup chunk stays short. Post-specific packages
# (sf, leaflet, cansim, ...) are loaded in the post itself.
library(tidyverse)   # dplyr, ggplot2, tidyr, readr, purrr, stringr, forcats, lubridate
library(scales)      # label_number(), label_percent(), alpha(), rescale()
library(ggtext)      # element_markdown(): lets titles and axis labels use **bold** markdown
library(patchwork)   # combine plots (small multiples)
library(grid)        # textGrob() and gpar() used by the scatter/bubble templates
library(gt)          # tables
library(gtExtras)    # gt add-ons (sparklines, themes)
library(here)        # project-relative file paths
library(janitor)     # clean_names()
library(conflicted)  # make function-name clashes explicit instead of silent

# Prefer the dplyr versions of these common names over stats/base versions.
conflicts_prefer(
  dplyr::filter,
  dplyr::select,
  dplyr::mutate,
  dplyr::lag,
  dplyr::summarise,
  .quiet = TRUE
)

# Print numbers without a thousands separator by default (scales >= 1.3).
# Individual scales override this with label_number(big.mark = ",").
number_options(big.mark = "")

# ---- 2. House colours ----------------------------------------------------
# Three base colours, each with a 50 % tint (mixed with white). Use them with
# consistent meaning across posts so readers learn the code:
#   dodgerblue   = the focus (Waterloo Region, WRPS, Kitchener)
#   habsred      = the main comparison (Canada) or a highlight
#   cowboysilver = context (other cities, national averages, background bars)
#   ontgreen     = Ontario, when it needs its own colour
habsred             <- "#AF1E2D"
habsred50           <- prismatic::clr_mix(habsred, "white", ratio = 0.5)
cowboysilver        <- "#869397"
cowboysilver50      <- prismatic::clr_mix(cowboysilver, "white", ratio = 0.5)
cowboysilver30      <- prismatic::clr_mix(cowboysilver, "white", ratio = 0.7)   # lighter tint for background bars
cowboysilver_alpha30 <- adjustcolor(cowboysilver, alpha.f = 0.3)               # translucent version
dodgerblue          <- "dodgerblue4"                                            # hex #104E8B
dodgerblue50        <- prismatic::clr_mix(dodgerblue, "white", ratio = 0.5)
ontgreen            <- "darkgreen"

# Ready-made palettes for 2 to 5 groups. Order matters: the first colour goes
# to the first group. Rename with set_names() to map colours to group labels:
#   scale_colour_manual(values = set_names(manual_3_colours, c("A", "B", "C")))
manual_5_colours <- c(habsred, habsred50, cowboysilver, dodgerblue50, dodgerblue)
manual_4_colours <- c(habsred, habsred50, dodgerblue50, dodgerblue)
manual_3_colours <- c(habsred, cowboysilver, dodgerblue)
manual_2_colours <- c(cowboysilver, dodgerblue)

# Named palettes for the comparisons that recur across posts.
comp_colours <- c(
  "Waterloo Region" = dodgerblue,
  "Canada"          = habsred,
  "Ontario"         = ontgreen,
  "Other cities"    = cowboysilver50
)

local_colours <- c(
  "Waterloo Region" = dodgerblue,
  "Guelph"          = dodgerblue50,
  "London"          = cowboysilver,
  "Hamilton"        = habsred
)

# ---- 3. Font -------------------------------------------------------------
# The site uses Inter. Charts use it too when it is installed on the rendering
# machine (download from https://rsms.me/inter/ and install once). If it is not
# installed, ggplot2 falls back to its default sans font; charts still render.
cwr_font <- function(family = "Inter") {
  if (family %in% systemfonts::system_fonts()$family) family else ""
}

# ---- 4. Theme ------------------------------------------------------------
# Tufte-inspired: as little ink as possible that is not data. Horizontal
# gridlines only, no y axis line, no axis titles (put units in the subtitle),
# left-aligned title block, legend tucked above the plot at the left.
# All text sizes are multiples of base_size so one number scales everything.
# base_size 15 (not ggplot2's default 11) because charts are drawn 8.3 inches
# wide, shown at that size on a desktop (axis text a little smaller than the
# body text) and shrunk to about 320 px on a phone, where the text is small
# but the shapes still read; readers can tap a chart to enlarge it.
theme_cwr <- function(base_size = 15, base_family = cwr_font()) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements. element_markdown() (from ggtext) means titles can
      # contain **bold** or <span style='color:...'> markup.
      plot.title = element_markdown(
        size = base_size * 1.0,
        hjust = 0,
        vjust = 1,
        margin = margin(0, 0, 5, 0),
        face = "bold"
      ),
      plot.subtitle = element_markdown(
        size = base_size * 0.7,
        hjust = 0,
        vjust = 1,
        margin = margin(0, 0, 10, 0),
        color = "black",
        face = "bold"
      ),
      plot.caption = element_markdown(
        size = base_size * 0.6,
        hjust = 0,
        vjust = 1,
        margin = margin(t = 10),
        color = cowboysilver,
        # A caption is more than one line whenever cwr_caption() is given
        # notes, or whenever a long source line wraps on a phone. gridtext
        # sets those lines solid, so they need opening up here.
        lineheight = 1.3
      ),

      # Axes: no titles (state units in the subtitle instead), readable text
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(
        size = base_size * 0.8,
        margin = margin(t = base_size * 0.25)
      ),
      axis.text.y = element_markdown(
        size = base_size * 0.8,
        margin = margin(r = base_size * 0.25),
        hjust = 0,
        # The two lines of a label that cwr_wrap() has broken sit tight
        # together on purpose. What separates one label from the next is the
        # space left over in its row, so the looser these lines are, the less
        # of that is left - at 1.15 a two-line label filled its row completely
        # and ran straight into the label below. Set solid, each label reads as
        # one block with a clear gap after it, which is the way round it should
        # be. The room this needs comes from `height`, not from here: allow
        # about 0.45 in of chart height per row when labels wrap.
        lineheight = 1.0
      ),

      # ggplot2's complete theme defines axis.text.y.left as a plain
      # element_text, and the more specific element wins: set axis.text.y alone
      # and the left axis inherits its size and margin but is still drawn as
      # plain text, so `<br>` and `**bold**` come out literally on the chart.
      # Naming it here makes the left axis markdown-capable everywhere, which
      # is what both house tricks need - the bold focus row, and the line
      # breaks cwr_wrap() puts into long category names. It is deliberately
      # empty: every property still comes from axis.text.y above.
      axis.text.y.left = element_markdown(),

      # One axis line only, where the data meets the baseline
      axis.line.x = element_line(color = "black", linewidth = 0.5, linetype = "solid"),
      axis.line.y = element_blank(),

      # Ticks on x only; minor ticks are half length
      axis.ticks.x = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length.x = unit(0.15, "cm"),
      axis.minor.ticks.length = rel(0.5),

      # Gridlines: light horizontal only. Horizontal charts swap this
      # (see the templates) so gridlines run vertically.
      panel.grid.major.y = element_line(color = "grey80", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),

      # Legend: horizontal, above the panel, left-aligned. Most templates
      # drop the legend entirely in favour of direct labels.
      legend.position = "inside",
      legend.position.inside = c(-0.0, 1.02),
      legend.justification = "left",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size * 0.8, margin = margin(r = 10)),
      legend.spacing = unit(0.1, "cm"),
      legend.box.spacing = unit(0.1, "cm"),

      # Facets: bold left-aligned strip text, no strip background
      strip.text = element_text(
        hjust = 0,
        size = base_size * 0.8,
        margin = margin(b = base_size * 0.5),
        face = "bold"
      ),
      strip.background = element_blank(),

      # Put the panel heading outside the axis rather than between the axis and
      # the panel. It matters on a ranked horizontal chart, where the house
      # style moves the x axis to the top: left "inside", the heading would be
      # read after the numbers it is meant to introduce.
      strip.placement = "outside",
      panel.spacing.y = unit(1.5, "lines"),

      # Whole-plot settings: title and caption align to the plot edge, not the panel.
      # The margin is deliberately tight (6 pt, about 8 px on screen). It used to be
      # base_size, 15 pt, which put a 20 px white border on all four sides of every
      # PNG: on the page that read as a gap under the chart, and it inset the chart
      # from the text column instead of matching the paragraph width.
      plot.margin = margin(t = 6, r = 6, b = 6, l = 6),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Make it the default for every ggplot in the session
theme_set(theme_cwr(base_size = 15))

# The templates refer to base_size for text sizes inside geoms and annotations
base_size <- 15

# ---- 5. Helpers ----------------------------------------------------------
# What a chart is actually showing when its numbers are published for the
# Kitchener census metropolitan area rather than for the Region itself. The note
# names the geography and not the CMA: a reader wants to know which places are in
# the chart, and the acronym is the source line's business, not theirs.
#
# Statistics Canada builds a CMA out of whole municipalities around an urban
# core, and this one is six of Waterloo Region's seven: Kitchener, Cambridge,
# Waterloo, Woolwich, Wilmot and North Dumfries. Wellesley Township is the only
# exclusion, and nothing from outside the Region is included - checked against
# the 2021 census geographic attribute file, not assumed. So a CMA figure is a
# subset of the Region, never a claim about somewhere else, and Wellesley is
# about 1.7% of the Region's population, which makes the difference immaterial
# to everything except land area (it is a fifth of that).
#
# Written down once rather than typed per chart so every post says the same
# thing, and so one edit fixes them all if the boundaries are redrawn - CMA
# definitions are revisited at every census.
cwr_cma_note <- "Waterloo Region excluding Wellesley Township"

# Standard caption: "Source: Statistics Canada, Table 35-10-0177-01".
#
# The caption names whoever published the numbers, and nothing else. A chart that
# plots a publisher's figures as published is their data, not ours, so putting our
# name beside theirs would overstate our part in it.
#
# `credit = TRUE` adds "| *Charting Waterloo Region*" for a chart whose numbers we
# worked out ourselves - a rate per 100,000 we calculated, an index we based, a
# model we fitted, several sources we combined. There the arithmetic is ours and is
# worth standing behind. Where the line is a judgement call: say the source alone.
#
# `source` takes one source or several. Several are joined with commas. The label
# is "Sources:" whenever the line names more than one, and **the byline counts**:
# a chart that credits its own arithmetic beside a published table has two sources
# on it, so `credit = TRUE` makes it plural on its own. Pass sources as separate
# strings rather than writing "X and Y" into one, or the plural cannot be worked
# out:
#
#   cwr_caption(c("Statistics Canada Table 17-10-0155-01",
#                 "the 2021 census boundary files"))
#
# `notes` takes footnotes to qualify something in the title or subtitle. They are
# numbered here, in the order given, so a note and its key cannot drift apart;
# write the note alone and put the matching key in the title or subtitle by hand,
# as `<sup>1</sup>` - the title and subtitle are drawn with ggtext too, so the
# same markup works there. Numerals rather than symbols: they are easier to type,
# and they say how many notes there are. Never key a note with a bare `*` - an
# asterisk opens italics in a ggtext caption and swallows the rest of the line.
#
# `cma = TRUE` puts `cwr_cma_note` at the head of the notes, for a chart drawn
# from CMA data. It is numbered along with the rest, so it needs its key in the
# subtitle like any other note; first, because the geography qualifies the
# subject of the chart and that is the earliest thing a subtitle names. Set it
# rather than typing the note, so that the wording cannot drift from post to post.
#
# The notes are drawn above the source line, which is where a reader looks for a
# qualification and where Datawrapper puts them, with a blank line between the two
# so the qualifications do not read as part of the source:
#
#   cwr_caption(
#     "Statistics Canada Table 14-10-0468-01",
#     credit = TRUE,
#     notes = c("Waterloo Region excl. Wellesley Township",
#               "Two industries suppressed by Statistics Canada")
#   )
cwr_caption <- function(source, credit = FALSE, notes = NULL, cma = FALSE) {
  if (cma) notes <- c(cwr_cma_note, notes)

  # The byline is a source like any other, so it decides the plural too
  label <- if (length(source) > 1 || credit) "Sources: " else "Source: "
  caption <- paste0(label, paste(source, collapse = ", "))
  if (credit) caption <- paste0(caption, " | *Charting Waterloo Region*")

  if (length(notes) > 0) {
    # A blank line between the notes and the source line. <br><br> is how a
    # markdown caption spells one; the caption's lineheight decides how wide it
    # actually sits.
    # Superscript, the way a footnote key is set, so the number reads as a
    # reference rather than as the first word of the note.
    keyed <- paste0("<sup>", seq_along(notes), "</sup> ", notes)
    caption <- paste0(paste(keyed, collapse = "<br>"), "<br><br>", caption)
  }

  caption
}

# Wrap long category labels onto more than one line.
#
# A horizontal chart pays for a long category name twice: the name eats the
# width the bars need, and on a phone, at half the width, it can take more of
# the picture than the data. Wrapping is the cheap fix - it costs height, which
# a chart has more of than width.
#
# str_wrap() breaks on spaces only, so a name is never split mid-word, and
# `width` is in characters. 22 suits the phone, which is the harder of the two
# sizes; pass a larger number for a chart that only ever runs wide.
#
# The line break is emitted as `<br>`, not "\n", because the house theme draws
# the left axis with ggtext (see axis.text.y.left above) and markdown treats a
# bare newline as a space. Use it as a labeller, which hands it the levels:
#
#   scale_y_discrete(labels = cwr_wrap)
#   scale_y_discrete(labels = \(x) cwr_wrap(x, width = 30))
cwr_wrap <- function(x, width = 22) {
  str_replace_all(str_wrap(x, width = width), "\n", "<br>")
}

# ggplot2 text sizes for geom_text/geom_label are in mm, not points.
# 4 mm is roughly 11 pt, the standard label size used across the templates
# (scaled up with base_size so direct labels stay readable on a phone).
label_size <- 4

# ---- 6. Responsive figures ------------------------------------------------
# How deep one category row is drawn, in inches: the bar plus the gap below it.
# Taken from the welcome post's income chart, the one that read best: a bar of
# the standard width = 0.7 is then about a quarter of an inch thick, deep enough
# to hold a direct label with room around it. The phone value is smaller by the
# same factor cwr_figure() shrinks label text by there (phone_text_scale), so a
# label fills its bar the same way on both.
cwr_row_height <- 0.37
cwr_phone_row_height <- 0.30

# The height a chart must be drawn at for its category rows to come out
# `row_height` inches deep, or NULL if its y axis is not categories.
#
# Everything in a ggplot except the panels - title, subtitle, panel headings,
# legend, caption, margins - has a fixed height once the width is known, and
# the panels share whatever is left. So the chart is drawn once, off screen, at
# a trial height; the panels are measured; what is not panel is the fixed part.
# The panels then need, per row of panels, the number of rows their y axis
# spans - including the expansion above and below the outer bars, so a chart
# that asks for extra room above its top bar gets it - times `row_height`.
#
# Measuring a real drawing, rather than adding up the parts, is on purpose:
# wrapped titles and captions only know their height once they know their
# width, and the drawing is the one place that is certain.
cwr_fit_height <- function(plot, width, row_height, dpi) {
  built <- ggplot_build(plot)
  y_scale <- built$layout$panel_scales_y[[1]]
  if (is.null(y_scale) || !y_scale$is_discrete()) return(NULL)

  # Each panel's y range is in row units: a discrete scale puts its categories
  # at 1, 2, 3 ..., and the range adds the expansion either side. Panels in the
  # same row of a facet share a height, so each row of panels needs as much as
  # its tallest.
  rows_needed <- tibble(
    facet_row = built$layout$layout$ROW,
    span = map_dbl(built$layout$panel_params, \(params) diff(params$y.range))
  ) |>
    summarise(span = max(span), .by = facet_row) |>
    pull(span) |>
    sum()

  # Draw at a trial height into a throwaway PNG device of the real width and
  # resolution, so text is measured exactly as ggsave() will measure it.
  # Closing a device makes R switch to the next one in its list, which need not
  # be the one that was in use - RStudio's Plots pane while drafting, say - so
  # the one in use is noted first and switched back to afterwards.
  trial_height <- 20
  trial_file <- tempfile(fileext = ".png")
  previous_device <- dev.cur()
  ragg::agg_png(trial_file, width = width, height = trial_height,
                units = "in", res = dpi)
  on.exit({
    dev.off()
    if (previous_device > 1) dev.set(previous_device)
    unlink(trial_file)
  }, add = TRUE)
  table <- ggplotGrob(plot)
  grid.newpage()
  grid.draw(table)

  # A ggplot is drawn as a table (a gtable) whose cells are laid out in a
  # viewport called "layout". Stepping into one panel's row of that table and
  # asking how tall it is ("1 npc", the full height of where we are) gives the
  # height the panels in that row were actually drawn at.
  panel_cells <- table$layout |>
    filter(str_starts(name, "panel")) |>
    distinct(t, .keep_all = TRUE)
  downViewport("layout")
  panel_height <- map2_dbl(panel_cells$t, panel_cells$l, function(row, col) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    on.exit(popViewport())
    convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  }) |>
    sum()

  fixed_height <- trial_height - panel_height
  round(fixed_height + rows_needed * row_height, 2)
}

# A chart drawn for the desktop column (8.3 in, 797 px) is shrunk to about
# 40% on a phone, so its text becomes unreadable however large it is drawn.
# Sites like Datawrapper solve this by redrawing the chart for the phone.
# cwr_figure() does the same with two PNGs:
#
#   figures/<id>.png        8.3 in wide, for screens 768 px and wider
#   figures/<id>-phone.png  4.2 in wide, for narrower screens
#
# and writes an HTML <picture> element so the browser picks the right one.
# The phone version is the same ggplot with the same text sizes, so text is
# about twice as large relative to the chart, and the title and subtitle are
# allowed to wrap.
#
# Use it in a chunk with `output: asis` (see the post template):
#
#   ```{r}
#   #| label: answered
#   #| output: asis
#   p <- ggplot(...) + ...
#   cwr_figure(p, "fig-answered",
#     alt = "Line chart showing ...",
#     height = 5, phone_height = 4.5)
#   ```
#
# Heights. A chart whose y axis is categories - every bar, lollipop and dot
# chart - leaves `height` and `phone_height` out, and cwr_figure() works them
# out so that every such chart, in every post, draws its rows at the same
# depth: `cwr_row_height` inches per row on a desktop, `cwr_phone_row_height`
# on a phone (see cwr_fit_height() below). A chart with more rows, more panels
# or more text above and below simply comes out taller. Give a height only to a
# chart with no category axis (a line chart, a map), or to override the rule
# for one chart on purpose.
#
# Numbering follows the post type, read from the first entry in the post's
# `categories`, so there is nothing to set by hand:
#
#   Snapshot   bare <picture>. No "Figure 1", nothing under the chart. A
#              handful of charts read in five minutes do not need numbers, and
#              the chart's own title and subtitle already say what it shows.
#   Deep dive  a Quarto figure div carrying the id, which renders as a plain
#              "Figure 1" underneath and makes @fig-answered work in the prose.
#              Long posts need a way to point back at a chart from far below it.
#
# Quarto produces the bare label by itself when the div holds no caption
# paragraph, and marks it .quarto-uncaptioned (styled in custom.scss).
# `number = TRUE` or `FALSE` overrides the post type for one chart.
#
# `caption` is optional and usually left out: the source line is already drawn
# inside the chart by cwr_caption(), and the subtitle carries the explanation.
# Pass one only when a chart needs a note that does not belong in the image,
# such as a break in the series. `alt` is never optional - with no caption it
# is the only description a screen reader has.
#
# The PNGs live in the post's figures/ folder and are committed with the
# post, so a full site render never needs to re-run the R code.
#
# Running the chunk in RStudio instead of rendering puts both versions in the
# Plots pane, one page each, so the desktop and the phone chart can be compared
# with the pane's back and forward arrows. `draft_phone = TRUE` leaves the phone
# version on top instead of the desktop one. Nothing is written to the post
# while drafting.
#
# Three things are changed automatically for the phone version, because the
# panel is half as wide: titles wrap; right-hand axis labels move outside the
# panel (the templates tuck them inside, above the gridlines, which collides
# with the data on a narrow panel); and text drawn with geom_text/geom_label
# is scaled by phone_text_scale so value labels stay narrower than bars.
# Anything else that only the phone version needs goes in `phone`, a list of
# ggplot pieces added to it with `+`, for example:
#   phone = list(scale_x_date(date_breaks = "2 years", date_labels = "%Y"))
#   phone = list(theme(legend.position = "none"))
cwr_figure <- function(plot, id, alt, caption = NULL, number = NULL,
                       width = 8.3, height = NULL,
                       phone_width = 4.2, phone_height = NULL,
                       phone = list(), phone_text_scale = 0.8,
                       row_height = cwr_row_height,
                       phone_row_height = cwr_phone_row_height,
                       dpi = 288, draft_phone = FALSE) {
  stopifnot(str_starts(id, "fig-"))

  # Snapshots go unnumbered, deep dives numbered. rmarkdown::metadata is the
  # post's own YAML as R sees it during the render, so the first category - the
  # post type - decides it and no post has to set anything. Outside a render (a
  # chart tried in the console) there is no metadata, so nothing is numbered.
  if (is.null(number)) {
    categories <- unlist(rmarkdown::metadata$categories)
    number <- length(categories) > 0 && identical(categories[[1]], "Deep dive")
  }

  # Is this a render, or is somebody running the chunk in RStudio to see how the
  # chart looks?
  #
  # knitr.in.progress is the usual test: knitr sets it while it is knitting. On
  # its own it is wrong here, because RStudio runs a notebook chunk through
  # knitr too and so sets it as well. Testing that alone made running a chunk
  # look like a render: the chart was never drawn to look at, raw <picture> HTML
  # was printed instead, and - worse - the post's committed figures/ PNGs were
  # overwritten by a half-finished chart.
  #
  # rstudio.notebook.executing is the option RStudio sets for exactly this
  # distinction: it is TRUE while RStudio is running a chunk and unset during a
  # real render. So this is a render only when knitr is running and RStudio is
  # not the one running it.
  drafting <- !isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("rstudio.notebook.executing"))

  # During a render, write next to the post: knitr runs with the post folder as
  # the working directory, and the relative path "figures/..." then works in the
  # HTML too. Drafting, the working directory is the project root instead, where
  # a figures/ folder does not belong and the committed PNGs must not be touched
  # by a half-finished chart - so a draft goes to a temporary folder that the
  # session throws away.
  out_dir <- if (drafting) file.path(tempdir(), "cwr-figures") else "figures"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  desktop_file <- file.path(out_dir, paste0(id, ".png"))
  phone_file   <- file.path(out_dir, paste0(id, "-phone.png"))

  # Phone version: same plot, but titles wrap instead of running off the edge.
  # element_textbox_simple() (ggtext) is element_markdown() with word wrap.
  # theme() merges with what the plot already has, so only the named
  # properties change (the right-axis text keeps its size and vjust).
  phone_plot <- plot +
    theme(
      plot.title = element_textbox_simple(
        size = base_size * 1.0, face = "bold", lineheight = 1.1,
        margin = margin(0, 0, 5, 0)
      ),
      plot.subtitle = element_textbox_simple(
        size = base_size * 0.7, face = "bold", lineheight = 1.1,
        margin = margin(0, 0, 10, 0)
      ),
      plot.caption = element_textbox_simple(
        size = base_size * 0.6, colour = cowboysilver, lineheight = 1.1,
        margin = margin(t = 10)
      ),
      axis.text.y.right = element_text(hjust = 0, margin = margin(l = 6, r = 0)),
      plot.margin = margin(t = 8, r = 8, b = 8, l = 8)
    )

  # Per-chart phone adjustments supplied by the caller
  for (piece in phone) phone_plot <- phone_plot + piece

  # Heights not given are worked out from the rows (see cwr_fit_height() above).
  # The phone version is measured on its own, after its adjustments, because
  # they can change its shape - stacking side-by-side panels doubles the rows.
  # A chart with no category axis and no height given falls back to 5 in, and
  # 90% of that on a phone.
  if (is.null(height)) {
    height <- cwr_fit_height(plot, width, row_height, dpi) %||% 5
  }
  if (is.null(phone_height)) {
    phone_height <- cwr_fit_height(phone_plot, phone_width, phone_row_height, dpi) %||%
      (height * 0.9)
  }

  ggsave(desktop_file, plot, width = width, height = height,
         dpi = dpi, bg = "white", device = ragg::agg_png)

  # Shrink text geoms for the phone. Layers are ggproto objects, which behave
  # like references: the phone plot and the desktop plot share them. So the
  # desktop PNG is saved first (above), the sizes are changed in place for the
  # phone PNG, and then put back so the caller's plot is left as it was.
  text_layers <- keep(phone_plot$layers, function(layer) {
    (inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomLabel")) &&
      !is.null(layer$aes_params$size)
  })
  original_sizes <- map(text_layers, \(layer) layer$aes_params$size)
  restore_sizes <- function() {
    walk2(text_layers, original_sizes, \(layer, size) layer$aes_params$size <- size)
  }
  on.exit(restore_sizes(), add = TRUE)
  walk(text_layers, \(layer) layer$aes_params$size <- layer$aes_params$size * phone_text_scale)

  ggsave(phone_file, phone_plot, width = phone_width, height = phone_height,
         dpi = dpi, bg = "white", device = ragg::agg_png)

  # Drafting, there is no document for the HTML below to go into, so cat()ing it
  # would print tags to the console and show no chart at all. Draw the PNGs into
  # the graphics device instead and they appear in RStudio's Plots pane.
  #
  # The finished PNG rather than the plot object on purpose: printing `plot`
  # would redraw it at whatever shape the pane happens to be, and this house
  # style pins label positions to a fixed size, so the pane would show a chart
  # with the labels in the wrong places. grid.raster draws what ragg actually
  # produced, at the real proportions. Use the Plots pane's zoom button to see
  # either one full size.
  #
  # Both versions are drawn, one page each, so running the chunk shows the
  # desktop chart and the phone chart without setting any argument. RStudio
  # records each page as its own plot, so the pane's back and forward arrows
  # move between them. Checking the phone version is not optional - it is the
  # one most readers see, and it is the one that breaks - so it should not need
  # asking for.
  #
  # Whichever is drawn last is the one left showing. `draft_phone` picks it:
  # FALSE (the default) leaves the desktop version on top with the phone version
  # one arrow back, TRUE swaps them.
  if (drafting) {
    if (requireNamespace("png", quietly = TRUE)) {
      order <- if (isTRUE(draft_phone)) c(desktop_file, phone_file) else c(phone_file, desktop_file)
      walk(order, function(file) {
        grid.newpage()
        grid.raster(png::readPNG(file))
      })
    } else {
      message('Install the "png" package to see the charts here: install.packages("png")')
    }
    message("Drafting, so nothing was written to the post. Both versions are in the ",
            "Plots pane - use its back and forward arrows to move between them.",
            "
  desktop: ", desktop_file,
            "
  phone:   ", phone_file)
    return(invisible(list(desktop = desktop_file, phone = phone_file, numbered = number)))
  }

  # The width attributes are the CSS-pixel sizes (inches x 96) so the browser
  # shows each image at its drawn size and only shrinks it if the column is
  # narrower. Escape quotes in the alt text so the HTML stays valid.
  alt <- str_replace_all(alt, '"', "&quot;")

  # Bootstrap's .figure-img adds margin-bottom: 0.5rem to separate an image from
  # the caption below it. With no caption that is just a gap, so the class only
  # goes on a numbered figure, which is the case that can carry one.
  img_class <- if (number) "img-fluid figure-img" else "img-fluid"

  picture <- paste0(
    "<picture>\n",
    '<source media="(max-width: 767px)" srcset="', phone_file, '" ',
    'width="', round(phone_width * 96), '" height="', round(phone_height * 96), '">\n',
    '<img src="', desktop_file, '" alt="', alt, '" class="', img_class, '" ',
    'width="', round(width * 96), '" height="', round(height * 96), '">\n',
    "</picture>\n"
  )

  if (number) {
    # A div named #fig-... is what makes Quarto number the chart and resolve
    # @fig-... in the prose. The last paragraph inside such a div becomes the
    # caption, so with no caption the div holds the picture alone and Quarto
    # renders the bare label by itself (marked .quarto-uncaptioned).
    cat("::: {#", id, "}\n", picture, sep = "")
    if (!is.null(caption)) cat("\n", caption, "\n", sep = "")
    cat(":::\n")
  } else {
    # No div: no number, no figure block, just the chart. A caption here is a
    # plain paragraph wearing Quarto's caption class so it is styled the same.
    cat(picture)
    if (!is.null(caption)) {
      cat('\n<p class="figure-caption">', caption, "</p>\n", sep = "")
    }
  }

  invisible(list(desktop = desktop_file, phone = phone_file, numbered = number))
}

# Session information for the Reproducibility box at the end of each post.
# sessioninfo::session_info() would also print the pandoc and quarto install
# paths, which expose the local user name and folder layout, so this prints
# only what a reader needs: R version, OS, date, and attached package versions.
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
cwr_sources_table <- function(slug, readme = here::here("posts", slug, "README.md")) {
  if (!file.exists(readme)) {
    stop("No README.md for post '", slug, "' at ", readme, call. = FALSE)
  }
  lines <- read_lines(readme)

  # Everything under the "## Data sources" heading, stopping at the next heading
  start <- which(str_trim(lines) == "## Data sources")
  if (length(start) == 0) {
    stop("README.md for '", slug, "' has no '## Data sources' heading", call. = FALSE)
  }
  after <- lines[(start[1] + 1):length(lines)]
  next_heading <- which(str_starts(str_trim(after), "## "))
  section <- if (length(next_heading) > 0) after[seq_len(next_heading[1] - 1)] else after

  # The table is the run of pipe-delimited lines in that section
  table_lines <- section[str_starts(str_trim(section), fixed("|"))]
  if (length(table_lines) == 0) {
    stop("No table under '## Data sources' in ", readme, call. = FALSE)
  }

  cat(table_lines, sep = "\n")
  cat("\n")
  invisible(table_lines)
}

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
