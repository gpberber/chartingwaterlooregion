# theme_cwr.R
# ---------------------------------------------------------------------------
# House chart style for Charting Waterloo Region.
#
# Every post sources this file once in its setup chunk:
#
#   source(here::here("R", "theme_cwr.R"))
#
# It does five things:
#   1. loads the packages every post needs,
#   2. defines the house colours (the same hex values as custom.scss),
#   3. defines and sets theme_cwr(), the Tufte-inspired ggplot2 theme,
#   4. defines small helpers (caption text, font lookup),
#   5. sources R/maps.R, the house map style.
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

# The house map style: the projection, label points and nudges, the map theme.
# Kept in its own file because cleaning scripts need part of it and do not
# load the rest of the theme.
source(here::here("R", "maps.R"))

# Print numbers without a thousands separator by default (scales >= 1.3).
# Individual scales override this with label_number(big.mark = ",").
number_options(big.mark = "")

# ---- 2. House colours ----------------------------------------------------
# Three base colours, each with a 50 % tint (mixed with white). Use them with
# consistent meaning across posts so readers learn the code:
#   dodgerblue   = the focus (the Region - see cwr_region below - WRPS, Kitchener)
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

# The stock label for Waterloo Region as a whole in chart text: axis and
# category labels, legend keys, direct labels, tooltips and caption notes. The
# whole site is about Waterloo Region, so "Region" is enough on a chart and
# leaves room for the data; the capital R keeps it distinct from the region
# of an ordinary sentence and from the City of Waterloo. Use the object, not
# the typed word, so a chart cannot drift back to the long name. Titles and
# subtitles are the author's prose and are not bound by it. (A post's
# data-cleaning script, which does not load this file, types "Region" and
# says why in a comment.)
cwr_region <- "Region"

# Municipal names too long for a chart's label column or for the shape they
# label on a map, and the short form every chart uses instead (Greg,
# 2026-09-18). Chart text only - axis labels, direct labels, map labels,
# legend keys, tooltips, caption notes: the same reach as cwr_region. Data
# files, alt text, titles, subtitles and prose keep
# the full name, so a screen reader and a downloaded table still say "North
# Dumfries". Add a name here and every chart that passes its labels through
# cwr_short_name() picks it up.
cwr_short_names <- c("North Dumfries" = "N. Dumfries")

# Shorten any of those names wherever they appear in `x`. Works as a labeller -
# scale_y_discrete(labels = cwr_short_name) - or on a label column in mutate().
# Matched as whole words, so a longer name that merely contains one is left
# alone.
cwr_short_name <- function(x) {
  str_replace_all(
    as.character(x),
    set_names(cwr_short_names, paste0("\\b", names(cwr_short_names), "\\b"))
  )
}

# Named palettes for the comparisons that recur across posts. The Region's
# entry is named by cwr_region, so data labelled with it matches.
comp_colours <- set_names(
  c(dodgerblue, habsred, ontgreen, cowboysilver50),
  c(cwr_region, "Canada", "Ontario", "Other cities")
)

local_colours <- set_names(
  c(dodgerblue, dodgerblue50, cowboysilver, habsred),
  c(cwr_region, "Guelph", "London", "Hamilton")
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
cwr_cma_note <- paste(cwr_region, "excluding Wellesley Township")

# The stock notes for charts drawn from sample data, one per sample, picked by
# name with cwr_caption(sample = ). A reader is told every time a chart's numbers
# are estimates from a sample rather than a full count, in the same words every
# time, so the note is recognised at a glance and one edit fixes every chart.
#
# The census long form goes to one household in four; the short form (age,
# gender, households, dwellings, most language questions) is a full count and
# needs no note. Add a survey the first time a post uses it, with the sample
# size taken from the survey's own documentation.
cwr_sample_notes <- c(
  census_2021 = "Estimates from the 2021 census long-form questionnaire, a 25% sample of households",
  census_2016 = "Estimates from the 2016 census long-form questionnaire, a 25% sample of households"
)

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
# `sample` names the sample a chart's numbers come from - one of the names in
# `cwr_sample_notes`, such as "census_2021" - and puts its stock note at the end
# of the notes, directly above the source line, since it is a note about the
# source. Numbered with the rest, so it too needs its key in the subtitle. Any
# confidence interval the chart draws or states goes in `notes` as usual.
#
# The notes are drawn above the source line, which is where a reader looks for a
# qualification and where Datawrapper puts them, with a blank line between the two
# so the qualifications do not read as part of the source:
#
#   cwr_caption(
#     "Statistics Canada Table 14-10-0468-01",
#     credit = TRUE,
#     notes = c("Figures for 2020 are estimates",
#               "Two industries suppressed by Statistics Canada")
#   )
cwr_caption <- function(source, credit = FALSE, notes = NULL, cma = FALSE, sample = NULL) {
  if (cma) notes <- c(cwr_cma_note, notes)
  if (!is.null(sample)) {
    if (!sample %in% names(cwr_sample_notes)) {
      stop("No stock note for sample '", sample, "'. Add it to cwr_sample_notes in R/theme_cwr.R.",
           call. = FALSE)
    }
    notes <- c(notes, cwr_sample_notes[[sample]])
  }

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

# Name the segments of a horizontal stacked bar on the bars themselves, the way
# the New York Times labels one, instead of in a legend (Greg, 2026-09-18). Each
# name then sits beside the colour it names, and there is no key to look away
# to. The first segment's name goes above the top bar, starting where the bar
# starts; the last segment's goes above the top bar, ending where the bar ends;
# any segment in between is named under the bottom bar, centred on its own
# segment there and joined to it by a short tick. Names are in capitals.
#
# Built from the data, so the names follow their segments when the numbers
# change - never type their coordinates. It returns layers, added with `+`:
#
#   p <- ggplot(bars, aes(x = percent, y = district, fill = destination)) +
#     geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
#     cwr_stack_keys(bars, percent, district, destination,
#                    labels = c(`In their own municipality` = "HOME DISTRICT", ...),
#                    colours = destination_colours) +
#     scale_fill_manual(values = destination_colours, guide = "none") +
#     scale_y_discrete(expand = expansion(add = c(1.1, 0.9))) +
#     coord_cartesian(clip = "off")
#
# What the chart itself must do:
#   - stack with position_stack(reverse = TRUE), so the first level of `fill`
#     is at the left, and pass the same `width` as geom_col();
#   - `y` and `fill` must be factors, in the order drawn (bottom bar first);
#   - reserve a row above and below for the names, with
#     scale_y_discrete(expand = expansion(add = c(1.1, 0.9))) - cwr_figure()
#     counts it into the height - and turn the legend off;
#   - no baseline of its own: this draws one, from just under the bottom bar
#     to the top edge of the top one, so it cannot run up beside the first
#     name. Drop it with `baseline = FALSE`.
#
# The outer names are set in their segment's colour. The in-between names sit
# on white under the bar rather than against it, where a pale segment colour
# is too faint to read, so they are grey30, the house text grey.
cwr_stack_keys <- function(data, x, y, fill, labels, colours, width = 0.7,
                           middle_colour = "grey30", baseline = TRUE) {
  segments <- data |>
    mutate(.x = {{ x }}, .y = {{ y }}, .fill = {{ fill }}) |>
    arrange(.y, .fill) |>
    # Where each segment starts and ends along its bar
    mutate(xmax = cumsum(.x), xmin = xmax - .x, .by = .y)

  fill_levels <- levels(pull(segments, .fill))
  top_row <- nlevels(pull(segments, .y))
  half <- width / 2
  first <- fill_levels[1]
  last <- fill_levels[length(fill_levels)]
  middle <- setdiff(fill_levels, c(first, last))

  # A category axis puts the first level at y = 1 and the last at y = the
  # number of rows; each bar reaches `half` either side of that
  keys <- bind_rows(
    segments |>
      filter(as.integer(.y) == top_row, .fill == first) |>
      mutate(x = xmin, y = top_row + half, hjust = 0, vjust = -0.5),
    segments |>
      filter(as.integer(.y) == top_row, .fill == last) |>
      mutate(x = xmax, y = top_row + half, hjust = 1, vjust = -0.5),
    segments |>
      filter(as.integer(.y) == 1, .fill %in% middle) |>
      mutate(x = (xmin + xmax) / 2, y = 1 - half - 0.2, hjust = 0.5, vjust = 1.2)
  ) |>
    mutate(
      label = unname(labels[as.character(.fill)]),
      colour = if_else(.fill %in% middle, middle_colour, unname(colours[as.character(.fill)]))
    ) |>
    select(label, x, y, hjust, vjust, colour)

  ticks <- keys |> filter(y < 1)

  list(
    if (baseline) {
      annotate("segment", x = 0, xend = 0, y = 0.5, yend = top_row + half,
               colour = "black", linewidth = 0.5)
    },
    geom_segment(
      data = ticks, aes(x = x, xend = x, y = 1 - half, yend = y),
      inherit.aes = FALSE, colour = "grey50", linewidth = 0.3
    ),
    # I() takes each colour as given rather than through the chart's colour
    # scale, so the names cannot collide with a scale the chart already has
    geom_text(
      data = keys,
      aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust, colour = I(colour)),
      inherit.aes = FALSE, size = label_size * 0.8
    )
  )
}

# Place line-chart labels against their own lines.
#
# A label typed at a hand-picked (x, y) drifts away from its line: the eye
# guesses the height, the data are revised, and the phone version draws the
# same label twice as wide relative to the chart. This helper takes the
# height from the data instead. A label is placed against the highest (above)
# or lowest (below) point its line reaches across `span` x units around the x
# it is centred on, so a line that slopes under the label does not cut
# through it. geom_label()'s own padding already leaves a sliver of air, so
# `gap` (extra space, as a share of the y range) is 0 unless a chart needs
# more. `span` should be about the label's width on the phone version, which
# is the wider of the two in x units: 3 suits a short name over 25 to 30
# years.
#
# Where each label goes along the line:
#   - `at` names a group and the x to centre its label on - a stretch where
#     that line has clear space and is fairly flat (over a jagged stretch
#     the label clears the far peak and floats off the near point).
#   - A group left out of `at` (or `at = NULL` for all of them) is placed
#     automatically. Every x along its line is tried, on both sides, and the
#     best spot wins, judged in this order: no other line runs through the
#     label; it does not cover a label already placed; it stays inside the
#     panel; it covers no gridline; it sits as close to its own line as
#     possible (a flat stretch); then the preferred side. The rightmost
#     `right_margin` of the x range is skipped, because the templates draw the
#     value axis's labels inside the panel there. Two labels count as
#     overlapping when their centres are less than `span * label_spacing`
#     apart (and their heights overlap): the 25% margin keeps phone labels,
#     which run a little wider than `span`, from touching. Groups named in `bold` are
#     placed first, so the focus gets the best spot, then the rest in the
#     order of `at` and the data.
#
# Which side of the line:
#   `side` ("above"/"below", one for all or one per group) is a preference,
#   not an order. A label's white box blots out any gridline it covers, so
#   both sides are checked, and the other side is taken when it is clear and
#   the preferred side is not. A side where another group's line runs
#   through the label, or where the label would cover one already placed, is
#   ruled out first, whatever the gridlines do. Ties go to the preferred
#   side. For these checks the helper needs:
#   `limits`       the y scale's limits, as given to scale_y_continuous()
#                  (NA for a limit the data set). The gridlines are the
#                  breaks ggplot2 draws for that range.
#   `breaks`       the gridlines themselves, if the chart sets its own.
#   `label_height` the label box's height as a share of the y range. 0.05
#                  fits the templates' 3.2 mm label on a 5 in line chart,
#                  desktop and phone alike; raise it for a shorter chart.
#   Set `avoid_gridlines = FALSE` to ignore gridlines altogether.
#
# Returns one row per label with x, y, label, vjust, side and fontface, for
# the templates' geom_label() layer with `aes(vjust = vjust)` and hjust = 0.5.
# x must be numeric (years); for a date axis pass as.numeric(date) and `at`
# as numbers too. For a faceted chart, call it once per panel (the checks
# should only see that panel's lines) and add the facet column to each result
# with mutate() so each label stays in its panel.
#
#   label_data <- cwr_line_labels(
#     plot_data, x = year, y = csi, group = region,
#     at = c(Canada = 2004, Ontario = 2014, Region = 2002),
#     side = c(Canada = "above", Ontario = "below", Region = "below"),
#     limits = c(45, NA),
#     bold = cwr_region
#   )
#
#   # every label placed automatically
#   label_data <- cwr_line_labels(
#     plot_data, x = year, y = rate, group = region,
#     limits = c(0, NA), bold = cwr_region
#   )
cwr_line_labels <- function(data, x, y, group, at = NULL, side = "above",
                            bold = NULL, span = 3, gap = 0,
                            limits = NULL, breaks = NULL,
                            label_height = 0.05, avoid_gridlines = TRUE,
                            right_margin = 0.12, step = 0.5,
                            label_spacing = 1.25) {
  lines <- data |>
    select(x = {{ x }}, y = {{ y }}, group = {{ group }}) |>
    mutate(x = as.numeric(x), group = as.character(group)) |>
    filter(!is.na(y))
  groups <- unique(lines$group)

  # One side for every label, or one per group; "above" where not given
  if (is.null(names(side))) side <- set_names(rep(side, length(groups)), groups)
  side <- c(side, set_names(rep("above", length(groups)), groups))[groups]
  stopifnot(
    is.null(at) || !is.null(names(at)),
    all(names(at) %in% groups),
    all(side %in% c("above", "below"))
  )

  # The y range the chart shows: the scale's limits where given, the data's
  # range where not. Label height and gap are shares of it. The panel's top
  # also has the templates' 5% expansion.
  y_range <- range(lines$y)
  if (!is.null(limits)) y_range <- coalesce(as.numeric(limits), y_range)
  y_span <- diff(y_range)
  y_gap <- gap * y_span
  box_height <- label_height * y_span
  panel <- c(y_range[1], y_range[2] + 0.05 * y_span)

  # The gridlines: ggplot2's default breaks for that range, unless given
  if (is.null(breaks)) breaks <- scales::breaks_extended()(y_range)
  breaks <- breaks[!is.na(breaks) & breaks >= y_range[1] & breaks <= y_range[2]]
  if (!avoid_gridlines) breaks <- numeric(0)

  # Where a line runs across the label's width: every point inside the
  # window, plus where the line crosses the window's two edges.
  heights_in <- function(line, window) {
    c(
      line |> filter(between(x, window[1], window[2])) |> pull(y),
      approx(line$x, line$y, xout = window, rule = 2)$y
    )
  }
  line_of <- map(set_names(groups), \(g) lines |> filter(group == g) |> arrange(x))

  # Candidate x positions for a group placed automatically: its own line's
  # extent, less half a label at each end and the axis labels' strip.
  x_range <- range(lines$x)
  auto_xs <- function(g) {
    own_x <- range(line_of[[g]]$x)
    seq(
      own_x[1] + span / 2,
      min(own_x[2], x_range[2] - right_margin * diff(x_range)) - span / 2,
      by = step
    )
  }

  # Everything that decides a spot, for one group at one x on one side.
  # `placed` holds the boxes of the labels already chosen.
  placed <- tibble(x = numeric(0), bottom = numeric(0), top = numeric(0))
  score <- function(g, x_c, where) {
    window <- x_c + c(-span, span) / 2
    own <- heights_in(line_of[[g]], window)
    y_pos <- if (where == "above") max(own) + y_gap else min(own) - y_gap
    box <- if (where == "above") c(y_pos, y_pos + box_height) else c(y_pos - box_height, y_pos)
    others <- map(line_of[names(line_of) != g], \(l) range(heights_in(l, window)))
    centre <- approx(line_of[[g]]$x, line_of[[g]]$y, xout = x_c, rule = 2)$y
    tibble(
      x = x_c,
      side = where,
      y = y_pos,
      bottom = box[1],
      top = box[2],
      hits_line = any(map_lgl(others, \(r) r[1] <= box[2] && r[2] >= box[1])),
      hits_label = any(abs(placed$x - x_c) < span * label_spacing &
                         placed$bottom < box[2] & placed$top > box[1]),
      outside = box[1] < panel[1] || box[2] > panel[2],
      hits_grid = any(breaks > box[1] & breaks < box[2]),
      float = abs(y_pos - centre),
      # read out of `side` here: inside tibble(), `side` is the column above
      preferred = where == preferred_side
    )
  }

  # Placement order: the bold (focus) groups first, then `at`, then the rest
  order <- unique(c(intersect(bold, groups), names(at), groups))

  result <- list()
  for (g in order) {
    preferred_side <- side[[g]]
    fixed <- g %in% names(at)
    xs <- if (fixed) at[[g]] else auto_xs(g)
    options <- map(xs, \(x_c) list_rbind(map(c("above", "below"), \(w) score(g, x_c, w)))) |>
      list_rbind()
    options <- if (fixed) {
      arrange(options, hits_line, hits_label, outside, hits_grid, desc(preferred))
    } else {
      arrange(options, hits_line, hits_label, outside, hits_grid, float, desc(preferred), x)
    }
    pick <- slice(options, 1)
    placed <- bind_rows(placed, select(pick, x, bottom, top))
    result[[g]] <- tibble(
      x = pick$x,
      y = pick$y,
      label = g,
      vjust = if (pick$side == "above") 0 else 1,
      side = pick$side,
      fontface = if (g %in% bold) "bold" else "plain"
    )
  }

  list_rbind(result)
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

# The theme changes every phone version of a chart gets, because its panel is
# half as wide: titles and the caption wrap, and right-hand axis labels move
# outside the panel. Shared by cwr_figure() and cwr_interactive().
#
# `width` is the width of the phone image in inches. The title, subtitle and
# caption boxes are given a fixed width worked out from it - the image less the
# plot margin either side - rather than ggtext's default of "the whole width of
# wherever it is drawn". The default measured and drew the text against two
# different widths: ggplot sizes the title's row with the box measured against
# the full image (4.2 in), then draws it in its cell, which is the image less
# the 8 pt margins (3.98 in). A line between those two widths therefore fitted
# when measured and wrapped when drawn, so the row was one line short: the
# second line landed on the subtitle and the first was cut off at the top of
# the PNG. "Pigs on farms in the four townships, 2021" (4.19 in of bold Inter)
# was the case that showed it (2026-09-18). A fixed width is the same number
# both times, so the row always fits what is drawn.
#
# The margins are taken from here too, so the two cannot drift apart. A chart
# that changes plot.margin in `phone = ` keeps these box widths; a wider side
# margin there would bring the problem back and needs the width to match.
cwr_phone_theme <- function(width = 4.2) {
  side_margin <- 8   # points, left and right
  box_width <- unit(width, "in") - unit(2 * side_margin, "pt")
  theme(
    plot.title = element_textbox_simple(
      size = base_size * 1.0, face = "bold", lineheight = 1.1,
      width = box_width,
      margin = margin(0, 0, 5, 0)
    ),
    plot.subtitle = element_textbox_simple(
      size = base_size * 0.7, face = "bold", lineheight = 1.1,
      width = box_width,
      margin = margin(0, 0, 10, 0)
    ),
    plot.caption = element_textbox_simple(
      size = base_size * 0.6, colour = cowboysilver, lineheight = 1.1,
      width = box_width,
      margin = margin(t = 10)
    ),
    axis.text.y.right = element_text(hjust = 0, margin = margin(l = 6, r = 0)),
    plot.margin = margin(t = 8, r = side_margin, b = 8, l = side_margin)
  )
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
  phone_plot <- plot + cwr_phone_theme(phone_width)

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

# ---- Interactive charts ------------------------------------------------------
# cwr_interactive() is cwr_figure() for a chart that shows a tooltip on hover.
# It uses ggiraph, which draws an ordinary ggplot as an SVG in the page, so the
# chart keeps theme_cwr() and the house colours; only the layers that should
# react to the mouse change, from geom_point() to geom_point_interactive() and
# so on, with a `tooltip` aesthetic holding the text to show. (plotly's
# ggplotly() was the alternative, but it redraws the chart in its own style and
# drops ggtext's markdown titles and captions.)
#
# Like cwr_figure(), it draws the chart twice - 8.3 in wide for desktops and
# 4.2 in wide for phones, with cwr_phone_theme() and `phone` added to the
# second - and custom.scss shows one or the other at the 767px breakpoint.
# Each SVG then stretches to the width of the column, as the PNGs do.
#
# Use it in a chunk WITHOUT `output: asis`: it returns its markdown through
# knitr::asis_output(), which also hands knitr the widget's JavaScript files
# so they get into the page.
#
#   ```{r}
#   #| label: csi
#   p <- ggplot(...) + geom_line() +
#     geom_point_interactive(aes(data_id = id), size = 2.5, alpha = 0) +
#     geom_point_interactive(aes(tooltip = tip, data_id = id), shape = 15, size = 7, alpha = 0)
#
# Layer order matters: the dots first, the squares over them. The topmost
# element under the mouse is the one that fires, and a dot on top (it has no
# tooltip) would swallow the hover at the exact point a reader aims for.
#   cwr_interactive(p, "fig-csi", alt = "Line chart showing ...",
#     height = 5, phone_height = 4.5)
#   ```
#
# Drafting (running the chunk in RStudio), it returns the desktop widget, which
# RStudio shows in the Viewer pane; `draft_phone = TRUE` shows the phone one.
#
# Numbering and `caption` work as in cwr_figure(): a Deep dive gets the two
# widgets inside a Quarto #fig- div, so "Figure 1" appears under the chart and
# @fig-csi works in the prose; a Snapshot gets them bare. `number = TRUE` or
# `FALSE` overrides the post type for one chart.
cwr_interactive <- function(plot, id, alt, caption = NULL, number = NULL,
                            width = 8.3, height = 5,
                            phone_width = 4.2, phone_height = height * 0.9,
                            phone = list(), phone_text_scale = 0.8,
                            draft_phone = FALSE) {
  stopifnot(str_starts(id, "fig-"))

  # Snapshot or Deep dive, read from the post's categories (see cwr_figure())
  if (is.null(number)) {
    categories <- unlist(rmarkdown::metadata$categories)
    number <- length(categories) > 0 && identical(categories[[1]], "Deep dive")
  }

  phone_plot <- plot + cwr_phone_theme(phone_width)
  for (piece in phone) phone_plot <- phone_plot + piece

  # What every hover does: the tooltip in the house font on a white card, and
  # round points sharing the hovered data_id made visible. A line chart draws
  # two invisible layers (alpha = 0) with the same data_id: large squares
  # (shape 15) that catch the mouse anywhere near the line, and small circles
  # that mark the year. girafe_css(point = ) styles circles only, so the
  # squares stay invisible while the circle under them appears.
  widget_options <- list(
    ggiraph::opts_tooltip(
      css = paste0(
        "font-family: Inter, sans-serif; font-size: 13px; line-height: 1.35;",
        "background: white; color: #222; padding: 6px 9px;",
        "border: 1px solid ", cowboysilver50, "; border-radius: 4px;"
      ),
      opacity = 1, use_fill = FALSE
    ),
    ggiraph::opts_hover(css = ggiraph::girafe_css(
      css = "", point = "fill-opacity: 1; stroke-opacity: 1;"
    )),
    ggiraph::opts_hover_inv(css = ""),
    ggiraph::opts_sizing(rescale = TRUE, width = 1),
    ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc")),
    ggiraph::opts_selection(type = "none")
  )

  # Fonts. Left to its default, girafe() attaches the three Liberation font
  # families to the page (18 MB, committed in _freeze/ and published) though
  # the charts never use them; naming Inter instead attaches the installed
  # Inter files (38 MB). The site already serves Inter from fonts/, so the
  # font set names Inter and its attached files are dropped.
  font_set <- gdtools::font_set(sans = cwr_font())
  font_set$dependencies <- list()

  make_widget <- function(p, w, h) {
    ggiraph::girafe(
      ggobj = p, width_svg = w, height_svg = h,
      bg = "white", options = widget_options,
      font_set = font_set
    )
  }

  desktop_widget <- make_widget(plot, width, height)

  # geom_text/geom_label sizes are shrunk for the phone exactly as in
  # cwr_figure(): changed in place, drawn, then put back.
  text_layers <- keep(phone_plot$layers, function(layer) {
    (inherits(layer$geom, "GeomText") || inherits(layer$geom, "GeomLabel")) &&
      !is.null(layer$aes_params$size)
  })
  original_sizes <- map(text_layers, \(layer) layer$aes_params$size)
  walk(text_layers, \(layer) layer$aes_params$size <- layer$aes_params$size * phone_text_scale)
  phone_widget <- make_widget(phone_plot, phone_width, phone_height)
  walk2(text_layers, original_sizes, \(layer, size) layer$aes_params$size <- size)

  drafting <- !isTRUE(getOption("knitr.in.progress")) ||
    isTRUE(getOption("rstudio.notebook.executing"))
  if (drafting) {
    return(if (isTRUE(draft_phone)) phone_widget else desktop_widget)
  }

  # role="img" plus aria-label gives screen readers the alt text, as the
  # PNG's alt attribute does for cwr_figure(). A Snapshot's outer div carries
  # the id; in a Deep dive the Quarto div below carries it instead, since an
  # id can appear only once on a page.
  widgets <- htmltools::div(
    id = if (number) NULL else id,
    htmltools::div(
      class = "cwr-interactive cwr-interactive-desktop",
      role = "img", `aria-label` = alt, desktop_widget
    ),
    htmltools::div(
      class = "cwr-interactive cwr-interactive-phone",
      role = "img", `aria-label` = alt, phone_widget
    )
  )

  # renderTags() turns the widgets into one HTML string plus the list of
  # JavaScript files they need. The HTML goes into a ```{=html} block, which
  # Pandoc passes through untouched (a bare HTML block would end at the first
  # blank line inside the widget), and the files go to knitr through `meta`.
  rendered <- htmltools::renderTags(widgets)
  html <- paste0("```{=html}\n", rendered$html, "\n```\n")

  # The same figure logic as cwr_figure(): a div named #fig-... makes Quarto
  # number the chart and resolve @fig-... references, and the last paragraph
  # inside it becomes the caption.
  markdown <- if (number) {
    paste0(
      "::: {#", id, "}\n", html,
      if (!is.null(caption)) paste0("\n", caption, "\n"),
      ":::\n"
    )
  } else {
    paste0(
      html,
      if (!is.null(caption)) paste0('\n<p class="figure-caption">', caption, "</p>\n")
    )
  }

  knitr::asis_output(markdown, meta = rendered$dependencies)
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
# The run of table lines under one "## " heading of a post's README. Shared by
# the two tables the post prints from its README: sources and reliability.
cwr_readme_table <- function(slug, heading, readme = here::here("posts", slug, "README.md")) {
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

  # The table is the run of pipe-delimited lines in that section
  table_lines <- section[str_starts(str_trim(section), fixed("|"))]
  if (length(table_lines) == 0) {
    stop("No table under '## ", heading, "' in ", readme, call. = FALSE)
  }
  table_lines
}

cwr_sources_table <- function(slug, readme = here::here("posts", slug, "README.md")) {
  table_lines <- cwr_readme_table(slug, "Data sources", readme)
  cat(table_lines, sep = "\n")
  cat("\n")
  invisible(table_lines)
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
cwr_reliability_table <- function(slug, readme = here::here("posts", slug, "README.md")) {
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
    cat(c("| Issue |", "|---|", str_c("| ", shown, " |")), sep = "\n")
    cat("\n")
  }
  invisible(shown)
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
