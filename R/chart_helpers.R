# chart_helpers.R
# ---------------------------------------------------------------------------
# The pieces a chart is built from: the source line under it, the labels drawn
# inside it, and the small calculations they need.
#
# Sourced by R/theme_cwr.R, which every post sources in turn - so a post never
# names this file. Split out of theme_cwr.R on 2026-09-26, unchanged.
# ---------------------------------------------------------------------------

# ---- 5. Helpers ----------------------------------------------------------
# Every note on a chart is written out in the post's own chunk, as plain text in
# cwr_caption(notes = ), so Greg can reword it or delete it where he reads it
# (his rule, 2026-09-21). Nothing here adds a note behind his back. The stock
# wording for the notes that recur - the CMA geography, a survey sample,
# confidence intervals, a share not shown - is kept in the cwr-charts skill (rule 1d),
# which is where a new chart copies it from. A note with a number that comes
# from the data writes the sentence out and leaves a {placeholder} for the
# number, filled by str_glue(); cwr_ci_widest() below is one such number.

# The widest 95% confidence interval on a chart, in percentage points, for the
# note "95% confidence intervals are within ±{widest} percentage points" (style
# rule 9b): measured from the data rather than typed, and rounded up to a whole
# point, or to a tenth when every interval is under one point. Pass the plotted
# shares and their bounds, in percent.
cwr_ci_widest <- function(estimate, lower, upper) {
  widest <- max(estimate - lower, upper - estimate, na.rm = TRUE)
  if (widest < 1) ceiling(widest * 10) / 10 else ceiling(widest)
}

# The multiplier for a gap written in data units, so the same gap can be a
# different number of data units on the phone. A phone draws the same x scale
# across half the width, so a label nudged clear of its point on a desktop is
# half as far from it there, and can touch it. Write such a nudge as
# `nudge * cwr_gap()` inside the aes, and pass `phone_gap` to cwr_figure():
# it is 1 while the desktop version is drawn and `phone_gap` while the phone
# version is (2 keeps the gap the same distance on the page). Vertical nudges
# on a category axis need none of this: a row is about as deep on both.
# Worked case: the year labels on fig-work-at-home in the commuting post.
cwr_gap <- function() getOption("cwr.gap", 1)

# A text label inside the plot area: the house version of geom_text()
# (Greg, 2026-09-23).
#
# Anything drawn over the panel - a value beside a dot, a year above a dumbbell,
# a series name on a line chart, a place name on a map - sooner or later lands
# on a gridline, a line or another point, and plain text is unreadable there.
# The old fix was geom_label() with fill = "white" and linewidth = 0, but a
# label is a box: it blots out a rectangle rather than the letters, its padding
# shoves the text off the point it marks, and the padding has to be tuned chart
# by chart. shadowtext draws a halo of the background colour around each glyph
# instead, so only the letters clear their own space and the label sits exactly
# where geom_text() would have put it.
#
# Takes everything geom_text() takes - aes(), data, size, colour, fontface,
# hjust, vjust, nudge_x, nudge_y, lineheight - plus the halo:
#   bg.colour  its colour: white, to match the chart background. Pass the fill
#              behind the text where that is not white, or NA for no halo.
#   bg.r       its thickness, as a share of the text size, so it shrinks with
#              the label on the phone version.
#
#   cwr_label(
#     data = top_row,
#     aes(x = percent, y = district, label = year),
#     colour = "grey30", size = label_size * 0.8, fontface = "bold"
#   )
#
# Text drawn inside a filled bar stays geom_text(): the bar is its background,
# and a white halo around white text would eat it. cwr_figure() shrinks these
# labels for the phone exactly as it shrinks geom_text() ones.
cwr_label <- function(..., bg.colour = "white", bg.r = 0.15) {
  geom_shadowtext(..., bg.colour = bg.colour, bg.r = bg.r)
}

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
# Every note is written out in full in the chunk, including the recurring ones
# (the CMA geography, a sample, confidence intervals): the stock wording is in
# the cwr-charts skill, rule 1d, to be copied, so Greg can edit or delete any
# note where he sees it. A note about the source - the sample note - goes last,
# directly above the source line.
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
cwr_caption <- function(source, credit = FALSE, notes = NULL) {
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
#                    labels = c(`In their own district` = "HOME DISTRICT", ...),
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
# through it. The label is a cwr_label(), whose halo keeps the letters legible
# where they land, so `gap` (extra space, as a share of the y range) is 0
# unless a chart needs more. `span` should be about the label's width on the
# phone version, which is the wider of the two in x units: 3 suits a short name
# over 25 to 30 years.
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
#   not an order. A label's halo breaks every gridline it crosses, so both
#   sides are checked, and the other side is taken when it is clear and the
#   preferred side is not. A side where another group's line runs
#   through the label, or where the label would cover one already placed, is
#   ruled out first, whatever the gridlines do. Ties go to the preferred
#   side. For these checks the helper needs:
#   `limits`       the y scale's limits, as given to scale_y_continuous()
#                  (NA for a limit the data set). The gridlines are the
#                  breaks ggplot2 draws for that range.
#   `breaks`       the gridlines themselves, if the chart sets its own.
#   `label_height` the label's height as a share of the y range. 0.05
#                  fits the templates' 3.2 mm label on a 5 in line chart,
#                  desktop and phone alike; raise it for a shorter chart.
#   Set `avoid_gridlines = FALSE` to ignore gridlines altogether.
#
# Returns one row per label with x, y, label, vjust, side and fontface, for
# the templates' cwr_label() layer with `aes(vjust = vjust)` and hjust = 0.5.
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
    # A label on top of another label is never readable, so staying clear of
    # the labels already placed comes first, then staying off other lines.
    # (It was the other way round until 2026-09-25, when the Vital
    # Statistics page printed "Region" over "Canada": a spot clear of the
    # lines beat one clear of the labels. Where a clear spot for both exists
    # the choice is the same either way.)
    options <- if (fixed) {
      arrange(options, hits_label, hits_line, outside, hits_grid, desc(preferred))
    } else {
      arrange(options, hits_label, hits_line, outside, hits_grid, float, desc(preferred), x)
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

# ggplot2 text sizes for geom_text/geom_label/cwr_label are in mm, not points.
# 4 mm is roughly 11 pt, the standard label size used across the templates
# (scaled up with base_size so direct labels stay readable on a phone).
label_size <- 4
