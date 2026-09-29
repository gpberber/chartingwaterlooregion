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

# cwr_label() for labels that cannot be placed by rule: ggrepel's
# geom_text_repel(), which pushes labels apart from each other and from the
# points until none overlap, drawing a thin leader line where one has moved
# far from its point. Same halo, font and grey as cwr_label().
#
# Use it only where the positions really are crowded and unpredictable: a
# scatter with many named points, a cluster of dot-plot values that collide.
# Not for line names (cwr_line_labels() places those), not for bar values, not
# for a handful of points a nudge can clear - a repelled label is less
# predictable than a placed one, and its leader lines add ink. (Greg,
# 2026-09-28: "use ggrepel where it would help, but don't overuse it.")
#
# ggrepel works out the positions as the chart is drawn, at the drawing's own
# size, so the phone version repels at phone size by itself (cwr_figure()
# shrinks this text for the phone like any other label). `seed` fixes the
# random start, so the same chart always draws the same way and re-rendering
# does not churn the committed PNGs. max.overlaps = Inf never silently drops a
# label: a chart too crowded for that needs fewer labels, not hidden ones.
#
#   cwr_label_repel(
#     data = labelled_points,
#     aes(x = income, y = rent, label = place),
#     colour = "grey30", size = label_size * 0.8
#   )
cwr_label_repel <- function(..., bg.colour = "white", bg.r = 0.15, seed = 1,
                            box.padding = 0.3, point.padding = 0.2,
                            min.segment.length = 0.3, segment.colour = "grey60",
                            segment.size = 0.3, max.overlaps = Inf) {
  ggrepel::geom_text_repel(
    ..., bg.colour = bg.colour, bg.r = bg.r, seed = seed,
    box.padding = box.padding, point.padding = point.padding,
    min.segment.length = min.segment.length, segment.colour = segment.colour,
    segment.size = segment.size, max.overlaps = max.overlaps
  )
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
# `unnumbered` takes notes that nothing in the title points to and that come
# and go with the data: "Region not shown for November 2023 because ...", a
# figure graded acceptable, a place not shown. They are written out in the
# chunk like any note, with str_glue() filling in the data, and printed after
# the numbered notes with no numeral. A note that appears or disappears when
# the data are refreshed then never renumbers the notes the title keys (Greg,
# 2026-09-28). cwr_check_note_keys() below warns at render time when the keys
# and the numbered notes disagree.
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
cwr_caption <- function(source, credit = FALSE, notes = NULL, unnumbered = NULL) {
  # The byline is a source like any other, so it decides the plural too
  label <- if (length(source) > 1 || credit) "Sources: " else "Source: "
  caption <- paste0(label, paste(source, collapse = ", "))
  if (credit) caption <- paste0(caption, " | *Charting Waterloo Region*")

  # The numbered notes, then the unnumbered ones. A note str_glue() leaves
  # empty (nothing to report this time) has length zero, so c() drops it.
  # Superscript, the way a footnote key is set, so the number reads as a
  # reference rather than as the first word of the note.
  lines <- c(
    if (length(notes) > 0) paste0("<sup>", seq_along(notes), "</sup> ", notes),
    unnumbered
  )

  if (length(lines) > 0) {
    # A blank line between the notes and the source line. <br><br> is how a
    # markdown caption spells one; the caption's lineheight decides how wide it
    # actually sits.
    caption <- paste0(paste(lines, collapse = "<br>"), "<br><br>", caption)
  }

  caption
}

# Warn at render time when a chart's note keys and its numbered notes
# disagree: a key in the title or subtitle (^2^ or <sup>2</sup>) with no note
# 2, or a numbered note that nothing points to. cwr_figure() and
# cwr_interactive() call it on every chart, so a key left behind after a note
# is deleted or added shows up in the render's output, not on the live page.
# Unnumbered notes (cwr_caption(unnumbered = )) are not counted. Printed to
# stderr, like cwr_right_axis()'s warnings, so a chunk's `warning: false`
# cannot hide it.
cwr_check_note_keys <- function(plot, id) {
  heading <- str_c(plot$labels$title %||% "", " ", plot$labels$subtitle %||% "")
  keys <- str_match_all(heading, "\\^(\\d+)\\^|<sup>(\\d+)</sup>")[[1]]
  keys <- sort(unique(as.integer(coalesce(keys[, 2], keys[, 3]))))
  # cwr_caption() starts each numbered note with its <sup>n</sup>
  notes <- str_match_all(plot$labels$caption %||% "", "<sup>(\\d+)</sup> ")[[1]][, 2]
  numbered <- seq_along(notes)

  dangling <- setdiff(keys, numbered)
  unkeyed <- setdiff(numbered, keys)
  if (length(dangling) > 0 || length(unkeyed) > 0) {
    cat(
      "WARNING - ", id, ": ",
      str_flatten(c(
        if (length(dangling) > 0) str_c("the title or subtitle keys note ", str_flatten_comma(dangling, " and "),
                                        ", which the caption does not have"),
        if (length(unkeyed) > 0) str_c(if (length(unkeyed) > 1) "notes " else "note ",
                                       str_flatten_comma(unkeyed, " and "),
                                       if (length(unkeyed) > 1) " have" else " has",
                                       " no key in the title or subtitle")
      ), "; "),
      ". Renumber the keys, or pass a note that nothing points to in cwr_caption(unnumbered = ).\n",
      sep = "", file = stderr()
    )
  }
  invisible(plot)
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
# same label twice as large relative to the chart. This helper takes the
# height from the data instead, and since 2026-09-28 it works in millimetres
# on the page, not in shares of the axis: the labels used to be placed with
# a guessed width and height and no clearance, so they sat on their lines on
# the phone ("Region" at a peak, its "g" hanging into the line) and floated
# off them where a guessed width was too wide.
#
# How it measures. cwr_figure() and cwr_interactive() draw each chart twice,
# desktop and phone, and before each drawing they measure that version's
# panel in millimetres and each label's width in the font it is drawn in
# (cwr_fit_line_labels() in figures.R), then call this placement again with
# those measurements. The table this function returns is only a first guess
# for a chart drawn some other way. The phone version is placed first, since
# its labels are largest relative to the chart and so hardest to fit, and the
# desktop version keeps the phone's spot and side, so both read alike.
#
# What a spot is. A label sits over (or under) a stretch of its own line as
# wide as the label plus 1 mm either side, `gap` millimetres clear of the
# highest (or lowest) point the line reaches there - clear of the line's
# edge, so its thickness (`linewidth`, as in geom_line()) is allowed for. The
# ink is measured, not the text box: ggplot2 sets a label drawn above a line
# (vjust = 0) on its baseline, so a "g", "p" or "y" hangs below it, and the
# height is raised by that descender for a name that has one.
#
# Where each label goes along the line:
#   - `at` names a group and the x to centre its label on. Rarely needed now;
#     use it only when the automatic spot is readable but wrong for the story.
#   - A group left out of `at` (or `at = NULL` for all of them) is placed
#     automatically. Every x along its line is tried, a millimetre apart, on
#     both sides, and the best spot wins, judged in this order: it does not
#     cover a label already placed; no other line comes within `gap` of it;
#     it stays inside the panel; no other line runs within 3 mm of it (so a
#     name between two close lines cannot be read as the other one's); the
#     line under it is flat, so the label does not float off one end (the
#     average distance from label to line, to the half millimetre); then the
#     preferred side; then the leftmost. The rightmost `right_margin` of the x
#     range is skipped, because the templates draw the value axis's labels
#     inside the panel there. Groups named in `bold` are placed first, so the
#     focus gets the best spot, then the rest in the order of `at` and the
#     data.
#
# Which side of the line:
#   `side` ("above"/"below", one for all or one per group) is a preference,
#   not an order: both sides are tried and the ranking above decides. A label
#   over a gridline is fine - the halo is there for exactly that (Greg,
#   2026-09-28) - so gridlines play no part unless `avoid_gridlines = TRUE`,
#   which steers labels off them where that costs nothing else.
#   `limits` and `breaks` are the y scale's, as given to scale_y_continuous();
#   they only shape the first guess (the fitted placement reads the drawn
#   scale) and, with `avoid_gridlines`, the gridlines.
#
# Returns one row per label with x, y, label, vjust, side and fontface, for
# the templates' cwr_label() layer with `aes(vjust = vjust)` and hjust = 0.5.
# x must be numeric (years); for a date axis pass as.numeric(date) and `at`
# as numbers too, then turn x back into dates - the fitted placement does
# that too. For a faceted chart, call it once per panel (the checks should
# only see that panel's lines) and add the facet column to each result with
# mutate() so each label stays in its panel; a faceted chart keeps the first
# guess, since the fitting measures one panel.
#
#   # every label placed automatically - the usual call
#   label_data <- cwr_line_labels(
#     plot_data, x = year, y = rate, group = region,
#     limits = c(0, NA), bold = cwr_region
#   )
#
#   # one label pinned to a stretch the story is about
#   label_data <- cwr_line_labels(
#     plot_data, x = year, y = csi, group = region,
#     at = c(Region = 2002), side = c(Region = "below"),
#     limits = c(45, NA), bold = cwr_region
#   )
cwr_line_labels <- function(data, x, y, group, at = NULL, side = "above",
                            bold = NULL, gap = 0.8, linewidth = 0.8,
                            limits = NULL, breaks = NULL,
                            avoid_gridlines = FALSE, right_margin = 0.12) {
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
  line_of <- map(set_names(groups), \(g) lines |> filter(group == g) |> arrange(x))
  face_of <- set_names(if_else(groups %in% bold, "bold", "plain"), groups)

  # Where a line runs across a window of x: every point inside it, plus where
  # the line crosses the window's two edges.
  heights_in <- function(line, window) {
    c(
      line |> filter(between(x, window[1], window[2])) |> pull(y),
      approx(line$x, line$y, xout = window, rule = 2)$y
    )
  }

  # The placement itself, for one drawing of the chart. `panel` describes it:
  #   x_range, y_range   the panel's data range, expansion included
  #   width_mm, height_mm  the panel's size on the page
  #   label_mm           each label's width in mm, named by group
  #   size               the labels' text size (ggplot2 mm)
  #   breaks             the y gridlines
  # `keep` (optional) is an earlier placement whose x and side are kept, as
  # the desktop version keeps the phone's.
  place <- function(panel, keep = NULL) {
    x_per_mm <- diff(panel$x_range) / panel$width_mm
    y_per_mm <- diff(panel$y_range) / panel$height_mm

    # Inter's ink, as a share of the text size: capitals and ascenders reach
    # 0.74 of it above the baseline, descenders 0.21 below (measured from
    # drawn labels, 2026-09-28).
    rise <- 0.74 * panel$size * y_per_mm
    drop <- 0.21 * panel$size * y_per_mm
    has_drop <- set_names(str_detect(groups, "[gjpqy,;]"), groups)
    # From the line's centre to the ink: half the line's width, then `gap`
    clear <- (linewidth * .pt / 96 * 25.4 / 2 + gap) * y_per_mm
    crowd <- 3 * y_per_mm
    pad <- 1 * x_per_mm
    x_stop <- panel$x_range[2] - right_margin * diff(panel$x_range)
    grid_y <- if (avoid_gridlines) panel$breaks else numeric(0)

    placed <- tibble(left = numeric(0), right = numeric(0),
                     bottom = numeric(0), top = numeric(0))

    score <- function(g, x_c, where, preferred_side) {
      half <- panel$label_mm[[g]] / 2 * x_per_mm + pad
      window <- x_c + c(-half, half)
      own <- heights_in(line_of[[g]], window)
      if (where == "above") {
        edge <- max(own)
        ink <- c(edge + clear, edge + clear + rise + if (has_drop[[g]]) drop else 0)
        y_pos <- ink[1] + if (has_drop[[g]]) drop else 0
      } else {
        edge <- min(own)
        ink <- c(edge - clear - rise - if (has_drop[[g]]) drop else 0, edge - clear)
        y_pos <- ink[2]
      }
      # How far other lines come to the label, in y units (negative = through it)
      others <- map_dbl(line_of[names(line_of) != g], \(l) {
        r <- range(heights_in(l, window))
        if (r[1] > ink[2]) r[1] - ink[2] else if (r[2] < ink[1]) ink[1] - r[2] else -1
      })
      nearest <- if (length(others)) min(others) else Inf
      # The average distance from the label to its own line across the label
      along <- approx(line_of[[g]]$x, line_of[[g]]$y, xout = seq(window[1], window[2], length.out = 11),
                      rule = 2)$y
      float_mm <- mean(abs(edge - along)) / y_per_mm
      tibble(
        x = x_c,
        side = where,
        y = y_pos,
        hits_label = any(placed$left < window[2] & placed$right > window[1] &
                           placed$bottom < ink[2] & placed$top > ink[1]),
        hits_line = nearest < clear,
        outside = ink[1] < panel$y_range[1] || ink[2] > panel$y_range[2] ||
          window[1] < panel$x_range[1] || window[2] > panel$x_range[2],
        crowded = nearest < crowd,
        hits_grid = any(grid_y > ink[1] & grid_y < ink[2]),
        float = round(float_mm * 2) / 2,
        preferred = where == preferred_side,
        left = window[1], right = window[2], bottom = ink[1], top = ink[2]
      )
    }

    # Placement order: the bold (focus) groups first, then `at`, then the rest
    order <- unique(c(intersect(bold, groups), names(at), groups))
    result <- list()
    for (g in order) {
      if (!is.null(keep)) {
        # The same spot and side as the earlier placement, at this drawing's height
        k <- keep |> filter(label == g)
        options <- score(g, k$x, k$side, k$side)
      } else {
        if (g %in% names(at)) {
          xs <- at[[g]]
        } else {
          # Every millimetre along its own line, less half a label at each end
          # and the axis labels' strip
          half <- panel$label_mm[[g]] / 2 * x_per_mm + pad
          own_x <- range(line_of[[g]]$x)
          last <- min(own_x[2], x_stop) - half
          xs <- if (last > own_x[1] + half) seq(own_x[1] + half, last, by = x_per_mm) else mean(own_x)
        }
        options <- map(xs, \(x_c) list_rbind(map(c("above", "below"), \(w) score(g, x_c, w, side[[g]])))) |>
          list_rbind() |>
          arrange(hits_label, hits_line, outside, crowded, hits_grid, float, desc(preferred), x)
      }
      pick <- slice(options, 1)
      # No clear spot anywhere: say so once, while the phone version is placed.
      # Lines that weave that closely want the house legend row instead, or
      # cwr_label_repel() if the names must stay on the lines.
      if (isTRUE(panel$warn) && (pick$hits_label || pick$hits_line)) {
        warning("cwr_line_labels(): no spot for \"", g, "\" is clear of the other ",
                if (pick$hits_label) "labels" else "lines",
                ". Use the legend row for lines this close (cwr-charts rule 3a).",
                call. = FALSE)
      }
      placed <- bind_rows(placed, select(pick, left, right, bottom, top))
      result[[g]] <- tibble(
        x = pick$x,
        y = pick$y,
        label = g,
        vjust = if (pick$side == "above") 0 else 1,
        side = pick$side,
        fontface = face_of[[g]]
      )
    }
    list_rbind(result)
  }

  # First guess, for a chart not drawn by cwr_figure() or cwr_interactive():
  # a phone-sized panel (95 x 65 mm), the templates' 3.2 mm text at the
  # phone's 0.8, and a width of 0.55 of the text size per character.
  y_guess <- range(lines$y)
  if (!is.null(limits)) y_guess <- coalesce(as.numeric(limits), y_guess)
  y_guess[2] <- y_guess[2] + 0.05 * diff(y_guess)
  guess_size <- 3.2 * 0.8
  guess <- place(list(
    x_range = range(lines$x) + c(-0.02, 0.05) * diff(range(lines$x)),
    y_range = y_guess,
    width_mm = 95, height_mm = 65,
    size = guess_size,
    label_mm = set_names(0.55 * guess_size * str_length(groups) * if_else(groups %in% bold, 1.08, 1), groups),
    breaks = breaks %||% scales::breaks_extended()(y_guess)
  ))

  # The fitting in cwr_figure() / cwr_interactive() finds the placement by
  # this attribute and calls it again with the real measurements
  attr(guess, "cwr_line_labels") <- place
  guess
}

# ggplot2 text sizes for geom_text/geom_label/cwr_label are in mm, not points.
# 4 mm is roughly 11 pt, the standard label size used across the templates
# (scaled up with base_size so direct labels stay readable on a phone).
label_size <- 4
