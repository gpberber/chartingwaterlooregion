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

# TRUE while cwr_figure() or cwr_interactive() draws the phone version, FALSE
# otherwise - for anything inside an aes that should differ there, such as a
# label wrapped to fit half the width:
#   aes(label = if (cwr_on_phone()) "Watson\nforecast" else "Watson forecast")
# Worked case: the forecast line's label on fig-population-cagr in
# nd-population-forecast.
cwr_on_phone <- function() getOption("cwr.phone", FALSE)

# Whether a bar's value label fits inside the bar, measured on the page
# (cwr-charts rule 3g; Greg, 2026-10-02: "Bar labels always go inside bars
# unless they don't fit"). Called inside an aes, it takes each label as it will
# be printed, the bar's length in data units (its value, for a bar from zero)
# and the label's size, and answers TRUE where the bar is at least as long, in
# millimetres, as the text plus `room` (mm, the gap at the bar's end and a
# margin at its start). The text is measured in the house font at the size it
# is drawn, shrunk on the phone as cwr_figure() shrinks it; the bar is measured
# with the panel's own scale, which cwr_figure() and cwr_interactive() measure
# on each drawn version and hand over in the cwr.panel_mm option. `axis` is the
# direction the bars run as drawn: "x" for horizontal bars (the text's width
# counts), "y" for upright ones (its height). Before a chart is measured (and
# outside cwr_figure()) there is no scale yet, and every label counts as
# fitting. Write the inside layer and the outside layer over the same rows,
# blanking the label the other one draws:
#   geom_text(aes(label = if_else(cwr_fits_inside(value_label, value, 3.2), value_label, "")), ...)
#   cwr_label(aes(label = if_else(cwr_fits_inside(value_label, value, 3.2), "", value_label)), ...)
# Never decide "fits" by a share of the longest bar or a value in data units:
# a guess like that put labels of 2.8 and 6.3 outside bars with room to spare
# (population-since-1971, 2026-10-13).
cwr_fits_inside <- function(label, length, size, axis = "x", room = 3) {
  panel_mm <- getOption("cwr.panel_mm")
  if (is.null(panel_mm)) return(rep(TRUE, length(label)))
  if (cwr_on_phone()) size <- size * getOption("cwr.text_scale", 1)
  # A label's height is its size (ggplot2 sizes are millimetres)
  text_mm <- if (axis == "x") cwr_text_mm(label, size) else rep(size, length(label))
  abs(length) * panel_mm[[axis]] >= text_mm + room
}

# The width in mm of each label at a ggplot2 text size, in the house font, as
# ragg draws it. Measured with grid in a throwaway ragg device, as
# cwr_measure_panel() (figures.R) measures line names, then switched back to
# whichever device was drawing (cwr_fits_inside() runs in the middle of a
# ggsave()). systemfonts::string_width() is not used: on this machine it
# matched a different Inter and measured "2.8" at 6.3 mm, which ragg draws at
# 4.8 mm (2026-10-13).
cwr_text_mm <- function(label, size) {
  label <- replace_na(as.character(label), "")
  if (length(label) == 0) return(numeric(0))
  previous_device <- dev.cur()
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, width = 1, height = 1, units = "in", res = 72)
  on.exit({
    dev.off()
    if (previous_device > 1) dev.set(previous_device)
    unlink(file)
  })
  map_dbl(label, \(text) {
    convertWidth(grobWidth(textGrob(text, gp = gpar(fontfamily = cwr_font(), fontsize = size * .pt))),
                 "mm", valueOnly = TRUE)
  })
}

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
# `credit` may also be a description of that arithmetic, in place of TRUE
# (Greg, 2026-10-14): the chart shows the same byline, and the "Detailed table
# sources" table gives Charting Waterloo Region a row of its own for the
# figure, after the publishers', saying what the site did to their numbers:
#
#   credit = "Calculated each district's share of commuters by main mode, and
#             the 95% confidence intervals of those shares, from the published
#             counts and bounds."
#
# Write what was done, not why, opening each sentence with a verb that says
# exactly what (Greg, 2026-10-14): "Calculated each district's share ...",
# "Grouped ... into three", "Combined ...", "Picked out the ten largest". Each
# description ends with a period; cwr_detailed_sources_table() adds one if it
# is missing. A chart credited with plain TRUE gets the row with an empty
# cell, and cwr_detailed_sources_table() warns.
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
# Sources from different publishers are separated by semicolons, and a
# publisher's own items keep their commas (Greg, 2026-10-01). Pass them as a
# list with one element per publisher, each a string or a vector of that
# publisher's items:
#
#   cwr_caption(list(
#     c("Statistics Canada, censuses of 1971 to 2001", "Table 17-10-0155-01"),
#     "Ontario Ministry of Finance, population projections, Summer 2026"
#   ))
#   -> "Sources: Statistics Canada, censuses of 1971 to 2001, Table
#       17-10-0155-01; Ontario Ministry of Finance, population projections,
#       Summer 2026"
#
# A plain vector is read as one publisher's items, so every caption with a
# single publisher is written exactly as before.
#
# `details` moves the table numbers, report names and file names off the chart
# and into the post's "Detailed table sources" table (Greg, 2026-10-14). The
# chart then names only the publisher, and `source` holds just the name - one
# string, or a list of names for several publishers - while `details` holds
# what used to follow it, in the same shape: a string or a vector of items for
# one publisher, a list with one element per publisher for several. Each item
# is what the source line used to say - a table number, a report's name, a
# file - not a table's title (Greg, 2026-10-14). Items may carry markdown
# links, since they are printed in the table and never drawn on the chart:
#
#   cwr_caption(
#     "Statistics Canada",
#     details = c(
#       "[Table 98-10-0459-01](https://...)",
#       "2021 census boundary files"
#     ),
#     credit = TRUE
#   )
#   -> "Sources: Statistics Canada | *Charting Waterloo Region*" on the chart
#   -> "Table 98-10-0459-01, 2021 census boundary files" in the table
#
# The details ride along on the caption string as an attribute, which ggplot
# keeps; cwr_figure() and cwr_interactive() read it off the plot and log it
# under the chart's id (cwr_log_sources() below), and
# cwr_detailed_sources_table() prints the log. A caption without `details`
# works as it always has, and is logged with its source line alone.
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
cwr_caption <- function(source, credit = FALSE, notes = NULL, unnumbered = NULL,
                        details = NULL) {
  # `credit` given as a description of the site's own arithmetic: the byline
  # goes on as for TRUE, and the description goes into the sources table
  contribution <- if (is.character(credit)) str_flatten(credit, " ") else NA_character_
  credit <- isTRUE(credit) || is.character(credit)

  # The byline is a source like any other, so it decides the plural too
  # One publisher unless a list says otherwise; commas within a publisher,
  # semicolons between publishers
  if (!is.list(source)) source <- list(source)
  label <- if (length(unlist(source)) > 1 || credit) "Sources: " else "Source: "
  publishers <- map_chr(source, \(items) str_flatten(items, ", "))
  caption <- paste0(label, str_flatten(publishers, "; "))
  if (credit) caption <- paste0(caption, " | *Charting Waterloo Region*")

  # One row per publisher for the "Detailed table sources" table: the name the
  # chart shows, and its items, kept as a vector so that
  # cwr_detailed_sources_table() can gather every item a figure takes from one
  # publisher into one cell. With no `details`, the row carries the source
  # line's own wording and no items, and cwr_detailed_sources_table() warns.
  if (!is.null(details)) {
    if (!is.list(details)) details <- list(details)
    if (length(details) != length(publishers)) {
      stop("cwr_caption(): `details` needs one element per publisher in `source` (",
           length(publishers), "), not ", length(details), call. = FALSE)
    }
    sources <- tibble(source = publishers, details = map(details, as.character))
  } else {
    sources <- tibble(source = publishers, details = list(NA_character_))
  }
  sources$credit <- FALSE
  # The site's own row, after the publishers', when the chart credits it
  if (credit) {
    sources <- add_row(sources, source = "Charting Waterloo Region",
                       details = list(contribution), credit = TRUE)
  }

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

  caption <- structure(caption, cwr_sources = sources)

  # A gt table has no cwr_figure() to log its sources, so a caption written in
  # a chunk labelled "tbl-..." - which is what makes Quarto number the table -
  # logs itself under that label. Outside a render there is no label.
  label <- knitr::opts_current$get("label")
  if (isTRUE(str_starts(label, "tbl-"))) cwr_log_caption(caption, label)

  caption
}

# ---- The log behind "Detailed table sources" --------------------------------
# Every chart cwr_figure(), cwr_interactive() or cwr_leaflet() draws during a
# render adds its sources here, under its id, in the order the charts appear,
# and so does every gt table's cwr_caption(), under its chunk's "tbl-" label.
# cwr_detailed_sources_table() (post_sections.R) prints it at the foot of the
# post, after every chart has run. A post renders in an R session of its own,
# so the log starts empty for each post; running a chunk again in RStudio
# replaces that chart's entry rather than adding a second one.
#
# `part_of` is the id of the figure a chart is a part of: each tab of a tabset
# is a sub-figure, (a), (b), ..., of one numbered figure (see cwr_figure()).
cwr_source_log <- new.env()
cwr_source_log$charts <- list()

cwr_log_sources <- function(plot, id, part_of = NULL) {
  cwr_log_caption(plot$labels$caption, id, part_of)
}

# The same for a caption on its own: a leaflet map's or a gt table's
cwr_log_caption <- function(caption, id, part_of = NULL) {
  sources <- attr(caption, "cwr_sources")
  # A chart with no cwr_caption() at all has nothing to list
  if (is.null(sources)) return(invisible(NULL))
  cwr_source_log$charts[[id]] <- mutate(sources, id = id, part_of = part_of %||% NA_character_,
                                        .before = 1)
  invisible(sources)
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

# The placement a label helper returns travels two ways: as the attribute
# "cwr_line_labels" on the table, and on every row, in a list column
# `cwr_place` with an id for the call (`cwr_call`). An attribute is lost
# when tables are bound together - bind_rows(), list_rbind() - which is how
# a faceted chart builds its labels, one call per panel; the columns are
# not, so cwr_figure() and cwr_interactive() can still place each call's
# labels against its own panel, measured as drawn (Greg, 2026-10-06: names
# on faceted charts kept their first guess and sat too far from their
# lines). The id is a count kept for the session (cwr_call_counter, below):
# each call takes the next number, so no two calls ever share one, and making
# it never touches the random number stream a chart's simulation may use.
# (It was a temporary file name until 2026-10-07, but tempfile() only avoids
# names of files that exist, and these are never made, so two calls could in
# principle be given the same name and one call's labels placed as the
# other's.)
# `measure` lists every label the placement needs the width of - all the
# groups, even those the caller drops from the result afterwards (a name
# kept off the chart still stands in the way of the others) - and `bold`
# those of them set in bold; both ride on the placement as attributes.
# `kind` is "point" for cwr_point_labels(), whose layers cwr_figure() gives a
# leader line layer.
# Kept when the house style is sourced again (drafting in RStudio), so tables
# made before and after cannot share a number
if (!exists("cwr_call_counter", inherits = FALSE) || !is.environment(cwr_call_counter)) {
  cwr_call_counter <- new.env()
  cwr_call_counter$n <- 0L
}

cwr_carry_place <- function(guess, place, measure, bold = NULL, kind = "line") {
  attr(place, "measure") <- measure
  attr(place, "bold") <- bold
  attr(place, "kind") <- kind
  cwr_call_counter$n <- cwr_call_counter$n + 1L
  carried <- guess |>
    mutate(cwr_call = str_c("cwr", cwr_call_counter$n), cwr_place = list(place))
  attr(carried, "cwr_line_labels") <- place
  carried
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
# height is raised by that descender for a name that has one. Points drawn
# on the line (geom_point(), measured on each version at their drawn size)
# are cleared the same way, `gap` mm from their edge, so a name never sits on
# a dot; a point on another line is kept clear of as that line is (Greg,
# 2026-10-07).
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
#   which steers labels off them where that costs nothing else. Groups listed
#   in `fixed` keep their `side` on every version, as an order - for a name
#   placed by hand, which the phone's tighter panel would otherwise flip.
#   `limits` and `breaks` are the y scale's, as given to scale_y_continuous();
#   they only shape the first guess (the fitted placement reads the drawn
#   scale) and, with `avoid_gridlines`, the gridlines.
#
# Which lines get a name:
#   `named` lists the groups to label; NULL (the default) labels them all. A
#   group left out is still drawn and still kept clear of - only its own name
#   goes - for a chart that names a few highlighted lines among faint context
#   lines (nd-population-forecast, 2026-10-01).
#
# Which way a label runs from its x:
#   `hjust` (one per group, named; 0.5 where not given) is the label's
#   justification at its x, as in geom_text(): 1 ends the label at x, 0
#   starts it there. The spot is measured over the stretch the label really
#   covers, and the result carries an `hjust` column for the label layer's
#   aes(hjust = hjust). Mostly useful with `at`: a name that should end at a
#   given year.
#
# A different spot on the phone:
#   `phone_at` (named, like `at`) replaces `at` for the phone version, where a
#   hand-picked spot that fits the desktop can be too tight: half the width
#   and larger text. NA leaves that group to the automatic placement on the
#   phone, both sides tried even if it is in `fixed`. The desktop then keeps
#   its own `at` spot (and side) for these groups instead of copying the
#   phone's.
#   `phone_side` (named, like `side`) replaces `side` for those groups on the
#   phone, and the desktop then keeps its own side for them, as with
#   phone_at - for a name Greg wants above its line on the desktop and below
#   it on the phone (Greg, 2026-10-07).
#
# Returns one row per label with x, y, label, vjust, side, fontface and hjust, for
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
                            bold = NULL, named = NULL, hjust = NULL, fixed = NULL, phone_at = NULL,
                            phone_side = NULL, gap = 0.8, linewidth = 0.8,
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
  hjust_of <- c(hjust, set_names(rep(0.5, length(groups)), groups))[groups]

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
      # The stretch the label covers: its width, split around x_c by its
      # hjust, plus `pad` either side
      label_w <- panel$label_mm[[g]] * x_per_mm
      window <- x_c + c(-hjust_of[[g]] * label_w - pad, (1 - hjust_of[[g]]) * label_w + pad)
      own <- heights_in(line_of[[g]], window)
      # The points drawn on this line under the label's stretch (the panel's
      # geom_point() markers, measured by cwr_figure() with their radius in
      # mm): the label clears their edge by `gap`, as it clears the line, so
      # a name never sits on a year's dot (Greg, 2026-10-07, Figures 20 and
      # 22 of nd-population-forecast-sanity-check: "this shouldn't keep
      # happening"). A point on another line counts as that line, below.
      markers <- panel$markers %||% tibble(x = numeric(0), y = numeric(0), radius = numeric(0))
      markers <- markers |>
        filter(x + radius * x_per_mm > window[1], x - radius * x_per_mm < window[2])
      on_line <- abs(markers$y - approx(line_of[[g]]$x, line_of[[g]]$y, xout = markers$x, rule = 2)$y) <=
        markers$radius * y_per_mm + 1e-9
      own_markers <- markers[on_line, ]
      other_markers <- markers[!on_line, ]
      if (where == "above") {
        edge <- max(own)
        # The baseline clears the line everywhere; a descender ("g", "y") has
        # to clear it only where that letter is. The letters are taken as
        # equally wide, so a letter's stretch is its share of the label, plus
        # half the pad. On a sloping line this lets the label down to the line
        # where it carries no descender ("Region" ending at the top of a rise).
        baseline <- edge + clear
        if (has_drop[[g]]) {
          letters_at <- str_locate_all(g, "[gjpqy,;]")[[1]][, "start"]
          n_letters <- str_length(g)
          from <- window[1] + pad + (min(letters_at) - 1) / n_letters * label_w - pad / 2
          to <- window[1] + pad + max(letters_at) / n_letters * label_w + pad / 2
          baseline <- max(baseline, max(heights_in(line_of[[g]], c(from, to))) + clear + drop)
        }
        # Above the line's points: the lowest ink (a descender's, if any)
        # clears the top of each
        if (nrow(own_markers)) {
          baseline <- max(baseline, max(own_markers$y + (own_markers$radius + gap) * y_per_mm) +
                            if (has_drop[[g]]) drop else 0)
        }
        ink <- c(baseline - if (has_drop[[g]]) drop else 0, baseline + rise)
        y_pos <- baseline
      } else {
        edge <- min(own)
        top <- edge - clear
        # Below the line's points: the top of the ink clears the bottom of each
        if (nrow(own_markers)) {
          top <- min(top, min(own_markers$y - (own_markers$radius + gap) * y_per_mm))
        }
        ink <- c(top - rise - if (has_drop[[g]]) drop else 0, top)
        y_pos <- ink[2]
      }
      # How far other lines come to the label, in y units (negative = through
      # it), their points' edges included
      others <- map_dbl(line_of[names(line_of) != g], \(l) {
        r <- range(heights_in(l, window))
        if (r[1] > ink[2]) r[1] - ink[2] else if (r[2] < ink[1]) ink[1] - r[2] else -1
      })
      others <- c(others, pmap_dbl(other_markers, \(x, y, radius, ...) {
        r <- y + c(-1, 1) * radius * y_per_mm
        if (r[1] > ink[2]) r[1] - ink[2] else if (r[2] < ink[1]) ink[1] - r[2] else -1
      }))
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
    # (only the `named` groups, when given; the rest stay in line_of as obstacles)
    order <- unique(c(intersect(bold, groups), names(at), groups))
    if (!is.null(named)) order <- intersect(order, named)
    # The phone's own spots: phone_at replaces at, NA meaning automatic
    at_here <- at
    if (isTRUE(panel$phone) && !is.null(phone_at)) {
      at_here <- c(at[setdiff(names(at), names(phone_at))], phone_at[!is.na(phone_at)])
    }
    # The phone's own sides: phone_side replaces side
    side_here <- side
    if (isTRUE(panel$phone) && !is.null(phone_side)) side_here[names(phone_side)] <- phone_side
    result <- list()
    for (g in order) {
      if (!is.null(keep) && !g %in% c(names(phone_at), names(phone_side))) {
        # The same spot and side as the earlier placement, at this drawing's height
        k <- keep |> filter(label == g)
        options <- score(g, k$x, k$side, k$side)
      } else {
        if (g %in% names(at_here)) {
          xs <- at_here[[g]]
        } else {
          # Every millimetre along its own line, less the label (split by its
          # hjust) and the pad at each end, and the axis labels' strip
          label_w <- panel$label_mm[[g]] * x_per_mm
          own_x <- range(line_of[[g]]$x)
          first <- own_x[1] + hjust_of[[g]] * label_w + pad
          last <- min(own_x[2], x_stop) - (1 - hjust_of[[g]]) * label_w - pad
          xs <- if (last > first) seq(first, last, by = x_per_mm) else mean(own_x)
        }
        free_on_phone <- isTRUE(panel$phone) && g %in% names(phone_at) && is.na(phone_at[[g]])
        sides <- if (g %in% fixed && !free_on_phone) side_here[[g]] else c("above", "below")
        options <- map(xs, \(x_c) list_rbind(map(sides, \(w) score(g, x_c, w, side_here[[g]])))) |>
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
        fontface = face_of[[g]],
        hjust = hjust_of[[g]]
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
    phone = TRUE,
    label_mm = set_names(0.55 * guess_size * str_length(groups) * if_else(groups %in% bold, 1.08, 1), groups),
    breaks = breaks %||% scales::breaks_extended()(y_guess)
  ))

  # The fitting in cwr_figure() / cwr_interactive() finds the placement by
  # this attribute and calls it again with the real measurements
  attr(guess, "cwr_line_labels") <- place
  cwr_carry_place(guess, place, measure = groups, bold = bold)
}

# ---- Value labels on points ------------------------------------------------
# cwr_point_labels(): where to write a value beside a point - "11,038" over
# the first point of a line, "27,300" over its last (Greg, 2026-10-04: the
# old placements, nudges in data units, sat too far from their points on one
# version and too close on the other). Each label is placed in millimetres
# from its point, measured on each drawn version as cwr_line_labels() names
# are: cwr_figure() and cwr_interactive() find the layer by the same
# attribute and call the placement again with the real panel and text sizes.
#
#   position   one per row (or one for all): "above", "below", "left" or
#              "right" of the point
#   hjust      for "above" and "below", one per row: 0.5 centres the label
#              over the point, 1 right-justifies it (it ends at the point's
#              centre), 0 left-justifies it (it starts there)
#   phone_position, phone_hjust  the same for the phone version, where they
#              differ (NULL keeps the desktop's)
#   lines      optional, the chart's lines as a data frame with columns x, y
#              and group: an "above" label is raised, and a "below" one
#              lowered, until it clears every line across its width by `gap`;
#              a "left" label is moved left, and a "right" one right, until
#              no line runs through it (Greg, 2026-10-07). Leave out a line
#              that runs through the point itself, beside a left or right
#              label, or the label is pushed along it
#   gap        the space between the point's edge (or a line) and the label's
#              letters, in mm
#   point_size the points' size, as given to geom_point(); the phone draws
#              them at phone_point_scale of it (cwr_figure()'s default 0.65)
#   leader     how much further out, in mm, a label that could be read as
#              another point's is set, with a leader line to its own point;
#              one for all or one per row, NA for none (the label stays at
#              its usual distance, with no line; Greg, 2026-10-07)
#   phone_leader  the same for the phone version, where it differs (NULL
#              keeps the desktop's)
#
# A label that would run out of the panel sideways is slid back inside it.
# A label above or below a point that could be read as a neighbour's - one
# starting at its point, as at a panel's left edge, runs on over the next
# point - is lifted (or lowered) `leader` mm and joined to its point by a
# thin leader line in its colour (Greg, 2026-10-06). The result's
# leader_x, leader_from and leader_to columns hold the line (NA where there
# is none), and cwr_figure() and cwr_interactive() draw it themselves, so a
# chart adds nothing for it.
# Returns one row per label with x, y, label, hjust, vjust and fontface, for
# a cwr_label() layer with aes(x = x, y = y, label = label, hjust = hjust,
# vjust = vjust) and inherit.aes = FALSE. First worked case:
# fig-nd-population-from-2021 in nd-population-forecast.
#
#   value_labels <- cwr_point_labels(
#     end_points, x = year, y = population, label = label_comma()(population),
#     position = c("above", "below", "above"), hjust = c(1, 0, 0.5),
#     lines = plot_data |> select(x = year, y = population, group = segment)
#   )
cwr_point_labels <- function(data, x, y, label, position = "above", hjust = 0.5,
                             phone_position = NULL, phone_hjust = NULL, lines = NULL,
                             gap = 0.8, point_size = 2, phone_point_scale = 0.65, limits = NULL,
                             leader = 2.5, phone_leader = NULL) {
  points <- data |>
    cwr_point_columns(x = {{ x }}, y = {{ y }}, label = {{ label }})
  n <- nrow(points)
  position <- rep_len(position, n)
  hjust <- rep_len(hjust, n)
  leader <- rep_len(leader, n)
  phone_leader <- if (is.null(phone_leader)) leader else rep_len(phone_leader, n)
  phone_position <- if (is.null(phone_position)) position else rep_len(phone_position, n)
  phone_hjust <- if (is.null(phone_hjust)) hjust else rep_len(phone_hjust, n)
  stopifnot(all(c(position, phone_position) %in% c("above", "below", "left", "right")))
  # Each line's points in order along x. Sorted first and split by the sorted
  # rows' own groups: splitting the sorted rows by the unsorted group column
  # mixed the lines together, so a label was kept clear of lines that are not
  # drawn and landed on the real one (Fig 19's growth note, 2026-10-07)
  line_of <- if (is.null(lines)) list() else {
    sorted <- arrange(lines, group, x)
    split(sorted, sorted$group)
  }

  # Every line's heights across a stretch of x: the points inside it and where
  # the line crosses its two ends (a line that does not reach it plays no part)
  heights_in <- function(window) {
    map(line_of, \(l) {
      if (max(l$x) < window[1] || min(l$x) > window[2]) return(numeric(0))
      inside <- l |> filter(between(x, window[1], window[2])) |> pull(y)
      ends <- approx(l$x, l$y, xout = window)$y
      c(inside, ends[!is.na(ends)])
    }) |>
      unlist()
  }

  place <- function(panel, keep = NULL) {
    phone <- isTRUE(panel$phone)
    x_per_mm <- diff(panel$x_range) / panel$width_mm
    y_per_mm <- diff(panel$y_range) / panel$height_mm
    # Text sizes are mm of font size (ggplot2's); Inter's capitals and figures
    # are 0.7275 of it tall and its comma drops 0.21 below the baseline - the
    # same measures as cwr_line_labels() and cwr_right_axis() use
    cap <- 0.7275 * panel$size
    drop <- 0.21 * panel$size
    # A point's radius: geom_point()'s filled circle is 0.375 of its font
    # size, which is its size in mm times .pt plus half its 0.5 stroke
    size_now <- point_size * if (phone) phone_point_scale else 1
    radius <- 0.375 * (size_now * .pt + 0.5 * .stroke / 2) * 25.4 / 72.27
    pos <- if (phone) phone_position else position
    hj <- if (phone) phone_hjust else hjust
    ld <- if (phone) phone_leader else leader

    pmap(list(points$x, points$y, points$label, pos, hj, ld), \(x0, y0, lab, p, h, l) {
      width <- panel$label_mm[[lab]] * x_per_mm
      has_drop <- str_detect(lab, "[gjpqy,;]")
      if (p %in% c("above", "below")) {
        # Slide a label that would leave the panel back inside it
        x_left <- x0 - h * width
        x_left <- min(max(x_left, panel$x_range[1]), panel$x_range[2] - width)
        x_pos <- x_left + h * width
        window <- c(x_left, x_left + width)
        near <- heights_in(window)
        # Could the label be read as another point's? (Greg, 2026-10-06: a
        # 1976 value starting at its point ran on over the 1981 point.) It
        # could when another drawn point near it - within 8 mm up or down of
        # this one - is less than twice as far from the label's middle as its
        # own point is. The label is then lifted `leader` mm further out and
        # a thin leader line drawn from its point to it (by cwr_figure(),
        # which adds the leader layer itself). The drawn points are the
        # panel's geom_point() markers, which cwr_figure() measures
        # (`markers`); a line's vertices without a marker do not count, as a
        # reader cannot see them. The first guess knows only the labelled
        # points. The line stops 1.2 mm short of the label, so it is never
        # read as a "1" in front of the figure.
        middle <- x_left + width / 2
        own_mm <- abs(middle - x0) / x_per_mm
        others <- (panel$markers %||% tibble(x = points$x, y = points$y)) |>
          filter(abs(x - x0) > 1e-9, abs(y - y0) / y_per_mm <= 8)
        other_mm <- if (nrow(others)) min(abs(middle - others$x)) / x_per_mm else Inf
        unclear <- !is.na(l) && own_mm > 1 && other_mm < 2 * own_mm
        lift <- if (unclear) l else 0
        if (p == "above") {
          # The baseline, high enough for the comma to clear the point and
          # every line under the label
          base <- y0 + (radius + gap + lift + if (has_drop) drop else 0) * y_per_mm
          if (length(near)) base <- max(base, max(near) + (gap + lift + if (has_drop) drop else 0) * y_per_mm)
          tibble(x = x_pos, y = base, hjust = h, vjust = 0,
                 leader_x = if (unclear) x0 else NA_real_,
                 leader_from = if (unclear) y0 + (radius + 0.4) * y_per_mm else NA_real_,
                 leader_to = if (unclear) base - 1.2 * y_per_mm else NA_real_)
        } else {
          # The tops of the figures, low enough to clear the point and every
          # line over the label
          top <- y0 - (radius + gap + lift) * y_per_mm
          if (length(near)) top <- min(top, min(near) - (gap + lift) * y_per_mm)
          tibble(x = x_pos, y = top, hjust = h, vjust = 1,
                 leader_x = if (unclear) x0 else NA_real_,
                 leader_from = if (unclear) y0 - (radius + 0.4) * y_per_mm else NA_real_,
                 leader_to = if (unclear) top + 1.2 * y_per_mm else NA_real_)
        }
      } else {
        # Left or right of the point, its near edge `gap` mm out from the
        # point's edge, then pushed further out sideways until no line runs
        # through it (Greg, 2026-10-07: "Growth 2.6 times" kept landing on
        # North Dumfries' line, "this shouldn't keep happening"). The label's
        # height is its rows of text, about 0.9 of the text size apart, plus
        # `gap` above and below; each line is followed in steps of a quarter
        # of a millimetre, and any step inside that band and inside the
        # label's width (plus `gap`) is a hit. A left label then moves to end
        # `gap` mm short of the leftmost hit, a right one to start `gap` mm
        # past the rightmost, and the check runs again, until nothing hits.
        side <- if (p == "right") 1 else -1
        rows <- str_count(lab, "\n") + 1
        half_mm <- (cap + (rows - 1) * 0.9 * panel$size) / 2 + gap
        band <- y0 + c(-1, 1) * half_mm * y_per_mm
        edge <- x0 + side * (radius + gap) * x_per_mm
        for (i in 1:100) {
          reach <- sort(c(edge - side * gap * x_per_mm, edge + side * (width + gap * x_per_mm)))
          hits <- map(line_of, \(l) {
            from <- max(reach[1], min(l$x))
            to <- min(reach[2], max(l$x))
            if (from > to) return(numeric(0))
            xs <- seq(from, to, by = 0.25 * x_per_mm)
            ys <- approx(l$x, l$y, xout = xs)$y
            xs[!is.na(ys) & between(ys, band[1], band[2])]
          }) |>
            unlist()
          if (!length(hits)) break
          edge <- if (side < 0) min(hits) - gap * x_per_mm else max(hits) + gap * x_per_mm
        }
        tibble(x = edge, y = y0,
               hjust = if (p == "right") 0 else 1, vjust = 0.5,
               leader_x = NA_real_, leader_from = NA_real_, leader_to = NA_real_)
      }
    }) |>
      list_rbind() |>
      mutate(label = points$label, fontface = "plain")
  }

  # First guess, for a chart not drawn by cwr_figure() or cwr_interactive(),
  # as cwr_line_labels() makes one: a phone-sized panel (95 x 65 mm), the
  # templates' 3.2 mm text at the phone's 0.8, 0.55 of the text size per
  # character
  all_x <- c(points$x, if (!is.null(lines)) lines$x)
  all_y <- c(points$y, if (!is.null(lines)) lines$y)
  y_guess <- range(all_y)
  if (!is.null(limits)) y_guess <- coalesce(as.numeric(limits), y_guess)
  y_guess[2] <- y_guess[2] + 0.05 * diff(y_guess)
  guess_size <- 3.2 * 0.8
  guess <- place(list(
    x_range = range(all_x) + c(-0.02, 0.05) * diff(range(all_x)),
    y_range = y_guess,
    width_mm = 95, height_mm = 65,
    size = guess_size,
    phone = TRUE,
    label_mm = set_names(0.55 * guess_size * str_length(points$label), points$label)
  ))
  # The hook cwr_figure() and cwr_interactive() look for (named after the
  # line-name helper that first used it)
  attr(guess, "cwr_line_labels") <- place
  cwr_carry_place(guess, place, measure = points$label, kind = "point")
}

# The x, y and label columns cwr_point_labels() works from, picked out of the
# caller's data by name or expression
cwr_point_columns <- function(data, x, y, label) {
  data |>
    mutate(x = as.numeric({{ x }}), y = {{ y }}, label = as.character({{ label }}), .keep = "none")
}

# ---- Half dots where two dots overlap ----------------------------------------
# cwr_half_points(): a dot drawn as half a circle, its left or right half, so
# two dots that would cover each other - a dumbbell row whose two years are
# nearly equal - both show (Greg, 2026-10-09, kwc-da-pop-changes). Map `side`
# to "left" or "right" and give the same size as the chart's geom_point()
# dots: each half is that dot's exact half, measured in mm on the page (a
# filled circle is 0.375 of its font size, as cwr_point_labels() measures
# it), so it is round on every chart whatever the axes' units. Its geom is a
# kind of GeomPoint, so cwr_figure() shrinks it on the phone with the other
# dots. No legend key: the dots it pairs with carry the colours.
#
#   cwr_half_points(data = close_pairs, aes(x = population, y = neighbourhood,
#                   colour = year, side = side), size = 4)
GeomCwrHalfPoint <- ggproto(
  "GeomCwrHalfPoint", GeomPoint,
  required_aes = c("x", "y", "side"),
  draw_panel = function(data, panel_params, coord) {
    coords <- coord$transform(data, panel_params)
    # The radius geom_point() would draw: 0.375 of the font size (size in mm
    # times .pt, plus half the stroke), plus half the border line it draws
    # round a filled circle, in mm
    radius <- (0.375 * (coords$size * .pt + coords$stroke * .stroke / 2) +
                 coords$stroke * .stroke / 4 * 72.27 / 96) * 25.4 / 72.27
    outline <- pmap(list(seq_len(nrow(coords)), coords$side, radius), \(i, side, r) {
      angle <- if (side == "left") seq(pi / 2, 3 * pi / 2, length.out = 40) else seq(-pi / 2, pi / 2, length.out = 40)
      tibble(id = i, dx = r * cos(angle), dy = r * sin(angle))
    }) |>
      list_rbind()
    grid::polygonGrob(
      x = unit(coords$x[outline$id], "npc") + unit(outline$dx, "mm"),
      y = unit(coords$y[outline$id], "npc") + unit(outline$dy, "mm"),
      id = outline$id,
      gp = grid::gpar(fill = alpha(coords$colour, coords$alpha), col = NA)
    )
  }
)

cwr_half_points <- function(mapping = NULL, data = NULL, ..., inherit.aes = TRUE) {
  layer(
    geom = GeomCwrHalfPoint, stat = "identity", position = "identity",
    mapping = mapping, data = data, params = list(...),
    inherit.aes = inherit.aes, show.legend = FALSE
  )
}

# ---- Year labels, Economist style --------------------------------------------
# cwr_year_labels(): the phone version's x-axis years (Greg, 2026-10-04), as
# The Economist writes them - the first year in full, then the last two
# digits, and the first year of a new century in full again: 2021, 26, 31 /
# 1996, 2001, 06, 11. Only the phone uses it (Greg: "Economist-style year
# labels for phone only"); the desktop keeps every year in full. A house
# standard: cwr_figure() and cwr_interactive() put it on every phone chart
# whose x axis shows plain years (cwr_phone_year_labels() in figures.R), so a
# chart never passes it itself.
cwr_year_labels <- function(breaks) {
  labels <- as.character(breaks)
  shown <- !is.na(breaks)
  years <- breaks[shown]
  full <- c(TRUE, diff(years %/% 100) != 0)
  labels[shown] <- if_else(full, as.character(years), str_sub(as.character(years), -2))
  labels
}

# ggplot2 text sizes for geom_text/geom_label/cwr_label are in mm, not points.
# 4 mm is roughly 11 pt, the standard label size used across the templates
# (scaled up with base_size so direct labels stay readable on a phone).
label_size <- 4
