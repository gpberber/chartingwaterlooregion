# right_axis.R
# ---------------------------------------------------------------------------
# cwr_right_axis(): the house treatment of the value axis on a vertical chart.
# The numbers sit at the end of the gridlines rather than beside the panel,
# shared trailing zeros come off, zero is labelled where the axis reaches it,
# and an axis that does not reach zero gets a zig-zag and a short baseline.
#
# Sourced by R/theme_cwr.R, which every post sources in turn - so a post never
# names this file. Split out of theme_cwr.R on 2026-09-26, unchanged.
# ---------------------------------------------------------------------------

# ---- The gap between a gridline and its number --------------------------------
# A stock distance (Greg, 2026-10-03: "the y labels are sitting pretty far above
# the gridlines. Make them closer, but without touching the line"): the bottom
# of each number sits cwr_number_gap points above its gridline, at every text
# size, desktop and phone. ggplot2 can only raise text by a share of its own
# height (vjust), so cwr_number_vjust() turns the gap into that share: the
# numbers are lining figures, as tall as Inter's capitals, 0.7275 of the font
# size (Inter's cap height, 1,490 of its 2,048 units), and a negative vjust
# lifts the text's baseline by that share of the figures' height. The old
# vjust = -0.5 left about 4.4 points at the desktop size.
cwr_number_gap <- 1.5
cwr_figure_height <- 0.7275
cwr_number_vjust <- function(size) -cwr_number_gap / (cwr_figure_height * size)

# ---- Right-axis numbers and the baseline --------------------------------------
# cwr_right_axis() draws a vertical chart's value axis and baseline the house
# way (cwr-charts rules 6 and 6a, Greg 2026-09-25). Add it to the plot after
# its scales - last before labs() is the safe place - since it reads the
# numbers the y scale will draw and the data the layers hold:
#
#   p <- ggplot(...) + geom_line() + scale_y_continuous(position = "right") +
#     cwr_right_axis() +
#     labs(...)
#
# 1. The numbers (rule 6) sit inside the panel, each cwr_number_gap points
#    above its gridline and ending exactly where the gridline ends. theme_cwr() draws right-axis
#    numbers beside the panel; a negative left margin exactly as wide as the
#    widest number pulls them back inside, and hjust = 1 lines up their right
#    ends with the panel's edge. The width is measured with systemfonts (no
#    device needed) in the axis font, base_size * 0.8 points of cwr_font().
#    The phone version (cwr_phone_theme()) still moves them outside the panel.
#
#    Numbers that all end in the same zeros ("0.0, 1.0, 2.0") lose them
#    (rule 9, cwr_trim_zeros()).
#
# 2. Zero and the baseline (rule 6a). What the chart's own data decide:
#    - The values start at zero (none below it, and the scale reaches it, as
#      `limits = c(0, NA)` or a bar chart does): the scale is made to start
#      exactly at 0, so the black baseline lies on the zero gridline, the full
#      length of the gridlines, and 0 is labelled.
#    - The values cross zero: the baseline stays the full length and 0 is
#      labelled (a zero gridline is added if the scale's breaks skip it).
#    - The values do not reach zero: the axis is cut, and says so. A small
#      upright zig-zag (three strokes) sits beneath the lowest gridline, right-aligned with the numbers, and
#      the black baseline stops at the last data point while the gridlines
#      run on to the numbers. To leave the zig-zag room, the panel reaches at
#      least half a gridline step below the lowest gridline; the breaks are
#      fixed first, so that extra room cannot grow a new gridline.
#    - An index chart (`index = 100`, or whatever its base is) whose axis
#      reaches the base: the base does the job zero does on other charts
#      (Greg, 2026-09-26), so there is no zig-zag, the baseline runs the full
#      length of the gridlines, the base is labelled, and its gridline is drawn
#      in the baseline's colour rather than the gridlines' grey. An index chart
#      whose axis does not reach its base (a Crime Severity Index of 50 to 80,
#      2006 = 100) is cut like any other, zig-zag and all.
#    - Several panels with a y scale each (facets, scales = "free_y"), none
#      reaching zero: each panel is cut on its own, with its own gridlines,
#      floor, zig-zag and baseline (section 2b in the code, Greg 2026-10-03).
#      Before that, the first panel's gridlines and zig-zag were drawn in
#      every panel. If any panel reaches zero, the first panel still decides.
#
# 2c. A gridline beyond the data (Greg, 2026-10-05). Where the data run more
#    than a quarter of a gridline step (cwr_gridline_reach) above the top
#    gridline, a gridline is added a step higher and the panel stretched to
#    show it; the same below the lowest gridline, except that a cut axis
#    never gets one at or below zero. Text and blank layers do not count as
#    data. Panels with their own y scales are each checked on their own,
#    cut or reaching zero.
#
# 3. It sets the room for the numbers (Greg, 2026-10-02). The room between
#    the right end of the data (the last point of a line, the edge of the last
#    bar) and the panel's edge is made exactly the widest number's width plus
#    4 points of air (and a dot's radius when the chart draws points), by
#    setting a continuous x scale's right-hand expansion - narrower or wider
#    than the chunk's own `expand` asked for, on desktop and phone alike
#    (cwr_fit_right_room()). A discrete x scale (bars by category) cannot be
#    set that way, so there it is only checked: if the number needs more room
#    than there is, a warning is printed to the console while the post
#    renders, written out rather than raised because posts set
#    `warning: false`. Numbers that are wide in themselves are still better
#    rescaled - thousands with label_number(scale = 1e-3), and the subtitle
#    says so (rule 1c). `width` is the desktop image width in inches
#    (cwr_figure()'s default), used to turn the room into a share.
#
# 4. A chart of several panels (facets) each with its own y scale has its
#    numbers written into each panel as text, since ggplot2 would misplace
#    the pulled-back axis numbers of all but the widest panel (section 4 in
#    the code below).
#
# `index` is an index chart's base - 100 - and is left NULL on every other
# chart: cwr_right_axis(index = 100).
cwr_right_axis <- function(width = 8.3, index = NULL) {
  structure(list(width = width, index = index), class = "cwr_right_axis")
}

# Sets a continuous x scale's right-hand expansion so the room between the
# data's right end and the panel's edge is `need` points, on a panel
# `panel_width` points wide; used by cwr_right_axis() for the desktop version
# and cwr_phone_right_axis() for the phone's. The expanded range runs from
# lo - a * span to hi + b * span (span = hi - lo, the scale's limits; a and b
# the left and right multiplicative expansion), and the room is the part of
# it past the data's right end, so b is solved for:
#   (hi - right + b * span) / (span * (1 + a + b)) = need / panel_width
# Any additive expansion on the right is dropped. A discrete x scale (bars by
# category) is returned unchanged.
cwr_fit_right_room <- function(plot, built, need, panel_width) {
  built_x <- built$layout$panel_scales_x[[1]]
  if (!inherits(built_x, "ScaleContinuous")) return(plot)
  x_scale <- plot$scales$get_scales("x")
  x_scale <- if (is.null(x_scale)) built_x$clone() else x_scale$clone()
  limits <- as.numeric(built_x$get_limits())
  span <- diff(limits)
  short <- (limits[2] - cwr_data_right(built)) / span
  expand <- if (inherits(x_scale$expand, "waiver")) expansion(mult = 0.05) else x_scale$expand
  share <- need / panel_width
  expand[3] <- max(0, (share * (1 + expand[1]) - short) / (1 - share))
  expand[4] <- 0
  x_scale$expand <- expand
  suppressMessages(plot + x_scale)
}

# The rightmost x any layer draws at: the last point of a line, or the right
# edge of the last bar (xmax). A layer with no x, like geom_hline(), is skipped.
cwr_data_right <- function(built) {
  built$data |>
    map(\(layer) {
      right <- if ("xmax" %in% names(layer)) layer$xmax else layer$x
      right <- as.numeric(right)
      if (any(is.finite(right))) max(right[is.finite(right)]) else NA_real_
    }) |>
    unlist() |>
    max(na.rm = TRUE)
}

# The lowest y any layer draws at (a line's points, a ribbon's or bar's
# bottom, a reference line's yintercept)
cwr_data_bottom <- function(built) {
  built$data |>
    map(\(layer) {
      values <- unlist(layer[intersect(c("y", "ymin", "yintercept"), names(layer))])
      values <- as.numeric(values)
      if (any(is.finite(values))) min(values[is.finite(values)]) else NA_real_
    }) |>
    unlist() |>
    min(na.rm = TRUE)
}

# ---- Gridlines at both ends of the data ----------------------------------------
# How far, as a share of a gridline step, the data may run past the top
# gridline (or below the lowest one) before another gridline is added (Greg,
# 2026-10-05: "I don't like it when a line extends far beyond the highest
# gridline. It's too hard to gauge the value properly"). A quarter of a step
# is close enough to read against the gridline; further than that, the value
# is judged against the next gridline out instead. Change it here and nowhere
# else; re-render the post in hand to see it (house-style changes reach other
# drafts when Greg next works on them).
cwr_gridline_reach <- 0.25

# The highest and lowest y the chart's data reach, panel by panel: lines,
# points, bars, ribbons and reference lines (y, ymin, ymax, yend,
# yintercept). Text and blank layers are left out - a label above a point, or
# a geom_blank() that makes room, is not a value to be read against the
# gridlines - and so are annotations.
cwr_data_extent <- function(plot, built) {
  skip <- c("GeomText", "GeomLabel", "GeomShadowText", "GeomTextRepel", "GeomLabelRepel",
            "GeomBlank", "GeomCustomAnn", "GeomCwrCutMarks")
  map2(plot$layers, built$data, \(layer, data) {
    columns <- intersect(c("y", "ymin", "ymax", "yend", "yintercept"), names(data))
    if (inherits(layer$geom, skip) || length(columns) == 0 || !"PANEL" %in% names(data)) return(NULL)
    values <- map(columns, \(column) as.numeric(data[[column]]))
    tibble(PANEL = rep(data$PANEL, length(columns)), y = unlist(values))
  }) |>
    list_rbind() |>
    filter(is.finite(y)) |>
    summarise(top = max(y), bottom = min(y), .by = PANEL)
}

# The gridlines with one added above, below or both, wherever the data run
# more than cwr_gridline_reach of a step past the outermost ones. `top` and
# `bottom` are the data's extent. `floor_at_zero` stops a gridline being added
# at or below zero on an axis that does not reach it (a cut axis): that would
# claim the axis starts at zero when it does not.
cwr_gridlines_around <- function(breaks, top, bottom, floor_at_zero = FALSE) {
  breaks <- sort(breaks)
  if (length(breaks) < 2 || !is.finite(top) || !is.finite(bottom)) return(breaks)
  step <- min(diff(breaks))
  if (top > max(breaks) + cwr_gridline_reach * step) breaks <- c(breaks, max(breaks) + step)
  below <- min(breaks) - step
  if (bottom < min(breaks) - cwr_gridline_reach * step && !(floor_at_zero && below <= 0)) {
    breaks <- c(below, breaks)
  }
  breaks
}

# For a chart of several panels each with its own y scale, one row per panel
# saying how its axis is cut: its facet values (so a layer can be drawn in it
# alone), its limits' top, its gridlines, their step, the lowest of them, and
# the floor half a step below it that leaves the zig-zag room (rule 6a). NULL
# when the chart is not such a chart, or when any panel reaches zero or has
# fewer than two gridlines - then the first panel decides for all, as before.
cwr_panel_cuts <- function(plot, built) {
  panels <- built$layout$panel_params
  if (length(panels) < 2 || !isTRUE(plot$facet$params$free$y)) return(NULL)
  layout <- built$layout$layout
  facet_columns <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y"))

  # The lowest y drawn in each panel, across every layer
  panel_bottom <- built$data |>
    map(\(layer) {
      columns <- intersect(c("y", "ymin"), names(layer))
      if (length(columns) == 0 || !"PANEL" %in% names(layer)) return(NULL)
      tibble(PANEL = layer$PANEL, y = do.call(pmin, c(unname(as.list(layer[columns])), na.rm = TRUE)))
    }) |>
    list_rbind() |>
    filter(is.finite(y)) |>
    summarise(bottom = min(y), .by = PANEL)

  # A cut axis needs two gridlines at least, one to stand on and one to read
  # the step from. A short panel can get only one from the scale's breaks
  # ("16" alone on a range of 12.7 to 19.2), so it is offered finer and finer
  # steps, as breaks_extended() would choose them, until two fit. Steps of
  # 2.5 are left out (Q is the list of "nice" step multiples breaks_extended()
  # picks from), so a panel added this way reads 14, 16, 18, not 15.0, 17.5
  two_gridlines <- \(breaks, limits) {
    for (n in 4:8) {
      if (length(breaks) >= 2) break
      breaks <- breaks_extended(n = n, Q = c(1, 5, 2, 4, 3))(limits)
      breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]
    }
    breaks
  }

  cuts <- layout |>
    select(PANEL, SCALE_Y, all_of(facet_columns)) |>
    left_join(panel_bottom, join_by(PANEL)) |>
    mutate(
      limits = map(SCALE_Y, \(i) built$layout$panel_scales_y[[i]]$get_limits()),
      breaks = map2(PANEL, limits, \(i, limits) two_gridlines(sort(purrr::discard(panels[[i]]$y$breaks, is.na)), limits)),
      gridlines = lengths(breaks)
    )
  if (any(is.na(cuts$bottom)) || any(cuts$bottom <= 0) ||
      any(map_dbl(cuts$limits, 1) <= 0) || any(cuts$gridlines < 2)) return(NULL)

  # Another gridline above (or below) where a panel's data run more than
  # cwr_gridline_reach of a step past its outermost gridline (Greg,
  # 2026-10-05); never one at or below zero, since these panels are cut. The
  # panel's top is raised to reach a gridline added above its data.
  extent <- cwr_data_extent(plot, built) |>
    rename(data_top = top, data_bottom = bottom)
  cuts <- cuts |>
    left_join(extent, join_by(PANEL)) |>
    mutate(
      breaks = pmap(list(breaks, data_top, data_bottom), \(b, top, bottom) {
        cwr_gridlines_around(b, top, bottom, floor_at_zero = TRUE)
      }),
      step = map_dbl(breaks, \(b) min(diff(b))),
      lowest = map_dbl(breaks, min),
      floor = pmin(map_dbl(limits, 1), lowest - step / 2),
      top = pmax(map_dbl(limits, 2), map_dbl(breaks, max))
    ) |>
    select(all_of(facet_columns), top, breaks, step, lowest, floor)
  attr(cuts, "facet_columns") <- facet_columns
  cuts
}

# The zig-zag and the baseline of a cut axis, drawn in one panel: what
# section 2 of ggplot_add.cwr_right_axis() draws with annotation_custom() on a
# one-panel chart, as a layer so each panel of a faceted chart gets its own
# (an annotation is the same in every panel). Its one row per panel gives the
# right end of the data (x) and the lowest gridline (y). The baseline runs from
# the panel's left edge to that x, lifted half its width so the panel's edge
# does not clip it; the zig-zag - three strokes running down, 6 pt wide and
# 7.5 pt high - is right-aligned with the numbers and centred between the
# panel's bottom and the lowest gridline.
GeomCwrCutMarks <- ggproto("GeomCwrCutMarks", Geom,
  required_aes = c("x", "y"),
  draw_panel = function(data, panel_params, coord, zig_colour = "grey30",
                        base_colour = "black", base_lwd = 1) {
    spot <- coord$transform(data, panel_params)[1, ]
    lift <- unit(base_lwd * 0.75 / 2, "pt")
    grid::gList(
      grid::segmentsGrob(
        x0 = unit(0, "npc"), x1 = unit(spot$x, "npc"), y0 = lift, y1 = lift,
        gp = grid::gpar(col = base_colour, lwd = base_lwd, lineend = "butt")
      ),
      grid::polylineGrob(
        x = unit(1, "npc") - unit(c(6, 0, 6, 0), "pt"),
        y = unit(spot$y / 2, "npc") + unit(c(3.75, 1.25, -1.25, -3.75), "pt"),
        gp = grid::gpar(col = zig_colour, lwd = 1.4, linejoin = "mitre")
      )
    )
  }
)

# Drops the zeros every axis number shares at the end of its decimals
# (cwr-charts rule 9, Greg 2026-09-25): "0.0, 1.0, 2.0" becomes "0, 1, 2"
# and "0.50, 1.00, 1.50" becomes "0.5, 1.0, 1.5", but "1.0, 1.5, 2.0" is left
# alone, since the .5 needs its decimal and the others keep it to match. An
# `accuracy` that is finer than the breaks need is what makes such zeros, and
# it is easy to miss when the breaks change with the data. Prefixes and
# suffixes ("$1.0", "5.0%") are kept; a set of labels where any has no
# decimal point, or that is not text, is returned as it is.
cwr_trim_zeros <- function(labels) {
  if (!is.character(labels)) return(labels)
  shown <- labels[!is.na(labels) & labels != ""]
  fractions <- str_match(shown, "\\.(\\d+)")[, 2]
  if (length(shown) == 0 || anyNA(fractions)) return(labels)
  zeros <- min(str_length(fractions) - str_length(str_remove(fractions, "0+$")))
  if (zeros == 0) return(labels)
  str_replace(labels, "\\.\\d+", \(point) {
    digits <- str_sub(point, 2, -1 - zeros)
    if_else(digits == "", "", str_c(".", digits))
  })
}

# ggplot2 calls this when `+ cwr_right_axis()` is added to a plot, handing it
# the plot built so far; it returns that plot with the axis changes added.
# (This is how ggplot2 lets a package add something that depends on the plot
# itself; registering it in ggplot2's namespace makes ggplot2 find it.)
ggplot_add.cwr_right_axis <- function(object, plot, ...) {
  theme_now <- complete_theme(plot$theme)
  built <- ggplot_build(plot)
  panel <- built$layout$panel_params[[1]]

  # ---- 2. Zero and the baseline ----------------------------------------------
  # Work on a copy of the chart's y scale (clone() gives it an untrained range)
  y_scale <- plot$scales$get_scales("y")
  if (is.null(y_scale)) y_scale <- scale_y_continuous(position = "right")
  continuous <- inherits(y_scale, "ScaleContinuous")

  if (continuous) {
    y_scale <- y_scale$clone()

    # Numbers without zeros they all share at the end (cwr_trim_zeros() above)
    labeller <- y_scale$labels
    if (is.function(labeller)) y_scale$labels <- \(x) cwr_trim_zeros(labeller(x))
    y_range <- panel$y$continuous_range
    breaks <- panel$y$breaks
    breaks <- breaks[!is.na(breaks)]
    bottom <- cwr_data_bottom(built)
    # The data's own top and bottom, text and blank layers left out (taken
    # now, while the plot's layers still line up with the built data)
    extent <- cwr_data_extent(plot, built)
    expand <- if (inherits(y_scale$expand, "waiver")) expansion(mult = 0.05) else y_scale$expand
    base <- object$index
    indexed <- !is.null(base) && y_range[1] <= base && y_range[2] >= base

    # Panels with a y scale each (facets with scales = "free_y"), every one of
    # them cut: no panel's data or scale reaches zero, and each has two
    # gridlines or more. Each is then cut on its own (section 2b below);
    # otherwise the first panel decides for all, as on a one-panel chart.
    panel_cuts <- cwr_panel_cuts(plot, built)
    facet_cut <- !indexed && !is.null(panel_cuts)

    if (facet_cut) {
      # ---- 2b. Several panels, each cut on its own -----------------------------
      # (Greg, 2026-10-03, fig-age-mix in nd-population-forecast.) Each panel
      # keeps the gridlines ggplot2 chose for its own range, plus one beyond
      # data that run well past them (2c, in cwr_panel_cuts()), reaches half a
      # step below its lowest one and up to its top one (blank layers at that
      # floor and top, in that panel only), and gets its own zig-zag and
      # baseline (GeomCwrCutMarks below).
      # With free scales ggplot2 asks the breaks function once per panel,
      # handing it that panel's limits alone, so the function recognises the
      # panel by them - the one whose floor and top are nearest the limits'
      # two ends - and returns its gridlines. Choosing again from the widened
      # limits could pick a coarser step and drop the top gridline.
      facet_columns <- attr(panel_cuts, "facet_columns")
      y_scale$breaks <- \(limits) panel_cuts |>
        slice_min(abs(floor - limits[1]) + abs(top - limits[2]), n = 1, with_ties = FALSE) |>
        pull(breaks) |>
        pluck(1)
      dated_x <- inherits(built$layout$panel_scales_x[[1]], "ScaleContinuousDate")
      as_x <- if (dated_x) as.Date else identity
      baseline <- calc_element("axis.line.x.bottom", theme_now)
      plot <- plot +
        geom_blank(data = panel_cuts |> select(all_of(facet_columns), floor),
                   aes(y = floor), inherit.aes = FALSE) +
        # ... and up to a gridline added above the data, if one was
        geom_blank(data = panel_cuts |> select(all_of(facet_columns), top),
                   aes(y = top), inherit.aes = FALSE) +
        layer(
          geom = GeomCwrCutMarks, stat = "identity", position = "identity",
          data = panel_cuts |>
            select(all_of(facet_columns), lowest) |>
            mutate(right = as_x(cwr_data_right(built))),
          mapping = aes(x = right, y = lowest), inherit.aes = FALSE, show.legend = FALSE,
          params = list(
            zig_colour = calc_element("axis.text.y.right", theme_now)$colour,
            base_colour = baseline$colour,
            base_lwd = baseline$linewidth * .pt
          )
        ) +
        theme(axis.line.x.bottom = element_blank())
      cut_axis <- FALSE
    } else if (indexed) {
      # An index whose axis reaches its base: the base is labelled, and its
      # gridline is drawn in the baseline's colour, under the data (so it is
      # put first among the layers). The gridline's own width is kept.
      if (!base %in% breaks) y_scale$breaks <- sort(c(base, breaks))
      base_line <- geom_hline(
        yintercept = base,
        colour = calc_element("axis.line.x.bottom", theme_now)$colour,
        linewidth = calc_element("panel.grid.major.y", theme_now)$linewidth
      )
      plot$layers <- c(list(base_line), plot$layers)
      cut_axis <- FALSE
    } else if (bottom >= 0 && y_range[1] <= 0) {
      # Starts at zero: begin exactly at 0, no room below, 0 among the breaks
      limits <- y_scale$limits
      y_scale$limits <- if (is.numeric(limits)) c(0, limits[2]) else c(0, NA)
      expand[1:2] <- 0
      y_scale$expand <- expand
      if (!0 %in% breaks) y_scale$breaks <- sort(c(0, breaks))
      cut_axis <- FALSE
    } else if (bottom < 0 && y_range[2] > 0) {
      # Crosses zero: make sure 0 is labelled
      if (!0 %in% breaks) y_scale$breaks <- sort(c(0, breaks))
      cut_axis <- FALSE
    } else {
      # Does not reach zero: fix the breaks, then make sure the panel reaches
      # half a step below the lowest one, for the zig-zag
      cut_axis <- length(breaks) >= 2
      if (cut_axis) {
        y_scale$breaks <- breaks
        step <- min(diff(sort(breaks)))
        lowest <- min(breaks)
      }
    }

    # ---- 2c. A gridline beyond data that run well past the outermost one ---
    # (Greg, 2026-10-05.) Where the data reach more than cwr_gridline_reach of
    # a step above the top gridline, or below the lowest, a gridline is added
    # a step further out and the panel stretched to show it, so every value
    # can be read against a gridline near it. On a cut axis none is added at
    # or below zero. (Several cut panels do this panel by panel, in
    # cwr_panel_cuts().) Panels with a y scale each that reach zero (or an
    # index's base), like fig-three-places, are done panel by panel below:
    # one set of breaks set here would be forced on every panel.
    added <- numeric(0)
    free_y <- isTRUE(plot$facet$params$free$y) && length(built$layout$panel_params) > 1
    # What every panel's gridlines must include: the index's base, or zero
    # on an axis that starts at or crosses zero; NULL on a cut axis
    anchor <- if (indexed) base else if (y_range[1] <= 0 && y_range[2] >= 0) 0 else NULL
    per_panel <- NULL
    if (!facet_cut && !free_y && nrow(extent) > 0) {
      gridlines <- if (is.numeric(y_scale$breaks)) y_scale$breaks else breaks
      around <- cwr_gridlines_around(gridlines, max(extent$top), min(extent$bottom), floor_at_zero = cut_axis)
      added <- setdiff(around, gridlines)
      if (length(added) > 0) {
        y_scale$breaks <- around
        if (cut_axis) lowest <- min(around)
      }
    } else if (!facet_cut && free_y && !is.null(anchor) && nrow(extent) > 0) {
      # Each panel keeps the gridlines ggplot2 chose for its own range, plus
      # the anchor, plus any gridline the rule adds, and reaches out to them
      # (blank layers in that panel only). As in section 2b, ggplot2 asks the
      # breaks function once per panel with that panel's limits, so the
      # function finds the panel whose reach is nearest those limits.
      layout <- built$layout$layout
      facet_columns <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y"))
      panels <- built$layout$panel_params
      per_panel <- layout |>
        select(PANEL, all_of(facet_columns)) |>
        left_join(extent, join_by(PANEL)) |>
        mutate(
          breaks = map(PANEL, \(i) sort(unique(c(anchor, purrr::discard(panels[[i]]$y$breaks, is.na))))),
          breaks = pmap(list(breaks, top, bottom), \(b, top, bottom) cwr_gridlines_around(b, top, bottom)),
          reach_top = pmax(top, map_dbl(breaks, max), na.rm = TRUE),
          reach_bottom = pmin(bottom, map_dbl(breaks, min), na.rm = TRUE)
        )
      y_scale$breaks <- \(limits) per_panel |>
        slice_min(abs(reach_bottom - limits[1]) + abs(reach_top - limits[2]), n = 1, with_ties = FALSE) |>
        pull(breaks) |>
        pluck(1)
    }

    plot <- suppressMessages(plot + y_scale)   # quiet "Scale for y is already present"
    if (length(added) > 0) plot <- plot + expand_limits(y = added)
    if (!is.null(per_panel)) {
      plot <- plot +
        geom_blank(data = per_panel |> select(all_of(facet_columns), reach_top),
                   aes(y = reach_top), inherit.aes = FALSE) +
        geom_blank(data = per_panel |> select(all_of(facet_columns), reach_bottom),
                   aes(y = reach_bottom), inherit.aes = FALSE)
    }

    if (cut_axis) {
      if (y_range[1] > lowest - step / 2) plot <- plot + expand_limits(y = lowest - step / 2)

      # A date x axis takes its positions as dates (a bare number there
      # works, but ggplot2 warns about it)
      dated_x <- inherits(built$layout$panel_scales_x[[1]], "ScaleContinuousDate")
      as_x <- if (dated_x) as.Date else identity

      # The zig-zag: three strokes running down, 6 pt wide and 7.5 pt high, in
      # the axis numbers' colour, right-aligned with them and centred in the
      # space between the bottom of the panel and the lowest gridline. (Greg
      # turned the first, four-stroke horizontal version upright, cut it to
      # two strokes, then added a third and squeezed it, 2026-09-25.)
      number_colour <- calc_element("axis.text.y.right", theme_now)$colour
      zigzag <- grid::polylineGrob(
        x = unit(1, "npc") - unit(c(6, 0, 6, 0), "pt"),
        y = unit(0.5, "npc") + unit(c(3.75, 1.25, -1.25, -3.75), "pt"),
        gp = grid::gpar(col = number_colour, lwd = 1.4, linejoin = "mitre")
      )
      plot <- plot +
        annotation_custom(zigzag, xmin = as_x(-Inf), xmax = as_x(Inf), ymin = -Inf, ymax = lowest)

      # The baseline, stopping at the last data point. theme_cwr()'s axis line
      # is switched off and drawn again as a segment from the panel's left edge
      # to that point, in the same colour and width, lifted half its width so
      # the panel's edge does not clip it
      baseline <- calc_element("axis.line.x.bottom", theme_now)
      lwd <- baseline$linewidth * .pt
      plot <- plot +
        annotation_custom(
          grid::segmentsGrob(
            x0 = unit(0, "npc"), x1 = unit(1, "npc"),
            y0 = unit(lwd * 0.75 / 2, "pt"), y1 = unit(lwd * 0.75 / 2, "pt"),
            gp = grid::gpar(col = baseline$colour, lwd = lwd, lineend = "butt")
          ),
          xmin = as_x(-Inf), xmax = as_x(cwr_data_right(built)), ymin = -Inf, ymax = Inf
        ) +
        theme(axis.line.x.bottom = element_blank())
    }

    built <- ggplot_build(plot)
  }

  # ---- 1. The numbers -----------------------------------------------------
  # Every number the y scale draws, on every panel
  panels <- built$layout$panel_params
  numbers <- panels |>
    map(\(panel) panel$y$get_labels()) |>
    unlist() |>
    purrr::discard(is.na)   # purrr::, since scales has a discard() too
  # Two gridlines with the same number means the labels are rounded more
  # coarsely than the gridlines are spaced ("2, 2, 2" for 1.5, 2.0 and 2.5):
  # the chunk needs a finer `accuracy`, whose shared zeros are dropped anyway.
  # Checked panel by panel: facets that share a y scale repeat its numbers.
  if (some(panels, \(panel) anyDuplicated(purrr::discard(panel$y$get_labels(), is.na)) > 0)) {
    cat(
      "WARNING - cwr_right_axis(): two gridlines share a number (", str_flatten_comma(numbers), "). ",
      "Give the axis labels a finer accuracy.\n",
      sep = "", file = stderr()
    )
  }

  number_width <- max(systemfonts::string_width(
    numbers, family = cwr_font(), size = base_size * 0.8, res = 72
  ))

  # ---- 3. Room for the numbers, and no more -----------------------------------
  # The room on the right, past the data's right end, is set to exactly what
  # the widest number needs plus 4 points of air (and a dot's radius when the
  # chart draws points), by the x scale's right-hand expansion (Greg,
  # 2026-10-02: "The gap is too big between the end of the lines and the y
  # labels. This keeps happening" - the room used to be set by hand in each
  # chunk and only checked for being too small). The panel is the image less
  # the plot's side margins, shared between the columns of a faceted chart.
  plot_margin <- calc_element("plot.margin", theme_now)
  panel_width <- (object$width * 72 - as.numeric(plot_margin[2]) - as.numeric(plot_margin[4])) /
    max(built$layout$layout$COL)
  has_points <- plot$layers |>
    map_lgl(\(layer) inherits(layer$geom, "GeomPoint") && !identical(layer$aes_params$alpha, 0)) |>
    any()
  plot <- cwr_fit_right_room(plot, built, number_width + 4 + (if (has_points) 3 else 0), panel_width)
  built <- ggplot_build(plot)

  # Then checked, for an x scale the fitting cannot change (a bar chart's
  # categories): the share of the expanded x range past the data's right end,
  # times the panel's width
  panels <- built$layout$panel_params
  x_range <- panels[[1]]$x$continuous_range
  data_right <- cwr_data_right(built)
  room <- (x_range[2] - data_right) / diff(x_range) * panel_width - 4 - (if (has_points) 3 else 0)

  # (half a point of slack, since a fitted room equals the width exactly)
  if (number_width > room + 0.5) {
    cat(
      "WARNING - cwr_right_axis(): the axis numbers (", str_flatten_comma(unique(numbers)), ") are ",
      round(number_width), " pt wide, more than the ", round(room), " pt that clears the right end ",
      "of the data. Rescale the axis (for example to thousands) and say so in the subtitle, ",
      "or, if the numbers are already short, give the x scale more room on the right.\n",
      sep = "", file = stderr()
    )
  }

  # Remember the width, so cwr_figure() and cwr_interactive() can place the
  # phone version's numbers the same way (cwr_phone_right_axis() below).
  # `meta` is ggplot2's slot for a plot's own notes; it is kept as layers and
  # themes are added.
  plot@meta$cwr_right_axis <- number_width

  # ---- 4. Charts of several panels -------------------------------------------
  # The pulled-back numbers below rely on there being one axis. With several
  # panels, ggplot2 centres each panel's axis in a column as wide as the widest
  # one's, so a panel whose numbers are narrower ("20" beside "1,000") is
  # pushed right by half the difference and its numbers run off the chart
  # (found 2026-10-02, fig-three-places in nd-population-forecast). So a
  # faceted chart has its numbers written into each panel as text instead:
  # right-aligned at the panel's right edge, where the gridlines end, and just
  # above each gridline, as the axis puts them on a one-panel chart. The axis's
  # own numbers are switched off, and cwr_phone_right_axis() leaves them off.
  # Only panels with a y scale each (scales = "free_y" or "free"): panels that
  # share one scale draw identical numbers, so their axes line up, and side by
  # side only the right-hand panel carries them (the hospital charts in
  # wrhn-fsa).
  if (length(panels) > 1 && isTRUE(plot$facet$params$free$y)) {
    axis_text <- calc_element("axis.text.y.right", theme_now)
    layout <- built$layout$layout
    facet_columns <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "AXIS_X", "AXIS_Y"))
    panel_numbers <- layout |>
      select(PANEL, all_of(facet_columns)) |>
      mutate(
        y = map(PANEL, \(i) panels[[i]]$y$get_breaks()),
        label = map(PANEL, \(i) panels[[i]]$y$get_labels())
      ) |>
      select(-PANEL) |>
      unnest(c(y, label)) |>
      filter(!is.na(y), !is.na(label))
    plot <- plot +
      geom_text(
        data = panel_numbers, aes(x = Inf, y = y, label = label), inherit.aes = FALSE,
        hjust = 1, vjust = cwr_number_vjust(axis_text$size), family = axis_text$family,
        colour = axis_text$colour, size = axis_text$size / .pt
      ) +
      theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank())
    # The top number sits above the top gridline and can reach past the panel,
    # which would cut it off, so the panels draw past their edges: the chart's
    # own Cartesian coordinates again, same limits and expansion, clipping off
    # (a flipped or other coordinate system is left alone)
    coords <- plot$coordinates
    if (identical(class(coords)[1], "CoordCartesian")) {
      plot <- suppressMessages(plot + coord_cartesian(
        xlim = coords$limits$x, ylim = coords$limits$y, expand = coords$expand, clip = "off"
      ))
    }
    plot@meta$cwr_right_axis_layer <- TRUE
    return(plot)
  }

  plot +
    theme(
      axis.text.y.right = element_text(
        size = rel(1),
        hjust = 1.0,
        vjust = cwr_number_vjust(calc_element("axis.text.y.right", theme_now)$size),
        margin = margin(r = 0, l = -number_width)
      )
    )
}
registerS3method("ggplot_add", "cwr_right_axis", ggplot_add.cwr_right_axis,
                 envir = asNamespace("ggplot2"))

# The phone version of a chart drawn with cwr_right_axis(). cwr_phone_theme()
# moves right-axis numbers outside the panel, which is what older charts need;
# a chart drawn with cwr_right_axis() keeps them inside, right-aligned with the
# gridlines' ends, as on the desktop (rule 6, Greg 2026-09-25). The phone
# panel is half as wide, so the room on the right is set again for it: the
# widest number plus the same 4 points of air (and a dot's radius), narrower
# or wider as needed (cwr_fit_right_room()), on the phone only. cwr_figure()
# and cwr_interactive() call this
# after the chart's own `phone =` pieces; any other chart passes through.
cwr_phone_right_axis <- function(plot, phone_width) {
  # (tryCatch: a patchwork of several charts may have no `meta` slot to read)
  number_width <- tryCatch(plot@meta$cwr_right_axis, error = \(e) NULL)
  if (is.null(number_width)) return(plot)

  plot_margin <- calc_element("plot.margin", complete_theme(plot$theme))
  panel_width <- phone_width * 72 - as.numeric(plot_margin[2]) - as.numeric(plot_margin[4])
  has_points <- plot$layers |>
    map_lgl(\(layer) inherits(layer$geom, "GeomPoint") && !identical(layer$aes_params$alpha, 0)) |>
    any()
  need <- number_width + 4 + (if (has_points) 3 else 0)

  # The room set to exactly that, narrower as well as wider than the phone's
  # own `expand` gave, as cwr_right_axis() does for the desktop version
  built <- ggplot_build(plot)
  plot <- cwr_fit_right_room(plot, built, need, panel_width / max(built$layout$layout$COL))

  # A chart of several panels has its numbers drawn as text in the panels
  # (section 4 of ggplot_add.cwr_right_axis() above), so the axis's own stay
  # off: cwr_phone_theme() has switched them back on
  if (isTRUE(tryCatch(plot@meta$cwr_right_axis_layer, error = \(e) NULL))) {
    return(plot + theme(axis.text.y.right = element_blank()))
  }

  # (the phone theme sets its own text size, so the gap is worked out from it)
  plot +
    theme(
      axis.text.y.right = element_text(
        hjust = 1.0,
        vjust = cwr_number_vjust(calc_element("axis.text.y.right", complete_theme(plot$theme))$size),
        margin = margin(r = 0, l = -number_width)
      )
    )
}
