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
# 1. The numbers (rule 6) sit inside the panel, each just above its gridline
#    and ending exactly where the gridline ends. theme_cwr() draws right-axis
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
#
# 3. It checks the numbers clear the data. The room between the right end of
#    the data (the last point of a line, the edge of the last bar) and the
#    panel's edge comes from the x scale's expansion; if the widest number
#    needs more than that, less 4 points of air (and a dot's radius when the
#    chart draws points), a warning is printed to the console while the post
#    renders, written out rather than raised because posts set
#    `warning: false`. The fix is to rescale the axis - thousands with
#    label_number(scale = 1e-3), and the subtitle says so (rule 1c) - rather
#    than to widen the room, which should be the same share on every chart of
#    a kind. Only numbers that are already short (a bar chart whose last bar
#    sits close to the edge, say) call for more room, through the x scale's
#    `expand`. `width` is the desktop image width in inches (cwr_figure()'s
#    default), used to turn that share into points.
#
# `index` is an index chart's base - 100 - and is left NULL on every other
# chart: cwr_right_axis(index = 100).
cwr_right_axis <- function(width = 8.3, index = NULL) {
  structure(list(width = width, index = index), class = "cwr_right_axis")
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
    expand <- if (inherits(y_scale$expand, "waiver")) expansion(mult = 0.05) else y_scale$expand
    base <- object$index
    indexed <- !is.null(base) && y_range[1] <= base && y_range[2] >= base

    if (indexed) {
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

    plot <- suppressMessages(plot + y_scale)   # quiet "Scale for y is already present"

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
  # the chunk needs a finer `accuracy`, whose shared zeros are dropped anyway
  if (anyDuplicated(numbers) > 0) {
    cat(
      "WARNING - cwr_right_axis(): two gridlines share a number (", str_flatten_comma(numbers), "). ",
      "Give the axis labels a finer accuracy.\n",
      sep = "", file = stderr()
    )
  }

  number_width <- max(systemfonts::string_width(
    numbers, family = cwr_font(), size = base_size * 0.8, res = 72
  ))

  # ---- 3. Do the numbers clear the data? -------------------------------------
  # The room on the right, in points: the share of the expanded x range past
  # the data's right end, times the panel's width (the image less the plot's
  # side margins)
  x_range <- panels[[1]]$x$continuous_range
  data_right <- cwr_data_right(built)
  plot_margin <- calc_element("plot.margin", theme_now)
  panel_width <- object$width * 72 - as.numeric(plot_margin[2]) - as.numeric(plot_margin[4])
  has_points <- plot$layers |>
    map_lgl(\(layer) inherits(layer$geom, "GeomPoint") && !identical(layer$aes_params$alpha, 0)) |>
    any()
  room <- (x_range[2] - data_right) / diff(x_range) * panel_width - 4 - (if (has_points) 3 else 0)

  if (number_width > room) {
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

  plot +
    theme(
      axis.text.y.right = element_text(
        size = rel(1),
        hjust = 1.0,
        vjust = -0.5,
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
# panel is half as wide, so the room the x scale leaves on the right is half
# as many points: if the widest number (plus the same 4 points of air, and a
# dot's radius) no longer fits, the x scale's right-hand expansion is widened
# just enough, on the phone only. cwr_figure() and cwr_interactive() call this
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

  built <- ggplot_build(plot)
  x_range <- built$layout$panel_params[[1]]$x$continuous_range
  room <- (x_range[2] - cwr_data_right(built)) / diff(x_range) * panel_width

  x_scale <- plot$scales$get_scales("x")
  if (room < need && inherits(x_scale, "ScaleContinuous")) {
    # With the data spanning 1, the panel spans 1 + left + right expansion;
    # the right expansion's share of that must be at least need / panel_width
    x_scale <- x_scale$clone()
    expand <- if (inherits(x_scale$expand, "waiver")) expansion(mult = 0.05) else x_scale$expand
    share <- need / panel_width
    expand[3] <- share * (1 + expand[1]) / (1 - share)
    x_scale$expand <- expand
    plot <- suppressMessages(plot + x_scale)
  }

  plot +
    theme(
      axis.text.y.right = element_text(
        hjust = 1.0,
        vjust = -0.5,
        margin = margin(r = 0, l = -number_width)
      )
    )
}
