# figures.R
# ---------------------------------------------------------------------------
# cwr_figure(): how a finished chart reaches the page. It saves each chart
# twice, at desktop and phone width, and writes the <picture> element that
# shows the right one; the helpers above it work out how deep a chart has to be
# for its rows, wrap long category names, and shrink text for the phone.
#
# Sourced by R/theme_cwr.R, which every post sources in turn - so a post never
# names this file. Split out of theme_cwr.R on 2026-09-26, unchanged.
# ---------------------------------------------------------------------------

# ---- 6. Responsive figures ------------------------------------------------
# How deep one category row is drawn, in inches: the bar plus the gap below it.
# Taken from the income chart (now in the households-and-income post), the one
# that read best: a bar of the standard width = 0.7 is then about a quarter of
# an inch thick, deep enough to hold a direct label with room around it. The phone value is smaller by the
# same factor cwr_figure() shrinks label text by there (phone_text_scale), so a
# label fills its bar the same way on both.
cwr_row_height <- 0.37
cwr_phone_row_height <- 0.30

# Category names wrap at this many characters (Greg, 2026-09-18). On a phone,
# 15 - the length of "Crime in Canada", the longest one-line label that left a
# phone dot chart enough room. On a desktop, 30: the image is twice as wide,
# and a name of up to 30 characters takes about a third of it, which leaves the
# plot most of the page. A narrower label column is a wider plot. A row whose
# name wraps needs more depth, or the second line runs into the next name
# (style rule 8), so a chart with any wrapped name draws its rows this deep.
cwr_label_width <- 30
cwr_phone_label_width <- 15
cwr_wrapped_row_height <- 0.45
cwr_phone_wrapped_row_height <- 0.45
# Those depths hold a two-line name. Each line past two adds the height of one
# line of axis text (15 pt x 0.8, set solid, is about 0.17 in), so a long name
# that wraps to three or four lines on a phone does not run into the next.
cwr_wrapped_line_height <- 0.17

# Row depth for a chart whose longest name runs to `lines` lines
cwr_wrapped_depth <- function(lines, two_line_depth) {
  two_line_depth + max(lines - 2, 0) * cwr_wrapped_line_height
}

# Wrap a chart's category names (the discrete y axis) at `width`
# characters, breaking only between words, with cwr_wrap(). It wraps whatever
# the chart's own y scale would print - its labeller, a named vector, or the
# categories themselves - so charts need do nothing. A name already broken with
# <br> is joined up and re-wrapped at this width, and a bold name (**Region**)
# is bolded line by line so the markdown still closes on each line. Names
# carrying other HTML are left alone.
#
# Returns the plot, with a replacement y scale when anything wrapped, and the
# most lines any name now runs to.
cwr_wrap_category_labels <- function(plot, width = cwr_label_width) {
  y_scale <- plot$scales$get_scales("y")
  built <- ggplot_build(plot)
  built_y <- built$layout$panel_scales_y[[1]]
  if (is.null(built_y) || !built_y$is_discrete() || !is.finite(width)) {
    return(list(plot = plot, lines = 1))
  }

  # What the chart's scale prints for a set of categories, before wrapping
  original_labels <- if (is.null(y_scale)) waiver() else y_scale$labels
  print_labels <- function(breaks) {
    if (inherits(original_labels, "waiver")) return(as.character(breaks))
    if (is.function(original_labels)) return(as.character(original_labels(breaks)))
    if (!is.null(names(original_labels))) {
      return(coalesce(unname(original_labels[as.character(breaks)]), as.character(breaks)))
    }
    as.character(original_labels)
  }

  wrap_one <- function(label) {
    plain <- str_replace_all(label, "<br>", " ")
    bold <- str_detect(plain, "^\\*\\*.*\\*\\*$")
    plain <- str_remove_all(plain, "^\\*\\*|\\*\\*$")
    # Other markup cannot be split safely, so it is left as the chart set it
    if (str_detect(plain, "<|\\*")) return(label)
    if (str_length(plain) <= width) return(label)
    lines <- str_split_1(str_wrap(plain, width = width), "\n")
    if (bold) lines <- paste0("**", lines, "**")
    paste(lines, collapse = "<br>")
  }
  wrap_labels <- function(breaks) map_chr(print_labels(breaks), wrap_one)

  # Did any name on the chart need wrapping? Checked against every panel's
  # categories, since free-scale facets each have their own.
  all_breaks <- built$layout$panel_scales_y |>
    map(\(scale) scale$get_breaks()) |>
    unlist() |>
    unique()

  # A word is never split, so the label column can never be narrower than the
  # longest single word. Wrapping names to less than that narrows nothing and
  # only deepens every row: on the commuting post, "Blandford-Blenheim" (18
  # characters, one word) would have kept the column as wide as before while
  # "Centre Wellington" broke onto two lines. So the width rises to the longest
  # word when that is longer.
  longest_word <- print_labels(all_breaks) |>
    str_replace_all("<br>", " ") |>
    str_remove_all("\\*\\*") |>
    str_split("\\s+") |>
    unlist() |>
    str_length() |>
    max()
  width <- max(width, longest_word)
  before <- print_labels(all_breaks)
  after <- wrap_labels(all_breaks)
  # The most lines any name runs to, whether wrapped here or already
  # broken by the chart itself, since either needs the deeper row
  lines <- max(str_count(after, "<br>")) + 1
  if (identical(before, after)) return(list(plot = plot, lines = lines))

  # Replace the chart's y scale with a copy that wraps what it would print,
  # keeping everything else about it (limits, position, expansion)
  new_scale <- if (is.null(y_scale)) scale_y_discrete() else y_scale$clone()
  new_scale$labels <- wrap_labels
  list(plot = suppressMessages(plot + new_scale), lines = lines)
}

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

# The height, in inches, of a chart's plot area when it is drawn `height`
# inches tall: from the top of its top panel to the bottom of its bottom one,
# so a chart with a panel for each place counts its panels, and the headings
# and gaps between them, together. Measured on a real drawing, as
# cwr_fit_height() measures, since titles and notes wrap to the width.
cwr_plot_area_height <- function(plot, width, height, dpi) {
  trial_file <- tempfile(fileext = ".png")
  previous_device <- dev.cur()
  ragg::agg_png(trial_file, width = width, height = height, units = "in", res = dpi)
  on.exit({
    dev.off()
    if (previous_device > 1) dev.set(previous_device)
    unlink(trial_file)
  }, add = TRUE)
  table <- ggplotGrob(plot)
  grid.newpage()
  grid.draw(table)

  panel_cells <- table$layout |> filter(str_starts(name, "panel"))
  downViewport("layout")
  pushViewport(viewport(layout.pos.row = min(panel_cells$t):max(panel_cells$b),
                        layout.pos.col = min(panel_cells$l)))
  on.exit(popViewport(), add = TRUE, after = FALSE)
  convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
}

# ---- A plot area never squeezed by its text ------------------------------------
# A chart drawn at a fixed height (a line chart, a map, a chart of panels) gives
# its plot area whatever its title, subtitle, legend and notes leave, so a long
# note used to shrink the chart itself (Greg, 2026-10-06: "I don't want plot
# areas to shrink because of the notes"). Its plot area - all its panels
# together, from the top of the first to the bottom of the last - is now never
# less than these, in inches; cwr_hold_plot_area() makes the image taller by
# whatever the text took below them. A chart whose data need more asks for it
# with a larger `height` / `phone_height`, which only ever adds to the plot
# area. Set from the post where the rule began: the index charts, drawn 5 in
# tall with two lines of notes, have plot areas of 2.6 to 2.9 in on the
# desktop and 3.6 to 4 in on the phone; the most squeezed had 1.7 and 1.1.
cwr_min_plot_area <- 2.6
cwr_phone_min_plot_area <- 3.0

# The height to draw a chart at so its plot area is at least `minimum` inches.
# Left as it is: a chart whose y axis is categories (its height comes from its
# rows, which no text can squeeze), and one with a fixed shape (a map, or
# coord_fixed()), whose plot area a taller image would not make taller. The
# text above and below the plot area keeps its height whatever the image's
# height, so adding the shortfall to the image adds it to the plot area.
cwr_hold_plot_area <- function(plot, width, height, minimum, dpi = 288) {
  y_scale <- ggplot_build(plot)$layout$panel_scales_y[[1]]
  if (!is.null(y_scale) && y_scale$is_discrete()) return(height)
  if (!plot$coordinates$is_free() || !is.null(plot$theme$aspect.ratio)) return(height)
  area <- cwr_plot_area_height(plot, width, height, dpi)
  if (area >= minimum) height else round(height + minimum - area, 2)
}

# ---- Panel headings the same distance above their top gridlines ----------------
# On a chart of panels each with its own y scale, every heading sits the same
# distance above its panel's top gridline (Greg, 2026-10-06: "I'd like it to be
# more like the third panel for all of them, in all facet charts"). The
# headings are all the same distance from their panels' top edges, but the top
# gridline falls a different way below each edge - on the charts whose panels
# share a multiple of a base year (panel_tops() in the North Dumfries posts)
# North Dumfries' 15 sat at the edge and Ontario's 20 a fifth of the panel
# below it - so the space from heading to gridline varied.
#
# The panels' ranges are left alone (equal slopes stay equal): instead each
# heading is lifted, by a row of space added between it and its panel, until
# its gap matches the panel with the most room above its top gridline. The
# image is made taller by the same amount, so every panel keeps the size it was
# measured at and the line names placed against it stay where they belong.
# Returns the table to save (a gtable) and its height, or the plot and height
# unchanged when there is nothing to even out: one panel, panels sharing a y
# scale (their gaps are equal already), or headings not above the panels.
# (Alternative not taken: raise each panel's top to a fixed share above its
# top gridline, which breaks equal slopes on the panel_tops() charts.)
cwr_even_heading_gaps <- function(plot, width, height, dpi) {
  unchanged <- list(plot = plot, height = height)
  if (!inherits(plot, "ggplot") || !isTRUE(plot$facet$params$free$y)) return(unchanged)
  built <- ggplot_build(plot)
  panels <- built$layout$panel_params
  if (length(panels) < 2) return(unchanged)

  # Each panel's room above its top gridline, as a share of its height
  headroom <- map_dbl(panels, \(panel) {
    range <- panel$y$continuous_range
    breaks <- purrr::discard(panel$y$get_breaks(), is.na)
    breaks <- breaks[breaks >= range[1] & breaks <= range[2]]
    if (length(breaks) == 0) return(NA_real_)
    (range[2] - max(breaks)) / diff(range)
  })
  if (all(is.na(headroom))) return(unchanged)

  # Draw once, at the real size, to measure each panel's height in inches, as
  # cwr_fit_height() does
  trial_file <- tempfile(fileext = ".png")
  previous_device <- dev.cur()
  ragg::agg_png(trial_file, width = width, height = height, units = "in", res = dpi)
  on.exit({
    dev.off()
    if (previous_device > 1) dev.set(previous_device)
    unlink(trial_file)
  }, add = TRUE)
  table <- ggplotGrob(plot)
  grid.newpage()
  grid.draw(table)

  # The panels in the table, matched to the built plot's panels: facet_wrap()
  # names each "panel-<column>-<row>" and its heading "strip-t-<column>-<row>",
  # and the heading's row is the one right above the panel's
  cells <- table$layout |>
    filter(str_detect(name, "^panel-\\d+-\\d+$")) |>
    mutate(
      COL = as.integer(str_match(name, "^panel-(\\d+)-(\\d+)$")[, 2]),
      ROW = as.integer(str_match(name, "^panel-(\\d+)-(\\d+)$")[, 3]),
      strip = str_replace(name, "^panel-", "strip-t-")
    ) |>
    inner_join(built$layout$layout |> select(PANEL, ROW, COL), join_by(ROW, COL)) |>
    mutate(headroom = headroom[as.integer(PANEL)]) |>
    left_join(table$layout |> select(strip = name, strip_row = b), join_by(strip))
  if (nrow(cells) < 2 || anyNA(cells$strip_row)) return(unchanged)

  # Each panel's drawn height, and the space its heading needs added: the
  # difference between the most room above a top gridline and its own, in
  # inches. Panels side by side share a row, so the row takes the most any of
  # them needs.
  downViewport("layout")
  cells$panel_height <- map2_dbl(cells$t, cells$l, \(row, col) {
    pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
    on.exit(popViewport())
    convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
  })
  upViewport(0)
  # The room is compared in inches, so panels drawn at different heights
  # (ggh4x::force_panelsizes(), as panel_heights() in the North Dumfries posts
  # does) are evened out by the space actually drawn; for panels of one height
  # this is the same as comparing shares
  added <- cells |>
    mutate(room = headroom * panel_height, extra = max(room, na.rm = TRUE) - room) |>
    summarise(extra = max(extra, na.rm = TRUE), .by = strip_row) |>
    filter(is.finite(extra), extra > 0.005) |>
    arrange(desc(strip_row))
  if (nrow(added) == 0) return(unchanged)

  # A row of that height under each heading, from the bottom up so the row
  # numbers above stay right as rows go in
  for (i in seq_len(nrow(added))) {
    table <- gtable::gtable_add_rows(table, unit(added$extra[i], "in"), pos = added$strip_row[i])
  }
  list(plot = table, height = round(height + sum(added$extra), 3))
}

# ---- Line labels fitted to each drawing ----------------------------------------
# cwr_line_labels() (chart_helpers.R) places line names in millimetres on the
# page, so it has to know how big the panel is and how wide each name is - and
# both differ between the desktop and phone versions. These two functions
# measure them and hand them back to it; cwr_figure() and cwr_interactive()
# call cwr_fit_line_labels() once both versions' sizes are known.

# The panel of one drawing: its size in mm, the data range it shows
# (expansion included) and its y gridlines, plus the width in mm of each
# label in `labels` (a tibble of label, fontface, size and family). The chart
# is drawn into a throwaway PNG device of the real size, as cwr_fit_height()
# does, so everything is measured the way ggsave() will draw it. On a chart
# with several panels (facets), `panel` is the one to measure, by its PANEL
# number; NULL there returns NULL, and the labels keep their first guess.
cwr_measure_panel <- function(plot, width, height, dpi, labels, panel = NULL) {
  built <- ggplot_build(plot)
  if (length(built$layout$panel_params) != 1 && is.null(panel)) return(NULL)
  panel <- panel %||% 1
  params <- built$layout$panel_params[[panel]]
  # The panel's cell in the drawn table: facet_wrap() and facet_grid() name
  # each "panel-<column>-<row>"
  # (.env$panel: a facet column may itself be called "panel", as on fig-age-mix)
  spot <- built$layout$layout |> filter(as.integer(PANEL) == .env$panel)
  cell_name <- str_c("panel-", spot$COL, "-", spot$ROW)

  trial_file <- tempfile(fileext = ".png")
  previous_device <- dev.cur()
  ragg::agg_png(trial_file, width = width, height = height, units = "in", res = dpi)
  on.exit({
    dev.off()
    if (previous_device > 1) dev.set(previous_device)
    unlink(trial_file)
  }, add = TRUE)
  table <- ggplotGrob(plot)
  grid.newpage()
  grid.draw(table)

  # Step into the panel's cell of the drawn table and ask its size (the
  # first panel cell, if none has the expected name)
  panels <- table$layout |> filter(str_starts(name, "panel"))
  cell <- panels |> filter(name == cell_name)
  if (nrow(cell) == 0) cell <- panels
  cell <- slice(cell, 1)
  downViewport("layout")
  pushViewport(viewport(layout.pos.row = cell$t, layout.pos.col = cell$l))
  width_mm <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
  height_mm <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)
  # Each name in its own font, weight and size (ggplot2 sizes are mm; .pt
  # turns them into the points grid wants)
  label_mm <- pmap_dbl(labels, \(label, fontface, size, family) {
    convertWidth(
      grobWidth(textGrob(label, gp = gpar(fontfamily = family, fontface = fontface,
                                          fontsize = size * .pt))),
      "mm", valueOnly = TRUE
    )
  })
  upViewport(0)

  breaks <- params$y$breaks
  # The points drawn in the panel (geom_point() markers), for
  # cwr_point_labels() to tell whether a value could be read as a
  # neighbouring point's, and for cwr_line_labels() to keep a line's name
  # off its points (`radius`, in mm: geom_point()'s filled circle is 0.375
  # of its font size, its size times .pt plus half its stroke). Invisible
  # hover targets (alpha 0) are not drawn points.
  markers <- map2(plot$layers, built$data, \(layer, data) {
    if (!inherits(layer$geom, "GeomPoint") || !all(c("x", "y", "PANEL") %in% names(data))) return(NULL)
    # ggplot_build() fills in size, stroke and alpha; geom_point()'s own
    # defaults stand in should one ever be missing
    defaults <- list(alpha = NA_real_, size = 1.5, stroke = 0.5)
    data |>
      mutate(!!!defaults[setdiff(names(defaults), names(data))]) |>
      filter(as.integer(PANEL) == .env$panel, is.na(alpha) | alpha > 0) |>
      mutate(
        x = as.numeric(x), y = as.numeric(y),
        radius = 0.375 * (size * .pt + stroke * .stroke / 2) * 25.4 / 72.27
      ) |>
      select(x, y, radius)
  }) |>
    list_rbind()
  list(
    x_range = params$x.range,
    y_range = params$y.range,
    width_mm = width_mm,
    height_mm = height_mm,
    label_mm = set_names(label_mm, labels$label),
    breaks = breaks[!is.na(breaks)],
    markers = if (is.null(markers) || nrow(markers) == 0) NULL else markers
  )
}

# Leader lines for value labels (Greg, 2026-10-06: a 1976 value starting at
# its point ran on over the 1981 point, so it read as either's). Under every
# text layer made from cwr_point_labels(), a geom_segment() layer is added
# with the same data: a thin line from each point to its label, in the
# label's colour (mapped or set, as the label layer has it), drawn only for
# the labels cwr_point_labels() found unclear (leader_x and the rest are NA
# for the others). The segment layer shares the labels' data, so
# cwr_fit_line_labels() places it again with them, version by version. A
# chart adds nothing for this; one with no unclear label draws no line.
cwr_add_point_leaders <- function(plot) {
  is_point_labels <- \(layer) {
    d <- layer$data
    is.data.frame(d) && "cwr_place" %in% names(d) && nrow(d) > 0 &&
      identical(attr(d$cwr_place[[1]], "kind"), "point") &&
      inherits(layer$geom, c("GeomText", "GeomShadowText", "GeomLabel"))
  }
  spots <- which(map_lgl(plot$layers, is_point_labels))
  # From the last back, so inserting a layer does not move the ones still to do
  for (i in rev(spots)) {
    layer <- plot$layers[[i]]
    colour <- layer$mapping$colour
    mapping <- if (is.null(colour)) {
      aes(x = leader_x, xend = leader_x, y = leader_from, yend = leader_to)
    } else {
      aes(x = leader_x, xend = leader_x, y = leader_from, yend = leader_to, colour = !!colour)
    }
    params <- list(inherit.aes = FALSE, linewidth = 0.3, na.rm = TRUE)
    if (!is.null(layer$aes_params$colour)) params$colour <- layer$aes_params$colour
    leaders <- do.call(geom_segment, c(list(data = layer$data, mapping = mapping), params))
    plot$layers <- append(plot$layers, list(leaders), after = i - 1)
  }
  plot
}

# Finds every label layer made from cwr_line_labels() or cwr_point_labels()
# and works out its labels for both drawings: the phone first, where labels
# are largest against the chart, then the desktop at the phone's spots and
# sides. Returns three functions - desktop() and phone() put that version's
# labels in the layers before it is saved, and restore() puts the chart back
# as it was. A chart with no such layer gets functions that do nothing.
#
# A layer's labels may come from one call (the placement is then the table's
# attribute, and on every row) or from several bound together - one call per
# panel of a faceted chart (then only the rows carry it, in `cwr_place`, with
# `cwr_call` telling the calls apart; see cwr_carry_place() in
# chart_helpers.R). Each call is placed again against the panel it is drawn
# in: on a one-panel chart, that panel; on a faceted one, the panel its rows'
# facet columns name (the call's labels must carry them, as they do when the
# caller adds the facet column with mutate()). A call whose panel cannot be
# told keeps its first guess. Only the labels the caller kept are drawn (a
# name filtered out after the call stays out), and every column the caller
# added (the facet column, a colour) is carried over by label.
cwr_fit_line_labels <- function(desktop_plot, phone_plot, width, height,
                                phone_width, phone_height, phone_text_scale,
                                phone_point_scale = 0.65, dpi = 288) {
  placed_by_helper <- \(d) is.function(attr(d, "cwr_line_labels")) || "cwr_place" %in% names(d)
  layers <- keep(desktop_plot$layers, \(layer) is.data.frame(layer$data) && placed_by_helper(layer$data))
  nothing <- \() invisible(NULL)
  if (length(layers) == 0) return(list(desktop = nothing, phone = nothing, restore = nothing))

  # Layers are shared by the two plots (they behave like references), so a
  # change to one is a change to both - hence restore().
  originals <- map(layers, \(layer) layer$data)
  set_data <- function(datas) walk2(layers, datas, \(layer, d) layer$data <- d)
  restore <- \() set_data(originals)

  # Measured with the labels taken out, so a first-guess label beyond the
  # lines cannot stretch the scale the real labels are placed against
  set_data(map(originals, \(d) d[0, ]))
  on.exit(restore(), add = TRUE)

  # Which panel a call's labels are drawn in: the only one, or on a faceted
  # chart the one whose facet values the labels carry (NA if they carry
  # none, or several)
  layout <- ggplot_build(desktop_plot)$layout$layout
  facet_columns <- setdiff(names(layout), c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y", "COORD", "AXIS_X", "AXIS_Y"))
  panel_of <- function(rows) {
    if (nrow(layout) == 1) return(1L)
    if (length(facet_columns) == 0 || !all(facet_columns %in% names(rows))) return(NA_integer_)
    values <- rows |> select(all_of(facet_columns)) |> distinct()
    if (nrow(values) != 1) return(NA_integer_)
    # Matched as text, so a factor in one and a character in the other agree
    as_text <- \(d) d |> mutate(across(all_of(facet_columns), as.character))
    match <- as_text(layout) |> semi_join(as_text(values), join_by(!!!facet_columns))
    if (nrow(match) == 1) as.integer(match$PANEL) else NA_integer_
  }

  # The text layer's size and font for each call, so a layer that draws no
  # text but shares a call's data (the leader lines of cwr_add_point_leaders())
  # is placed with its labels' measurements, not the house default
  # (read from `originals`: the layers' own data are emptied above)
  # The phone measured as the phone is drawn: its points shrunk
  # (cwr_shrink_phone_points()) and cwr_on_phone() true, so a line's name
  # keeps clear of the points' phone size, not their desktop size
  measure_phone <- function(labels, panel) {
    restore_points <- cwr_shrink_phone_points(phone_plot$layers, phone_point_scale)
    on_phone <- options(cwr.phone = TRUE)
    on.exit({
      restore_points()
      options(on_phone)
    })
    cwr_measure_panel(phone_plot, phone_width, phone_height, dpi, labels, panel = panel)
  }

  call_font <- map2(layers, originals, \(layer, original) {
    if (!inherits(layer$geom, c("GeomText", "GeomShadowText", "GeomLabel")) ||
        !"cwr_call" %in% names(original)) return(NULL)
    tibble(cwr_call = unique(original$cwr_call),
           size = layer$aes_params$size %||% label_size,
           family = layer$aes_params$family %||% cwr_font())
  }) |>
    list_rbind()

  fitted <- map2(layers, originals, \(layer, original) {
    # The layer's own size and font, or the house ones (the geom defaults are
    # stored as unevaluated expressions, so the house font is asked directly);
    # a layer without text takes its call's text layer's (above)
    own_call <- if ("cwr_call" %in% names(original)) original$cwr_call[1] else NA_character_
    font <- if (!is.null(call_font) && nrow(call_font) > 0) call_font |> filter(cwr_call == own_call) |> slice(1)
    is_text <- inherits(layer$geom, c("GeomText", "GeomShadowText", "GeomLabel"))
    size <- if (!is_text && length(font) && nrow(font) == 1) font$size else layer$aes_params$size %||% label_size
    family <- if (!is_text && length(font) && nrow(font) == 1) font$family else layer$aes_params$family %||% cwr_font()

    # One call at a time: by `cwr_call`, or the whole table for a table made
    # before the column existed
    calls <- if ("cwr_call" %in% names(original)) split(original, original$cwr_call, drop = TRUE) else list(original)
    placed <- map(calls, \(rows) {
      place <- if ("cwr_place" %in% names(rows)) rows$cwr_place[[1]] else attr(original, "cwr_line_labels")
      panel <- panel_of(rows)
      if (is.na(panel)) return(list(phone = rows, desktop = rows))
      # Every label the placement places, kept by the caller or not (a name
      # dropped afterwards still stands in the way of the others), each in its
      # face: bold where the call said so
      measure <- attr(place, "measure") %||% rows$label
      faces <- if_else(measure %in% attr(place, "bold"), "bold", "plain")
      labels_at <- \(s) tibble(label = measure, fontface = faces, size = s, family = family)
      phone_panel <- measure_phone(labels_at(size * phone_text_scale), panel)
      desktop_panel <- cwr_measure_panel(desktop_plot, width, height, dpi, labels_at(size), panel = panel)
      if (is.null(phone_panel) || is.null(desktop_panel)) return(list(phone = rows, desktop = rows))
      phone <- place(c(phone_panel, size = size * phone_text_scale, warn = TRUE, phone = TRUE))
      desktop <- place(c(desktop_panel, size = size), keep = phone)

      # Only the labels the caller kept, with the columns the caller added
      # (matched by label), and x back as dates on a date axis
      added <- setdiff(names(rows), names(phone))
      extra <- rows |> select(label, all_of(added)) |> distinct(label, .keep_all = TRUE)
      finish <- function(d) {
        d <- d |> filter(label %in% rows$label) |> left_join(extra, join_by(label))
        if (inherits(rows$x, "Date")) d$x <- as.Date(d$x)
        if (inherits(rows$x, "POSIXct")) d$x <- as.POSIXct(d$x, tz = attr(rows$x, "tzone") %||% "")
        d
      }
      list(phone = finish(phone), desktop = finish(desktop))
    })
    list(phone = list_rbind(map(placed, "phone")), desktop = list_rbind(map(placed, "desktop")))
  })

  list(
    desktop = \() set_data(map(fitted, "desktop")),
    phone = \() set_data(map(fitted, "phone")),
    restore = restore
  )
}

# How far a line's name may sit from its line, in mm, before
# cwr_check_line_names() warns. cwr_line_labels() puts names 0.8 mm clear
# of their line's edge; 2.5 mm leaves room for a hand-placed name that is
# close enough to read as belonging to its line, and catches one that floats
# (Greg, 2026-10-06: "Watson" hung from the top of the panel, well above its
# line's end, on Figure 20 of nd-population-forecast).
cwr_line_name_reach <- 2.5

# A check run on each drawn version of every chart (desktop and phone): every
# word or phrase of text in the plot area drawn in the same colour as a line
# (geom_line, geom_path, geom_step) in its panel is taken to be that line's
# name, and the gap in mm between the text's box and the nearest point of a
# line of its colour is measured as drawn. A name farther than
# cwr_line_name_reach prints a WARNING to stderr (so a chunk's `warning:
# false` cannot hide it), naming the chart, the version and the name. Text
# with a digit in it is a value or a note, not a name, and is left out, as is
# text in a colour no line has (the grey "Historical" and "Forecast", arrow
# labels). A name placed by hand - a typed x and y, a vjust in line heights,
# y = Inf for the panel's top - is right only for the data and axis it was
# placed against; this catches it when either changes. The fix is to place it
# with cwr_line_labels() (cwr-charts rule 3a). Facets are measured panel by
# panel, each the size of the first (facet_wrap()'s panels are all one size).
# Any failure to measure is silent: this is a check, never a reason for a
# render to stop.
cwr_check_line_names <- function(plot, id, version, width, height, dpi = 288) {
  tryCatch({
    built <- ggplot_build(plot)
    is_geom <- \(layer, geoms) inherits(layer$geom, geoms)
    hex <- \(colour) {
      rgb <- grDevices::col2rgb(colour, alpha = FALSE)
      grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
    }

    # The lines, each point with its panel, group and colour
    # Groups are numbered afresh in each layer, so each is offset by its
    # layer's place, or two layers' lines would join into one
    lines <- pmap(list(plot$layers, built$data, seq_along(plot$layers)), \(layer, data, i) {
      if (!is_geom(layer, "GeomPath") || !all(c("x", "y", "colour", "group") %in% names(data))) return(NULL)
      tibble(PANEL = data$PANEL, group = i * 1e4 + data$group, colour = hex(data$colour),
             x = as.numeric(data$x), y = as.numeric(data$y))
    }) |>
      list_rbind()
    if (is.null(lines) || nrow(lines) == 0) return(invisible(NULL))
    # Segments too (geom_segment()): a legend key's short stroke of the
    # line's dash beside its name ("Regression" on fig-three-places), or a
    # reference line drawn as one segment
    segments <- pmap(list(plot$layers, built$data, seq_along(plot$layers)), \(layer, data, i) {
      if (!is_geom(layer, "GeomSegment") || !all(c("x", "xend", "y", "yend", "colour") %in% names(data))) return(NULL)
      ends <- tibble(PANEL = data$PANEL, group = -(i * 1e6 + seq_len(nrow(data))),
                     colour = hex(data$colour))
      bind_rows(
        ends |> mutate(x = as.numeric(data$x), y = as.numeric(data$y)),
        ends |> mutate(x = as.numeric(data$xend), y = as.numeric(data$yend))
      )
    }) |>
      list_rbind()
    lines <- bind_rows(lines, segments) |> filter(is.finite(x), is.finite(y))

    # A band drawn around a line (geom_ribbon(), an interval) counts as part
    # of it: a name set just outside the band is next to its line. Its two
    # edges join the lines in that line's colour wherever the line runs
    # inside the band (at 80% or more of its points across the band's span).
    bands <- map2(plot$layers, built$data, \(layer, data) {
      if (!is_geom(layer, "GeomRibbon") || !all(c("x", "ymin", "ymax") %in% names(data))) return(NULL)
      tibble(PANEL = data$PANEL, band = data$group,
             x = as.numeric(data$x), ymin = as.numeric(data$ymin), ymax = as.numeric(data$ymax))
    }) |>
      compact()
    band_edges <- imap(bands, \(band, b) {
      band |>
        filter(is.finite(x), is.finite(ymin), is.finite(ymax)) |>
        nest(.by = c(PANEL, band)) |>
        pmap(\(PANEL, band, data) {
          inside <- lines |>
            filter(PANEL == !!PANEL, between(x, min(data$x), max(data$x))) |>
            mutate(low = approx(data$x, data$ymin, x)$y, high = approx(data$x, data$ymax, x)$y) |>
            summarise(share = mean(y >= low - 1e-9 & y <= high + 1e-9), .by = c(group, colour)) |>
            filter(share >= 0.8)
          map2(inside$colour, seq_along(inside$colour), \(colour, i) bind_rows(
            tibble(PANEL = PANEL, group = -1000 * b - 2 * i, colour = colour, x = data$x, y = data$ymin),
            tibble(PANEL = PANEL, group = -1000 * b - 2 * i - 1, colour = colour, x = data$x, y = data$ymax)
          )) |>
            list_rbind()
        }) |>
        list_rbind()
    }) |>
      list_rbind()
    if (!is.null(band_edges) && nrow(band_edges) > 0) {
      lines <- bind_rows(lines, band_edges |> mutate(group = as.integer(group)))
    }

    # The names: text in a line's colour, without a digit. A text layer whose
    # data carries a `not_a_line_name` column is left out: words that share a
    # line's colour without naming it ("Historical" and "Forecast" at a
    # divide, in the same grey30 as regression lines; Greg, 2026-10-07)
    names <- map2(plot$layers, built$data, \(layer, data) {
      if (!is_geom(layer, c("GeomText", "GeomShadowText")) || !"label" %in% names(data) || nrow(data) == 0) return(NULL)
      if (is.data.frame(layer$data) && "not_a_line_name" %in% names(layer$data)) return(NULL)
      tibble(
        PANEL = data$PANEL, label = as.character(data$label), colour = hex(data$colour),
        x = as.numeric(data$x), y = as.numeric(data$y),
        hjust = suppressWarnings(as.numeric(data$hjust %||% 0.5)),
        vjust = suppressWarnings(as.numeric(data$vjust %||% 0.5)),
        size = data$size %||% label_size, lineheight = data$lineheight %||% 1.2,
        fontface = data$fontface %||% 1, family = data$family %||% ""
      )
    }) |>
      list_rbind()
    if (is.null(names) || nrow(names) == 0) return(invisible(NULL))
    names <- names |>
      filter(!is.na(label), !str_detect(label, "\\d"), str_trim(label) != "") |>
      semi_join(lines, join_by(PANEL, colour)) |>
      mutate(hjust = coalesce(hjust, 0.5), vjust = coalesce(vjust, 0.5),
             family = if_else(family == "", cwr_font(), family))
    if (nrow(names) == 0) return(invisible(NULL))

    # The panel's size as drawn, and each name's box, measured in a
    # throwaway device of the version's real size
    trial_file <- tempfile(fileext = ".png")
    previous_device <- dev.cur()
    ragg::agg_png(trial_file, width = width, height = height, units = "in", res = dpi)
    on.exit({
      dev.off()
      if (previous_device > 1) dev.set(previous_device)
      unlink(trial_file)
    }, add = TRUE)
    table <- ggplotGrob(plot)
    grid.newpage()
    grid.draw(table)
    # Each panel's own size: facet_wrap() names its cells "panel-<column>-<row>",
    # matched here to the built plot's panels, since panels can be drawn at
    # different heights (ggh4x::force_panelsizes()); any other layout takes the
    # first panel's size for all
    cells <- table$layout |> filter(str_starts(name, "panel"))
    downViewport("layout")
    cells <- cells |>
      mutate(
        w = map2_dbl(t, l, \(row, col) {
          pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
          on.exit(popViewport())
          convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
        }),
        h = map2_dbl(t, l, \(row, col) {
          pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
          on.exit(popViewport())
          convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)
        }),
        COL = as.integer(str_match(name, "^panel-(\\d+)-(\\d+)$")[, 2]),
        ROW = as.integer(str_match(name, "^panel-(\\d+)-(\\d+)$")[, 3])
      )
    sizes <- built$layout$layout |> select(PANEL, ROW, COL) |> left_join(cells, join_by(ROW, COL))
    panel_size <- \(PANEL) {
      size <- sizes |> filter(PANEL == !!PANEL)
      if (nrow(size) == 1 && !is.na(size$w)) c(size$w, size$h) else c(cells$w[1], cells$h[1])
    }
    pushViewport(viewport(layout.pos.row = cells$t[1], layout.pos.col = cells$l[1]))
    box <- pmap(names |> select(label, size, lineheight, fontface, family),
                \(label, size, lineheight, fontface, family) {
      grob <- textGrob(label, gp = gpar(fontfamily = family, fontface = fontface,
                                        fontsize = size * .pt, lineheight = lineheight))
      c(w = convertWidth(grobWidth(grob), "mm", valueOnly = TRUE),
        h = convertHeight(grobHeight(grob), "mm", valueOnly = TRUE))
    })
    upViewport(0)

    # Data to mm within a panel; Inf and -Inf are the panel's edges
    to_mm <- \(v, range, length) (pmin(pmax(v, range[1]), range[2]) - range[1]) / diff(range) * length
    params <- built$layout$panel_params
    gaps <- names |>
      mutate(w = map_dbl(box, "w"), h = map_dbl(box, "h")) |>
      pmap_dbl(\(PANEL, label, colour, x, y, hjust, vjust, w, h, ...) {
        p <- params[[as.integer(PANEL)]]
        panel_w <- panel_size(PANEL)[1]
        panel_h <- panel_size(PANEL)[2]
        left <- to_mm(x, p$x.range, panel_w) - hjust * w
        bottom <- to_mm(y, p$y.range, panel_h) - vjust * h
        # Each line of the name's colour in its panel, filled in every 0.25 mm
        # or so along each segment, then the nearest of those points to the box
        own <- lines |> filter(PANEL == !!PANEL, colour == !!colour) |>
          mutate(x = to_mm(x, p$x.range, panel_w), y = to_mm(y, p$y.range, panel_h))
        points <- own |>
          group_by(group) |>
          reframe(
            px = if (n() > 1) unlist(map2(head(x, -1), tail(x, -1), \(a, b) seq(a, b, length.out = 40))) else x,
            py = if (n() > 1) unlist(map2(head(y, -1), tail(y, -1), \(a, b) seq(a, b, length.out = 40))) else y
          )
        dx <- pmax(left - points$px, 0, points$px - (left + w))
        dy <- pmax(bottom - points$py, 0, points$py - (bottom + h))
        min(sqrt(dx^2 + dy^2))
      })

    far <- names |> mutate(gap = gaps) |> filter(gap > cwr_line_name_reach)
    pwalk(far, \(label, gap, ...) cat(
      "WARNING - ", id, " (", version, "): the name \"", label, "\" is drawn ",
      format(round(gap, 1), nsmall = 1), " mm from its line (more than ", cwr_line_name_reach,
      " mm). Place it with cwr_line_labels() instead of by hand (cwr-charts rule 3a).\n",
      sep = "", file = stderr()
    ))
    invisible(names |> mutate(gap = gaps))
  }, error = \(e) invisible(NULL))
}

# A check run on each drawn version of every chart (desktop and phone): every
# number written in the plot area is in the same units as the axis it sits
# against (Greg, 2026-10-07: Figure 1 of nd-population-forecast-sanity-check
# wrote "5,044" people over an axis counting in thousands - "5", "10", "15").
# For each panel, the axis's scale is read from its own gridlines: a gridline
# at 10,000 labelled "10" means the axis counts in thousands (10,000 / 10 =
# 1,000). Each in-plot label that is a bare number ("5,044", "13.1", "0.8%",
# "$2.5") is then turned back into axis units with that scale and compared
# with where it is drawn: a value label sits at or near its point or bar end
# (or halfway along a bar, for a label inside it). A label that lands four
# times or more away from its spot, and more than a quarter of the axis's
# range away, is in other units - a thousand times out, or a hundred - and
# prints a WARNING to stderr naming the chart, the version and the label.
#
# The axis checked is y, or x when y is a list of categories (a horizontal bar
# or dot chart). Left out, since their position says nothing about their
# value: labels that are not bare numbers ("Growth 2.6 times", "CAGR, 0.8%",
# a year inside words), labels signed with a "+" (a change, written beside the
# level it changed), a percentage against an axis of counts or a count against
# a percentage axis, a log or other bent axis, and any text layer whose data
# carries a `not_axis_units` column (the opt-out, for a number that genuinely
# is not a value on the axis). Any failure to measure is silent: this is a
# check, never a reason for a render to stop.
cwr_check_label_units <- function(plot, id, version) {
  tryCatch({
    built <- ggplot_build(plot)
    params <- built$layout$panel_params
    # A bare number, with an optional minus, dollar sign, thousands commas,
    # decimals and per cent sign; and its value, those marks taken off
    bare <- \(x) str_detect(x, "^\\s*[-−]?\\$?[0-9][0-9,]*(\\.[0-9]+)?\\s*%?\\s*$")
    number <- \(x) suppressWarnings(as.numeric(str_remove_all(str_replace_all(x, "−", "-"), "[,$%\\s]")))

    # Each panel's value axis: which one, its range, how many data units one
    # unit of its labels stands for, and whether its labels are percentages
    axes <- map(params, \(p) {
      side <- if (p$y$is_discrete() && !p$x$is_discrete()) "x" else "y"
      scale <- p[[side]]
      if (scale$is_discrete()) return(NULL)
      breaks <- scale$get_breaks()
      labels <- as.character(scale$get_labels())
      keep <- !is.na(breaks) & !is.na(labels) & bare(labels)
      value <- number(labels[keep])
      ratios <- breaks[keep][value != 0] / value[value != 0]
      # Fewer than two gridlines to read, or a scale that is not one number
      # throughout (a log axis), cannot be checked
      if (length(ratios) < 2 || diff(range(ratios)) > 0.01 * abs(mean(ratios))) return(NULL)
      list(side = side, per = mean(ratios), range = scale$continuous_range,
           percent = any(str_detect(labels[keep], "%")))
    })

    labels <- map2(plot$layers, built$data, \(layer, data) {
      if (!inherits(layer$geom, c("GeomText", "GeomShadowText", "GeomLabel")) ||
          !"label" %in% names(data) || nrow(data) == 0) return(NULL)
      if (is.data.frame(layer$data) && "not_axis_units" %in% names(layer$data)) return(NULL)
      tibble(PANEL = as.integer(data$PANEL), label = as.character(data$label),
             x = suppressWarnings(as.numeric(data$x)), y = suppressWarnings(as.numeric(data$y)))
    }) |>
      list_rbind()
    if (is.null(labels) || nrow(labels) == 0) return(invisible(NULL))

    off <- labels |>
      filter(!is.na(label), bare(label)) |>
      pmap(\(PANEL, label, x, y) {
        axis <- axes[[PANEL]]
        if (is.null(axis) || str_detect(label, "%") != axis$percent) return(NULL)
        drawn_at <- if (axis$side == "y") y else x
        if (!is.finite(drawn_at)) return(NULL)
        says <- number(label) * axis$per
        far <- abs(says - drawn_at) > 0.25 * diff(axis$range)
        out <- drawn_at == 0 || says == 0 || says / drawn_at >= 4 || says / drawn_at <= 0.25
        if (far && out) tibble(label = label, says = says, drawn_at = drawn_at, per = axis$per)
      }) |>
      list_rbind()

    if (!is.null(off) && nrow(off) > 0) pwalk(off, \(label, says, drawn_at, per) cat(
      "WARNING - ", id, " (", version, "): the label \"", label, "\" reads as ",
      label_comma(accuracy = signif(abs(says), 2) / 100)(says), " in the axis's units (its numbers count in ",
      label_comma()(per), "s) but is drawn at ", label_comma(accuracy = signif(abs(drawn_at), 2) / 100)(drawn_at),
      ". Write in-plot numbers in the axis's units - in thousands when the axis is - ",
      "and say so in the subtitle once (cwr-charts rule 1c).\n",
      sep = "", file = stderr()
    ))
    invisible(off)
  }, error = \(e) invisible(NULL))
}

# Years on the phone's x axis in the Economist's style, a house standard
# (Greg, 2026-10-04): the first year in full, then the last two digits, and a
# new century's first year in full again - 2021, 26, 31; 1996, 2001, 06 - by
# cwr_year_labels() (chart_helpers.R). Done here for every chart, so none has
# to ask: when every label the phone's x axis would draw is a plain four-digit
# year, its labels are replaced; any other axis (amounts, ages, "2021-22",
# months) is left as it is. Works for number, date ("%Y") and category axes
# alike, since it reads the drawn labels, not the scale's type. The desktop
# keeps every year in full.
cwr_phone_year_labels <- function(plot) {
  axis <- tryCatch(get_guide_data(plot, "x"), error = \(e) NULL)
  # Breaks outside the axis's range come back without a label
  drawn <- if (is.null(axis)) character(0) else purrr::discard(as.character(axis$.label), is.na)
  if (length(drawn) == 0 || !all(str_detect(drawn, "^\\d{4}$"))) return(plot)
  # The chart's own x scale, or, when it never named one, the default ggplot2
  # chose for it as it was built
  scale <- plot$scales$get_scales("x") %||% ggplot_build(plot)$plot$scales$get_scales("x")
  if (is.null(scale)) return(plot)
  economist <- scale$clone()
  # The breaks arrive as the axis holds them: dates, numbers or category names
  economist$labels <- \(breaks) {
    years <- if (inherits(breaks, c("Date", "POSIXt"))) year(breaks) else as.numeric(as.character(breaks))
    cwr_year_labels(years)
  }
  # Replacing a scale prints a note that one was already there; that is the point
  suppressMessages(plot + economist)
}

# Points are drawn smaller on the phone (Greg, 2026-10-02: "points are too big
# on the phone. this is a recurring problem"). A point's size is in mm, the
# same on both versions, but the phone image is half as wide, so a dot that
# reads as a marker on the desktop looks like a blob on the phone. Every
# geom_point() layer (and ggiraph's interactive points, a kind of GeomPoint)
# is shrunk in place by `scale` while the phone version is drawn - a layer
# with no size of its own starts from the geom's default - and the returned
# function puts every size back. Left alone: a layer whose size is mapped to
# the data (a bubble chart's scale sets those), and invisible hover targets
# (alpha = 0), which are only there to be touched.
cwr_shrink_phone_points <- function(layers, scale) {
  point_layers <- keep(layers, function(layer) {
    inherits(layer$geom, "GeomPoint") &&
      is.null(layer$mapping$size) &&
      !identical(layer$aes_params$alpha, 0)
  })
  original_sizes <- map(point_layers, \(layer) layer$aes_params$size)
  walk2(point_layers, original_sizes, \(layer, size) {
    layer$aes_params$size <- (size %||% layer$geom$default_aes$size) * scale
  })
  # Back to what each layer had, which for a layer with no size of its own
  # means removing the one set here (assigning NULL removes it)
  \() walk2(point_layers, original_sizes, \(layer, size) layer$aes_params$size <- size)
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
# Five things are changed automatically for the phone version, because the
# panel is half as wide: titles wrap; right-hand axis labels move outside the
# panel (the templates tuck them inside, above the gridlines, which collides
# with the data on a narrow panel); category names longer than
# `phone_label_width` characters wrap, with deeper rows to hold them (see
# cwr_wrap_category_labels(); the desktop does the same past `label_width`);
# text drawn with geom_text/geom_label/cwr_label is scaled
# by phone_text_scale so value labels stay narrower than bars; and points
# are drawn at phone_point_scale of their size (cwr_shrink_phone_points()).
# Pass phone_point_scale = 1 to keep a chart's points at their desktop size.
# A sixth: years on the x axis are written the Economist's way, 2021, 26, 31
# (cwr_phone_year_labels()).
# Anything else that only the phone version needs goes in `phone`, a list of
# ggplot pieces added to it with `+`, for example:
#   phone = list(scale_x_date(date_breaks = "2 years", date_labels = "%Y"))
#   phone = list(theme(legend.position = "none"))
cwr_figure <- function(plot, id, alt, caption = NULL, number = NULL,
                       width = 8.3, height = NULL,
                       phone_width = 4.2, phone_height = NULL,
                       phone = list(), phone_text_scale = 0.8, phone_point_scale = 0.65, phone_gap = 1,
                       row_height = cwr_row_height,
                       phone_row_height = cwr_phone_row_height,
                       label_width = cwr_label_width,
                       phone_label_width = cwr_phone_label_width,
                       dpi = 288, draft_phone = FALSE) {
  stopifnot(str_starts(id, "fig-"))

  # Note keys in the title and subtitle match the numbered notes (chart_helpers.R)
  cwr_check_note_keys(plot, id)

  # Leader lines for value labels that need one (cwr_add_point_leaders() above)
  plot <- cwr_add_point_leaders(plot)

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
  # Years on the x axis in the Economist's style (2021, 26, 31)
  phone_plot <- cwr_phone_year_labels(phone_plot)
  # A chart drawn with cwr_right_axis() keeps its numbers inside the panel,
  # right-aligned with the gridlines, on the phone too
  phone_plot <- cwr_phone_right_axis(phone_plot, phone_width)

  # Long category names wrap - past 30 characters on the desktop, past 15
  # ("Crime in Canada") on the phone - which narrows the label column and
  # widens the plot (cwr_wrap_category_labels() above). A wrapped name needs a
  # deeper row, so that version's rows deepen, unless the chart set its own
  # row_height / phone_row_height, and its bars and tiles are thinned in step
  # further down so they stay the house thickness. Pass label_width = Inf or
  # phone_label_width = Inf to keep a chart's names on one line.
  desktop_wrap <- cwr_wrap_category_labels(plot, label_width)
  desktop_plot <- desktop_wrap$plot
  desktop_thickness <- 1
  if (desktop_wrap$lines > 1 && missing(row_height)) {
    wrapped_depth <- cwr_wrapped_depth(desktop_wrap$lines, cwr_wrapped_row_height)
    desktop_thickness <- row_height / wrapped_depth
    row_height <- wrapped_depth
  }

  phone_wrap <- cwr_wrap_category_labels(phone_plot, phone_label_width)
  phone_plot <- phone_wrap$plot
  phone_thickness <- 1
  if (phone_wrap$lines > 1 && missing(phone_row_height)) {
    wrapped_depth <- cwr_wrapped_depth(phone_wrap$lines, cwr_phone_wrapped_row_height)
    phone_thickness <- phone_row_height / wrapped_depth
    phone_row_height <- wrapped_depth
  }

  # Heights not given are worked out from the rows (see cwr_fit_height() above).
  # The phone version is measured on its own, after its adjustments, because
  # they can change its shape - stacking side-by-side panels doubles the rows.
  # A chart with no category axis and no height given falls back to 5 in, and
  # 90% of that on a phone.
  if (is.null(height)) {
    height <- cwr_fit_height(desktop_plot, width, row_height, dpi) %||% 5
  }
  if (is.null(phone_height)) {
    phone_height <- cwr_fit_height(phone_plot, phone_width, phone_row_height, dpi) %||%
      (height * 0.9)
  }
  # A plot area at least cwr_min_plot_area / cwr_phone_min_plot_area tall,
  # however long the notes (cwr_hold_plot_area() above)
  height <- cwr_hold_plot_area(desktop_plot, width, height, cwr_min_plot_area, dpi)
  phone_height <- cwr_hold_plot_area(phone_plot, phone_width, phone_height, cwr_phone_min_plot_area, dpi)

  # Deeper rows (wrapped names, above) would draw thicker bars, since a bar's
  # thickness is a share of its row. Thin them by the same factor, in place,
  # the way the phone text is shrunk below: a bar's `width`, a tile's `height`.
  # Layers are shared by the desktop and phone plots, so each version sets its
  # own factor from the original before it is saved, and the original is put
  # back afterwards.
  thickness_param <- function(layer) {
    if (inherits(layer$geom, "GeomBar") && !is.null(layer$aes_params$width)) return("width")
    if (inherits(layer$geom, "GeomTile") && !is.null(layer$aes_params$height)) return("height")
    NA_character_
  }
  thick_layers <- keep(plot$layers, \(layer) !is.na(thickness_param(layer)))
  thick_params <- map_chr(thick_layers, thickness_param)
  original_thickness <- map2(thick_layers, thick_params, \(layer, param) layer$aes_params[[param]])
  set_thickness <- function(factor) {
    pwalk(list(thick_layers, thick_params, original_thickness),
          \(layer, param, value) layer$aes_params[[param]] <- value * factor)
  }
  on.exit(set_thickness(1), add = TRUE)

  # Line names from cwr_line_labels(), placed against each version's own
  # measurements (see cwr_fit_line_labels() above)
  line_labels <- cwr_fit_line_labels(desktop_plot, phone_plot, width, height,
                                     phone_width, phone_height, phone_text_scale,
                                     phone_point_scale = phone_point_scale, dpi = dpi)
  on.exit(line_labels$restore(), add = TRUE)

  set_thickness(desktop_thickness)
  line_labels$desktop()
  # Panel headings the same distance above their top gridlines, the image
  # taller by the space added (cwr_even_heading_gaps() above)
  desktop_out <- cwr_even_heading_gaps(desktop_plot, width, height, dpi)
  ggsave(desktop_file, desktop_out$plot, width = width, height = desktop_out$height,
         dpi = dpi, bg = "white", device = ragg::agg_png)
  # Every line's name close to its line, as drawn (see above)
  cwr_check_line_names(desktop_plot, id, "desktop", width, height, dpi)
  # Numbers in the plot in the same units as their axis (see above)
  cwr_check_label_units(desktop_plot, id, "desktop")

  # Shrink text geoms for the phone. Layers are ggproto objects, which behave
  # like references: the phone plot and the desktop plot share them. So the
  # desktop PNG is saved first (above), the sizes are changed in place for the
  # phone PNG, and then put back so the caller's plot is left as it was.
  text_layers <- keep(phone_plot$layers, function(layer) {
    # GeomShadowText (cwr_label()) and GeomTextRepel (cwr_label_repel()) are
    # geoms of their own, not kinds of GeomText, so they have to be named here
    # or a haloed label would keep its desktop size
    inherits(layer$geom, c("GeomText", "GeomLabel", "GeomShadowText", "GeomTextRepel")) &&
      !is.null(layer$aes_params$size)
  })
  original_sizes <- map(text_layers, \(layer) layer$aes_params$size)
  restore_sizes <- function() {
    walk2(text_layers, original_sizes, \(layer, size) layer$aes_params$size <- size)
  }
  on.exit(restore_sizes(), add = TRUE)
  walk(text_layers, \(layer) layer$aes_params$size <- layer$aes_params$size * phone_text_scale)
  # Points smaller too, and put back the same way
  restore_points <- cwr_shrink_phone_points(phone_plot$layers, phone_point_scale)
  on.exit(restore_points(), add = TRUE)

  set_thickness(phone_thickness)
  line_labels$phone()
  # A gap written in data units is half as wide on a phone, where the same
  # scale is drawn across half the width, so a label nudged clear of its point
  # on a desktop can end up touching it. A chart writes such a gap as
  # `nudge * cwr_gap()` and passes `phone_gap`, the number the gap is
  # multiplied by while the phone version is drawn (2 keeps it the same
  # distance on the page). The aes is evaluated as the chart is saved, so
  # setting the option here reaches it. cwr.phone does the same for
  # cwr_on_phone(), for text that differs on the phone (a label wrapped).
  phone_gap_option <- options(cwr.gap = phone_gap, cwr.phone = TRUE)
  phone_out <- cwr_even_heading_gaps(phone_plot, phone_width, phone_height, dpi)
  ggsave(phone_file, phone_out$plot, width = phone_width, height = phone_out$height,
         dpi = dpi, bg = "white", device = ragg::agg_png)
  cwr_check_line_names(phone_plot, id, "phone", phone_width, phone_height, dpi)
  cwr_check_label_units(phone_plot, id, "phone")
  options(phone_gap_option)

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
    'width="', round(phone_width * 96), '" height="', round(phone_out$height * 96), '">\n',
    '<img src="', desktop_file, '" alt="', alt, '" class="', img_class, '" ',
    'width="', round(width * 96), '" height="', round(desktop_out$height * 96), '">\n',
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
