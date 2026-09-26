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
# Four things are changed automatically for the phone version, because the
# panel is half as wide: titles wrap; right-hand axis labels move outside the
# panel (the templates tuck them inside, above the gridlines, which collides
# with the data on a narrow panel); category names longer than
# `phone_label_width` characters wrap, with deeper rows to hold them (see
# cwr_wrap_category_labels(); the desktop does the same past `label_width`);
# and text drawn with geom_text/geom_label/cwr_label is scaled
# by phone_text_scale so value labels stay narrower than bars.
# Anything else that only the phone version needs goes in `phone`, a list of
# ggplot pieces added to it with `+`, for example:
#   phone = list(scale_x_date(date_breaks = "2 years", date_labels = "%Y"))
#   phone = list(theme(legend.position = "none"))
cwr_figure <- function(plot, id, alt, caption = NULL, number = NULL,
                       width = 8.3, height = NULL,
                       phone_width = 4.2, phone_height = NULL,
                       phone = list(), phone_text_scale = 0.8, phone_gap = 1,
                       row_height = cwr_row_height,
                       phone_row_height = cwr_phone_row_height,
                       label_width = cwr_label_width,
                       phone_label_width = cwr_phone_label_width,
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

  set_thickness(desktop_thickness)
  ggsave(desktop_file, desktop_plot, width = width, height = height,
         dpi = dpi, bg = "white", device = ragg::agg_png)

  # Shrink text geoms for the phone. Layers are ggproto objects, which behave
  # like references: the phone plot and the desktop plot share them. So the
  # desktop PNG is saved first (above), the sizes are changed in place for the
  # phone PNG, and then put back so the caller's plot is left as it was.
  text_layers <- keep(phone_plot$layers, function(layer) {
    # GeomShadowText (cwr_label()) is its own geom, not a kind of GeomText, so
    # it has to be named here or a haloed label would keep its desktop size
    inherits(layer$geom, c("GeomText", "GeomLabel", "GeomShadowText")) &&
      !is.null(layer$aes_params$size)
  })
  original_sizes <- map(text_layers, \(layer) layer$aes_params$size)
  restore_sizes <- function() {
    walk2(text_layers, original_sizes, \(layer, size) layer$aes_params$size <- size)
  }
  on.exit(restore_sizes(), add = TRUE)
  walk(text_layers, \(layer) layer$aes_params$size <- layer$aes_params$size * phone_text_scale)

  set_thickness(phone_thickness)
  # A gap written in data units is half as wide on a phone, where the same
  # scale is drawn across half the width, so a label nudged clear of its point
  # on a desktop can end up touching it. A chart writes such a gap as
  # `nudge * cwr_gap()` and passes `phone_gap`, the number the gap is
  # multiplied by while the phone version is drawn (2 keeps it the same
  # distance on the page). The aes is evaluated as the chart is saved, so
  # setting the option here reaches it.
  phone_gap_option <- options(cwr.gap = phone_gap)
  ggsave(phone_file, phone_plot, width = phone_width, height = phone_height,
         dpi = dpi, bg = "white", device = ragg::agg_png)
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
