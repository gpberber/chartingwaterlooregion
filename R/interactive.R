# interactive.R
# ---------------------------------------------------------------------------
# cwr_interactive(): the same house chart, drawn as an SVG in the page with
# ggiraph so it can answer the mouse. Two versions again, desktop and phone,
# and every id named after the chart so a re-render changes nothing by itself.
#
# Sourced by R/theme_cwr.R, which every post sources in turn - so a post never
# names this file. Split out of theme_cwr.R on 2026-09-26, unchanged.
# ---------------------------------------------------------------------------

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
  # A chart drawn with cwr_right_axis() keeps its numbers inside the panel,
  # right-aligned with the gridlines, on the phone too
  phone_plot <- cwr_phone_right_axis(phone_plot, phone_width)

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

  # Every id in the widget is named after the chart, not drawn at random.
  #
  # ggiraph gives each SVG a random id and names its CSS classes after it, and
  # htmlwidgets gives the <div> around it another one. Left alone, both change
  # every time the page runs - so re-rendering a page whose charts are
  # identical rewrites every one of them. On the Vital Statistics page that is
  # a 7 MB `_freeze/` file replaced in full for no change at all, and a diff
  # nobody can read. Seeding the random numbers from the chart's own id makes
  # ggiraph's ids repeat, and `elementId` sets the outer one outright, so an
  # unchanged chart comes out byte for byte the same and a real change shows up
  # as itself. The two versions seed differently so their ids never collide.
  #
  # The seed is restored afterwards: a post may use random numbers of its own,
  # and a chart must not quietly move its sequence along.
  seed_from_id <- function(text) {
    codes <- utf8ToInt(text)
    as.integer(sum(codes * seq_along(codes)) %% 100000L)
  }

  make_widget <- function(p, w, h, version) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else {
        rm(".Random.seed", envir = globalenv())
      }
    }, add = TRUE)

    set.seed(seed_from_id(str_c(id, "-", version)))
    widget <- ggiraph::girafe(
      ggobj = p, width_svg = w, height_svg = h,
      bg = "white", options = widget_options,
      font_set = font_set
    )
    # girafe() passes its ... to the SVG device, so the element id is set on
    # the widget afterwards rather than through the call above.
    widget$elementId <- str_c("cwr-", id, "-", version)
    widget
  }

  desktop_widget <- make_widget(plot, width, height, "desktop")

  # geom_text/geom_label/cwr_label sizes are shrunk for the phone exactly as in
  # cwr_figure(): changed in place, drawn, then put back.
  text_layers <- keep(phone_plot$layers, function(layer) {
    # GeomShadowText (cwr_label()) is its own geom, not a kind of GeomText, so
    # it has to be named here or a haloed label would keep its desktop size
    inherits(layer$geom, c("GeomText", "GeomLabel", "GeomShadowText")) &&
      !is.null(layer$aes_params$size)
  })
  original_sizes <- map(text_layers, \(layer) layer$aes_params$size)
  walk(text_layers, \(layer) layer$aes_params$size <- layer$aes_params$size * phone_text_scale)
  phone_widget <- make_widget(phone_plot, phone_width, phone_height, "phone")
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
