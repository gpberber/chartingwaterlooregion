# theme_cwr.R
# ---------------------------------------------------------------------------
# House chart style for Charting Waterloo Region.
#
# Every post sources this file once in its setup chunk:
#
#   source(here::here("R", "theme_cwr.R"))
#
# It holds the foundations - the packages every post needs, the house colours
# (the same hex values as custom.scss), the font, and theme_cwr() itself, the
# Tufte-inspired ggplot2 theme - and then sources the rest of the house style
# from the files beside it, so one line in a post still brings in everything:
#
#   maps.R           the map projection, label points and map theme
#   chart_helpers.R  cwr_caption(), cwr_label(), cwr_stack_keys(), cwr_line_labels()
#   figures.R        cwr_figure(): the desktop and phone PNG of every chart
#   right_axis.R     cwr_right_axis(): the value-axis numbers and the baseline
#   interactive.R    cwr_interactive(): hover charts drawn with ggiraph
#   post_sections.R  key terms, sources and reliability tables, session info
#
# A new chart helper goes in the file it belongs to, not back in here. Colours,
# the font and the theme stay here, which is what keeps this file the one place
# the palette has to agree with custom.scss.
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
library(shadowtext)  # geom_shadowtext(): the halo behind every in-plot label, cwr_label()
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
source(here::here("R", "maps.R"), local = TRUE)

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
#
# `width` is the width of the image the chart will be saved at, in inches, and
# is only used to size the title, subtitle and caption. They are drawn as text
# boxes (element_textbox_simple(), from ggtext) rather than plain markdown, so
# a line longer than the image wraps instead of running off the right edge; a
# box needs to be told how wide it is, and "the image less the plot margin
# either side" is that width. A chart saved at another width, or one that
# widens plot.margin, passes its own `width` so the boxes still match what is
# drawn - the phone version does exactly this in cwr_phone_theme(), and the
# note there says what goes wrong when the two drift apart.
theme_cwr <- function(base_size = 15, base_family = cwr_font(), width = 8.3) {
  side_margin <- 6   # points, left and right; used again in plot.margin below
  box_width <- unit(width, "in") - unit(2 * side_margin, "pt")

  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements. element_textbox_simple() (from ggtext) means titles can
      # contain **bold** or <span style='color:...'> markup, and wrap.
      plot.title = element_textbox_simple(
        size = base_size * 1.0,
        width = box_width,
        lineheight = 1.1,
        margin = margin(0, 0, 5, 0),
        face = "bold"
      ),
      plot.subtitle = element_textbox_simple(
        size = base_size * 0.7,
        width = box_width,
        lineheight = 1.1,
        margin = margin(0, 0, 10, 0),
        color = "black",
        face = "bold"
      ),
      plot.caption = element_textbox_simple(
        size = base_size * 0.6,
        width = box_width,
        margin = margin(t = 10),
        color = cowboysilver,
        # A caption is more than one line whenever cwr_caption() is given
        # notes, or whenever a long source line wraps. gridtext sets those
        # lines solid, so they need opening up here.
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

      # A ranked horizontal chart puts its x axis on top (style rule 6), where
      # the margin above spaces the numbers away from the gridline tops they
      # label rather than towards them. The top axis gets its own small gap
      # below instead, so each number sits just above its gridline (Greg,
      # 2026-09-22, on the commuting post's work-at-home chart).
      axis.text.x.top = element_text(margin = margin(b = 2)),

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
      # The side margins are the ones the title boxes were sized against above.
      plot.margin = margin(t = 6, r = side_margin, b = 6, l = side_margin),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Make it the default for every ggplot in the session
theme_set(theme_cwr(base_size = 15))

# Text drawn inside the plot is Inter too (Greg, 2026-09-26). geom_text(),
# geom_label() and annotate("text") already get it from the theme set above:
# ggplot2 hands the theme's font to those geoms. geom_shadowtext() - behind
# cwr_label() and annotate("shadowtext") - and ggtext's geom_richtext() carry
# their own fixed default instead, the graphics device's plain sans font
# (Arial on Windows), so their default is changed here, once, for every chart.
update_geom_defaults(GeomShadowText, aes(family = cwr_font()))
update_geom_defaults(GeomRichText, aes(family = cwr_font()))

# The templates refer to base_size for text sizes inside geoms and annotations
base_size <- 15

# ---- 7. The rest of the house style ---------------------------------------
# Split into files of their own on 2026-09-26, when this one reached 2,000
# lines and every change to a chart meant scrolling through the theme to find
# it. Nothing moved between them, and nothing changed for a post: sourcing this
# file still brings in everything below. `local = TRUE` passes on whatever
# environment this file is being read into, so sourcing it inside a function
# or a test environment keeps all of it there rather than leaking to the
# global one.
source(here::here("R", "chart_helpers.R"), local = TRUE)  # captions, in-plot labels, stack keys, line labels
source(here::here("R", "figures.R"), local = TRUE)        # cwr_figure(): desktop and phone PNGs
source(here::here("R", "right_axis.R"), local = TRUE)     # cwr_right_axis(): numbers at the gridline ends
source(here::here("R", "interactive.R"), local = TRUE)    # cwr_interactive(): hover charts through ggiraph
source(here::here("R", "post_sections.R"), local = TRUE)  # key terms, sources, reliability, session info
