# theme_cwr.R
# ---------------------------------------------------------------------------
# House chart style for Charting Waterloo Region.
#
# Every post sources this file once in its setup chunk:
#
#   source(here::here("R", "theme_cwr.R"))
#
# It does four things:
#   1. loads the packages every post needs,
#   2. defines the house colours (the same hex values as custom.scss),
#   3. defines and sets theme_cwr(), the Tufte-inspired ggplot2 theme,
#   4. defines small helpers (caption text, font lookup).
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

# Print numbers without a thousands separator by default (scales >= 1.3).
# Individual scales override this with label_number(big.mark = ",").
number_options(big.mark = "")

# ---- 2. House colours ----------------------------------------------------
# Three base colours, each with a 50 % tint (mixed with white). Use them with
# consistent meaning across posts so readers learn the code:
#   dodgerblue   = the focus (Waterloo Region, WRPS, Kitchener)
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

# Named palettes for the comparisons that recur across posts.
comp_colours <- c(
  "Waterloo Region" = dodgerblue,
  "Canada"          = habsred,
  "Ontario"         = ontgreen,
  "Other cities"    = cowboysilver50
)

local_colours <- c(
  "Waterloo Region" = dodgerblue,
  "Guelph"          = dodgerblue50,
  "London"          = cowboysilver,
  "Hamilton"        = habsred
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
theme_cwr <- function(base_size = 13, base_family = cwr_font()) {
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
        color = cowboysilver
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
        hjust = 0
      ),

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
      panel.spacing.y = unit(1.5, "lines"),

      # Whole-plot settings: title and caption align to the plot edge, not the panel
      plot.margin = margin(t = base_size, r = base_size, b = base_size, l = base_size),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# Make it the default for every ggplot in the session
theme_set(theme_cwr(base_size = 13))

# The templates refer to base_size for text sizes inside geoms and annotations
base_size <- 13

# ---- 5. Helpers ----------------------------------------------------------
# Standard caption: "Source: Statistics Canada, Table 35-10-0177-01 | *Charting Waterloo Region*"
cwr_caption <- function(source) {
  paste0("Source: ", source, " | *Charting Waterloo Region*")
}

# ggplot2 text sizes for geom_text/geom_label are in mm, not points.
# 3.2 mm is roughly 9 pt, the standard label size used across the templates.
label_size <- 3.2
