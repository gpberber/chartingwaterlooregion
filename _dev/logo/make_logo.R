# make_logo.R
# ---------------------------------------------------------------------------
# Builds the site's logo, favicon and full-colour mark from the Waterloo Region
# municipal boundaries. Run it when the mark needs rebuilding, which should be
# almost never:
#
#   Rscript _dev/logo/make_logo.R
#
# Then open _dev/logo/preview.html to check the result at the sizes it is
# actually used at. Quarto ignores _dev/, so nothing in this folder is published.
#
# It writes three files into images/:
#
#   logo.svg        the navbar wordmark: the region glyph plus the site name, for
#                   the blue navbar. _quarto.yml sets `title: false`, so this file
#                   is the only place the site's name appears up there - if you
#                   ever replace it with a picture alone, turn the title back on.
#   favicon.svg     the browser-tab icon: a rounded blue tile with the glyph on it
#   logo-mark.svg   the seven municipalities shaded by population, on a transparent
#                   background, for anywhere the drawing gets room: the About page,
#                   a social card, print.
#
# ---------------------------------------------------------------------------
# Why the three are not the same drawing
#
# Shading the municipalities by population is the idea behind the mark, and it
# works - at size. It does not survive being shrunk. The navbar glyph is about
# 36 px tall and a favicon is 16 px, and at 16 px seven shapes separated by
# hairline gaps turn into a smudge: the eye gets no shape and no shading, just
# texture. Six treatments were drawn and compared side by side at 16, 24, 32, 48
# and 96 px before settling this one; the losers were scratch work and are gone.
#
# So the small versions keep the region's outline, which is the part that still
# reads, and reduce the shading to one deliberate division: the three cities
# (Kitchener, Waterloo, Cambridge) in the house red, the four rural townships
# left white. That is a real fact about the region - most people live in that red
# patch - rather than decoration, and two big shapes survive 16 px where seven
# small ones do not. logo-mark.svg keeps the full seven-way population ramp.
#
# The SVG is written by hand rather than through ggsave(). A ggplot SVG carries
# clip paths, a background rectangle and the device's own margins, and the
# viewBox lands wherever the device put it; a logo wants a tight, predictable box
# and a handful of paths. Everything below is plain geometry arithmetic.
# ---------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)
library(rmapshaper)
library(prismatic)
library(grid)         # grid.path() and grid.text() draw the social card

here::i_am("_quarto.yml")

# ---- 1. House colours ------------------------------------------------------
# The same hex values as R/theme_cwr.R and custom.scss, written out literally so
# this script does not pull in the whole charting theme just to draw a logo. If
# the palette ever changes, change it here too.
cwr_blue <- "#104E8B"   # dodgerblue4  - navbar background and favicon tile
cwr_red  <- "#AF1E2D"   # habsred      - the accent: the urban core, and the
                        #                dark end of the population ramp

# The pale end of the population ramp: the accent mixed most of the way to white.
pale_red <- as.character(clr_mix(cwr_red, "white", ratio = 0.7))

# ---- 2. Data ---------------------------------------------------------------
# Municipal boundaries, kept beside this script so the logo can be rebuilt from
# the repository alone. Source: Region of Waterloo Open Data, municipal
# boundaries; a copy of the file used across the other Waterloo Region projects.
wr_boundaries <- read_rds(here("_dev", "logo", "waterloo_region_cities.rds"))

# Population is deliberately frozen rather than fetched live.
#
# A logo that re-shaded itself whenever Statistics Canada published would be a
# slightly different logo every year, which is not what a logo is for. These are
# the 2025 estimates from CANSIM table 17-10-0155-01, read once on 8 September
# 2026 with:
#
#   cansim::get_cansim("17-10-0155-01") |>
#     janitor::clean_names() |>
#     filter(str_detect(geo, "Ontario"), ref_date == max(ref_date)) |>
#     select(municipality = geo, population = val_norm) |>
#     mutate(municipality = str_remove(municipality, " \\(.*"))
#
# Re-run that and paste the numbers in if the mark is ever redrawn.
population <- tribble(
  ~municipality,     ~population,
  "Kitchener",            323917,
  "Cambridge",            165354,
  "Waterloo",             142450,
  "Woolwich",              31382,
  "Wilmot",                22890,
  "North Dumfries",        13051,
  "Wellesley",             12413
)

# The three cities, as opposed to the four rural townships.
urban_core <- c("Kitchener", "Waterloo", "Cambridge")

# ---- 3. Shapes -------------------------------------------------------------
# One polygon per municipality, projected, thinned to a logo's worth of detail.
municipalities <- wr_boundaries |>
  st_make_valid() |>
  group_by(municipality) |>
  summarise(.groups = "drop") |>        # unions the pieces of each municipality
  # UTM zone 17N: a projected CRS, so the region is drawn in its true proportions
  # instead of the sideways stretch you get plotting raw lat/long at 43 N.
  st_transform(32617) |>
  # Drop detached fragments under 1 km2 - river islands and boundary slivers that
  # would otherwise show up as specks of noise around the mark.
  ms_filter_islands(min_area = 1e6) |>
  # Thin the outlines. The full file is about 7,400 vertices, which is detail no
  # one can see at 36 px and a needlessly large SVG; keep = 0.02 leaves about 185
  # and still reads as Waterloo Region. ms_simplify works on the shared borders
  # rather than each polygon separately, so simplified neighbours still tile with
  # no gaps or overlaps - simplifying them one at a time tears the seams open.
  ms_simplify(keep = 0.02, keep_shapes = TRUE) |>
  left_join(population, by = join_by(municipality))

# The cities dissolved into one shape and the townships into another. Dissolving
# *after* simplifying matters: the borders are already identical by then, so the
# core sits exactly inside the region outline with no sliver showing between.
two_tone <- municipalities |>
  mutate(part = if_else(municipality %in% urban_core, "core", "township")) |>
  group_by(part) |>
  summarise(.groups = "drop")

# The whole region as a single silhouette.
region <- two_tone |>
  summarise(.groups = "drop") |>
  mutate(part = "region")

core <- two_tone |> filter(part == "core")

# ---- 4. Geometry to SVG paths ----------------------------------------------

# One ring - an outer boundary or a hole - as an SVG path fragment: move to the
# first point, line to the rest, close.
ring_to_path <- function(xy) {
  paste0(
    "M", paste(sprintf("%.1f %.1f", xy[, 1], xy[, 2]), collapse = "L"), "Z"
  )
}

# All the rings of one feature, whether it is a POLYGON (a list of rings) or a
# MULTIPOLYGON (a list of polygons, each a list of rings). Holes are dealt with
# by fill-rule="evenodd" on the path element rather than by winding direction.
feature_rings <- function(geom) {
  if (inherits(geom, "MULTIPOLYGON")) {
    list_flatten(unclass(geom))
  } else {
    unclass(geom)
  }
}

# Turn an sf object into one <path> per row, scaled into a `size`-unit square with
# `pad` units of clear space around the drawing.
#
# `fills` is a named vector mapping the value in column `key` to a colour.
# `frame` is the shape whose bounding box sets the scaling: pass the same frame to
# two layers and they line up exactly, which is how the red core is drawn on top
# of the white region without drifting.
#
# SVG's y axis points down and a map's points up, so y is flipped on the way in.
fit_box <- function(frame, width, height, pad, origin = c(0, 0)) {
  bb <- st_bbox(frame)
  # The smaller of the two ratios: whichever dimension runs out of room first
  # sets the scale, so the shape fits without being squashed.
  scale <- min(
    (width  - 2 * pad) / (bb$xmax - bb$xmin),
    (height - 2 * pad) / (bb$ymax - bb$ymin)
  )
  list(
    bb = bb, scale = scale,
    # Centre the drawing: whichever dimension is shorter gets half the slack each side.
    dx = origin[1] + pad + ((width  - 2 * pad) - (bb$xmax - bb$xmin) * scale) / 2,
    dy = origin[2] + pad + ((height - 2 * pad) - (bb$ymax - bb$ymin) * scale) / 2
  )
}

# One feature's rings as matrices in box coordinates.
# SVG's y axis points down and a map's points up, so y is flipped on the way in.
place_rings <- function(geom, tr) {
  map(feature_rings(geom), \(r) {
    cbind(
      (r[, 1] - tr$bb$xmin) * tr$scale + tr$dx,
      (tr$bb$ymax - r[, 2]) * tr$scale + tr$dy
    )
  })
}

build_paths <- function(shape, fills, key, size, pad,
                        stroke = "none", stroke_width = "0", frame = shape) {
  tr <- fit_box(frame, size, size, pad)

  map_chr(seq_len(nrow(shape)), \(i) {
    d <- map_chr(place_rings(st_geometry(shape)[[i]], tr), ring_to_path) |>
      paste(collapse = "")

    sprintf(
      '  <path d="%s" fill="%s" fill-rule="evenodd" stroke="%s" stroke-width="%s" stroke-linejoin="round"/>',
      d, fills[[shape[[key]][i]]], stroke, stroke_width
    )
  }) |>
    paste(collapse = "\n")
}

# Population to colour along a two-colour ramp.
#
# On a square-root scale, not a linear one: Kitchener has 26 times the population
# of Wellesley, so on a linear ramp the six smaller municipalities would all sit
# at the pale end and the mark would read as one dark shape and six blank ones.
# The square root pulls the small ones far enough apart to tell apart.
population_fills <- function(shape, low, high) {
  ramp <- colorRamp(c(low, high))
  scaled <- sqrt(shape$population)
  scaled <- (scaled - min(scaled)) / (max(scaled) - min(scaled))
  set_names(rgb(ramp(scaled), maxColorValue = 255), shape$municipality)
}

# The small-size glyph: white region, red urban core, no tile of its own, so it
# sits on whatever is behind it. `size` and `pad` set how much of its box it fills.
glyph <- function(size, pad) {
  paste(
    build_paths(region, c(region = "#FFFFFF"), "part", size, pad, frame = region),
    build_paths(core, c(core = cwr_red), "part", size, pad, frame = region),
    sep = "\n"
  )
}

# ---- 5. The three files ----------------------------------------------------

# The navbar wordmark. The glyph sits in a square at the left, the site name to
# its right, laid out in a 436 x 64 box and then cropped to the ink (see below)
# before Quarto scales it down to the navbar height.
#
# The text is left as a <text> element rather than converted to outlines, which
# is how the previous wordmark did it too. Worth knowing: an SVG loaded through
# <img> cannot reach the page's webfonts, so this renders in the reader's Segoe
# UI or Helvetica rather than in Inter. It is close enough that it has never
# looked wrong, but that is why the two are not identical.
glyph_box <- 56
glyph_pad <- 3
text_x <- glyph_box + 14

# The wordmark's own settings, named because the box below is derived from them.
wordmark_size     <- 27
wordmark_baseline <- 42
# What the eye reads as the centre of a line of type is the middle of the capital
# letters, not the middle of the em box: descenders hang below the baseline with
# nothing to balance them above. Measured in the browser, the capitals of this
# font stand 19 units tall at 27, so the cap band runs 23 to 42 and its middle
# line - the one the glyph and the navbar links should both sit on - is 32.5.
wordmark_cap <- 19
text_centre  <- wordmark_baseline - wordmark_cap / 2

# Drop the glyph onto that line. fit_box centres a drawing inside the box it is
# given, so the glyph's ink centre is that box's middle: 28 for a 56-unit box,
# which is 4.5 units above the wordmark's centre line. Left uncorrected the glyph
# rides high and the name sits low, and because Quarto centres the whole image in
# the navbar, "Charting Waterloo Region" ended up with its baseline about 2.7 px
# below the baseline of "Posts" beside it.
glyph_dy <- text_centre - glyph_box / 2

# The viewBox is cropped tight to the ink rather than left at the 0 0 436 64 the
# drawing is laid out in. Quarto scales the logo to the navbar's height, so any
# empty band inside the box is height the mark does not get to use: leaving the
# box 64 tall made the visible wordmark a quarter smaller than the space allowed.
# The same goes sideways - the text ends at about x 394, and the slack after it
# was showing up as a gap before "Posts".
#
# The glyph is the tallest thing in the drawing, so its shifted ink sets the top
# and bottom edges. Recomputed from the same fit_box call that places it, so the
# box cannot drift out of step with the drawing if the boundary file is redrawn:
# fit_box leaves the shape starting at dy and running its scaled height, and the
# region is wider than it is tall, so width is the binding dimension and the
# drawing floats in the middle of its box vertically.
glyph_tr     <- fit_box(region, glyph_box, glyph_box, glyph_pad)
glyph_height <- (st_bbox(region)$ymax - st_bbox(region)$ymin) * glyph_tr$scale
glyph_top    <- glyph_tr$dy + glyph_dy

# Because the glyph is symmetric about the wordmark's centre line once shifted,
# so is this box - which is what makes the whole image centre on that line, and
# the baseline land where the navbar links' baseline already is.
#
# The right edge keeps a little more slack than the left because the text width
# depends on the reader's fonts: an SVG loaded through <img> cannot use the page's
# webfonts, so this falls back to whatever the reader has (Segoe UI on Windows,
# Helvetica on a Mac) and only matches Inter on a machine with Inter installed.
# Measured at 394 in the fallback font; 408 leaves room for a wider face rather
# than risking the last letter being clipped.
logo_box    <- sprintf("0 %.1f 408 %.1f", glyph_top, glyph_height)
logo_height <- sprintf("%.1f", glyph_height)

logo_svg <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="%s" width="408" height="%s">
  <!-- Charting Waterloo Region wordmark: the region, with the three cities in red.
       Built by _dev/logo/make_logo.R - edit that script, not this file. -->
  <g transform="translate(0 %.1f)">
%s
  </g>
  <text x="%d" y="%d" font-family="Inter, \'Segoe UI\', Helvetica, Arial, sans-serif" font-size="%d" font-weight="700" fill="#FFFFFF" letter-spacing="-0.3">Charting Waterloo Region</text>
</svg>
',
  logo_box,
  logo_height,
  glyph_dy,
  glyph(size = glyph_box, pad = glyph_pad),
  text_x,
  wordmark_baseline,
  wordmark_size
)

# The favicon: the same glyph on a rounded tile, so it has an edge of its own in a
# tab strip. The tile is the navbar blue, which is what makes it read as this site
# rather than as a generic map pin.
favicon_svg <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64" width="64" height="64">
  <!-- Charting Waterloo Region favicon. Built by _dev/logo/make_logo.R. -->
  <rect width="64" height="64" rx="12" fill="%s"/>
%s
</svg>
',
  cwr_blue,
  glyph(size = 64, pad = 5)
)

# The full-colour mark: the population ramp as designed, on a transparent ground,
# at a size where the shading can actually be read. White borders separate the
# municipalities, so it needs a light background behind it.
mark_svg <- sprintf(
  '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 256" width="256" height="256">
  <!-- Charting Waterloo Region mark: the seven municipalities shaded by population
       (2025 estimates). Built by _dev/logo/make_logo.R. -->
%s
</svg>
',
  build_paths(
    municipalities,
    population_fills(municipalities, low = pale_red, high = cwr_red),
    key = "municipality", size = 256, pad = 6,
    stroke = "#FFFFFF", stroke_width = "2.4"
  )
)

write_lines(logo_svg, here("images", "logo.svg"))
write_lines(favicon_svg, here("images", "favicon.svg"))
write_lines(mark_svg, here("images", "logo-mark.svg"))

# ---- 6. The social card ----------------------------------------------------
# images/social-card.png is what Facebook, LinkedIn, Slack and the rest show when
# somebody pastes a link to the site (_quarto.yml sets it as the default `image`).
# It was a placeholder: the site name and strapline in the top-left corner and
# then two-thirds of an empty white rectangle. The mark fills it.
#
# Drawn with grid rather than ggplot2 because this is layout, not a chart: text
# and polygons at chosen pixel positions, with no scales, panel or theme to fight.
# The device is opened at 72 dpi so one point of font size is exactly one pixel
# and the sizes below can be read as pixels.
card_w <- 1200
card_h <- 630

card_mark_box <- list(x = 700, y = 95, size = 440)  # where the mark sits

png_font <- if ("Inter" %in% systemfonts::system_fonts()$family) "Inter" else ""

ragg::agg_png(
  here("images", "social-card.png"),
  width = card_w / 72, height = card_h / 72, units = "in", res = 72,
  background = "white"
)

grid.newpage()
# yscale runs 630 -> 0 so y counts down from the top, matching the SVG above.
pushViewport(viewport(xscale = c(0, card_w), yscale = c(card_h, 0)))

# The mark, in the same population ramp as logo-mark.svg.
card_fills <- population_fills(municipalities, low = pale_red, high = cwr_red)
card_tr <- fit_box(
  municipalities, card_mark_box$size, card_mark_box$size, pad = 0,
  origin = c(card_mark_box$x, card_mark_box$y)
)

walk(seq_len(nrow(municipalities)), \(i) {
  rings <- place_rings(st_geometry(municipalities)[[i]], card_tr)
  xy <- do.call(rbind, rings)
  grid.path(
    x = unit(xy[, 1], "native"), y = unit(xy[, 2], "native"),
    id = rep(seq_along(rings), map_int(rings, nrow)),
    rule = "evenodd",
    gp = gpar(
      fill = card_fills[[municipalities$municipality[i]]],
      col = "white", lwd = 3, linejoin = "round"
    )
  )
})

# The name, set in two lines so it clears the mark, and the strapline under it.
card_text <- function(label, x, y, size, colour, face = "plain") {
  grid.text(
    label, x = unit(x, "native"), y = unit(y, "native"),
    just = c("left", "centre"),
    gp = gpar(fontsize = size, col = colour, fontfamily = png_font, fontface = face)
  )
}

card_text("Charting", 80, 250, 62, cwr_blue, "bold")
card_text("Waterloo Region", 80, 318, 62, cwr_blue, "bold")
card_text("Charts and plain-language analysis about life in", 80, 383, 24, "#4a5457")
card_text("Waterloo Region, built from open data.", 80, 416, 24, "#4a5457")

invisible(dev.off())

walk(c("logo.svg", "favicon.svg", "logo-mark.svg", "social-card.png"), \(f) {
  message(f, ": ", file.size(here("images", f)), " bytes")
})
