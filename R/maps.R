# maps.R
# ---------------------------------------------------------------------------
# House map style for Charting Waterloo Region: the few pieces every map of
# the Region needs, so no post works them out again.
#
# Posts get this file for free - theme_cwr.R sources it - and a cleaning
# script that needs cwr_label_point() sources it on its own:
#
#   source(here::here("R", "maps.R"))
#
# It holds:
#   cwr_map_crs     the projection every map is drawn in
#   cwr_label_point()   the roomiest point inside each shape, for its label
#   cwr_map_nudges  the three district labels that need moving off that point
#   cwr_map_theme() the theme every map adds (no axes, no gridlines)
#   cwr_text_on_fill()  white or black label text, whichever the fill needs
#
# sf is not loaded here: a post or script that draws a map loads it itself
# (library(sf)), and these functions call sf:: explicitly so they work either
# way.
# ---------------------------------------------------------------------------

# EPSG:3161 is "NAD83 / Ontario MNR Lambert", a projection made for Ontario:
# shapes this size keep their true proportions on it, and its coordinates are
# metres, so distances on a map can be given in metres. Boundary data is
# stored in plain latitude and longitude (EPSG:4326) and projected at drawing
# time, never the other way round.
cwr_map_crs <- 3161

# Where a label sits inside a shape.
#
# A centroid is the obvious choice and the wrong one: for a crescent or an
# L-shaped municipality it can fall outside the shape altogether.
# st_point_on_surface() is guaranteed to land inside but only promises
# "inside", not "roomy", and it happily returns a point in a narrow arm where
# no label would fit. st_inscribed_circle() answers the question actually
# being asked - what is the largest circle that fits inside this shape? - and
# its centre is the point furthest from any edge.
#
# Returns one row per shape: the point as longitude and latitude (so it can be
# written to a plain csv), and the circle's radius in kilometres, which says
# how much room the label has there.
#
# Two details. The work is done in metres, because "largest circle" is a
# question about a flat plane. And GEOS is asked one shape at a time, because
# given several at once it returns all their circles in one unlabelled bag
# with no way to tell which belongs to which.
cwr_label_point <- function(shapes, dTolerance = 10) {
  shapes |>
    sf::st_transform(cwr_map_crs) |>
    sf::st_geometry() |>
    purrr::map(\(shape) {
      circle <- sf::st_inscribed_circle(
        sf::st_sfc(shape, crs = cwr_map_crs), dTolerance = dTolerance
      )
      circle <- circle[!sf::st_is_empty(circle)]
      # The circle comes back as a polygon, so its centre is the middle of its
      # bounding box and its radius half that box's width
      box <- sf::st_bbox(circle)
      tibble::tibble(
        x = unname((box[["xmin"]] + box[["xmax"]]) / 2),
        y = unname((box[["ymin"]] + box[["ymax"]]) / 2),
        label_room_km = unname((box[["xmax"]] - box[["xmin"]]) / 2 / 1000)
      )
    }) |>
    purrr::list_rbind() |>
    sf::st_as_sf(coords = c("x", "y"), crs = cwr_map_crs) |>
    sf::st_transform(4326) |>
    (\(points) dplyr::bind_cols(
      sf::st_drop_geometry(points),
      sf::st_coordinates(points) |>
        tibble::as_tibble() |>
        rlang::set_names(c("lon", "lat"))
    ))()
}

# Where a label of a given size sits inside a shape.
#
# cwr_label_point() above answers "where is there most room for a point"; this
# answers "where is there most room for these words", which is what a map label
# actually needs. The two differ whenever the label is wide or deep next to its
# shape - on a map of the Region with a few lines of caption, "Waterloo" over
# its figure is about 8 km wide, which is most of the width of Waterloo.
#
# `width` is the label's width in metres and `height` its total height (measure
# the text, do not guess it). **Give `width` one entry per line, top line
# first**, and each line is treated at its own width: a name over a share is a
# wide line above a narrow one, not a rectangle as wide as the name, and the
# corners under the short line are room the label does not need. Modelling it
# as one rectangle keeps a label higher in a shape that narrows downwards than
# it has to be.
#
# The shape is covered in a grid of candidate positions; at each, the label's
# lines are sampled and the distance from every sample to the nearest border
# measured, counting as negative where a sample falls outside. The position
# whose worst sample is best wins - the widest gap between borders that holds
# the whole label. A label may still overhang a little where the shape is too
# narrow to hold it, which reads fine; see rule 12a in the cwr-charts skill.
cwr_label_spot <- function(shape, width, height, cellsize = 250) {
  shape <- sf::st_geometry(shape)
  border <- sf::st_cast(shape, "MULTILINESTRING")

  candidates <- sf::st_make_grid(shape, cellsize = cellsize, what = "centers")
  candidates <- candidates[sf::st_within(candidates, shape, sparse = FALSE)[, 1]]
  xy <- sf::st_coordinates(candidates)

  # Each line as a row of sample points at its own width, stacked from the top
  # down, relative to the middle of the whole label
  line_height <- height / length(width)
  offsets <- purrr::imap(width, \(line_width, line) {
    middle <- height / 2 - (line - 0.5) * line_height
    tidyr::expand_grid(
      dx = seq(-line_width / 2, line_width / 2, length.out = 7),
      dy = middle + c(-line_height / 2, 0, line_height / 2)
    )
  }) |>
    purrr::list_rbind()

  # Every candidate's samples in one table, so sf is asked once rather than
  # once per candidate
  samples <- tidyr::expand_grid(
    candidate = seq_len(nrow(xy)),
    offsets
  ) |>
    dplyr::mutate(x = xy[candidate, 1] + dx, y = xy[candidate, 2] + dy)

  sample_points <- sf::st_as_sf(samples, coords = c("x", "y"), crs = cwr_map_crs)
  distance <- as.numeric(sf::st_distance(sample_points, border))
  inside <- sf::st_within(sample_points, shape, sparse = FALSE)[, 1]

  best <- samples |>
    dplyr::mutate(clearance = dplyr::if_else(inside, distance, -distance)) |>
    dplyr::summarise(clearance = min(clearance), .by = candidate) |>
    dplyr::slice_max(clearance, n = 1, with_ties = FALSE)

  tibble::tibble(
    x = xy[best$candidate, 1],
    y = xy[best$candidate, 2],
    clearance = best$clearance
  )
}

# The roomiest point is not always where a label looks centred once the words
# are drawn, so three of the seven are moved, in metres east and north. These
# came from rendering the welcome post's map and looking at it, the phone
# version especially, where the words are largest against the shapes.
# Waterloo's was measured rather than judged: its name is shifted until the
# nearest border point on its left is as far away as the nearest on its right,
# which on its slanting north-west edge is further east than it looks.
#
# **Map labels are never tilted and are not shrunk to fit.** A label that
# overhangs its border slightly is normal and reads fine; the fixes, in order,
# are this table, then shorter wording, then fewer lines.
cwr_map_nudges <- tibble::tribble(
  ~name,             ~dx,    ~dy,
  "Waterloo",        2000,   1200,
  "Cambridge",        350,  -2200,
  "North Dumfries", -1000,   -300
)

# theme_cwr() is built for charts with axes; a map wants none of them. Each
# element is named individually because the theme sets them individually, and
# a blanket axis.text would not override the more specific setting.
cwr_map_theme <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )
}

# Label text on a shaded shape: white where the fill is dark, black where it is
# light. The fill colour is worked out the way scale_fill_gradient() works it
# out, and its lightness read off in the Lab colour space, where 0 is black and
# 100 white. Below `switch_at`, white text reads better.
cwr_text_on_fill <- function(values, low, high, limits, switch_at = 55) {
  fills <- scales::pal_seq_gradient(low, high)(scales::rescale(values, from = limits))
  lightness <- farver::decode_colour(fills, to = "lab")[, "l"]
  dplyr::if_else(lightness < switch_at, "white", "black")
}
