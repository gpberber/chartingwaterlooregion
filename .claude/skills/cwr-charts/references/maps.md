# Maps of the Region

Use when the story is about where, not how much - and only then, because a map spends a lot of space on shapes a reader already knows. R/maps.R holds the house map style: cwr_map_crs (the projection), cwr_label_point() (the roomiest point inside a shape, worked out in the cleaning script and stored as lon/lat), cwr_label_spot() (the roomiest place for a label of a given size, which is what a two-line label needs), cwr_map_nudges (the three district labels the welcome map moves by hand), cwr_map_theme() (no axes, no gridlines) and cwr_text_on_fill(). Labels go inside their shapes, level - never tilted, never shrunk to fit; a label that overhangs its border a little is normal. A map passes its own height, since it has no category axis.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggmap

Outline map of the Region, one shape per district, labelled inside the shapes.

```r
# Outline map of the Region, one shape per district, labelled inside.
# Shapes and label points come from a post's data/ (02_clean_data.R writes
# them with cwr_label_point()); R/maps.R supplies the projection, the nudges
# and the theme.
library(sf)

shapes <- st_read(here("posts", "<slug>", "data", "region_shapes.geojson"), quiet = TRUE) |>
	clean_names() |>
	st_transform(cwr_map_crs)

# One row per shape: name, the roomiest point inside it, and whatever the
# label says. The three nudges in cwr_map_nudges move the labels that do not
# look centred once the words are drawn. Labels are never tilted.
labels <- read_csv(here("posts", "<slug>", "data", "<places>.csv"), show_col_types = FALSE) |>
	clean_names() |>
	st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
	st_transform(cwr_map_crs) |>
	(\(points) bind_cols(
		st_drop_geometry(points),
		st_coordinates(points) |> as_tibble() |> set_names(c("x", "y"))
	))() |>
	left_join(cwr_map_nudges, join_by(name)) |>
	mutate(x = x + replace_na(dx, 0), y = y + replace_na(dy, 0))

p <- ggplot() +
	# Borders only: no fill, thin grey lines. Filled shapes take white borders
	# instead (colour = "white", linewidth = 0.6), which separate neighbours
	# without drawing a second thing to look at.
	geom_sf(data = shapes, fill = NA, colour = cowboysilver, linewidth = 0.4) +
	geom_text(
		data = labels,
		# cwr_short_name(): the house short form of a long municipal name
		aes(x, y, label = paste0(cwr_short_name(name), "\n", <value_label>)),
		size = label_size * 0.9, lineheight = 0.95
	) +
	# datum = NA drops the graticule, the faint grid of latitude and longitude
	# lines coord_sf() draws by default
	coord_sf(datum = NA) +
	labs(
		title = "Title: the finding, in one line",
		subtitle = "Subtitle: what is measured, for whom, and when",
		caption = cwr_caption("<source>")
	) +
	cwr_map_theme()

# A map has no category axis, so it passes its own heights
cwr_figure(p, "fig-<slug>", alt = "<what the map shows>", height = 7.5, phone_height = 6)
```

## ggmapshaded

Choropleth: shapes shaded by a value, each labelled with its name and that value.

```r
# Choropleth: each shape shaded by a value, labelled with its name and that
# value inside it. Same ingredients as ggmap, plus a fill scale.
library(sf)

# One scale for every version of the map (every tab, every year), so the same
# blue always means the same value
fill_low <- "#EEF3F8"
fill_limits <- c(0, <60>)

shaded <- shapes |> left_join(<values>, join_by(csd))

# Each label goes in the widest gap its shape has that holds the whole
# label, which needs the label's size in metres: measure the text as drawn,
# short names and all (see the commuting post for the measurement), rather
# than guessing it.
labels <- shapes |>
	st_drop_geometry() |>
	mutate(
		spot = pmap(
			list(st_geometry(shapes), <label_width_m>, <label_height_m>),
			\(shape, w, h) cwr_label_spot(st_sfc(shape, crs = cwr_map_crs), w, h)
		)
	) |>
	unnest(spot) |>
	left_join(st_drop_geometry(shaded) |> select(csd, value), join_by(csd)) |>
	# White text on the darker fills, black on the lighter ones, decided by
	# the fill's own lightness
	mutate(text_colour = cwr_text_on_fill(value, fill_low, dodgerblue, fill_limits))

p <- ggplot() +
	geom_sf(data = shaded, aes(fill = value), colour = "white", linewidth = 0.6) +
	scale_fill_gradient(low = fill_low, high = dodgerblue, limits = fill_limits, guide = "none") +
	geom_text(
		data = labels,
		# cwr_short_name(): the house short form of a long municipal name
		aes(x, y, label = paste0(cwr_short_name(name), "\n", <value_label>), colour = text_colour),
		size = label_size * 0.9, lineheight = 0.95
	) +
	scale_colour_identity() +
	# xlim/ylim keep every version of the map in the same frame, so switching
	# tabs or years changes the shading and never moves the map
	coord_sf(xlim = frame_x, ylim = frame_y, expand = FALSE, datum = NA) +
	labs(
		title = "Title: the finding, in one line",
		subtitle = "Subtitle: what is measured, for whom, and when",
		caption = cwr_caption("<source>")
	) +
	cwr_map_theme()

cwr_figure(p, "fig-<slug>", alt = "<what the map shows>", height = 7.5, phone_height = 6)
```

