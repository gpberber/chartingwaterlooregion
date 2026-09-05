# Bar charts

Use when each category has one value (or two to compare). Horizontal bars when category labels are long or there are more than ~6 categories. Bold the focus row with the y_label trick.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggvertbar

Vertical bars, one value per category, right-hand y axis; optional direct labels replace the axis.

```r
<plot_data> |>
  mutate(<x_variable> = fct_reorder(<x_variable>, <y_variable>)) |>
  ggplot(aes(x = <x_variable>, y = <y_variable>)) +

  geom_col(
    fill = dodgerblue,
    width = 0.7
  ) +

  # OPTION: direct bar labels - if using, see notes below
  # threshold controls inside/outside split - set to min(y_variable) for all inside,
  # Inf for all outside
  # geom_text(
  #   data = <plot_data> |> filter(<y_variable> >= <threshold>),
  #   aes(x = <x_variable>, y = <y_variable>,
  #       label = label_number(big.mark = ",", scale = <1>, suffix = "<>")(<y_variable>)),
  #   nudge_y = <-2>,      # negative = inside top of bar; in data units
  #   hjust = 0.5,
  #   size = 3.2,
  #   colour = "white"
  # ) +
  # geom_text(
  #   data = <plot_data> |> filter(<y_variable> < <threshold>),
  #   aes(x = <x_variable>, y = <y_variable>,
  #       label = label_number(big.mark = ",", scale = <1>, suffix = "<>")(<y_variable>)),
  #   nudge_y = <2>,       # positive = above bar; in data units
  #   hjust = 0.5,
  #   size = 3.2,
  #   colour = "grey30"
  # ) +
  # Notes if using direct labels:
  # - comment out scale_y_continuous below and replace with:
  #   scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
  # - comment out axis.text.y.right in theme() below
  # - add to theme(): panel.grid.major.y = element_blank()
  # - consider removing right margin from plot.margin in theme() below

  scale_x_discrete(
    #breaks = seq(min, max, by),
    #minor_breaks = seq(min, max, by),
    #labels = label_number(big.mark = "", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.15)),
    position = "bottom"
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    position = "right",
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0, 0.05))
  ) +

  guides(x = guide_axis(minor.ticks = FALSE)) +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -12)
    ),
    plot.margin = margin(t = 13, l = 13, b = 13, r = 200),
    axis.ticks.x = element_blank()
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## gghorbar

Horizontal bars (ranked categories), optional fill groups, gridlines run vertically, one label can be bolded.

```r
plot_data <- <plot_data> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <x_variable>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>, fill = <group_variable>)) +

  geom_col(
    #fill = dodgerblue,
    width = 0.7
  ) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  # OPTION: direct bar labels - if using, see notes below
  # threshold controls inside/outside split - set to min(x_variable) for all inside,
  # -Inf for all outside
  # geom_text(
  #   data = plot_data |> filter(<x_variable> >= <threshold>),
  #   aes(x = <x_variable>, y = <y_variable>,
  #       label = label_number(big.mark = ",", scale = <1>, suffix = "<>")(<x_variable>)),
  #   nudge_x = <-2>,     # negative = inside right of bar; in data units
  #   hjust = 1,
  #   vjust = 0.5,
  #   size = 3.2,
  #   colour = "white"
  # ) +
  # geom_text(
  #   data = plot_data |> filter(<x_variable> < <threshold>),
  #   aes(x = <x_variable>, y = <y_variable>,
  #       label = label_number(big.mark = ",", scale = <1>, suffix = "<>")(<x_variable>)),
  #   nudge_x = <2>,      # positive = right of bar; in data units
  #   hjust = 0,
  #   vjust = 0.5,
  #   size = 3.2,
  #   colour = "grey30"
  # ) +
  # Notes if using direct labels:
  # - comment out scale_x_continuous below and replace with:
  #   scale_x_continuous(expand = expansion(mult = c(0, 0.05)))
  # - comment out axis.ticks.x and panel.grid.major.x in theme() below
  # - add to theme(): axis.text.x = element_blank(), axis.line.x = element_blank()
  # - add coord_cartesian(clip = "off") if outside labels are clipped

  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    position = "top"
  ) +

  scale_y_discrete(
    position = "left",
    labels = y_labels
  ) +

  # adjust number of colors/names to suit data
  scale_fill_manual(
    values = set_names(
      manual_5_colours,
      c(
        "<group1>",
        "<group2>",
        "<group3>",
        "<group4>",
        "<group5>"
      )
    ),
    #guide = "none"    # uncomment if want no legend
  ) +

  # Horizontal chart: swap tufte's default grid orientation
  theme(
    axis.text.y.left = element_markdown(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank()
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## gghorbarrect

Horizontal bars plus a boxed value column on the right (e.g. bar = count, box = rate).

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <plot_data> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <x_variable>),
    <rect_variable> = round(<rect_variable>, <0>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>, fill = <group_variable>)) +

  geom_col(
    #fill = dodgerblue,
    width = 0.7
  ) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  # Boxed right-side data labels
  geom_label(
    aes(
      x = <rect_x>,
      y = <y_variable>,
      label = paste0(formatC(<rect_variable>, format = "f", digits = <1>, flag = " "), "<rect_suffix>")
    ),
    hjust = 1,
    size = 3.2,
    fill = "grey90",
    colour = "grey30",
    linewidth = 0,
    label.padding = unit(0.3, "lines")
  ) +

  # Shaded column header above rectangles
  annotate(
    "label",
    x = <rect_x>,
    y = Inf,
    label = "<rect_header>",
    hjust = 1,
    vjust = -1.0,
    size = 3.2,
    fontface = "plain",
    fill = "grey90",
    colour = "grey30",
    linewidth = 0,
    label.padding = unit(0.3, "lines")
  ) +

  scale_x_continuous(
    limits = c(<x_min>, <rect_x>),
    #breaks = seq(<x_min>, <x_max>, <by>),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.3)),
    position = "top"
  ) +

  scale_y_discrete(
    position = "left",
    labels = y_labels
  ) +

  # adjust number of colors/names to suit data
  scale_fill_manual(
    values = set_names(
      manual_5_colours,
      c(
        "<group1>",
        "<group2>",
        "<group3>",
        "<group4>",
        "<group5>"
      )
    ),
    #guide = "none"    # uncomment if want no legend
  ) +

  # Horizontal chart: swap tufte's default grid orientation
  # clip = "off" needed to show rectangles and header beyond panel
  theme(
    axis.text.y.left = element_markdown(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank()
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggcompbar_horiz

Two horizontal bars per category (wide focus bar + narrow comparison bar or tick).

```r
<data_source> |>
  pivot_wider(
    id_cols = <y_variable>,
    names_from = <group_var>,
    values_from = <value_var>
  ) |>
  mutate(<y_variable> = fct_reorder(<y_variable>, `<x1_name>`)) |>
  ggplot() +
  geom_col(aes(y = <y_variable>, x = `<x1_name>`, fill = "<x1_name>"), width = 0.5) +
  geom_col(aes(y = <y_variable>, x = `<x2_name>`, fill = "<x2_name>"), width = 0.3) +
  # OPTION: replace second geom_col above with this code if want line instead of bar
  # geom_segment(
  #   aes(
  #     y = as.numeric(<y_variable>) - 0.3,
  #     yend = as.numeric(<y_variable>) + 0.3,
  #     x = `<x2_name>`,
  #     xend = `<x2_name>`
  #   ),
  #   colour = cowboysilver,
  #   linewidth = <1.0>
  # ) +
  scale_fill_manual(
    values = c(
      "<x1_name>" = dodgerblue,
      "<x2_name>" = cowboysilver   # remove if using comparison line instead of bar
    )
  ) +
  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0, 0.05)),
    position = "top"
  ) +
  scale_y_discrete(
    position = "left"
  ) +
  # Horizontal chart: swap tufte's default grid orientation
  theme(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    # Inside legend, stacked, anchored at bottom-right corner, white background
    legend.position = "inside",
    legend.position.inside = c(<0.95>, <0.05>),  # x, y as plot proportions
    legend.justification = c(1, 0),                      # anchor: right edge, bottom edge
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggcompbar_vert

Two vertical bars per category (wide focus bar + narrow comparison bar or tick).

```r
# set fig.width to help reduce gap between bars
<data_source> |>
  pivot_wider(
    id_cols = <x_variable>,
    names_from = <group_var>,
    values_from = <value_var>
  ) |>
  mutate(<x_variable> = fct_reorder(<x_variable>, `<y1_name>`)) |>
  ggplot() +
  geom_col(aes(x = <x_variable>, y = `<y1_name>`, fill = "<y1_name>"), width = 0.5) +
  geom_col(aes(x = <x_variable>, y = `<y2_name>`, fill = "<y2_name>"), width = 0.3) +
  # OPTION: replace second geom_col above with this code if want line instead of bar
	# geom_segment(
	#   aes(
	#     x = as.numeric(<x_variable>) - 0.3,
	#     xend = as.numeric(<x_variable>) + 0.3,
	#     y = `<y2_name>`,
	#     yend = `<y2_name>`
	#   ),
	#   colour = cowboysilver,
	#   linewidth = <1.0>
	# ) +
  scale_fill_manual(
    values = c(
      "<y1_name>" = dodgerblue,
      "<y2_name>" = cowboysilver   # remove if using comparison line instead of bar
    )
  ) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.15)),
    position = "bottom"
  ) +
  scale_y_continuous(
    #limits = c(min, max),
    position = "right",
    expand = expansion(mult = c(0, 0.05)),
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = "")
  ) +
  theme(
    # Inside legend, stacked, anchored at top-left corner, white background
    legend.position = "inside",
    legend.position.inside = c(<0.05>, <0.95>),  # x, y as plot proportions
    legend.justification = c(0, 1),                      # anchor: left edge, top edge
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -13)
    ),
		axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggshadedbars

Bars whose thickness encodes a second variable, with background reference bars and column headers.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Select bar length variable
bar_data <- <data_source> |>
  filter(<group_col> == "<x_group>") |>
  select(<y_column>, <x_column>) |>
  mutate(<x_column> = round(<x_column>, <0>))

# Pivot bar width variable and join
plot_data <- <data_source> |>
  filter(<group_col> == "<width_group>") |>
  pivot_wider(
    id_cols = <y_column>,
    names_from = <group_col>,
    values_from = <value_col>
  ) |>
  mutate(`<width_group>` = round(`<width_group>`, <0>)) |>
  left_join(bar_data, by = "<y_column>") |>
  arrange(<x_column>) |>
  mutate(
    # Normalise width to 0-1; adjust <0.9> to control max bar thickness
    width_norm = `<width_group>` / max(`<width_group>`, na.rm = TRUE) * <0.9>,
    # Compute y positions with fixed gap; adjust <0.025> to control spacing
    y_pos = cumsum(width_norm) - width_norm / 2 + row_number() * <0.025>,
    y_min = y_pos - width_norm / 2,
    y_max = y_pos + width_norm / 2
  )

# Top bar values for bracket positioning
top_bar_ymax <- plot_data |> slice_max(y_pos) |> pull(y_max)
top_bar_ymin <- plot_data |> slice_max(y_pos) |> pull(y_min)

plot_data |>
  ggplot() +

  # Background bar
  geom_rect(
    aes(xmin = 0, xmax = <max_x>, ymin = y_min, ymax = y_max),
    fill = cowboysilver30
  ) +

  # Foreground bar
  geom_rect(
    aes(xmin = 0, xmax = <x_column>, ymin = y_min, ymax = y_max),
    fill = dodgerblue
  ) +

  # Label at end of dark bar
  geom_text(
    aes(x = <x_column>, y = y_pos, label = label_number(accuracy = 1, scale = 1, suffix = "<bar_suffix>")(<x_column>)),
    hjust = -0.2,
    size = 3.2
  ) +

  # Right-side value labels
  geom_text(
    aes(x = <label_x>, y = y_pos, label = label_number(accuracy = <1>, scale = <1>)(`<width_group>`)),
    hjust = 0,
    size = 3.2
  ) +

  # Left column header above chart - comment out if using inside bar label below
  # annotate(
  #   "text",
  #   x = 0, y = Inf,
  #   label = "<left_header>",
  #   hjust = 0, vjust = 0,
  #   size = 3.2,
  #   fontface = "bold"
  # ) +

  # OPTION: Left column header inside a bar - comment out header above if using this
  # Filter to the bar you want to place the label in
  annotate(
    "text",
    x = 1,
    y = plot_data |> filter(<y_column> == "<label_bar>") |> pull(y_pos),
    label = "<left_header>",
    hjust = 0,
    vjust = 0.5,
    size = 3.2,
    fontface = "bold",
    colour = "white"
  ) +

  # Right column header
  annotate(
    "text",
    x = <right_header_x>, y = Inf,
    label = "<right_header>",
    hjust = 1, vjust = 0.3,
    size = 3.2, fontface = "bold"
  ) +

  # Bracket connecting right header to top bar
  # Top horizontal tick at top of top bar
  annotate(
    "segment",
    x = <bracket_x_inner>, xend = <bracket_x_outer>,
    y = top_bar_ymax, yend = top_bar_ymax,
    colour = "black", linewidth = 0.3
  ) +
  # Bottom horizontal tick at bottom of top bar
  annotate(
    "segment",
    x = <bracket_x_inner>, xend = <bracket_x_outer>,
    y = top_bar_ymin, yend = top_bar_ymin,
    colour = "black", linewidth = 0.3
  ) +
  # Vertical line from bottom tick up toward header; adjust + <0.05> to stop below header
  annotate(
    "segment",
    x = <bracket_x_outer>, xend = <bracket_x_outer>,
    y = top_bar_ymin, yend = top_bar_ymax + <0.05>,
    colour = "black", linewidth = 0.3
  ) +

  scale_x_continuous(
    limits = c(0, <right_header_x>)
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    breaks = plot_data$y_pos,
    labels = plot_data$<y_column>,
    expand = expansion(mult = c(0.05, 0.05))
  ) +

  # Remove all x axis elements - bars and direct labels replace them
  # clip = "off" needed to show column headers above panel
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggvertbar_text

Building block: value labels inside stacked/vertical bars.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning 
geom_text(
  data = \(x) filter(x, filter_var == "filter_value"),  # filter if want to label subset of stacked bars
  aes(label = comma(value_var, accuracy = 1), y = y_value),
  color = "white",
  size = 3.2
) +
```

