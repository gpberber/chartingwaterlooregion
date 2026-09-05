# Scatter and bubble charts

Use when the story is the relationship between two measures. Label points by hand with nudge values after the first render.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggscatter

Scatter with optional loess line and a y-axis title placed above the axis.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_source> |>
  pivot_wider(
    id_cols = <column_id>,
    names_from = <column_names>,
    values_from = <column_values>
  )

# Point label nudge values - set per-point after initial render
# nudge values are in the same units as the x and y axes
# nudge_data <- plot_data |>
#   mutate(
#     nudge_x = case_when(
#       <column_id> == "id1" ~ 0,
#       <column_id> == "id2" ~ 0,
#       <column_id> == "id3" ~ 0,
#       <column_id> == "id4" ~ 0,
#       <column_id> == "id5" ~ 0,
#       .default = 0
#     ),
#     nudge_y = case_when(
#       <column_id> == "id1" ~ 0,
#       <column_id> == "id2" ~ 0,
#       <column_id> == "id3" ~ 0,
#       <column_id> == "id4" ~ 0,
#       <column_id> == "id5" ~ 0,
#       .default = 0
#     )
#   )

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>)) +

  # Point labels - drawn before geom_point so points render on top
  # commented out until nudge values determined
  # geom_label(
  #   data = nudge_data,
  #   aes(
  #     x = <x_variable> + nudge_x,
  #     y = <y_variable> + nudge_y,
  #     label = `<label_variable>`
  #   ),
  #   size = 3.2,
  #   fill = "white",
  #   linewidth = 0,
  #   inherit.aes = FALSE
  # ) +

  geom_point(size = 2, colour = dodgerblue) +

  #geom_smooth(
  #  linewidth = 1.0, 
  #  colour = dodgerblue,
  #  method = "loess",
  #  formula = y ~ x,
  #  na.rm = TRUE
  #) +

  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    #minor_breaks = seq(min, max, by),
    expand = expansion(mult = c(0.02, 0.05)),
    position = "bottom"
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    #breaks = seq(min, max, by), 
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    position = "right",
    expand = expansion(mult = c(0, 0.05))
  ) +

  guides(x = guide_axis(minor.ticks = TRUE)) +

  # place y axis title above y axis, aligned with labels
  annotation_custom(
    grob = textGrob(
      "<y_title>",
      x = 1, y = 1,
      hjust = 1,
      vjust = -1.4,
      gp = gpar(fontsize = base_size * 0.8, fontface = "bold")
    )
  ) +

  coord_cartesian(clip = "off") +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -15)
    ),
    axis.title.x = element_text(
      size = base_size * 0.8,
      face = "bold",
      margin = margin(t = 5)
    )
  ) +

  labs(
    x = "<x_title>",
    title = "Title",
    subtitle = "Subtitle<br>",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggbubble

Bubble chart (size = third variable) with a hand-built size legend.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_source> |>
  #filter(str_detect(<column_names>, "<filter_text>")) |>
  pivot_wider(
    id_cols = <column_id>,
    names_from = <column_names>,
    values_from = <column_values>
  )

x_min <- plot_data |> pull(`<x_variable>`) |> min(na.rm = TRUE)
x_max <- plot_data |> pull(`<x_variable>`) |> max(na.rm = TRUE)

# Bubble size legend - uncomment and adjust positions/values after initial render
# legend_x: x-axis data value for the centre of both reference bubbles
# legend_y_large/small: y-axis data values for bubble centres; separate them
# enough that the large bubble doesn't overlap the small one - adjust by eye
# large_value/small_value: pick two representative values from your size variable
# (e.g. round numbers near the max and median)
# label_x_nudge: rightward offset from bubble centre to label, in x data units
# label_y_nudge: upward offset from bubble centre to label, in y data units
# size_legend <- tibble(
#   x            = c(legend_x, legend_x),
#   y            = c(legend_y_large, legend_y_small),
#   size         = c(large_value, small_value),
#   label_x_nudge = c(nudge_x, nudge_x),
#   label_y_nudge = c(nudge_y_large, nudge_y_small),
#   label        = c("large_label", "small_label")
# )

# Point label nudge values - set per-point after initial render
# nudge values are in the same units as the x and y axes
# nudge_data <- plot_data |>
#   mutate(
#     nudge_x = case_when(
#       <column_id> == "id1" ~ 0,
#       <column_id> == "id2" ~ 0,
#       <column_id> == "id3" ~ 0,
#       <column_id> == "id4" ~ 0,
#       <column_id> == "id5" ~ 0,
#       .default = 0
#     ),
#     nudge_y = case_when(
#       <column_id> == "id1" ~ 0,
#       <column_id> == "id2" ~ 0,
#       <column_id> == "id3" ~ 0,
#       <column_id> == "id4" ~ 0,
#       <column_id> == "id5" ~ 0,
#       .default = 0
#     )
#   )

plot_data |>
  ggplot(aes(x = `<x_variable>`, y = `<y_variable>`, size = `<size_variable>`)) +

  # Point labels - drawn before geom_point so bubbles render on top
  # commented out until nudge values determined
  # geom_label(
  #   data = nudge_data,
  #   aes(
  #     x = `<x_variable>` + nudge_x,
  #     y = `<y_variable>` + nudge_y,
  #     label = `<label_variable>`
  #   ),
  #   size = 3.2,
  #   fill = "white",
  #   linewidth = 0,
  #   inherit.aes = FALSE
  # ) +

  geom_point(alpha = 0.7, colour = dodgerblue) +

  # Bubble size legend - uncomment after initial render
  ## legend title uses annotate("label") for white background with no border
  # annotate(
  #   "label",
  #   x = legend_x, y = legend_y_title,
  #   label = "legend_title",
  #   hjust = 0.5,
  #   vjust = 0.0,
  #   fill = "white",
  #   linewidth = 0,
  #   fontface = "bold",
  #   size = 3.2
  # ) +
  ## shape = 21 with fill = "white" gives white-filled outline circles
  # geom_point(
  #   data = size_legend,
  #   aes(x = x, y = y, size = size),
  #   shape = 21,
  #   colour = "black",
  #   fill = "white",
  #   inherit.aes = FALSE
  # ) +
  ## segments run horizontally from bubble centre to label position
  # geom_segment(
  #   data = size_legend,
  #   aes(x = x, xend = x + label_x_nudge, y = y + label_y_nudge, yend = y + label_y_nudge),
  #   colour = "black",
  #   linewidth = 0.3,
  #   inherit.aes = FALSE
  # ) +
  # geom_label(
  #   data = size_legend,
  #   aes(x = x + label_x_nudge, y = y + label_y_nudge, label = label),
  #   hjust = 0,
  #   size = 3.2,
  #   fill = "white",
  #   linewidth = 0,
  #   inherit.aes = FALSE
  # ) +

  scale_size_area(
    max_size = <15>,
    guide = "none"
  ) +

  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(x_min, x_max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    minor_breaks = seq(x_min, x_max, <1>),    # set by = based on x scale
    expand = expansion(mult = c(0.02, 0.05)),
    position = "bottom"
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    position = "right"
    expand = expansion(mult = c(0.0, 0.05))   
  ) +

  guides(x = guide_axis(minor.ticks = TRUE)) +

  # place y axis title above y axis, aligned with labels
  annotation_custom(
    grob = textGrob(
      "<y_title>",
      x = 1, y = 1,
      hjust = 1,
      vjust = -1.4,
      gp = gpar(fontsize = base_size * 0.8, fontface = "bold")
    )
  ) +

  coord_cartesian(clip = "off") +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -15)
    ),
    axis.title.x = element_text(
      size = base_size * 0.8,
      face = "bold",
      margin = margin(t = 5)
    )
  ) +

  labs(
    x = "<x_title>",
    title = "Title",
    subtitle = "Subtitle<br>",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

