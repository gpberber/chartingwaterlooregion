# Comparison charts: dumbbells and arrows

Use when the story is the gap or the change between two values per category (before/after, us/them).

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggdumbbell

Dumbbell: two dots per category joined by a segment, legend inside the panel.

```r
plot_data <- <data_object> |>
  filter(<x_column> %in% c(<value1>, <value2>)) |>
  pivot_wider(
    id_cols = <y_column>,
    names_from = <x_column>,
    values_from = <value_column>
  ) |>
  mutate(
    <y_column> = reorder(<y_column>, `<value1>`),
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", as.character(<y_column>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

plot_data |>
  ggplot() +

  # Background reference segment spanning full x range
  geom_segment(aes(
    x = 0,
    xend = <max_x>,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey80",
    linewidth = 0.3
  ) +

  # Dumbbell segment connecting x1 to x2
  geom_segment(aes(
    x = `<value1>`,
    xend = `<value2>`,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey50",
    linewidth = 0.7
  ) +

  geom_point(aes(
    x = `<value1>`,
    y = <y_column>,
    color = "<x1_label>"
    ),
    shape = 19, size = 4
  ) +

  geom_point(aes(
    x = `<value2>`,
    y = <y_column>,
    color = "<x2_label>"
    ),
    shape = 19, size = 4
  ) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  scale_color_manual(
    values = c(
      "<x1_label>" = cowboysilver,
      "<x2_label>" = dodgerblue
    ),
    name = NULL,
    breaks = c("<x1_label>", "<x2_label>")
  ) +

  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(0, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.05)),
    position = "top"
  ) +

  scale_y_discrete(
    position = "left",
    labels = y_labels
  ) +

  # Horizontal chart: swap tufte's default grid orientation
  theme(
    axis.text.y.left = element_markdown(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "inside",
    legend.justification = c(0, 1),
    legend.position.inside = c(0.05, 0.9),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA)
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggdumbbellrect

Dumbbell plus a boxed value column on the right (e.g. the change).

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_object> |>
  filter(<x_column> %in% c(<value1>, <value2>)) |>
  pivot_wider(
    id_cols = <y_column>,
    names_from = <x_column>,
    values_from = <value_column>
  ) |>
  mutate(
    <y_column> = reorder(<y_column>, `<value1>`),
    <rect_variable> = round(<rect_variable>, <0>),
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", as.character(<y_column>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

plot_data |>
  ggplot() +

  # Background reference segment spanning full x range
  geom_segment(aes(
    x = 0,
    xend = <max_x>,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey80",
    linewidth = 0.3
  ) +

  # Dumbbell segment connecting x1 to x2
  geom_segment(aes(
    x = `<value1>`,
    xend = `<value2>`,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey50",
    linewidth = 0.7
  ) +

  geom_point(aes(
    x = `<value1>`,
    y = <y_column>,
    color = "<x1_label>"
    ),
    shape = 19, size = 4
  ) +

  geom_point(aes(
    x = `<value2>`,
    y = <y_column>,
    color = "<x2_label>"
    ),
    shape = 19, size = 4
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
      y = <y_column>,
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

  scale_color_manual(
    values = c(
      "<x1_label>" = cowboysilver,
      "<x2_label>" = dodgerblue
    ),
    name = NULL,
    breaks = c("<x1_label>", "<x2_label>")
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

  # Horizontal chart: swap tufte's default grid orientation
  # clip = "off" needed to show rectangles and header beyond panel
  theme(
    axis.text.y.left = element_markdown(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    legend.position = "inside",
    legend.justification = c(0, 1),
    legend.position.inside = c(0.05, 0.9),
    legend.direction = "vertical",
    legend.background = element_rect(fill = "white", color = NA)
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggarrow

Arrow from value 1 to value 2 per category; one annotation label.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_source> |>
  filter(<x_column> %in% c(<value1>, <value2>)) |>
  pivot_wider(
    id_cols = <y_column>,
    names_from = <x_column>,
    values_from = <value_column>
  ) |>
  mutate(
    <y_column> = reorder(<y_column>, `<value1>`),
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", as.character(<y_column>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

plot_data |>
  ggplot(aes(y = <y_column>)) +

  # Background reference segment spanning full x range
  geom_segment(aes(
    x = 0,
    xend = <max_x>,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey80",
    linewidth = 0.3
  ) +

  geom_vline(xintercept = 0, linewidth = 0.5) +

  geom_segment(
    aes(x = `<value1>`, xend = `<value2>`, yend = <y_column>),
    arrow = arrow(length = unit(0.3, "cm"), type = "open"),
    linewidth = 1.0,
    color = dodgerblue
  ) +

  annotate(
    "label",
    x = <label_x>,
    y = <label_y>,
    label = "<annotation_text>",
    fill = "white",
    color = "grey30",
    linewidth = 0,
    size = 3.2
  ) +

  scale_x_continuous(
    #limits = c(min, max),
    #breaks = seq(0, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.05)),
    position = "top"
  ) +

  scale_y_discrete(
    position = "left",
    labels = y_labels
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

## ggarrowrect

Arrow chart plus a boxed value column on the right.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_source> |>
  filter(<x_column> %in% c(<value1>, <value2>)) |>
  pivot_wider(
    id_cols = <y_column>,
    names_from = <x_column>,
    values_from = <value_column>
  ) |>
  mutate(
    <y_column> = reorder(<y_column>, `<value1>`),
    <rect_variable> = round(<rect_variable>, <0>),
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", as.character(<y_column>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

plot_data |>
  ggplot(aes(y = <y_column>)) +

  # Background reference segment spanning full x range
  geom_segment(aes(
    x = 0,
    xend = <max_x>,
    y = <y_column>,
    yend = <y_column>
    ),
    color = "grey80",
    linewidth = 0.3
  ) +

  geom_vline(xintercept = 0, linewidth = 0.5) +

  geom_segment(
    aes(x = `<value1>`, xend = `<value2>`, yend = <y_column>),
    arrow = arrow(length = unit(0.3, "cm"), type = "open"),
    linewidth = 1.0,
    color = dodgerblue
  ) +

  annotate(
    "label",
    x = <label_x>,
    y = <label_y>,
    label = "<annotation_text>",
    fill = "white",
    linewidth = 0,
    size = 3.2
  ) +

  # Boxed right-side data labels
  geom_label(
    aes(
      x = <rect_x>,
      y = <y_column>,
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

