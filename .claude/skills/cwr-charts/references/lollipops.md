# Lollipop charts

Use instead of bars when there are many categories or values are close together; the thin segment is easier to compare. Always ranked (reorder()).

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## gglollipop

Lollipop (segment + dot) for ranked values with an x axis on top.

```r
plot_data <- <data_object> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <x_variable>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>)) +

  geom_segment(aes(
    x = 0, xend = <x_variable>,
    y = <y_variable>,
    yend = <y_variable>),
    color = "grey50", linewidth = 0.7) +

  geom_point(
    aes(
      x = <x_variable>,
      y = <y_variable>
    ),
    shape = 19, size = 4,
    color = dodgerblue
  ) +
  
  geom_vline(
    xintercept = 0, 
    color = "black", 
    linewidth = 0.5
  ) +

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

  # Horizontal chart: swap tufte's default grid orientation
  theme(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
		axis.text.y.left = element_markdown()
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## gglollipoplabel

Lollipop with direct value labels replacing the x axis entirely.

```r
plot_data <- <data_object> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <x_variable>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>)) +

  geom_segment(aes(
    x = 0, xend = <x_variable>,
    y = <y_variable>,
    yend = <y_variable>),
    color = "grey50", linewidth = 0.7) +

  geom_point(
    aes(
      x = <x_variable>,
      y = <y_variable>
    ),
    shape = 19, size = 4,
    color = dodgerblue
  ) +

  # Direct point labels replacing x axis
  geom_text(
    aes(
      x = <x_variable>,
      y = <y_variable>,
      label = label_number(big.mark = ",", scale = <1>, suffix = "<>")(<x_variable>)
    ),
    nudge_x = <2>,    # in data units; adjust after initial render
    hjust = 0,
    vjust = 0.5,
    size = 3.2,
    colour = "grey30"
  ) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0))
  ) +

  scale_y_discrete(
    position = "left",
    labels = y_labels
  ) +

  # Horizontal chart: no x axis or gridlines - direct labels replace them
  # clip = "off" allows labels to extend beyond panel edge
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y.left = element_markdown()
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## gglollipoprect

Lollipop plus a boxed value column on the right with a shaded header.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
plot_data <- <data_object> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <x_variable>),
    <x_variable> = round(<x_variable>, <0>),
    <rect_variable> = round(<rect_variable>, <0>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(x = <x_variable>, y = <y_variable>)) +

  geom_segment(aes(
    x = 0, xend = <x_variable>,
    y = <y_variable>, yend = <y_variable>),
    color = "grey50", linewidth = 0.7
  ) +

  geom_point(
    aes(x = <x_variable>, y = <y_variable>),
    shape = 19, size = 4,
    color = dodgerblue
  ) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  # Boxed right-side data labels
  # formatC pads positive numbers with a space to match width of negatives
  geom_label(
    aes(
      x = <rect_x>,
      y = <y_variable>,
      label = paste0(formatC(<rect_variable>, format = "f", digits = <1>, flag = " "), "<rect_suffix>")
    ),
    hjust = 1,
    size = 3.2,
    fill = "grey90",
    colour = "black",
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
    vjust = -1.0,  # lower is higher
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
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.text.y.left = element_markdown()
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

