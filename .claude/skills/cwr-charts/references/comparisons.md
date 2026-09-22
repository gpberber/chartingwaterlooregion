# Comparison charts: dumbbells, arrows and dots with intervals

Use when the story is the gap or the change between two values per category (before/after, us/them).

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggdumbbell

Dumbbell: two dots per category joined by a segment, the two named on the top row.

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
    # cwr_short_name(): the house short form of a long district name (rule 2b)
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", cwr_short_name(as.character(<y_column>)))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

# The two dots are named on the top row instead of in a legend (rule 3): each
# label beside its own dot, on the side facing the other, lifted just above the
# joining line. `nudge` is in data units, which are half as wide on a phone, so
# it is multiplied by cwr_gap() and cwr_figure() is passed phone_gap = 2
# (rule 11a).
top_row <- plot_data |>
  filter(<y_column> == last(levels(<y_column>))) |>
  pivot_longer(c(`<value1>`, `<value2>`), names_to = "series", values_to = "value") |>
  mutate(
    hjust = if_else(series == "<value1>", 0, 1),
    nudge = if_else(series == "<value1>", <0.6>, -<0.6>)
  )

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

  # The series names, on the top row only. geom_label() rather than
  # geom_text() so each sits on white rather than on the line it overlaps;
  # linewidth = 0 drops the box's border, and grey30 is the colour
  # theme_cwr() gives the axis labels.
  geom_label(
    data = top_row,
    aes(x = value + nudge * cwr_gap(), y = <y_column>, label = series, hjust = hjust),
    vjust = 0, nudge_y = 0.1,
    colour = "grey30", fill = "white", linewidth = 0,
    label.padding = unit(0.08, "lines"),
    size = label_size * 0.8, fontface = "bold"
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
    breaks = c("<x1_label>", "<x2_label>"),
    guide = "none"
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
    labels = y_labels,
    # Room above the top row for the series names
    expand = expansion(add = c(0.6, 1))
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
    caption = cwr_caption("Source")
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
    # cwr_short_name(): the house short form of a long district name (rule 2b)
    y_label = if_else(<y_column> == "<highlight_value>", "**<highlight_value>**", cwr_short_name(as.character(<y_column>)))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_column>, y_label) |> deframe()

# The two dots are named on the top row instead of in a legend (rule 3): each
# label beside its own dot, on the side facing the other, lifted just above the
# joining line. `nudge` is in data units, which are half as wide on a phone, so
# it is multiplied by cwr_gap() and cwr_figure() is passed phone_gap = 2
# (rule 11a).
top_row <- plot_data |>
  filter(<y_column> == last(levels(<y_column>))) |>
  pivot_longer(c(`<value1>`, `<value2>`), names_to = "series", values_to = "value") |>
  mutate(
    hjust = if_else(series == "<value1>", 0, 1),
    nudge = if_else(series == "<value1>", <0.6>, -<0.6>)
  )

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

  # The series names, on the top row only. geom_label() rather than
  # geom_text() so each sits on white rather than on the line it overlaps;
  # linewidth = 0 drops the box's border, and grey30 is the colour
  # theme_cwr() gives the axis labels.
  geom_label(
    data = top_row,
    aes(x = value + nudge * cwr_gap(), y = <y_column>, label = series, hjust = hjust),
    vjust = 0, nudge_y = 0.1,
    colour = "grey30", fill = "white", linewidth = 0,
    label.padding = unit(0.08, "lines"),
    size = label_size * 0.8, fontface = "bold"
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
    breaks = c("<x1_label>", "<x2_label>"),
    guide = "none"
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
    labels = y_labels,
    # Room above the top row for the series names
    expand = expansion(add = c(0.6, 1))
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
    caption = cwr_caption("Source")
  )
```

## ggdoterror

Dot per category with its confidence interval as a fading, flat-ended bar behind it, x axis on top.

```r
# One estimate per row, a dot, with its confidence interval as a fading bar
# behind it (the New York Times poll-chart look). For sample data (style
# rule 9b): lower and upper are the published 95% bounds, kept beside the
# estimate in 02_clean_data.R. Write the sample's stock note into
# cwr_caption(notes = ) after this one (cwr-charts rule 1d).
plot_data <- <data_object> |>
  mutate(
    <y_variable> = reorder(<y_variable>, <estimate>),
    y_label = if_else(<y_variable> == "<bold_y_value>", "**<bold_y_value>**", as.character(<y_variable>))
  )

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(<y_variable>, y_label) |> deframe()

plot_data |>
  ggplot(aes(y = <y_variable>)) +

  # The interval: a flat-ended bar from lower to upper bound, strongest in the
  # middle and fading to both ends. A tile is centred on x and as wide as the
  # interval; height is a share of the row. The gradient is a grid pattern
  # (R 4.1+): group = FALSE runs it across each bar on its own, not across
  # the whole panel. Translucent blue, so gridlines show through as on the NYT.
  geom_tile(
    aes(x = (<lower> + <upper>) / 2, width = <upper> - <lower>),
    height = 0.24,
    fill = grid::linearGradient(
      colours = c(alpha(dodgerblue, 0.08), alpha(dodgerblue, 0.5), alpha(dodgerblue, 0.08)),
      group = FALSE
    )
  ) +

  # The estimate: a dot with a white rim so it stands off the bar
  geom_point(
    aes(x = <estimate>),
    shape = 21, size = 4.5, stroke = 1,
    fill = dodgerblue, colour = "white"
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

  # Horizontal chart: vertical gridlines only, no x ticks or axis line
  theme(
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3),
    panel.grid.major.y = element_blank()
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = cwr_caption("Source", notes = "Bars show 95% confidence intervals")
  )
```

## ggnumcomp

Two figures written large with their names above and a ratio below, no axes.

```r
# Two figures as the picture: each count written large with its name above
# it, and a ratio on a line of its own underneath. No bars, no axes. The
# welcome post's people-and-cows chart is the worked example.
figures <- <data_object> |>
  summarise(<First> = sum(<first_count>), <Second> = sum(<second_count>)) |>
  pivot_longer(everything(), names_to = "what", values_to = "count") |>
  mutate(
    what = fct_inorder(what),
    label = label_comma(accuracy = 1)(count),
    # The first figure on the left, the second on the right
    x = if_else(what == "<First>", 0, 1)
  )

# The ratio, worked out from the figures rather than typed, and rounded: a
# decimal would be false precision on top of two counts
per_100 <- round(
  (figures |> filter(what == "<Second>") |> pull(count)) /
    (figures |> filter(what == "<First>") |> pull(count)) * 100
)

# The three rows, each centred on its own y: the name on top, the figure under
# it, the ratio below. Tighten or loosen together, checking the ratio clears
# the descenders of the figure above it.
row_word  <-  0.26
row_count <-  0
row_ratio <- -0.26

p <- ggplot(figures, aes(x = x)) +
  geom_text(
    aes(y = row_word, label = str_to_lower(what)),
    colour = "grey30", size = label_size * 1.1
  ) +
  geom_text(
    aes(y = row_count, label = label, colour = what),
    fontface = "bold", size = label_size * 3.2
  ) +
  # Centred between the two figures
  annotate(
    "text", x = 0.5, y = row_ratio,
    label = paste0(per_100, " <second> for every 100 <first>"),
    colour = dodgerblue, size = label_size * 1.1
  ) +
  # The comparison figure in grey, the one the story is about in blue
  scale_colour_manual(
    values = c(<First> = cowboysilver, <Second> = dodgerblue),
    guide = "none"   # each figure is named above it
  ) +
  # Fixed ranges, which set how much room is left around the three rows
  scale_x_continuous(limits = c(-0.35, 1.35), expand = expansion(0)) +
  scale_y_continuous(limits = c(-0.32, 0.34), expand = expansion(0)) +
  coord_cartesian(clip = "off") +
  # Nothing here is a scale a reader reads, so every axis and gridline goes.
  # axis.text.y.left is named too: theme_cwr() sets it separately, and the
  # more specific element would otherwise win over axis.text.y.
  theme(
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
    axis.line.x = element_blank(), axis.line.y = element_blank(),
    axis.title.x = element_blank(), axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Title: the finding, in one line",
    subtitle = "Subtitle: what is measured, for whom, and when",
    caption = cwr_caption("<source>")
  )

# No category axis to set a height from, so it is given one
cwr_figure(p, "fig-<slug>", alt = "<the figures, written out>", height = 2.2, phone_height = 2.2)
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
    caption = cwr_caption("Source")
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
    caption = cwr_caption("Source")
  )
```

