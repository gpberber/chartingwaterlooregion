# Worked examples

Finished charts from the crime post, exactly as written with the templates. Use them as the
ground truth for how a template looks once placeholders are filled and positions tuned.
Data objects (`incident_summary`, `big_12`, `csi`, ...) are that post's cleaned tables.

## Line chart: Waterloo Region vs Canada vs Ontario (ggline + comp_colours)

```r
# ggline
plot_data <- incident_summary |>
  filter(str_detect(region, "WRPS|Canada|Ontario"))

# Set x min and max
x_min <- plot_data |> pull(year) |> min()
x_max <- plot_data |> pull(year) |> max()

# Define scale colours
group_colours <- comp_colours

ggplot(plot_data, aes(x = year, y = incidents_per_100k_all, colour = region)) +

  geom_line(linewidth = 0.5) +

  scale_x_continuous(
    labels = label_number(big.mark = "", scale = 1, suffix = ""),
    breaks = seq(x_min, x_max, 5),
    minor_breaks = seq(x_min, x_max, 1),
    expand = expansion(mult = c(0.02, 0.08)),
    position = "bottom"
  ) +

  scale_y_continuous(
    position = "right",
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.05))   # lines should not sit on the axis
  ) +

  scale_colour_manual(
    values = group_colours
  ) +

  guides(x = guide_axis(minor.ticks = TRUE)) +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 15, l = -25)
    )
  ) +

  labs(
    title = str_glue("Number of Criminal Incidents: {first_year_incident_summary}-{latest_year_incident_summary}"),
    subtitle = "Per 100,000 population<br>",
    caption = "Source: Statistics Canada | *Charting Waterloo Region*"
  )
```

## Heatmap of raw values, focus row bolded (ggheatraw)

```r
# ggheatraw
# Order rows by mean of raw value (highest at top)
row_order <- big_12_ranked |>
  group_by(region) |>
  summarise(avg_val = mean(weighted_clearance_rate_violent, na.rm = TRUE)) |>
  arrange(avg_val) |>
  pull(region)

# Define special formatting
highlight_row <- "WRPS"

# Prepare data with ordering and special formatting
plot_data <- big_12_ranked |>
  mutate(
    row_factor = factor(region, levels = row_order),
    row_label = if_else(region == highlight_row, paste0("**", highlight_row, "**"), as.character(region))
  )

# Set x min and max
x_min <- plot_data |> pull(year) |> min()
x_max <- plot_data |> pull(year) |> max()

# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(row_factor, row_label) |> deframe()

# Define fill scale parameters
fill_colors <- alpha(dodgerblue, c(0.1, 0.4, 0.7, 1.0))

ggplot(plot_data, aes(x = year, y = row_factor, fill = weighted_clearance_rate_violent)) +

  geom_tile(color = "white", linewidth = 0.5) +

  scale_fill_stepsn(
    colors = fill_colors,
    n.breaks = 5,
    labels = label_number(big.mark = ",", scale = 1, suffix = "%"),
    na.value = "grey80",
    guide = guide_coloursteps(
      reverse = FALSE,
      show.limits = FALSE,
      barwidth = unit(4, "cm"),
      barheight = unit(0.4, "cm"),
      frame.colour = "black",
      frame.linewidth = 0.3,
      label.vjust = -0.0
    )
  ) +

  scale_x_continuous(
    labels = label_number(big.mark = "", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.0, 0.0)),
    position = "bottom"
  ) +

  scale_y_discrete(
    labels = y_labels
  ) +

  guides(x = guide_axis(minor.ticks = TRUE)) +

  theme(
    axis.text.y.left = element_markdown(),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +

  labs(
    title = "CSI-Weighted Clearance Rates for Violent Crimes",
    subtitle = "Big 12, ordered best to worst by average clearance rate",
    caption = "Source: Statistics Canada | *Charting Waterloo Region*"
  )
```

## Slope chart with labels computed from the data (ggslope)

Note how `label_data` is derived from the data rather than typed by hand, and how
`hjust` is mapped per label so the middle group sits between the two years.

```r
# ggslope
plot_data <- big_12 |>
  filter(year %in% c(min(year, na.rm = TRUE), max(year, na.rm = TRUE)))

# Set x min and max
x_min <- plot_data |> pull(year) |> min()
x_max <- plot_data |> pull(year) |> max()

# Define line colours
group_colours <- comp_colours

# Label positions - taken from each group's value in the first year
label_data <- plot_data |>
  filter(year == x_min) |>
  summarise(
    y = mean(officers_per_100k, na.rm = TRUE),
    .by = comparison_group
  ) |>
  mutate(
    x = if_else(comparison_group == "Other Big 12", mean(c(x_min, x_max)), x_min - 0.5),
    label = comparison_group,
    fontface = if_else(comparison_group == "WRPS", "bold", "plain"),
    hjust = if_else(comparison_group == "Other Big 12", 0.5, 1)
  )

ggplot(plot_data, aes(x = year, y = officers_per_100k, colour = comparison_group, group = region)) +

  geom_line(linewidth = 0.5) +

  geom_point(size = 3.1, colour = "white") +  # white point to obscure line ends

  geom_point(size = 3.0) +

  geom_label(
    data = label_data,
    aes(
      x = x,
      y = y,
      label = label,
      fontface = fontface,
      hjust = hjust
    ),
    colour = "grey30",
    fill = "white",
    linewidth = 0,
    size = 3.2,
    vjust = 0.5,
    inherit.aes = FALSE
  ) +

  scale_x_continuous(
    breaks = c(x_min, x_max),
    expand = expansion(mult = c(0.20, 0.08))
  ) +

  scale_y_continuous(
    position = "right",
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.05))   # lines should not sit on the axis
  ) +

  scale_colour_manual(
    values = group_colours,
    guide = "none"
  ) +

  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -20)
    ),
    plot.margin = margin(t = 13, l = 13, b = 13, r = 100)   # make plot area thinner
  ) +

  labs(
    title = str_glue("Officer Staffing {first_year_personnel} vs {latest_year_personnel}"),
    subtitle = "Officers per 100,000 people<br>",
    caption = str_glue("Source: Statistics Canada | *Charting Waterloo Region*")
  )
```

## Faceted ranked bars with direct labels (gghorbar + ggfacet + tidytext::reorder_within)

Chunk header used: `fig-height: 6`, `fig-width: 10`. `reorder_within()` and `scale_y_reordered()`
come from tidytext and let each facet rank its own bars.

```r
# gghorbar (direct bar labels option) + ggfacet
plot_data <- csi |>
  filter(
    in_big_12,
    year == latest_year_csi
  ) |>
  select(region, comparison_group, csi, csi_violent, csi_nonviolent) |>
  pivot_longer(
    cols = c(csi, csi_violent, csi_nonviolent),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric_label = case_when(
      metric == "csi" ~ "All",
      metric == "csi_violent" ~ "Violent",
      metric == "csi_nonviolent" ~ "Non-Violent"
    ),
    metric_label = factor(metric_label, levels = c("All", "Violent", "Non-Violent"))
  ) |>
  group_by(metric) |>
  mutate(region = reorder_within(region, value, metric)) |>
  ungroup()

plot_data |>
  ggplot(aes(x = value, y = region, fill = comparison_group)) +

  geom_col(width = 0.7) +

  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +

  geom_text(
    aes(x = value, y = region,
        label = label_number(big.mark = ",", scale = 1, accuracy = 0.1)(value)),
    hjust = 1.15,
    vjust = 0.5,
    size = 3.2,
    colour = "white"
  ) +

  facet_wrap(
    ~ metric_label,
    ncol = 3,
    scales = "free"
  ) +

  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +

  scale_y_reordered() +

  scale_fill_manual(
    values = set_names(manual_2_colours, c("Other Big 12", "WRPS"))
  ) +

  theme(
    axis.text.y.left = element_markdown(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    # sit the legend above the facet strips rather than on top of them
    legend.position = "top",
    legend.justification = "left"
  ) +

  coord_cartesian(clip = "off") +

  labs(
    title = str_glue("Crime Severity Index: {latest_year_csi}"),
    subtitle = "<br>",
    caption = "Source: Statistics Canada | *Charting Waterloo Region*"
  )
```
