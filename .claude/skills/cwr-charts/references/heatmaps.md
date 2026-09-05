# Heatmaps

Use for a category x time grid where the pattern matters more than exact values (ranks across years, rates across places).

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggheatrank

Heatmap of ranks (rows ordered by average rank, best at top), stepped fill legend.

```r
# HEATMAP TEMPLATE FOR RANKED DATA
# Replace placeholders: data_ranked, x_var, y_var, ranking_var, colors, breaks,
# labels, x axis breaks, legend coords

# Create y-axis ordering vector
# Calculate average ranking and reorder regions (best at top)
row_order <- <data_ranked> |> 
  group_by(<y_var>) |> 
  summarise(avg_rank = mean(<ranking_var>, na.rm = TRUE)) |>  # adjust for ranking method
  arrange(avg_rank) |>  # add desc() if lower avg is better
  pull(<y_var>)

# Define special formatting
highlight_row <- "<special_row_name>"  # Row to highlight (optional)

# Prepare data with ordering and special formatting
plot_data <- <data_ranked> |> 
  mutate(
    row_factor = factor(<y_var>, levels = row_order),
    row_label = if_else(<y_var> == highlight_row, paste0("**", highlight_row, "**"), as.character(<y_var>))
  )
  
# Set x min and max
x_min = plot_data |> pull(<x_var>) |> min()
x_max = plot_data |> pull(<x_var>) |> max()
  
# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(row_factor, row_label) |> deframe()

# Define fill scale parameters
fill_colors <- alpha(<dodgerblue4>, c(0.1, 0.4, 0.7, 1.0))  # Adjust alphas as needed (one fewer than num breaks)
#fill_breaks <- c(break1, break2, break3, break4, break5)          # Adjust breaks
fill_labels <- c("label1", "label2", "label3", "label4", "label5")  # Adjust labels

ggplot(plot_data, aes(x = <x_var>, y = row_factor, fill = <ranking_var>)) +
  
  geom_tile(color = "white", linewidth = 0.5) +
  
  scale_fill_stepsn(
    colors = fill_colors,
    n.breaks = 5,
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    #breaks = fill_breaks,
    #labels = fill_labels,
    na.value = "black",
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
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    #minor_breaks = seq(x_min, x_max, 1),
    labels = label_number(big.mark = "", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.0, 0.0)),
    position = "bottom"
  ) +
  
  scale_y_discrete(
    labels = y_labels
  ) +
  
  guides(x = guide_axis(minor.ticks = TRUE)) +
  
  # Heatmap: remove all gridlines (tiles define the structure),
  theme(
  	axis.text.y.left = element_markdown(),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  
  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggheatraw

Heatmap of raw values (rows ordered by mean), stepped fill legend, optional cell labels.

```r
# HEATMAP (RAW VALUES) TEMPLATE
# Replace placeholders: data_source, x_var, y_var, value_var, colors, breaks, labels

# Create y-axis ordering vector
# Order rows by mean of raw value (highest at top)
row_order <- <data_source> |> 
  group_by(<y_var>) |> 
  summarise(avg_val = mean(<value_var>, na.rm = TRUE)) |>
  arrange(desc(avg_val)) |>  # remove desc() if lower avg should be at top
  pull(<y_var>)

# Define special formatting
highlight_row <- "<special_row_name>"  # Row to highlight (optional)

# Prepare data with ordering and special formatting
plot_data <- <data_source> |> 
  mutate(
    row_factor = factor(<y_var>, levels = row_order),
    row_label = if_else(<y_var> == highlight_row, paste0("**", highlight_row, "**"), as.character(<y_var>))
  )
  
# Set x min and max
x_min = plot_data |> pull(<x_var>) |> min()
x_max = plot_data |> pull(<x_var>) |> max()
  
# Pre-compute labels to avoid data masking issue in scale_y_discrete
y_labels <- plot_data |> select(row_factor, row_label) |> deframe()

# Define fill scale parameters
fill_colors <- alpha(<dodgerblue4>, c(0.1, 0.4, 0.7, 1.0))  # Adjust alphas as needed (one fewer than num breaks)
#fill_breaks <- c(break1, break2, break3, break4, break5)           # Set breaks based on data range
fill_labels <- c("label1", "label2", "label3", "label4", "label5")  # Adjust labels

ggplot(plot_data, aes(x = <x_var>, y = row_factor, fill = <value_var>)) +
  
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Optional: add value labels inside tiles
  #geom_text(
  #  aes(label = label_number(big.mark = ",", scale = 1, suffix = "")(<value_var>)),
  #  colour = "white",
  #  size = 3.2
  #) +
  
  scale_fill_stepsn(
    colors = fill_colors,
    n.breaks = 5,
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    #breaks = fill_breaks,
    #labels = fill_labels,
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
    #limits = c(min, max),
    #breaks = seq(min, max, by),
    #minor_breaks = seq(x_min, x_max, 1),
    labels = label_number(big.mark = "", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.0, 0.0)),
    position = "bottom"
  ) +
  
  scale_y_discrete(
    labels = y_labels
  ) +
  
  guides(x = guide_axis(minor.ticks = TRUE)) +
  
  # Heatmap: remove all gridlines (tiles define the structure),
  theme(
  	axis.text.y.left = element_markdown(),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  ) +
  
  labs(
    title = "Title",
    subtitle = "Subtitle",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

