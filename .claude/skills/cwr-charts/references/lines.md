# Line, area, ribbon and slope charts

Use for change over time. Colour by group with manual_n_colours, drop the legend and place labels by hand at the line ends once positions are known.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggline

Multi-series line chart, colours by group, labels placed manually once positions are known.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Define groups based on y variable
orig_groups <- c("<group1_orig>", "<group2_orig>", "<group3_orig>", "<group4_orig>", "<group5_orig>")
renamed_groups <- c("<group1_name>", "<group2_name>", "<group3_name>", "<group4_name>", "<group5_name>") # define display names

# OPTION 1: Multiple columns that need pivoting (no predefined groups)
# Uncomment and use this section if you have separate columns to pivot
# # group_columns <- c("group1_column", "group2_column", "group3_column", "group4_column")  # Corresponding column names
# 
# plot_data <- data |>
#   	filter(grouping_variable == "group_value") |>
#   	select(
# 		<x_variable>, 
#  		all_of(set_names(group_columns, renamed_groups))  # Rename columns during selection
#   	) |> 
#   	pivot_longer(
#     		-<x_variable>, 
#     		names_to = "<colour_variable>", 
#     		values_to = "<y_variable>"
#   	)

# OPTION 2: Data already in long format (predefined groups, no pivoting)
# Use this section if your data already has grouping and y columns
# Define plot data
plot_data <- <source> |> 
	mutate(
		<colour_variable> = str_replace_all(<colour_variable>, set_names(renamed_groups, orig_groups))
	)
	
# Set x min and max
x_min = plot_data |> pull(<x_variable>) |> min()
x_max = plot_data |> pull(<x_variable>) |> max()

# Define scale colours
group_colours <- set_names(manual_5_colours, renamed_groups)  # replace manual_5_colours if need fewer/custom colours


# Create explicit label positions - manually positioned to avoid overlaps - uncomment once values known
#label_data <- tibble(
#  	x = c(x_group1, x_group2, x_group3, x_group4, x_group5),
#  	y = c(y_group1, y_group2, y_group3, y_group4, y_group5),
#  	label = renamed_groups,
#		fontface = c("plain", "plain", "plain", "plain", "plain"),	 # set desired to "bold"
## 	If want label scoped to one facet panel (facet_level), uncomment and edit spec below
## 	Note facet_var must be a factor with the same levels as in plot_data (need to factor it in plot_data if not already done)
## 	facet_var = factor("facet_level", levels = levels(<plot_data>$facet_var))
#)

ggplot(plot_data, aes(x = <x_variable>, y = <y_variable>, colour = <colour_variable>)) +

	# commented out until label placements determined
	#geom_label(
	#  data = label_data,
	#  aes(
	#    x = x,
	#    y = y,
	#    label = label,
	#    fontface = fontface
	#  ),
	#  colour = "grey30",
	#  fill = "white", 
	#  linewidth = 0,
	#  size = 3.2,
	#  hjust = 0,       # 0 left, 1 right, 0.5 centered
	#  vjust = 0,	    # 0 bottom, 1 top, 0.5 centered
	#  inherit.aes = FALSE
	#) +

	geom_line(linewidth = 0.8) +
	
	scale_x_continuous(
	  #limits = c(min, max),
	  #breaks = seq(min, max, by),
	  labels = label_number(big.mark = "", scale = 1, suffix = ""),
	  minor_breaks = seq(x_min, x_max, 1),
	  expand = expansion(mult = c(0.02, 0.05)),
	  position = "bottom"
	) +
	
	scale_y_continuous(
	  #limits = c(min, max),
	  position = "right",
	  #breaks = seq(min, max, by),
	  labels = label_number(big.mark = ",", scale = 1, suffix = ""),
	  expand = expansion(mult = c(0, 0.05))
	) +
	
	scale_colour_manual(
	  values = group_colours,
	  guide = "none"
	) +
	
	guides(x = guide_axis(minor.ticks = TRUE)) +
	
	# Right-axis label positioning: sit labels above gridlines
	theme(
	    axis.text.y.right = element_text(
	      size = rel(1),
	      hjust = 1.0,
	      vjust = -0.5,
	      margin = margin(r = 12, l = -20)
	    )
	  ) +
	
	labs(
	  title = "Title",
	  subtitle = "Subtitle<br>",
	  caption = "Source: Source | *Charting Waterloo Region*"
	)
```

## ggarea

Stacked area chart by group.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Define groups based on y variable
orig_groups <- c("<group1_orig>", "<group2_orig>", "<group3_orig>", "<group4_orig>", "<group5_orig>")
renamed_groups <- c("<group1_name>", "<group2_name>", "<group3_name>", "<group4_name>", "<group5_name>") # define display names

# OPTION 1: Multiple columns that need pivoting (no predefined groups)
# Uncomment and use this section if you have separate columns to pivot
# # group_columns <- c("group1_column", "group2_column", "group3_column", "group4_column")  # Corresponding column names
# 
# plot_data <- data |>
#   	filter(grouping_variable == "group_value") |>
#   	select(
# 		<x_variable>, 
#  		all_of(set_names(group_columns, renamed_groups))  # Rename columns during selection
#   	) |> 
#   	pivot_longer(
#     		-<x_variable>, 
#     		names_to = "<colour_variable>", 
#     		values_to = "<y_variable>"
#   	)

# OPTION 2: Data already in long format (predefined groups, no pivoting)
# Use this section if your data already has grouping and y columns
# Define plot data
plot_data <- <source> |> 
	mutate(
		<colour_variable> = str_replace_all(<colour_variable>, set_names(renamed_groups, orig_groups))
	)
	
# Set x min and max
x_min = plot_data |> pull(<x_variable>) |> min()
x_max = plot_data |> pull(<x_variable>) |> max()

# Define scale colours
group_colours <- set_names(manual_5_colours, renamed_groups)  # replace manual_5_colours if need fewer/custom colours


# Create explicit label positions - manually positioned to avoid overlaps - uncomment once values known
#label_data <- tibble(
#  	x = c(x_group1, x_group2, x_group3, x_group4, x_group5),
#  	y = c(y_group1, y_group2, y_group3, y_group4, y_group5),
#  	label = renamed_groups,
#		fontface = c("plain", "plain", "plain", "plain", "plain"),	 # set desired to "bold"
## 	If want label scoped to one facet panel (facet_level), uncomment and edit spec below
## 	Note facet_var must be a factor with the same levels as in plot_data (need to factor it in plot_data if not already done)
## 	facet_var = factor("facet_level", levels = levels(<plot_data>$facet_var))
#)

ggplot(plot_data, aes(x = <x_variable>, y = <y_variable>, fill = <colour_variable>)) +

	geom_area() +

	# commented out until label placements determined
	#geom_text(
	#  data = label_data,
	#  aes(
	#    x = x,
	#    y = y,
	#    label = label,
	#    fontface = fontface
	#  ),
	#	 colour = "white",
	#  size = 3.2,
	#  hjust = 0,       # 0 left, 1 right, 0.5 default centered
	#	 vjust = 0,				# 0 bottom, 1 top, 0.5 default centered
	#  inherit.aes = FALSE
	#) +
	
	scale_x_continuous(
	  #limits = c(min, max),
	  #breaks = seq(min, max, by),
	  labels = label_number(big.mark = "", scale = 1, suffix = ""),
	  minor_breaks = seq(x_min, x_max, 1),
	  expand = expansion(mult = c(0.02, 0.05)),
	  position = "bottom"
	) +
	
	scale_y_continuous(
	  #limits = c(min, max),
	  position = "right",
	  #breaks = seq(min, max, by),
	  labels = label_number(big.mark = ",", scale = 1, suffix = ""),
	  expand = expansion(mult = c(0.0, 0.05))
	) +
	
	scale_fill_manual(
	  values = group_colours,
	  guide = "none"
	) +
	
	guides(x = guide_axis(minor.ticks = TRUE)) +
	
	# Right-axis label positioning: sit labels above gridlines
	theme(
	    axis.text.y.right = element_text(
	      size = rel(1),
	      hjust = 1.0,
	      vjust = -0.5,
	      margin = margin(r = 12, l = -20)
	    )
	  ) +
	
	labs(
	  title = "Title",
	  subtitle = "Subtitle<br>",
	  caption = "Source: Source | *Charting Waterloo Region*"
	)
```

## ggribbon

Two lines with a shaded band between them (min/max, range, confidence).

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Filter to rows needed for ribbon/line layers
plot_data <- <data_source> |>
  filter(<group_var> %in% c("<ymin_name>", "<ymax_name>"))

# Prep ribbon data (wide format needed for ymin/ymax)
ribbon_data <- plot_data |>
  pivot_wider(
    id_cols = <x_var>,
    names_from = <group_var>,
    values_from = <y_var>
  )

# create x axis min and max
x_min <- plot_data |> pull(<x_var>) |> min()
x_max <- plot_data |> pull(<x_var>) |> max()

# Label data - adjust x and y positions once known
# ribbon label
# ribbon_label <- tibble(
#   x = x_position,
#   y = y_position,
#   label = "label",
# 	If want label scoped to one facet panel (facet_level), uncomment and edit spec below
# 	Note facet_var must be a factor with the same levels as in plot_data (need to factor it in plot_data if not already done)
# 	facet_var = factor("facet_level", levels = levels(<plot_data>$facet_var)),
# )
  
# line labels
# label_data <- tibble(
#   x = c(y_min_x, y_max_x),
#   y = c(y_min_y, y_max_y),
#   label = c("<ymin_name>", "<ymax_name>"),
# 	If want label scoped to one facet panel (facet_level), uncomment and edit spec below
# 	Note facet_var must be a factor with the same levels as in plot_data (need to factor it in plot_data if not already done)
# 	facet_var = factor("facet_level", levels = levels(<plot_data>$facet_var)),
#   fontface = c("plain", "plain")    # edit if want any bolded
# )

plot_data |>
  ggplot(aes(x = <x_var>, y = <y_var>, color = <group_var>)) +

  geom_ribbon(
    data = ribbon_data,
    aes(x = <x_var>, ymin = `<ymin_name>`, ymax = `<ymax_name>`),
    inherit.aes = FALSE,
    fill = cowboysilver,
    alpha = 0.3
  ) +
  
  # line labels - commented out until label placements determined
  # geom_label(
  #   data = label_data,
  #   aes(
  #     x = x,
  #     y = y,
  #     label = label,
  #     fontface = fontface   
  #   ),
  #   colour = "grey30",
  #   fill = "white",
  #   linewidth = 0,
  #   size = 3.2,
  #   hjust = 0,
  #   vjust = 0,
  #   inherit.aes = FALSE
  # ) +

  geom_line(linewidth = 0.8) +

  # ribbon label
  # geom_text(
  #   data = ribbon_label,
  #   aes(x = x, y = y, label = label),
  #   inherit.aes = FALSE,
  #   colour = "grey30",
  #   vjust = 0,
  #   size = 3.2
  # ) +

  scale_color_manual(
    values = c(
      "<ymin_name>" = cowboysilver,
      "<ymax_name>" = dodgerblue
    ),
    guide = "none"
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    position = "right",
    #scale_y_log10(
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0, 0.05))
  ) +

  scale_x_continuous(
    #limits = c(min, max),
    position = "bottom",
    #breaks = seq(x_min, x_max, by),
    minor_breaks = seq(x_min, x_max, 1),
    labels = label_number(big.mark = "", scale = 1, suffix = ""),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  #scale_x_date(
  #  breaks = c(x_min, seq.Date(
  #    as.Date(paste0(year(x_min) + 5, "-01-01")),
  #    x_max,
  #    by = "5 years"
  #  )),
  #	 date_breaks = "5 years",  # delete this if specifying breaks in line above
  #  date_labels = "%Y",
  #  minor_breaks = seq.Date(x_min, x_max, by = "year"),
  #  expand = expansion(mult = c(0.02, 0.05))
  #) +
  
  guides(x = guide_axis(minor.ticks = TRUE)) +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -20)
    )
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle<br>",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

## ggslope

Slope chart: two time points, one line per group, points masked at the ends.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Define groups based on y variable
orig_groups <- c("<group1_orig>", "<group2_orig>", "<group3_orig>", "<group4_orig>", "<group5_orig>")
renamed_groups <- c("<group1_name>", "<group2_name>", "<group3_name>", "<group4_name>", "<group5_name>") # define display names

# Define plot data
plot_data <- <source> |> 
	filter(
	  <x_variable> %in% c(<x1_variable>, <x2_variable>)
	) |>
	mutate(
		<colour_var> = str_replace_all(<colour_var>, set_names(renamed_groups, orig_groups))
	)
	
# Set x min and max
x_min = plot_data |> pull(<x_variable>) |> min()
x_max = plot_data |> pull(<x_variable>) |> max()

# Define line colours
group_colours <- set_names(manual_5_colours, renamed_groups)  # adjust for number of groups

# Label positions - adjust x and y values once known
#label_data <- tibble(
#  x     = c(x1, x2, x3, x4, x5),
#  y     = c(y1, y2, y3, y4, y5),
#  label = renamed_groups,
#	 fontface = c("plain", "plain", "plain", "plain", "plain"),  # set desired to bold
#  If want label scoped to one facet panel (facet_level), uncomment and edit spec below
#	 Note facet_var must be a factor with the same levels as in plot_data (need to factor it in plot_data if not already done)
#	 facet_var = factor("facet_level", levels = levels(<plot_data>$facet_var)),
#)

ggplot(plot_data, aes(x = <x_variable>, y = <y_variable>, colour = <colour_var>, group = <group_var>)) +

  # commented out until label placements determined
	#geom_label(
	#  data = label_data,
	#  aes(
	#    x = x,
	#    y = y,
	#    label = label,
	#    fontface = fontface
	#  ),
	#	 colour = "grey30",
	#  fill = "white", 
	#  linewidth = 0,
	#  size = 3.2,
	#  hjust = 0,       # 0 left, 1 right, 0.5 default centered
	#	 vjust = 0,				# 0 bottom, 1 top, 0.5 default centered
	#  inherit.aes = FALSE
	#) +

  geom_line(linewidth = 0.8) +
  
  geom_point(size = 3.1, colour = "white") +  # add white point to obscure line ends

  geom_point(size = 3.0) +

  scale_x_continuous(
    breaks = c(x_min, x_max),
    expand = expansion(mult = c(0.05, 0.08))
  ) +

  scale_y_continuous(
    #limits = c(min, max),
    position = "right",
    #breaks = seq(min, max, by),
    labels = label_number(big.mark = ",", scale = 1, suffix = ""),
    expand = expansion(mult = c(0, 0.05))
  ) +

  scale_colour_manual(
    values = group_colours,
    guide = "none"
  ) +

  # Right-axis label positioning: sit labels above gridlines
  theme(
    axis.text.y.right = element_text(
      size = rel(1),
      hjust = 1.0,
      vjust = -0.5,
      margin = margin(r = 12, l = -20)
    ),
    plot.margin = margin(t = 13, l = 13, b = 13, r = 200)   # make plot area thinner
  ) +

  labs(
    title = "Title",
    subtitle = "Subtitle<br>",
    caption = "Source: Source | *Charting Waterloo Region*"
  )
```

