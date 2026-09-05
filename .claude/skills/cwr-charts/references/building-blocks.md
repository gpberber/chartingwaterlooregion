# Building blocks: scales, labels, legends, annotations

Fragments to add to any template. Each ends with `+` so it can be pasted into a ggplot chain.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggtitles

labs() with the standard title / subtitle / Source caption.

```r
labs(
	title = "<Title>",
	subtitle = "<Subtitle>",
	caption = "Source: <Source> | *Charting Waterloo Region*"
) +
```

## ggscalex_cont

Continuous x scale with minor ticks.

```r
scale_x_continuous(
  #limits = c(min, max),
  #breaks = c(min, max, by), end_max),
  labels = label_number(scale = <1>, suffix = "<>", big.mark = ""),
  #minor_breaks = seq(min, max, 1),
  expand = expansion(mult = c(0.02, 0.05)),
  position = "<bottom>"
) +
guides(x = guide_axis(minor.ticks = TRUE)) +
```

## ggscaley_cont

Continuous y scale on the right with explicit breaks.

```r
scale_y_continuous(
  #limits = c(<min>, <max>),
  breaks = c(seq(<min>, <max>, <by>), <max>),
  labels = label_number(scale = <1>, suffix = "<>", big.mark = ","),
  #minor_breaks = seq(<min>, <max>, <1>),
  expand = expansion(mult = c(0.0, 0.05)),
  position = "right"
) +
```

## ggscalex_date

Date x scale with automatic or anchored breaks.

```r
# x_min and x_max must be defined before using this scale
scale_x_date(
  # Option 1: automatic breaks
  date_breaks = "<5 years>",
  # Option 2: manual breaks anchored to data start - comment out date_breaks above
  # breaks = c(
  #   x_min,   # change if want start at different date
  #   seq.Date(
  #     as.Date(paste0(year(x_min) + <5>, "-01-01")),  # adjust month/day number if first date isn't in January 1
  #     x_max,   # change if want start at different date
  #     by = "<5> years"
  # )),
  date_labels = "<%Y>",
  # Option 1: automatic breaks
  date_minor_breaks = "<1 year>",
  # Option 2: manual breaks anchored to desired start - comment out date_breaks above
  #minor_breaks = seq.Date(
	  #  as.Date("min_minor_date"),
	  #  as.Date("max_minor_date"),
	  #  by = "1 years"
	#),
  expand = expansion(mult = c(0.02, 0.05))
) +
guides(x = guide_axis(minor.ticks = TRUE)) +
```

## ggscalex_disc

Discrete x scale with relabelled levels.

```r
scale_x_discrete(
  #limits = c("<level1>", "<level2>"),  # use to reorder or restrict levels
  labels = c(
    "<level1>" = "<label1>",
    "<level2>" = "<label2>"
  ),
  expand = expansion(mult = c(0.05, 0.05)),
  position = "<bottom>"
) +
```

## ggscaley_disc

Discrete y scale using pre-computed markdown labels (bold one row).

```r
# Pre-compute labels to avoid data masking issue in scale_y_discrete
# y_labels <- plot_data |> select(y_variable, y_label) |> deframe()
scale_y_discrete(
  #limits = c("<level1>", "<level2>"),  # use to reorder or restrict levels
  labels = y_labels,
  expand = expansion(mult = c(0.05, 0.05)),
  position = "left"
) +
theme(
  axis.text.y.left = element_markdown()
) +
```

## ggscale_color

Manual colour scale from manual_5_colours mapped to group names.

```r
# adjust number of colors/names to suit data
scale_color_manual(
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
```

## ggscale_fill

Manual fill scale from manual_5_colours mapped to group names.

```r
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
```

## ggviridis

Viridis colour/fill scale.

```r
scale_<color>_viridis_<d>(name = "<Legend title>")
```

## gglegend

Theme block for the standard inside-top-left horizontal legend.

```r
theme(
	legend.position = "inside",
	  legend.position.inside = c(-0.0, 1.02),
	  legend.justification = "left",
	  legend.box = "horizontal",
	  #legend.margin = margin(t = 20, b = 20),
	  legend.direction = "horizontal",
	  legend.title = element_blank(),
	  legend.text = element_text(
	    size = base_size * 0.8,
	    margin = margin(r = 10)
	  ),
	  #legend.key = element_blank(),
	  legend.spacing = unit(0.1, "cm"),
	  legend.box.spacing = unit(0.1, "cm")
) +
```

## ggannotate

annotate() text at a data position.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
annotate(
	"<text>",
	x = <x_position>, 
	y = <y_position>,  
	# If have discrete y labels but a continuous y axis, use line below
	# y = plot_data |> filter(discrete_y == "y_label") |> pull(cont_y_variable),
	hjust = 0,    # left-justified at x position
	vjust = 0.5,  # centre-aligned at y positionn
	label = "<Annotation text>",
	size = <3.2>,
	fontface = "bold",
	color = "<grey30>"
) +
```

## gggeom_text

geom_text() with every positioning argument spelled out.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
geom_text(
  aes(
    x = <x_variable>,
    y = <y_variable>,
    label = <label_variable>
  ),
  colour = "<grey30>",
  size = <3.2>,
  fontface = "<plain>",      # plain, bold, italic, bold.italic
  hjust = <0.5>,             # 0 = left, 0.5 = centre, 1 = right
  vjust = <0.5>,             # 0 = bottom, 0.5 = centre, 1 = top
  nudge_x = <0>,
  nudge_y = <0>,
  angle = <0>,
  lineheight = <1.2>,       # for multi-line labels via \n
  na.rm = TRUE,
  inherit.aes = <FALSE>
) +
```

## gglabel

geom_label() (boxed text) with every positioning argument spelled out.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
geom_label(
  aes(
    x = <x_variable>,
    y = <y_variable>,
    label = <label_variable>
  ),
  colour = "<grey30>",        # text colour
  fill = "<white>",          # box background colour
  linewidth = <0>,           # box border width; 0 = no border
  size = <3.2>,
  fontface = "<plain>",      # plain, bold, italic, bold.italic
  hjust = <0.5>,             # 0 = left, 0.5 = centre, 1 = right
  vjust = <0.5>,            # 0 = bottom, 0.5 = centre, 1 = top
  nudge_x = <0>,
  nudge_y = <0>,
  label.padding = unit(<0.25>, "lines"),  # internal padding around text
  label.r = unit(<0.15>, "lines"),        # corner radius; 0 = square corners
  na.rm = TRUE,
  inherit.aes = <FALSE>
) +
```

## gggridx

Theme block that swaps gridlines to vertical for horizontal charts.

```r
theme(
	axis.line.x = element_blank(),
	axis.ticks.x = element_blank(),
	axis.text.x = element_text(margin = margin(t = 0, b = 1)),
	panel.grid.major.x = element_line(
		color = cowboysilver, 
		linewidth = 0.3
	)
) +
```

## ggguides

Minor ticks on the x axis.

```r
guides(x = guide_axis(minor.ticks = TRUE)) +
```

## ggyaxis_break_label

Squiggle marker showing the y axis does not start at zero.

```r
# fix fig.height=5, fig.width=7 (or whatever dimensions you need) in cell header before tweaking positioning
# Axis break indicator - squiggly line below lowest y gridline
# Indicates y axis does not start at zero
#annotate(
#  "text",
#  x = Inf,
#  y = label_y,    # set to halfway between lowest y break and min y limit
#  label = "~",  
#  colour = "grey30",
#  hjust = 1.0,  # adjust to suit regular axis labels above (> 0.5 shifts left)
#  vjust = 0.5,  # centered at y position set above
#  size = 6      # adjust if needed
#) +

# needed if squiggly is clipped at panel edge
#coord_cartesian(clip = "off") +
```

## ggsave

ggsave() with the standard size and dpi.

```r
ggsave(
	filename = here("<output>", "<plot_name>.png"),
	width = <8>, 
	height = <6>,
	dpi = 300,
	bg = "white"
) +
```

