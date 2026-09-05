# Small multiples and facets

Use when one chart per group beats one crowded chart. make_multiples_plot() wraps any template; patchwork stacks the results.

All templates assume `source(here::here("R", "theme_cwr.R"))` has run: it provides the colours
(`dodgerblue`, `habsred`, `cowboysilver`, tints, `manual_n_colours`), `theme_cwr()` as the default theme,
`base_size`, `label_size`, and `cwr_caption()`. Angle-bracket words like `<x_variable>` are placeholders to replace.

## ggmultiples

Small multiples via a plotting function + patchwork; insert any template inside.

```r
make_multiples_plot <- function(data, multiple_label, show_title = FALSE) {
	data |>
	  filter(<facet_variable> == multiple_label) |>
	  mutate(<y_variable> = fct_reorder(<y_variable>, <x_variable>)) |>
	  # INSERT CHART SNIPPET HERE (must start with ggplot(...))
	  # Replace labs(title = ...) with: 
	  labs(title = multiple_label) +
		theme(
		  plot.title = element_markdown(size = base_size * 0.8),
		  legend.position = "none"
		)
	}
	
plot_data <- <main_table> |>
	select(
	  <x_variable> = <x_column>,
	  <y_variable> = <y_column>,
	  <facet_variable> = <facet_column>
	)

p1 <- make_multiples_plot(plot_data, "<multiple_label1>")
p2 <- make_multiples_plot(plot_data, "<multiple_label2>")
p3 <- make_multiples_plot(plot_data, "<multiple_label3>")
p4 <- make_multiples_plot(plot_data, "<multiple_label4>")

(p1 / p2 / p3 / p4) +
plot_annotation(
  title = "Title",
  subtitle = "Subtitle",
  caption = "Source: Source | *Charting Waterloo Region*"
)
```

## ggfacet

Building block: facet_wrap with free y scales.

```r
facet_wrap(
	~ <variable>,
	ncol = <3>,
	scales = "<free_y>"
) +
```

