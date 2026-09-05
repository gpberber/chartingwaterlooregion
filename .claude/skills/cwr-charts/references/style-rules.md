# Style rules and why they exist

The look is Tufte-inspired: maximise the share of ink that carries data. These are the
decisions baked into `theme_cwr()` and the templates, with the reasoning, so you can make
the same call in a situation the templates do not cover.

## Text

- **Title = the finding**, written as a sentence a reader could repeat ("Violent crime in
  Waterloo Region rose faster than in Canada after 2014"), not a variable name.
- **Subtitle = units and scope** ("Incidents per 100,000 people, 2004 to 2024"). It ends with
  `<br>` when the top of the panel needs space (line charts with labels near the top).
- **Caption = source**, always through `cwr_caption()`, so every chart ends with
  `| *Charting Waterloo Region*`. Cite the table number for Statistics Canada.
- No axis titles. Units live in the subtitle. The scatter templates place a y title above
  the axis with `annotation_custom(textGrob(...))` because there the axis meaning is not obvious.
- Text sizes are multiples of `base_size` (13). Inside geoms use `label_size` (3.2 mm).
  Change `base_size` only for a whole post, never per chart.

## Colour

- `dodgerblue` (#104E8B) = the focus: Waterloo Region, WRPS, Kitchener.
- `habsred` (#AF1E2D) = the main comparison (Canada) or the single highlighted item.
- `cowboysilver` (#869397) = context: other cities, "everyone else", background bars.
- `ontgreen` = Ontario when it needs to be distinct from Canada.
- Tints (`*50`, `cowboysilver30`) for secondary series or backgrounds; `alpha()` steps for
  sequential fills in heatmaps (`alpha(dodgerblue, c(0.1, 0.4, 0.7, 1.0))`).
- Never map colour to more than five groups; collapse the rest to grey.
- Bars use `fill`, lines and points use `colour`. White outlines (`geom_point(colour = "white")`
  under the real point) mask line ends in slope charts.

## Axes and gridlines

- Vertical charts: y axis on the right, labels raised above their gridline
  (`vjust = -0.5`, negative left margin), x axis line black at the bottom, minor ticks yearly.
- Horizontal charts: x axis on top, vertical gridlines, no x axis line, y axis text
  left-aligned (`hjust = 0`) and markdown-capable so one row can be bold.
- Ranked charts start the value axis at zero with `geom_vline(xintercept = 0)` as the baseline.
- When the y axis does not start at zero, say so: use the `ggyaxis_break_label` squiggle.
- `expand = expansion(mult = c(0, 0.05))` so bars sit on the baseline; lines get a small
  bottom expansion so they do not touch the axis.
- `big.mark = ","` on value axes, `big.mark = ""` on year axes.

## Labels and legends

- Direct labels first. Place them with `label_data` tibbles (line charts) or `nudge_*`
  columns (scatter) after the first render. Bold the focus label.
- Bar value labels go inside the bar in white when the bar is long enough, outside in
  grey30 otherwise; the templates use a `threshold` to split them.
- Legends only when direct labels would collide (many groups, crossing lines). Then use the
  theme default: inside, top-left, horizontal, no title.
- `coord_cartesian(clip = "off")` whenever a label or header sits outside the panel.

## Layout

- Fix `fig-height` and `fig-width` in the chunk before tuning positions; positions are in data
  units and shift when the panel size changes.
- Ranked horizontal charts: height grows with rows (about 0.3 in per row plus 1.5 in).
- Slope and vertical bar charts squeeze the panel with a large right `plot.margin` so the
  eye reads the comparison, not the empty space.
- Small multiples: same y scale across panels unless the story is within-panel shape;
  strip text left-aligned and bold; `panel.spacing.y = 1.5 lines`.

## Tables

`gt` only. Use `gtExtras::gt_theme_538()` or plain `gt()` with `tab_options(table.font.names = "Inter")`.
One table per chunk, chunk label `tbl-<slug>`, with `tbl-cap`. Source note via `tab_source_note()`.
