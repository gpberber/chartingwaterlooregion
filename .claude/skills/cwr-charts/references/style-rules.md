# Style rules and why they exist

The look is Tufte-inspired: maximise the share of ink that carries data. These are the
decisions baked into `theme_cwr()` and the templates, with the reasoning, so you can make
the same call in a situation the templates do not cover.

## Text

- **Title = the finding**, written as a sentence a reader could repeat ("Violent crime in
  Waterloo Region rose faster than in Canada after 2014"), not a variable name.
- **Subtitle = units and scope** ("Incidents per 100,000 people, 2004 to 2024"). A `<br>` is
  added at its end by hand when the top of the panel needs space (line charts with labels near the
  top); the templates leave it out.
- **Caption = source**, always through `cwr_caption()`, naming whoever published the numbers.
  Cite the table number for Statistics Canada. The caption does **not** carry a Charting
  Waterloo Region byline by default: plotting a publisher's figures as published is their
  work, not ours. Pass `credit = TRUE` only when the numbers on the chart were worked out
  here - a rate we calculated, an index we based, a model we fitted, several sources combined.
- **The subtitle carries the scale as well as the unit**, so no tick label has to. An axis in
  thousands or millions is rescaled (`label_number(scale = 1e-3)` plus explicit `breaks`) and
  named in the subtitle rather than labelled "4,000" or "4K" at every break: shorter labels are
  what let a phone show five breaks instead of three, and the desktop and phone versions can then
  share one scale. Greg writes the subtitle, so flag the rescaling when handing the chart over.
- No axis titles. Units live in the subtitle. The scatter templates place a y title above
  the axis with `annotation_custom(textGrob(...))` because there the axis meaning is not obvious.
- Text sizes are multiples of `base_size` (13). Inside geoms use `label_size` (3.2 mm).
  Change `base_size` only for a whole post, never per chart.

## Colour

- `dodgerblue` (#104E8B) = the focus: the Region, WRPS, Kitchener.
- **The Region's label is `cwr_region` ("Region")** in every piece of chart text - axes,
  legends, direct labels, tooltips, notes. The whole site is about Waterloo Region, so the
  long name only costs width, and "Waterloo" alone is ambiguous with the city.
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
- A horizontal stacked bar names its segments on the bars, not in a legend: first and last above
  the top bar, at its two ends, and any in between under the bottom bar with a tick
  (`cwr_stack_keys()`). Each name then sits beside the colour it names.
- Long municipal names are shortened in chart text through `cwr_short_name()` ("N. Dumfries"),
  as "Region" is through `cwr_region`: the label column and map shapes have no width to spare.
- Category names wrap between words automatically in `cwr_figure()`: past 30 characters on the
  desktop, past 15 ("Crime in Canada") on the phone, with deeper rows (more for three or four
  lines) and bars thinned to keep their thickness. A narrower label column is a wider plot. The
  wrap never goes below the longest single word, since splitting there would deepen rows
  without narrowing anything.
- Sample data carries its uncertainty. Every share from the census long form gets a 95% interval
  in the cleaning script (`R/census_ci.R`, Statistics Canada's own method); a chart draws it only
  where it could change what a reader concludes, and otherwise states the widest in a note
  (`cwr_ci_widest()` fills its number). A share rated E or F on Statistics Canada's scale (CV
  16.6% or more) is not reported: its row says "Not reported" with a note. A table that publishes no
  intervals gets the stock note that close shares may differ only by sampling error. The census
  long form itself is disclosed once, in the README's Reliability table, not on every chart.
  Readers are told what the numbers can bear, and only where it matters, so the notes are not
  skimmed past.
- Every chart note is written out in the chunk, stock ones included (rule 1d), so Greg can reword
  or delete it where he sees it; no function adds a note on its own.
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
