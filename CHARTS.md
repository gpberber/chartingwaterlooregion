# Choosing a chart

Every chart on the site is built from one of these templates, which live in `_dev/r.snippets`.
This page is for picking one. The code for each is in the `cwr-charts` skill
(`.claude/skills/cwr-charts/references/`), and Claude reads it there - so ask for a chart in plain
words and the template gets chosen for you. This is for when you want to know what is possible.

**Never a pie or a donut.** Use `ggvertbar` or `gghorbar` for parts of a whole.

## Start here: what do you want to show?

| What you want to show | Template |
|---|---|
| Rank places or categories | `gghorbar`, or `gglollipop` when there are many |
| ...and show a second number beside the ranking | `gghorbarrect`, `gglollipoprect` |
| A few categories, one value each | `ggvertbar` |
| Compare two groups, category by category | `ggcompbar_horiz`, `ggdumbbell` |
| Sample estimates with their margins of error | `ggdoterror` |
| Two totals, and which is bigger | `ggnumcomp` |
| A trend over months or years | `ggline` |
| Change between exactly two dates | `ggslope`, `ggarrow`, `ggdumbbell` |
| How a total splits, and how the split moves | `ggarea` |
| A range, not a single number, over time | `ggribbon` |
| Whether two measures move together | `ggscatter`, or `ggbubble` to weight by size |
| A pattern across categories and time | `ggheatraw`, or `ggheatrank` for positions |
| Too many groups for one readable chart | `ggmultiples` |

## The chart templates

### Bar charts

| Snippet | Chart | Good for |
|---|---|---|
| `ggvertbar` | Vertical bar chart | A handful of categories with one number each and short labels: spending by department, trips by mode, a count for each of a few years. |
| `gghorbar` | Horizontal bar chart, ranked | A ranking - and the default whenever the category names are long or there are more than about six of them: districts, offence types, wards. |
| `gghorbarrect` | Ranked bars with a value column | A ranking where a second number matters as much as the bar: ranked by count, with the rate printed beside it. |
| `ggcompbar_horiz` | Paired horizontal bars | Us against them, one pair per category: Waterloo Region beside Ontario, this year beside last. |
| `ggcompbar_vert` | Paired vertical bars | The same comparison when there are only a few categories and the labels are short. |
| `ggshadedbars` | Bars weighted by a second variable | A rate or share where the size of the group behind it matters: a per-person figure with population as the bar's thickness. |

### Lollipop charts

| Snippet | Chart | Good for |
|---|---|---|
| `gglollipop` | Lollipop chart, ranked | The same job as ranked bars, but easier to read when there are many categories or the values sit close together. |
| `gglollipoplabel` | Lollipop with value labels | A ranking where the exact numbers matter as much as the order, so the axis can be dropped entirely. |
| `gglollipoprect` | Lollipop with a value column | A ranking plus a second number per row: the value ranked, the change or rate boxed beside it. |

### Line, area, ribbon and slope charts

| Snippet | Chart | Good for |
|---|---|---|
| `ggline` | Line chart over time | A trend over months or years, for one group or a few compared. |
| `ggarea` | Stacked area chart | How a total divides up over time, and whether the mix is shifting. |
| `ggribbon` | Line with a shaded band | A range rather than a single figure over time: highest and lowest, a confidence interval, best and worst case. |
| `ggslope` | Slope chart | Exactly two dates: who rose, who fell, and whether the order changed between them. |

### Comparison charts: dumbbells, arrows and dots with intervals

| Snippet | Chart | Good for |
|---|---|---|
| `ggdumbbell` | Dumbbell chart | The gap between two values in each category: 2019 against 2024, one group against another. |
| `ggdumbbellrect` | Dumbbell with a value column | The same gap, with its size spelled out in a column rather than left to the eye. |
| `ggdoterror` | Dots with confidence intervals | Sample estimates side by side with their margins of error: poll results, survey shares, long-form census rates with published bounds. |
| `ggnumcomp` | Two figures, side by side | Two totals whose comparison is the whole story - more cows than people - shown as the numbers themselves, with the ratio between them. |
| `ggarrow` | Arrow chart | Change per category when the direction is the point: which went up, which fell, and how far. |
| `ggarrowrect` | Arrow chart with a value column | The same, with the size of each change printed beside its arrow. |

### Scatter and bubble charts

| Snippet | Chart | Good for |
|---|---|---|
| `ggscatter` | Scatter plot | Whether two measures move together across places or units: density against transit use, income against distance. |
| `ggbubble` | Bubble chart | The same relationship, with a third variable sizing each point - usually population, so big places read as big. |

### Heatmaps

| Snippet | Chart | Good for |
|---|---|---|
| `ggheatrank` | Rank heatmap | Where each place sat in a ranking in every period, and how positions moved. |
| `ggheatraw` | Value heatmap | A pattern across a category-by-time grid: calls by month and year, incidents by ward and season. |

### Maps of the Region

| Snippet | Chart | Good for |
|---|---|---|
| `ggmap` | Map of the Region, labelled | Where something is rather than how much of it there is: which district, which corner of the Region. Labels sit inside the shapes. |
| `ggmapshaded` | Shaded map (choropleth) | One value per district shown as shading, so the pattern reads before the numbers do: shares, rates, densities. |

### Small multiples and facets

| Snippet | Chart | Good for |
|---|---|---|
| `ggmultiples` | Small multiples | One small chart per group instead of one crowded chart. Any template above can go inside it. |

## Building blocks

Not charts on their own - pieces added to one. Most end with `+` so they paste into a ggplot chain.

| Snippet | What it is | Use it for |
|---|---|---|
| `ggvertbar_text` | Value labels inside bars | Printing each number on its bar, so the value axis, ticks and gridlines can come off. |
| `ggfacet` | Facets with free scales | facet_wrap() when each panel needs its own y scale. |
| `ggtitles` | Title, subtitle, source line | The standard labs() block. The source line comes from cwr_caption(). |
| `ggscalex_cont` | Continuous x scale | A numeric x axis with minor ticks. |
| `ggscaley_cont` | Continuous y scale, on the right | A numeric value axis on the right, with breaks you choose. |
| `ggscalex_date` | Date x scale | A time axis with automatic or anchored breaks. |
| `ggscalex_disc` | Discrete x scale | A categorical x axis with relabelled levels. |
| `ggscaley_disc` | Discrete y scale | A categorical y axis using markdown labels, which is how one row gets bolded. |
| `ggscale_color` | Manual colour scale | Mapping the house colours to named groups. |
| `ggscale_fill` | Manual fill scale | The same, for fills. |
| `ggviridis` | Viridis scale | A continuous colour or fill ramp; what the heatmaps use. |
| `gglegend` | Legend inside, top left | The house legend position, for the few charts that keep a legend instead of direct labels. |
| `ggannotate` | Annotation text | A note placed at a data position. |
| `gggeom_text` | Direct text labels | Labelling line ends, bar ends or points, with every positioning argument spelled out. |
| `gglabel` | Boxed text labels | The same with a box behind the text, for labels sitting over a busy background such as a map. |
| `gggridx` | Vertical gridlines | Swapping gridlines to vertical, which every horizontal chart needs. |
| `ggguides` | Minor x ticks | Minor ticks on the x axis. |
| `ggyaxis_break_label` | Axis-break marker | A squiggle showing the value axis does not start at zero. |
| `ggwraplabels` | Wrapped category labels | Long category names on a horizontal chart - industries, offence types - broken over two lines so they stop crowding the bars, and broken harder on the phone. |
| `cwrfigure` | Save and place the chart | The call every chart ends with: writes the desktop and phone PNGs and places the figure. |

---

Generated from `_dev/build_chart_references.R` on 2026-09-22. Do not edit this file by hand: add a snippet to `_dev/r.snippets`, describe it in that script's `catalogue` and `picker` tables, and run `Rscript _dev/build_chart_references.R`.
