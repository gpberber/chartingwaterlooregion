---
name: cwr-charts
description: House chart style and template library for Charting Waterloo Region posts. Use whenever writing or restyling a ggplot2 chart in a post - picking the chart type, applying the Tufte-style theme and three-colour palette, placing direct labels, and checking the rendered PNG.
---

# Charting Waterloo Region chart style

Every chart on the site is built from `R/theme_cwr.R` plus one of the templates in
`references/`. This skill tells you which template to use, the style rules that
are not negotiable, and how to iterate on a chart until labels sit right.

## Setup that every post already has

The post's setup chunk runs `source(here::here("R", "theme_cwr.R"))`, which provides:

| Object | What it is |
|---|---|
| `dodgerblue`, `habsred`, `cowboysilver`, `ontgreen` | base colours (focus, contrast, context, Ontario) |
| `*50` tints, `cowboysilver30`, `cowboysilver_alpha30` | lighter versions for secondary series and backgrounds |
| `manual_2_colours` ... `manual_5_colours` | ordered palettes; map to groups with `set_names()` |
| `cwr_region` (`"Region"`) | the stock label for Waterloo Region in all chart text (rule 2a) |
| `comp_colours`, `local_colours` | named palettes for recurring comparisons, keyed by `cwr_region` for the Region |
| `theme_cwr()` (already `theme_set`) | the Tufte-inspired theme |
| `base_size` (15), `label_size` (4) | text sizes used inside geoms and annotations |
| `cwr_caption(source, credit = FALSE, notes = NULL, cma = FALSE)` | builds the caption (rule 1a); `credit = TRUE` adds the CWR byline (rule 6); `cma = TRUE` adds the CMA note |
| `cwr_wrap(x, width = 22)` | breaks a long category label over two lines for a discrete axis (rule 11) |
| `cwr_line_labels(data, x, y, group, at, side, limits, bold)` | label positions that sit just above or below each line, off the gridlines (rule 3a) |
| `cwr_figure(p, "fig-id", alt)` (plus `height`, `phone_height` when y is not categories) | saves desktop and phone PNGs to `figures/` and writes the figure (rule 7) |
| `cwr_map_crs`, `cwr_label_point()`, `cwr_label_spot()`, `cwr_map_nudges`, `cwr_map_theme()`, `cwr_text_on_fill()` | the house map style, from `R/maps.R` (rule 12) |
| `cwr_interactive(p, "fig-id", alt, height, phone_height)` | the same for a hover chart (ggiraph); chunk without `output: asis`; numbered in Deep dives like `cwr_figure()` |

Never redefine these in a post. If a post needs a new palette, add it to `theme_cwr.R`.

Two things `theme_cwr()` now handles that a chart used to have to ask for:

- **The left axis reads markdown.** `axis.text.y.left` is an `element_markdown()`, so
  `**bold**` (rule 5) and `<br>` (rule 8) work without a per-chart `theme()` line. Several
  templates still set it explicitly; that line is a harmless no-op, not something to copy
  into a new chart.
- **Panel headings sit outside the axis.** `strip.placement = "outside"` puts a facet's
  heading above the axis labels rather than between them and the panel, which is what a
  ranked horizontal chart needs since the house style moves its x axis to the top. Do not
  set `strip.placement` in a post.

## Choosing a template

| The story is... | Data shape | Template (file) |
|---|---|---|
| ranking of categories | one value per category | `gglollipop`, `gglollipoplabel` (lollipops.md) or `gghorbar` (bars.md) |
| ranking plus a second number per row | value + rate/change | `gglollipoprect`, `gghorbarrect`, `ggdumbbellrect` |
| a few categories, one value each | short labels, <= 6 | `ggvertbar` (bars.md) |
| us vs them per category | two values per category | `ggcompbar_horiz` / `ggcompbar_vert` (bars.md), `ggdumbbell` (comparisons.md) |
| change between two points | before/after per category | `ggarrow`, `ggarrowrect` (comparisons.md), `ggslope` (lines.md) |
| change over time, few series | long time series | `ggline` (lines.md) |
| composition over time | stacked groups | `ggarea` (lines.md) |
| a range or band over time | min/max per period | `ggribbon` (lines.md) |
| relationship between two measures | x, y per unit | `ggscatter`, `ggbubble` (points.md) |
| category x time grid | rank or value per cell | `ggheatrank`, `ggheatraw` (heatmaps.md) |
| where something is, not how much | one value per municipality | `ggmap`, `ggmapshaded` (maps.md) |
| one chart per group | any of the above | `ggmultiples`, `ggfacet` (multiples.md) |
| one chart per group, chosen by the reader | any of the above | a `panel-tabset`, see **Tabs** below |
| bar thickness carries meaning | value + weight | `ggshadedbars` (bars.md) |

Fragments for scales, legends, annotations, text and labels are in `building-blocks.md`.
Finished, real charts from the crime post are in `worked-examples.md`; match their look.

Read only the reference file you need; each is self-contained.

## Style rules (see references/style-rules.md for the reasoning)

**0. Never a pie or a donut**, whatever the data looks like. The About page promises readers the site
is free of them, so this one is a published commitment rather than a preference. Parts of a whole go
in `ggvertbar` or `gghorbar`; if a reader needs to compare shares, bars are what let them.

1. **Title says the finding, subtitle says the units and scope.** Subtitle ends with `<br>`
   when the plot needs breathing room under it. There are no axis titles in this theme, so a
   single-panel chart has nowhere but the subtitle to say what its axis measures. Caption is
   always `cwr_caption("...")`, naming whoever published the numbers and nobody else. It carries no Charting Waterloo Region byline
   by default: plotting a publisher's figures as published is their work, not ours. Pass
   `credit = TRUE` only when the numbers shown were worked out here - a rate calculated, an
   index based, a model fitted, several sources combined.
1a. **Sources and notes are arguments, not strings you assemble.** Pass several sources as a
   vector - `cwr_caption(c("Table 17-10-0155-01", "the 2021 census boundary files"))` - and they
   are joined with commas under the label "Sources:". Writing "X and Y" into one string leaves it
   reading "Source:" in front of two of them. `credit = TRUE` counts as a source as well, so it
   turns the label plural on its own. Footnotes go in `notes`, a character vector: they are numbered in the
   order given and drawn above the source line with a blank line between. **Write the note
   without a key** - the numeral is added for you, superscripted, so a note and its key cannot
   drift apart - and put the matching `<sup>1</sup>` in the title or subtitle by hand, where
   ggtext renders it the same way. Numerals, not symbols; and never key a note with a bare `*`,
   which opens italics in a ggtext caption and swallows the line.
1b. **A chart drawn from CMA data sets `cma = TRUE` and writes nothing.** Several Statistics
   Canada series are published for the Kitchener census metropolitan area and nothing smaller.
   The CMA is six of Waterloo Region's seven municipalities - Wellesley Township is the only
   exclusion and nothing outside the Region is in it - so the figures are a subset of the
   Region, not a different place, and Greg's prose may simply call it Waterloo Region. The
   qualification belongs on the chart all the same, because a chart travels without the post:
   `cma = TRUE` makes it note 1, in wording that is identical across every post and fixable in
   one place. Never type the geography into `notes` by hand.
1c. **Units and scale live in the subtitle, so the axis can carry bare numbers.** A value axis
   never repeats a unit on every tick. Rescale the numbers instead - `label_number(scale = 1e-3)`
   with explicit `breaks` for thousands, `1e-6` for millions - and let the subtitle say which
   scale it is. Four-digit labels do not fit across a 4.2 in phone; three-digit ones do, which
   is usually enough on its own to save a phone-only replacement scale in `phone = list(...)`,
   so both versions keep one scale (Greg: "make the x scale in thousands so more labels fit on
   the phone. I'll note this in the subtitle, which is where units of measure will always be
   reported"). The subtitle itself is Greg's to write, so rescaling an axis is something to
   **say in the hand-off line** - "the x scale is in thousands" - not to write into the
   placeholder. He overrides this where a chart reads better the other way.

2. **Colour has meaning.** Blue = the Region / the focus. Red = the main comparison
   (Canada) or a highlight. Grey = everyone else. Never more than five colours; never rainbow.
2a. **Waterloo Region is "Region" in chart text - always `cwr_region`.** Axis and category
   labels, legend keys, direct labels, tooltips and caption notes all say "Region", never
   "Waterloo Region" or "Waterloo" (which is also a city). Write `cwr_region` rather than the
   word, so a chart cannot drift back. A post's cleaning script does not load `theme_cwr.R`, so
   it types "Region" with a comment pointing at `cwr_region`; a source that says "WRPS",
   "Waterloo (CD)" or "Kitchener - Cambridge - Waterloo" is recoded to it there. Titles,
   subtitles and prose are Greg's and may use the full name; alt text may too, for clarity.
3. **Direct labels beat legends.** Label line ends, bar ends, or points; then
   `guide = "none"`. Use a legend only when labels would collide.
3a. **Line labels come from `cwr_line_labels()`, never typed coordinates.** A hand-typed
   (x, y) drifts off its line (Greg caught labels floating well away from their lines in the
   first globe-csi chart). The helper takes the height from the line itself across `span` x
   units, so the label sits just clear of it at any data revision. Where along the line is
   either given - `at`, the x each label is centred on - or, for any group left out of `at`
   (`at = NULL` for all), found automatically: it tries every x on both sides and keeps the
   spot clear of other lines, of labels already placed, of the panel edge and of gridlines,
   closest to its own line, placing the `bold` focus first. **Use automatic placement by
   default**, and always when one function draws many charts (the globe-csi post draws
   eighteen that way); give `at` only to override a spot that reads badly. Use
   `aes(vjust = vjust)` and `hjust = 0.5` in the `geom_label()`. Check the phone version,
   where the label covers about twice as many x units; labels count as overlapping within
   `span * label_spacing` (1.25) so phone labels do not touch.
3b. **A line label never blots out a gridline when it can avoid it.** The label's white box
   hides any gridline it covers. `side` is a preference: `cwr_line_labels()` checks both sides
   and switches when the preferred side would cover a gridline and the other would not. It
   never switches onto another group's line, which counts for more than a gridline. For the
   check it needs the y scale's `limits` (pass the same vector as `scale_y_continuous()`,
   `NA` where the data decide) or explicit `breaks` if the chart sets its own. Its returned
   `side` column says where each label ended up. If a label still covers a gridline, both
   sides were blocked: move its `at` rather than accept it. Example: the globe-csi violent CSI
   chart asks for Canada above its line at 2003, where the box would hide the 100 gridline,
   so the helper puts it below.
3c. **Hover layers: dots first, squares on top.** An interactive line chart
   (`cwr_interactive()`) draws two invisible `geom_point_interactive()` layers sharing a
   `data_id`: small dots (`size = 2.5`) that appear on hover, then large squares
   (`shape = 15, size = 7`) carrying the `tooltip`. The order matters: the topmost element
   under the mouse fires, and a dot drawn over the squares has no tooltip, so pointing
   straight at a year showed nothing until this was caught.
4. **Drop what the data makes redundant.** If bars carry value labels, remove the value
   axis text, ticks, and gridlines. Horizontal charts swap gridlines to vertical (the
   templates include this `theme()` block).
5. **Bold the focus row** with the `y_label` trick (`**Region**`, from `cwr_region`, via `element_markdown`).
6. **Value axis on the right** for vertical charts, labels sitting above gridlines
   (the `axis.text.y.right` block in the templates). Ranked horizontal charts put the
   x axis on top.
7. **One chart per chunk, through `cwr_figure()`.** Build the plot as `p`, then call
   `cwr_figure(p, "fig-<slug>", alt = )` in a chunk
   with `#| output: asis` (chunk label without the `fig-` prefix; the id passed to the
   function carries it). It saves two PNGs to the post's `figures/` folder: 8.3 in wide for
   desktops (exactly the paragraph width) and 4.2 in wide for phones, and emits a `<picture>`
   so the browser shows the right one. Width is fixed. **Height is set by the rows, not by
   hand, for every chart whose y axis is categories** (horizontal bars, lollipops, dot plots,
   stacked bars, facets of them): leave `height` and `phone_height` out, and `cwr_figure()`
   draws each row `cwr_row_height` (0.37 in) deep on a desktop and `cwr_phone_row_height`
   (0.30 in) on a phone, so bars are the same thickness in every chart in every post. More
   rows, stacked phone panels, wrapped headings or a legend just make the chart taller.
   Room asked for with `scale_y_discrete(expand = )` counts as rows too. Only a chart with
   no category axis - a line chart, vertical bars, a map - passes `height` (5 for a line
   chart) and `phone_height` (a little squarer). Never type a height onto a category chart
   to make it "look right"; if its rows look wrong, the fix is to the constants in
   `R/theme_cwr.R`, for every chart at once. Never use knitr's `fig-width`/`fig-cap` chunk
   options for a chart.
7a. **Labels and captions follow the post type; do not set them by hand.** A **Deep dive**
   gets a plain "Figure 1" under each chart, so prose far below can say "as @fig-<slug>
   showed" - always write the reference as `@fig-<slug>`, never type the number, because
   inserting a chart renumbers everything after it. A **Snapshot** gets no label at all: a
   few charts read in five minutes need no numbering, and the chart's own title and subtitle
   already say what it shows. `cwr_figure()` reads the first entry in the post's
   `categories` to decide, so a correctly labelled post needs nothing extra.
   `caption =` is optional and usually omitted - the source line is already drawn inside the
   chart by `cwr_caption()`. Pass one only for a note that cannot live in the image, such as
   a break in the series. `alt =` is always required: with no caption it is the only
   description a screen reader has, so write what the chart shows, not what it is.
8. **Long category labels wrap; they do not shrink.** On a horizontal chart the
   category names compete with the bars for width, and on a phone they can take
   more of the picture than the data. Wrap them with `cwr_wrap()` as a labeller -
   `scale_y_discrete(labels = \(x) cwr_wrap(x, width = 30))` - and wrap harder for
   the phone by putting a second `scale_y_discrete()` in `phone`, which replaces
   the first rather than adding to it. Two things follow. A wrapped label is two
   lines deep, so the row has to be deeper - **pass `row_height = 0.45, phone_row_height =
   0.45` to `cwr_figure()` once labels wrap, and narrow the bars in proportion
   (`geom_col(width = 0.58)` for 0.37 → 0.45) so they stay the house thickness** - or the
   second line touches the row above; and the
   break is emitted as `<br>`, not `"\n"`, because `theme_cwr()` draws the left axis with
   ggtext. The gap between one label and the next is whatever is left of its row, so
   `theme_cwr()` sets a wrapped label's own lines solid (`lineheight = 1.0`) to leave as much
   of it as possible. Do not loosen that to buy breathing room: it spends the very space it is
   trying to make. Buy the room with `row_height`. Shortening the names themselves
   is better still where it can be done without changing their meaning, and belongs
   in the post's `02_clean_data.R`, not in the chart.
9. **Numbers**: `label_number(big.mark = ",")` on axes, `accuracy` chosen so labels
   have no more digits than the story needs. Percentages via `label_percent()`.
10. Tidyverse throughout, `|>` never `%>%`, `linewidth` not `size` for lines.
11. **Phones.** The phone render is the same ggplot drawn 4.2 in wide with the same text
    sizes, so text is twice as large relative to the chart. `cwr_figure()` already wraps
    the title and subtitle, moves right-hand axis labels outside the panel, and scales
    `geom_text`/`geom_label` sizes by 0.8. Anything else the phone version needs goes in
    the `phone = list(...)` argument, ggplot pieces added only to that version: a coarser
    scale (`scale_x_date(date_breaks = "2 years")`), a dropped legend, a wider expansion.
    Design for half the width from the start: one idea per chart, few categories, short
    labels, no annotation placed by a fixed x position near the right edge (put explanations
    in the subtitle instead), no dense small multiples or twelve-series lines. Always Read
    both PNGs in `figures/`, the `-phone` one scaled to about 320 px wide, before calling a
    chart done.

12. **Maps come from `R/maps.R`; never rebuild one from scratch.** It holds the projection
    (`cwr_map_crs`, EPSG:3161), `cwr_label_point()` (the roomiest *point* inside a shape - the
    centre of its largest inscribed circle - which the post's `02_clean_data.R` stores as
    `lon`/`lat` beside each shape), `cwr_label_spot()` (the roomiest place for a label of a given
    size), `cwr_map_nudges` (the three district labels the welcome map moves by hand),
    `cwr_map_theme()` (no axes, ticks or gridlines) and `cwr_text_on_fill()` (white or black label
    text, read off the fill's own lightness). Start from `ggmap` or `ggmapshaded` in maps.md.
12a. **Map labels sit inside their shapes, level, in the widest gap that holds them.** Place them
    with `cwr_label_spot(shape, width, height)`, passing the label's **measured** size in metres -
    not `cwr_label_point()`, which answers where a point has most room rather than where these
    words do. The two differ whenever a label is wide next to its shape: "Waterloo" over a figure
    is about as wide as Waterloo. Never tilt a label and never shrink one to fit. A label that
    overhangs its border slightly still reads fine (the welcome map's do); when one overhangs
    badly the fixes in order are **a bigger map** (tighten the frame - on the commuting maps,
    pulling the outside places in from 8 km to 5 km was what made Waterloo and Cambridge fit),
    shorter wording, then fewer lines. Borders are `cowboysilver` on an unfilled map and white on
    a shaded one. A map has no category axis, so it passes `height` and `phone_height` (7.5 and 6
    suit a map of the Region with a caption of two or three notes).
12b. **Every version of a map keeps one frame and one scale.** Tabs, years or facets of the same
    map set `coord_sf(xlim =, ylim =, expand = FALSE)` from the widest version, so switching
    between them changes the data and never moves the map, and share one `limits =` on the fill
    scale, so the same colour means the same number throughout.

## Tabs: one chart per tab

When a chart would be made of the same picture several times over - one municipality at a time, one
measure at a time - put each in a tab of a Quarto `panel-tabset` rather than in a grid of small
multiples that nothing fits into. The rules, from the welcome post's mother tongue charts and the
commuting post's flow maps:

- One chunk per tab, tab headings as `###` so they stay out of the two-level table of contents.
- The first tab's chunk does the shared work - the data, and a function that builds the chart for
  one group - and each tab then calls that function and adds its own `labs()`. Nothing is computed
  twice, and the tabs cannot drift apart.
- Keep the rows, scales and frame identical across tabs, so switching tabs changes one thing only.
- Each tab is a chart in its own right: its own `cwr_figure()` call, its own `fig-` id
  (`fig-flows-kitchener`), its own `alt`, built from the same numbers it draws.

```
::: panel-tabset
### Kitchener

```{r}
#| label: flows-kitchener
#| output: asis

# shared data and flow_map() / flow_caption() / flow_alt() go here, once
p <- flow_map("Kitchener") + labs(title = ..., subtitle = ..., caption = flow_caption("Kitchener"))
cwr_figure(p, "fig-flows-kitchener", alt = flow_alt("Kitchener"), height = 7.5, phone_height = 6)
```

### Waterloo

```{r}
#| label: flows-waterloo
#| output: asis

p <- flow_map("Waterloo") + labs(title = ..., subtitle = ..., caption = flow_caption("Waterloo"))
cwr_figure(p, "fig-flows-waterloo", alt = flow_alt("Waterloo"), height = 7.5, phone_height = 6)
```
:::
```

## Scope: the chart, and only the chart

A request for a chart is a request for the chart chunk. Do not write an introduction, a heading, a
finding, or a sentence interpreting it, and do not touch the post's other sections - the template's
placeholder comments and stock headings stay exactly as they are until Greg writes them himself.
Yours to write: the `cwr_caption()` source line, the `alt` text, and the code comments.

Not yours: the title and subtitle. Leave them as placeholders for Greg, exactly these lines, so
they are obvious and unfinished - the house rule they stand for is in style rule 1:

```r
    title = "Title: the finding, in one line",
    subtitle = "Subtitle: what is measured, for whom, and when<br>",
```

Keep the trailing `<br>` only where the chart needs room under the subtitle.

## Iteration loop for label placement

Templates leave label positions (`nudge_x`, `label_data`, legend coordinates) for you to set
after seeing the chart. Do not guess blind:

1. Write the chunk in the post. A category chart leaves its height to `cwr_figure()`; any
   other chart fixes `height` and `phone_height` first, since label positions depend on them.
2. Render just that chart with a short scratchpad script that `setwd()`s to the scratchpad:

   ```r
   here::i_am("posts/<slug>/index.qmd")
   source(here::here("R", "theme_cwr.R"))
   setwd("<scratchpad>")
   # cwr_figure() writes to figures/ during a render and to a temporary folder
   # otherwise, so that a half-finished chart cannot overwrite the post's
   # committed PNGs. Under Rscript it would take the temporary path; this line
   # makes it behave like a render, so the PNGs land in <scratchpad>/figures/
   # where they can be found and Read. Never set this in a post.
   options(knitr.in.progress = TRUE)
   # load the same data the post loads ...
   p <- <the ggplot code from the chunk>
   cwr_figure(p, "fig-<slug>", alt = "x")   # plus height = , phone_height = if y is not categories
   ```

   Run it with `Rscript`, then **Read both PNGs** in `<scratchpad>/figures/` and look at them.
3. Adjust nudges, breaks, margins, or label coordinates. Re-render. Two or three passes is normal.
4. Copy the final positions back into the post chunk. Delete the scratch script.

When restyling an old chart, keep its data pipeline and swap only the ggplot layers for
the matching template.
