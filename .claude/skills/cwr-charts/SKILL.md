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
| `comp_colours`, `local_colours` | named palettes for recurring comparisons |
| `theme_cwr()` (already `theme_set`) | the Tufte-inspired theme |
| `base_size` (15), `label_size` (4) | text sizes used inside geoms and annotations |
| `cwr_caption("Source text")` | builds the standard caption |
| `cwr_figure(p, "fig-id", alt, height, phone_height)` | saves desktop and phone PNGs to `figures/` and writes the figure (rule 7) |

Never redefine these in a post. If a post needs a new palette, add it to `theme_cwr.R`.

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
| one chart per group | any of the above | `ggmultiples`, `ggfacet` (multiples.md) |
| bar thickness carries meaning | value + weight | `ggshadedbars` (bars.md) |

Fragments for scales, legends, annotations, text and labels are in `building-blocks.md`.
Finished, real charts from the crime post are in `worked-examples.md`; match their look.

Read only the reference file you need; each is self-contained.

## Style rules (see references/style-rules.md for the reasoning)

1. **Title says the finding, subtitle says the units and scope.** Subtitle ends with `<br>`
   when the plot needs breathing room under it. Caption is always `cwr_caption("...")`.
2. **Colour has meaning.** Blue = Waterloo Region / the focus. Red = the main comparison
   (Canada) or a highlight. Grey = everyone else. Never more than five colours; never rainbow.
3. **Direct labels beat legends.** Label line ends, bar ends, or points; then
   `guide = "none"`. Use a legend only when labels would collide.
4. **Drop what the data makes redundant.** If bars carry value labels, remove the value
   axis text, ticks, and gridlines. Horizontal charts swap gridlines to vertical (the
   templates include this `theme()` block).
5. **Bold the focus row** with the `y_label` trick (`**Waterloo Region**` via `element_markdown`).
6. **Value axis on the right** for vertical charts, labels sitting above gridlines
   (the `axis.text.y.right` block in the templates). Ranked horizontal charts put the
   x axis on top.
7. **One chart per chunk, through `cwr_figure()`.** Build the plot as `p`, then call
   `cwr_figure(p, "fig-<slug>", alt = , height = , phone_height = )` in a chunk
   with `#| output: asis` (chunk label without the `fig-` prefix; the id passed to the
   function carries it). It saves two PNGs to the post's `figures/` folder: 8.3 in wide for
   desktops (exactly the paragraph width) and 4.2 in wide for phones, and emits a `<picture>`
   so the browser shows the right one. Width is fixed; vary `height` (4 for a simple bar
   chart, 5 for a line chart, more for ranked bars or facets) and `phone_height` (a little
   squarer). Never use knitr's `fig-width`/`fig-cap` chunk options for a chart.
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
8. **Numbers**: `label_number(big.mark = ",")` on axes, `accuracy` chosen so labels
   have no more digits than the story needs. Percentages via `label_percent()`.
9. Tidyverse throughout, `|>` never `%>%`, `linewidth` not `size` for lines.
10. **Phones.** The phone render is the same ggplot drawn 4.2 in wide with the same text
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

## Iteration loop for label placement

Templates leave label positions (`nudge_x`, `label_data`, legend coordinates) for you to set
after seeing the chart. Do not guess blind:

1. Write the chunk in the post with `height` and `phone_height` fixed.
2. Render just that chart with a short scratchpad script that `setwd()`s to the scratchpad:

   ```r
   here::i_am("posts/<slug>/index.qmd")
   source(here::here("R", "theme_cwr.R"))
   setwd("<scratchpad>")
   # load the same data the post loads ...
   p <- <the ggplot code from the chunk>
   cwr_figure(p, "fig-<slug>", alt = "x", height = <h>, phone_height = <ph>)
   ```

   Run it with `Rscript`, then **Read both PNGs** in `<scratchpad>/figures/` and look at them.
3. Adjust nudges, breaks, margins, or label coordinates. Re-render. Two or three passes is normal.
4. Copy the final positions back into the post chunk. Delete the scratch script.

When restyling an old chart, keep its data pipeline and swap only the ggplot layers for
the matching template.
