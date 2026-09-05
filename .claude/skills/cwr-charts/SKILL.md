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
| `base_size` (13), `label_size` (3.2) | text sizes used inside geoms and annotations |
| `cwr_caption("Source text")` | builds the standard caption |

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
7. **One chart per chunk**, chunk label `fig-<slug>`, with `fig-cap`, `fig-alt`, and
   explicit `fig-height`/`fig-width` set before tuning any label position.
8. **Numbers**: `label_number(big.mark = ",")` on axes, `accuracy` chosen so labels
   have no more digits than the story needs. Percentages via `label_percent()`.
9. Tidyverse throughout, `|>` never `%>%`, `linewidth` not `size` for lines.

## Iteration loop for label placement

Templates leave label positions (`nudge_x`, `label_data`, legend coordinates) for you to set
after seeing the chart. Do not guess blind:

1. Write the chunk in the post with `fig-height` and `fig-width` fixed.
2. Render just that chart to a PNG in the scratchpad with a short script:

   ```r
   source(here::here("R", "theme_cwr.R"))
   # load the same data the post loads ...
   p <- <the ggplot code from the chunk>
   ggsave("<scratchpad>/check.png", p, width = <fig-width>, height = <fig-height>,
          dpi = 150, bg = "white", device = ragg::agg_png)
   ```

   Run it with `Rscript`, then **Read the PNG** and look at it.
3. Adjust nudges, breaks, margins, or label coordinates. Re-render. Two or three passes is normal.
4. Copy the final positions back into the post chunk. Delete the scratch script.

When restyling an old chart, keep its data pipeline and swap only the ggplot layers for
the matching template.
