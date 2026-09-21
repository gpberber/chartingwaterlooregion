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
| `cwr_short_name()`, `cwr_short_names` | the house short form of a long municipal name, "N. Dumfries", for all chart text (rule 2b) |
| `cwr_stack_keys(data, x, y, fill, labels, colours)` | names a horizontal stacked bar's segments on the bars instead of a legend (rule 3d) |
| `comp_colours`, `local_colours` | named palettes for recurring comparisons, keyed by `cwr_region` for the Region |
| `theme_cwr()` (already `theme_set`) | the Tufte-inspired theme |
| `base_size` (15), `label_size` (4) | text sizes used inside geoms and annotations |
| `cwr_caption(source, credit = FALSE, notes = NULL, cma = FALSE, sample = NULL)` | builds the caption (rule 1a); `credit = TRUE` adds the CWR byline (rule 6); `cma = TRUE` adds the CMA note; `sample = "census_2021"` adds the stock sampling note (rule 9b) |
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
| sample estimates and their margins of error | estimate, lower, upper per category | `ggdoterror` (comparisons.md) |
| two totals, and which is bigger | two counts | `ggnumcomp` (comparisons.md) |
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

1. **Title says the finding, subtitle says the units and scope.** A subtitle can end with `<br>`
   when the plot needs breathing room under it, added by hand where needed - never by default, and
   not in the placeholder. A legend that needs the room is moved to its own row instead
   (`legend.position = "top"`), since theme_cwr() floats it above the panel reserving no height. There are no axis titles in this theme, so a
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
2b. **North Dumfries is "N. Dumfries" in chart text - always through `cwr_short_name()`.** Every
   chart, every post (Greg, 2026-09-18): axis labels (`scale_y_discrete(labels = cwr_short_name)`,
   composed with any labeller the chart already has), direct labels, map labels (measure the label
   with the short name too, so `cwr_label_spot()` places what is drawn), legend keys, tooltips and
   caption notes - the same reach as `cwr_region`. Data files, alt text, titles, subtitles and prose
   keep "North Dumfries", so a screen reader and a downloaded table still say it in full. Never type
   the short form: a new long name goes into `cwr_short_names` in `R/theme_cwr.R` and every chart
   picks it up. Joins and lookups (such as map nudges) stay keyed by the full name.
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
3d. **A horizontal stacked bar names its segments on the bars, never in a legend.** The New York
   Times way, adopted for the commuting post's first chart (Greg, 2026-09-18): the first segment's
   name, in capitals and its segment's colour, above the top bar starting where the bar starts; the
   last segment's above the top bar ending where the bar ends (hung from the end, not from the
   segment's start as the Times does, because a short segment is narrower than its name on a phone);
   any segment in between named under the bottom bar, centred on its own segment there with a short
   tick, in grey30 since a pale segment colour is too faint as text. `cwr_stack_keys()` places all of
   it from the data and draws the baseline (stopping at the top bar's top edge so it cannot run up
   beside the first name). The chart stacks with `position_stack(reverse = TRUE)`, turns the legend
   off, and reserves a row above and below with `scale_y_discrete(expand = expansion(add = c(1.1,
   0.9)))`. Worked case: `fig-commuting` in the commuting post.
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
   more of the picture than the data. **Wrapping is automatic in `cwr_figure()`, on
   both versions (Greg, 2026-09-18).** A category name wraps between words once it is
   longer than `cwr_label_width` (30 characters) on the desktop or `cwr_phone_label_width`
   (15, the length of "Crime in Canada") on the phone. That version's rows then deepen to
   `cwr_wrapped_row_height` / `cwr_phone_wrapped_row_height` (0.45 in) for a two-line name,
   plus `cwr_wrapped_line_height` (0.17 in) for each line past two, and its bars
   (`geom_col` width) and tiles (`geom_tile` height) are thinned by the same factor so
   they keep the house thickness. It wraps whatever the chart's own y scale prints
   (labeller, named vector, `**Region**` bold line by line), so write nothing for it: no
   `cwr_wrap()` labeller, no wrapping `scale_y_discrete()` in `phone`, no `row_height`,
   no narrowed `geom_col(width = )`. The width rises to the longest single word when that
   is longer - a word never splits, so wrapping below it narrows nothing
   ("Blandford-Blenheim" keeps the commuting feeders chart on one line). A chart that sets
   its own `row_height` / `phone_row_height` keeps it, and `label_width = Inf` /
   `phone_label_width = Inf` turns the wrap off for that version. The break is `<br>`,
   not `"\n"`, because `theme_cwr()` draws the left axis with ggtext. The gap between one
   label and the next is whatever is left of its row, so `theme_cwr()` sets a wrapped
   label's own lines solid (`lineheight = 1.0`); do not loosen that to buy breathing room,
   since it spends the very space it is trying to make. Shortening the names themselves
   is better still where it can be done without changing their meaning, and belongs in
   the post's `02_clean_data.R`, not in the chart: a name wrapped to four lines on a phone
   deepens every row of that chart.
9. **Numbers**: `label_number(big.mark = ",")` on axes, `accuracy` chosen so labels
   have no more digits than the story needs. Percentages via `label_percent()`.
9a. **Long-form census data is charted as shares or rates, never as counts.** Before drafting any
   chart from a census table, check which questionnaire the variable comes from. The 2021 long form
   went to one household in four, so its counts are estimates scaled up from a 25% sample, not
   headcounts, and a bar labelled "16,515 commuters" claims a precision the data does not have.
   Chart the share, rate or median instead (a share of the same sample is what it estimates well),
   and keep counts out of labels, axes and alt text; they may still be summed in code as a
   denominator. **If Greg asks for counts from a long-form table, say so before building it** - he
   asked for this check "so I don't make this mistake again". Long-form: commuting, place of work,
   labour, education, housing costs and condition, immigration and citizenship, ethnocultural and
   religious origin, Indigenous identity, language of work, mobility. Short form (a full count,
   counts are fine): population, age, gender, marital status, households and families, dwelling
   type, and the language questions other than language of work. **Income is split, so check the
   statistic, not the topic:** since 2016 census income comes from tax and benefit records linked to
   every respondent, and **medians are published for 100% of the population**, while averages and
   aggregates come only from the 25% sample (the 2021 Income Reference Guide says so). The
   households-and-income post's median incomes are therefore a full count and need no sampling
   note - a note put on them in error was taken off again the same day. Tables mark it in the member names of a statistics
   dimension ("... - 100% data", "... - 25% sample data"); where a statistic is unlabelled, look it up
   in the census reference guide for the topic rather than guessing. The commuting post's last three
   charts were rebuilt from counts to shares for this reason (2026-09-18).
9b. **A chart drawn from sample data says so in a note, and reports the uncertainty where it can.**
   This covers every sample, not only the census long form: the Labour Force Survey, the Canadian
   Community Health Survey, the General Social Survey, any poll. Greg wants the reader told every
   time (2026-09-18).
   - **The note is always there, and never typed.** Pass `cwr_caption(sample = "census_2021")`,
     which adds the stock note from `cwr_sample_notes` in `R/theme_cwr.R` ("Estimates from the 2021
     census long-form questionnaire, a 25% sample of households") as the last note, directly above
     the source line; like the CMA note it needs its key in the subtitle, which is Greg's, so say so
     at hand-off. A sample with no entry yet - a survey used for the first time - gets one added to
     `cwr_sample_notes` ("Estimates from the Labour Force Survey, a monthly sample of about N
     households", the size from the survey's own documentation, not from memory), so the wording is
     identical on every chart that uses it.
   - **Confidence intervals, when the source publishes them.** Census tables often carry a
     `Statistics` dimension - "Count", "95% confidence interval lower bound, Count", "... upper bound"
     (98-10-0462 does) - and surveys publish CIs or coefficients of variation. Do not filter them away
     in `02_clean_data.R`: keep `lower` and `upper` beside the estimate. Then, in order of preference:
     **draw them** when a reader will compare values close enough that the intervals change what the
     chart says (for one estimate per category, the `ggdoterror` template: a blue dot on a blue
     interval bar fading to its ends; otherwise whiskers with `geom_linerange()` on a bar, a `geom_ribbon()` around a
     line, in `cowboysilver`; and a note saying "Bars show 95% confidence intervals"); otherwise **state them in the note** ("95% confidence intervals are
     within about ±2 percentage points" - a range across the bars, measured, not guessed).
   - **A share worked out here has no published interval.** When the chart divides one sampled count
     by another, the source's count intervals do not give the share's. Do not invent one or improvise
     a formula: the note says the figures are sample estimates, and whether to model an interval is a
     question for Greg.
   - **Flag it at hand-off** when a source publishes intervals that the chart does not draw, so Greg
     can decide whether they belong on the chart.
   - **The chart is one of five places the sample is recorded.** The others, all set up in the post
     template: the **Sample** column of the README's Data sources table (printed in the post's Data
     sources section), the README's **Sampling** note (what the intervals are and whether the charts
     use them), the `sample` column of `data/tables.csv` (printed in the download bundle's README),
     and `data/dictionary.csv`, which describes any kept confidence bound as one. Fill in the README
     and the table when adding a sampled source; the Data sources prose above the table is Greg's.
9c. **Data-quality flags reach the chart only as a decision Greg has made.** Greg's principle is
   reliable data, and he asked to be told about every quality flag that applies to data he plans
   to use (2026-09-18). `cwr_quality_flags()` (`R/data_quality.R`) finds them - Statistics Canada's
   cell symbols and the table footnotes about quality - on the rows the cleaning script keeps and
   writes them to `data/quality_flags.csv`. Before drafting a chart from a source, read that file
   and **tell Greg every flag that reaches the chart**: the symbol, what it means, which bars or
   points, and the options. Do not settle it silently in either direction.
   - **unusable** (F too unreliable, x suppressed, `..` not available, **and E use with caution**):
     never charted. F, x and `..` have no figure; E has one, but **Greg never uses a figure flagged
     E** (2026-09-18) - not with a note, not in a lighter colour, not in a total or a share.
     `cwr_quality_flags()` blanks all of them to NA and returns the data, so the cleaning script
     must carry on with what it returns (`kept <- kept |> cwr_quality_flags(...)`). Never draw a
     blanked figure as zero, never let a line run through the gap as if a value were there, never
     fill it in or add it back. Leave the gap or the missing bar and say why in a note ("2019 not
     shown: Statistics Canada rates it too unreliable" or "... flags it for use with caution"). If
     a blanked figure leaves a chart unable to make its point, say so at hand-off rather than
     working around it.
   - **caution** (a symbol this site's legend does not know, or a publisher's own warning): the
     figure exists but comes with a caveat nobody here has weighed yet. Look the symbol up in the
     table's own legend and tell Greg what it means before charting it.
   - **note** (A to D grades, p preliminary, r revised, `...`, `0s`, quality footnotes): mention at
     hand-off; preliminary figures that the chart's story depends on also get a note. **Statistics
     Canada's quality ratings, A excellent to D acceptable, all sit here.** D is its lowest
     published rating, not a caution - "use with caution" is E, which this site never uses at all.
     A D figure is charted as it is, and **its row in the README's Reliability table is the whole
     disclosure: no note on the chart** (Greg's rule, 2026-09-20 - the table the post prints is
     enough, and a note crowds the chart for something the reader can look up). Never add one
     unasked; build one only if Greg asks for it on a particular chart. Still say at hand-off which
     values carry the rating, especially when the finding rests on one.
   - Every issue also gets a row in the README's `## Reliability` table, which the post prints under
     "Data sources and reliability" (`cwr_reliability_table()`); closely related flags share a row.
     One Greg decides not to disclose keeps its row, with his reason in the second column, so the
     record and the chart agree.
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
multiples that nothing fits into. The rules, from the language post's mother tongue charts and the
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

**The first chart in a post goes in the template's `first` chunk** (under "First finding as a
plain-language heading"), not in a new chunk beside it (Greg, 2026-09-18, after it happened in
three posts running). Replace everything inside that chunk - the placeholder `p <- ggplot()`, its
`cwr_figure(p, "fig-first", ...)` and the template's instructions comment - with the chart, and
rename the label and id after the chart (`label: commuting`, `"fig-commuting"`). If the first
chart is a set of tabs, the `panel-tabset` takes that chunk's place, its first tab's chunk doing the
shared work. Every later chart is a new chunk after it, still under the first heading unless Greg
says where. Delete the stale `figures/fig-first.png` and `fig-first-phone.png` if a render left
them. Once no `first` chunk is left, a new chart simply goes after the last one.

Yours to write: the `cwr_caption()` source line, the `alt` text, the code comments, and a
**working title**.

**The working title is brief and purely descriptive: it names the data shown and nothing else.**
Every chart Claude drafts gets one in place of the template's "Title: the finding, in one line",
so Greg can tell the charts apart while he works (his request, 2026-09-18, after a post of twelve
charts all titled the same placeholder). Say what is plotted, for which places, split which way -
"Where commuters from Kitchener work", "How commuters get to work, by municipality", "The ten
places sending the most commuters into the Region". Never a finding, a comparison, an adjective
that judges ("most", "only", "rising" are fine when they describe the selection, not the result),
or a number from the data. The finding title that replaces it is Greg's, per style rule 1. Tabs
built by one function get one title each, naming the tab's own group.

Not yours: the subtitle. Leave it as the placeholder, exactly this line, so it is obvious and
unfinished:

```r
    title = "Where commuters from Kitchener work",                   # working title, descriptive only
    subtitle = "Subtitle: what is measured, for whom, and when",
```

The placeholder never ends with `<br>`: Greg adds one by hand where a chart needs room under the
subtitle (2026-09-18). A chart with a legend gets that room by giving the legend its own row
instead, with no gap above it and its keys under the title's left edge:

```r
  theme(
    legend.position = "top",
    legend.justification.top = "left",
    legend.location = "plot",
    legend.margin = margin(0, 0, 0, 0)
  )
```

The commuting post's first chart is the worked case; on the phone it adds only
`guides(fill = guide_legend(ncol = 1))` and `theme(legend.direction = "vertical")`. When an axis is
rescaled (style rule 1c), say so in the hand-off line, since the subtitle is where it will go.

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
