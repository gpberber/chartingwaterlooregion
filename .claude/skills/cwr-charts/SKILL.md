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
| `cwr_caption(source, credit = FALSE, notes = NULL)` | builds the caption (rule 1a); `credit = TRUE` adds the CWR byline (rule 6); every note is written out in `notes` in the chunk, stock ones included (rule 1d) |
| `cwr_ci_widest(estimate, lower, upper)` | the widest 95% interval on a chart, in points, for the `{widest}` placeholder in the stock interval note (rules 1d, 9b) |
| `cwr_wrap(x, width = 22)` | breaks a long category label over two lines for a discrete axis (rule 11) |
| `cwr_label(...)` | the house label for text inside the plot area: `geom_text()` with a halo behind the letters, so it stays readable over a gridline, a line or a point (rule 3e) |
| `cwr_line_labels(data, x, y, group, at, side, limits, bold)` | label positions that sit just above or below each line, off the gridlines (rule 3a) |
| `cwr_figure(p, "fig-id", alt)` (plus `height`, `phone_height` when y is not categories) | saves desktop and phone PNGs to `figures/` and writes the figure (rule 7) |
| `cwr_gap()` | multiplies a sideways nudge written in data units; 1 on the desktop and `phone_gap` while `cwr_figure()` draws the phone version (rule 11) |
| `R/census_ci.R` (sourced by a cleaning script, not the post) | `cwr_var_from_bounds()`, `cwr_share_se()`, `cwr_add_share_ci()`: 95% intervals for shares from long-form counts with published bounds, by Statistics Canada's own method, with E and F shares blanked (rule 9b) |
| `cwr_census_tnr()` (in `R/data_quality.R`, run by `01_get_data.R`) | each area's long-form total non-response rate, from its Census Profile page (rule 9b) |
| `cwr_map_crs`, `cwr_label_point()`, `cwr_label_spot()`, `cwr_map_nudges`, `cwr_map_theme()`, `cwr_text_on_fill()` | the house map style, from `R/maps.R` (rule 12) |
| `cwr_interactive(p, "fig-id", alt, height, phone_height)` | the same for a hover chart (ggiraph); chunk without `output: asis`; numbered in Deep dives like `cwr_figure()` |

Never redefine these in a post. If a post needs a new palette, add it to `theme_cwr.R`.

Three things `theme_cwr()` now handles that a chart used to have to ask for:

- **The title, subtitle and caption wrap.** They are drawn as `element_textbox_simple()`
  boxes sized to the image (8.3 in less the plot margins), so a long line breaks on to a
  second line instead of running off the right edge, and `cwr_figure()` makes the image
  taller to hold it. Never break a title by hand with `<br>` to make it fit; a `<br>` is
  for a break you want in a particular place (rule 1). A chart saved at another width, or
  one that widens `plot.margin`, passes that width to `theme_cwr(width = )` so the boxes
  still match what is drawn.
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
| ranking of categories | one value per category | `gglollipop` (lollipops.md; always with its x axis, rule 4a) or `gghorbar` (bars.md) |
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
| where something is, not how much | one value per district | `ggmap`, `ggmapshaded` (maps.md) |
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
1b. **A chart drawn from CMA data carries the CMA note, first.** Several Statistics
   Canada series are published for the Kitchener census metropolitan area and nothing smaller.
   The CMA is six of Waterloo Region's seven districts - Wellesley Township is the only
   exclusion and nothing outside the Region is in it - so the figures are a subset of the
   Region, not a different place, and Greg's prose may simply call it Waterloo Region. The
   qualification belongs on the chart all the same, because a chart travels without the post:
   it is note 1, in the stock wording of rule 1d, written out in the chunk.
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

1d. **Every note Claude puts on a chart is written out in the chunk, where Greg can edit or delete
   it.** His rule (2026-09-21, and the same he set for data-quality notes a few days earlier): no
   note is added by a function argument or looked up from a list behind the scenes. Each is a plain
   string in `cwr_caption(notes = c(...))` in the post's own chunk - or in the one caption
   function a set of tabs shares - so rewording one or deleting it means editing that line. Stock
   wording keeps posts consistent, but it is copied in, not referenced. A note whose number comes
   from the data writes the sentence out with a `{placeholder}` and fills it with `str_glue()`, so
   the words stay editable and the number cannot go stale:

   ```r
   str_glue(
     "95% confidence intervals are within ±{widest} percentage points",
     widest = with(plot_data, cwr_ci_widest(percent, percent_lower, percent_upper))
   )
   ```

   The stock wording, in the order the notes go (a survey's sample note always last, directly above
   the source line; each note's key goes in the title or subtitle, which is Greg's, so say so at
   hand-off):

   | Note | When | Wording |
   |---|---|---|
   | CMA geography | the chart's figures are for the Kitchener CMA (rule 1b); first | `str_glue("{cwr_region} excluding Wellesley Township")` |
   | Intervals drawn | the chart draws them (rule 9b) | "Bars show 95% confidence intervals" (or "Lines", "The band") |
   | Intervals stated | narrow enough not to draw (rule 9b) | "95% confidence intervals are within ±{widest} percentage points", as above |
   | Not reported | keyed on each row whose share was blanked as E or F (rule 9b); only if one was | "The sampling error exceeds 16.6%, so the value is too unreliable to report." |
   | No intervals published | a ranking or comparison from a table without bounds (rule 9b) | "No confidence intervals are published for these figures; shares close together may differ only by sampling error" |
   | Liaison Strategies 2025 | the globe-csi perception survey | "Estimates from an October 2025 Liaison Strategies phone survey of 800 residents per city" |

   **A note that says who or what is counted is taken from the official definition, never from
   memory or from the look of the data.** Read the table's own notes and the census dictionary's
   "Reported for" line for the variable - they are in the README's Key terms table, which is drafted
   before the charts - and write the note to match. Greg's case (2026-09-22): a commuting note said
   people who work from home were "counted in their own municipality"; the dictionary says commuting
   covers only people "who reported having a usual place of work", so home workers are not in the
   figures at all. A universe can also differ between variables of one topic (main mode of
   commuting covers people with no fixed workplace address too; commuting destination does not), so
   check the variable the chart actually shows.

   A survey used for the first time gets its own row here, in the same pattern ("Estimates from the
   Labour Force Survey, a monthly sample of about N households", the size from the survey's own
   documentation, not from memory). **The census long form has no chart note** (Greg, 2026-09-22:
   too long to repeat on every chart): it is stated once, in the README's Reliability table, which
   the post prints (rule 9b). Data-quality flags follow rule 9c, which adds no chart note at
   all for a D rating; any note Greg does ask for is written the same way.
2. **Colour has meaning.** Blue = the Region / the focus. Red = the main comparison
   (Canada) or a highlight. Grey = everyone else. Never more than five colours; never rainbow.
2a. **Waterloo Region is "Region" in chart text - always `cwr_region`.** Axis and category
   labels, legend keys, direct labels, tooltips and caption notes all say "Region", never
   "Waterloo Region" or "Waterloo" (which is also a city). Write `cwr_region` rather than the
   word, so a chart cannot drift back. A post's cleaning script does not load `theme_cwr.R`, so
   it types "Region" with a comment pointing at `cwr_region`; a source that says "WRPS",
   "Waterloo (CD)" or "Kitchener - Cambridge - Waterloo" is recoded to it there. Titles,
   subtitles and prose are Greg's and may use the full name; alt text may too, for clarity.
2c. **The seven cities and townships are "districts", never "municipalities".** In Ontario
   "municipality" is a legal status, and Waterloo Region is officially The Regional Municipality of
   Waterloo (Greg, 2026-09-22), so the word means the Region as a whole and nothing smaller. Titles,
   labels, alt text, notes, Key terms, data values ("In their own district"), README text and code
   comments all say district (or city and township, when the type matters); a place outside the
   Region is a "place". Only a quotation from a source keeps its own wording.
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
   `aes(vjust = vjust)` and `hjust = 0.5` in the `cwr_label()`. Check the phone version,
   where the label covers about twice as many x units; labels count as overlapping within
   `span * label_spacing` (1.25) so phone labels do not touch.
3b. **A line label never blots out a gridline when it can avoid it.** The label's halo breaks
   every gridline it crosses. `side` is a preference: `cwr_line_labels()` checks both sides
   and switches when the preferred side would cover a gridline and the other would not. It
   never switches onto another group's line, which counts for more than a gridline. For the
   check it needs the y scale's `limits` (pass the same vector as `scale_y_continuous()`,
   `NA` where the data decide) or explicit `breaks` if the chart sets its own. Its returned
   `side` column says where each label ended up. If a label still covers a gridline, both
   sides were blocked: move its `at` rather than accept it. Example: the globe-csi violent CSI
   chart asks for Canada above its line at 2003, where the label would break the 100 gridline,
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
3e. **Every label inside the plot area is a `cwr_label()`** (Greg, 2026-09-23). It is
   `geom_text()` with a halo of the background colour drawn around each letter (the
   `shadowtext` package), so a value beside a dot, a year above a dumbbell, a series name on a
   line chart or a place name on a borders-only map stays readable wherever it lands. It
   replaces the old idiom of `geom_label(fill = "white", linewidth = 0)`, which blotted out a
   whole rectangle rather than the letters and shoved the text off the point it marked with its
   padding; no `label.padding` to tune, and the label sits exactly where `geom_text()` would
   have put it. `bg.colour` (white) and `bg.r` (0.15, a share of the text size, so it shrinks
   with the label on the phone) are the house defaults and are not usually passed. Two
   exceptions: text **inside** a filled bar, tile or map polygon stays `geom_text()`, because
   the fill is already its background and a white halo around white text would eat it; and a
   boxed second column (`gglollipoprect`, `gghorbarrect`) keeps its grey `geom_label()` box,
   which is a design element and not a fix for a busy background. A one-off note on the panel is
   `annotate("shadowtext", ..., bg.colour = "white", bg.r = 0.15)`, which is what the
   `ggannotate` block does. `cwr_figure()` shrinks these labels for the phone exactly as it
   shrinks `geom_text()` ones. Worked case: the change labels under
   `fig-work-at-home-industry-2021` in the commuting post, which is where Greg found the package.
4. **Drop what the data makes redundant.** If bars carry value labels, remove the value
   axis text, ticks, and gridlines. Horizontal charts swap gridlines to vertical (the
   templates include this `theme()` block).
4a. **A lollipop is the exception: it always shows its x axis and never value labels**
   (Greg, 2026-09-22, "always do this for lollipop charts"). A dot at the end of a thin
   segment is read against the scale, and a number printed beside every dot turns a
   ranking into a table. So `gglollipop`, with the axis on top and vertical gridlines, is
   the template for every lollipop on the site; the old `gglollipoplabel` was removed the
   day the rule was set. A second number per row still goes in the boxed column of
   `gglollipoprect`, which keeps its axis too. Labelled bars are unaffected - rule 4 is
   about bars.
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
9a. **Long-form census data is charted as shares or rates by default; a count only rounded and with
   its interval.** Before drafting any chart from a census table, check which questionnaire the
   variable comes from. The 2021 long form went to one private household in four. Each responding
   household carries a weight - about 4, adjusted for households that did not respond and matched to
   the full census counts of age, household size and more in areas of 5,000 to 15,000 people - and a
   published long-form "count" is the sum of those weights: Statistics Canada's estimate for everyone
   in private households, not a count of respondents (98-306-X, chapters 2-4). So a count is a
   legitimate figure, but "16,515 commuters" claims a precision it does not have. Chart the share,
   rate or median by default - it is also what readers compare across places of different sizes. A
   count, where the story needs one, is rounded to what its interval supports and given with it
   ("about 16,500, give or take 500"). **If Greg asks for counts from a long-form table, say so
   before building it** - he asked for this check "so I don't make this mistake again".
   - **Never divide a long-form figure by a short-form one.** Long-form estimates are matched to
     census counts only in whole weighting areas and only for some characteristics, so a long-form
     total does not equal the short-form count of the same people; a share takes its numerator and
     denominator from the same table.
   - **Private households only.** The long form leaves out everyone in collective dwellings (nursing
     and seniors' homes, student residences, group homes), so its totals are smaller than the full
     census's. The README's "Long-form census" row says so (rule 9b).
   - Long-form: commuting, place of work, labour, education, housing costs and condition,
     immigration and citizenship, ethnocultural and religious origin, Indigenous identity, language
     of work, mobility. Short form (a full count, counts are fine): population, age, gender, marital
     status, households and families, dwelling type, and the language questions other than language
     of work. **Income is split, so check the statistic, not the topic:** since 2016 census income
     comes from tax and benefit records linked to every respondent, and **medians are published for
     100% of the population**, while averages and aggregates come only from the 25% sample (the 2021
     Income Reference Guide says so). The households-and-income post's median incomes are therefore
     a full count and need no sampling record. Tables mark it in the member names of a statistics
     dimension ("... - 100% data", "... - 25% sample data"); where a statistic is unlabelled, look it
     up in the census reference guide for the topic rather than guessing. The commuting post's last
     three charts were rebuilt from counts to shares for this reason (2026-09-18).
9b. **Sample data is disclosed, and its uncertainty measured and shown where it matters.** This
   covers every sample: the census long form, the Labour Force Survey, the Canadian Community Health
   Survey, the General Social Survey, any poll. Greg wants the reader told every time (2026-09-18).
   Sources: Statistics Canada's 2021 Census Data Quality Guidelines (98-26-0006) and Sampling and
   Weighting Technical Report (98-306-X), both in the commuting post's background folder.
   - **Where the reader is told.** For the **census long form**, in the README's Reliability table
     only - no chart note (Greg, 2026-09-22). Its **"Long-form census"** row names the tables the post
     draws from it (and so which charts) and says: "Its figures are estimates for people in private
     households, from the 2021 census long-form questionnaire sent to 25% of households", that people
     in collective dwellings are not included, and how weighting and imputation work. For **any other
     survey**, the survey's stock note from rule 1d, written out as the last string in `notes`.
   - **Every share gets an interval, the way Statistics Canada builds its own** (adopted 2026-09-21,
     method matched to Statistics Canada's 2026-09-22). Census tables often carry a `Statistics`
     dimension - "Count", "95% confidence interval lower bound, Count", "... upper bound" (98-10-0462
     does). Keep all three rows through `cwr_quality_flags()` in `02_clean_data.R`, pivot them to
     `value`, `lower` and `upper`, and use `R/census_ci.R`:
     - `cwr_var_from_bounds(value, lower, upper)` backs each count's variance out of its published
       interval, which is a "modified Wilson" interval on Student's t with 32 degrees of freedom (the
       multiplier is 2.04, not 1.96); `sqrt()` it for a standard error. A sum of categories takes the
       square root of the summed variances.
     - `cwr_share_se()` gives the share's standard error (the US Census Bureau's proportion formula,
       which allows for the part being counted inside the total; its cautious fallback is too wide
       near 100%).
     - `cwr_add_share_ci(data, count_col)` builds the share's modified Wilson interval, as Statistics
       Canada does for proportions, and writes `percent_lower`, `percent_upper`, `cv` (coefficient of
       variation) and `quality` beside `percent`.
     A survey that publishes CIs or CVs keeps them as published. Worked example:
     `posts/commuting/R/02_clean_data.R`.
   - **What an interval covers, in any wording that describes it:** sampling error and the
     variability from households that did not respond. Not: bias if those households differ from the
     ones that did, answers filled in for blank questions, people missed or counted twice,
     misreported answers, rounding. Never write "sampling error only".
   - **E and F shares are never used** (Greg, 2026-09-22), on Statistics Canada's survey scale: a CV
     of 16.6% to 33.3% is E, "use with caution"; over 33.3% is F, "too unreliable to be published".
     The census prints no such letters, but the rule is the same as for Statistics Canada's own E
     and F flags (rule 9c): `cwr_add_share_ci()` blanks the share, its interval and its count to NA
     and records the letter in `quality`. The chart keeps the row and names it "Not reported" with a note
     key (the Unicode superscript, since `geom_text()` draws plain text), in grey at the baseline like
     the agriculture post's missing figures; the stock "Not reported" note (rule 1d) is written into
     `notes`, only when a row was blanked; the alt text says "not reported, sampling error too large".
     Never a zero, never a bridged gap. A share of zero has no interval and is charted as published.
   - **Then each chart shows the intervals only where they change the reading**, decided chart by
     chart:
     - **draw them** when values sit close enough that the intervals could reverse an order or a
       comparison a reader will make (for one estimate per category, the `ggdoterror` template: a
       blue dot on a blue interval bar fading to its ends; otherwise whiskers with
       `geom_linerange()` on a bar, a `geom_ribbon()` around a line, in `cowboysilver`; and a note
       saying "Bars show 95% confidence intervals");
     - otherwise **state the widest in a note**: the stock sentence written out, with its number
       measured from the data by `cwr_ci_widest(percent, percent_lower, percent_upper)` through
       `str_glue()` (rule 1d), never typed. Region-wide shares, whose intervals are a
       fraction of a point, get this and nothing more: drawing them implies a doubt that is not there.
   - **A table with no published bounds gets no interval.** Commuting flows (98-10-0459) are one.
     Nothing in `R/census_ci.R` applies, and none is invented or borrowed from another table. A
     ranking or comparison drawn from it carries the stock "No confidence intervals are published"
     note (rule 1d), written out. At hand-off, say which ranks are close enough to
     be in doubt and which shares rest on counts under about 50, which random rounding to a multiple
     of 5 makes rough whatever the interval.
   - **Check the response rates** for every area the post charts. `R/01_get_data.R` fetches the
     long-form total non-response rate for each area with `cwr_census_tnr()` (`R/data_quality.R`; the
     rates are only on each area's Census Profile page) and the topic's "Long-form data quality
     indicators" table (98-10-0572 for commuting; 98-10-0569 labour, 98-10-0566 mobility and so on),
     which gives each question's non-response and imputation rates. `02_clean_data.R` writes both to
     `data/census_quality.csv` and **stops if an area's total non-response rate is 50% or more**:
     Statistics Canada says to use such data "with caution", which this site treats as E. The README's
     **"Response rates"** Reliability row gives the ranges. Tell Greg if any rate is high.
   - **Report at hand-off** every chart's decision - drawn, stated, shares not reported, or no
     intervals published - so Greg can overrule it.
   - **The sample is recorded in five places, none of them a chart note for the census long form.**
     All are set up in the post template: the **Sample** column of the README's Data sources table
     (printed in the post's Data sources section); the README's **Sampling** note (how the intervals
     were worked out and what each chart does with them); the **Reliability table's "Long-form
     census", "Sample estimates" and "Response rates" rows**, the post's methodology note, which the
     post prints (stock wording in the template README, adjusted to the post's tables; their "Left
     out" column stays empty); the `sample` column of `data/tables.csv` (printed in the download
     bundle's README); and `data/dictionary.csv`, which describes `percent_lower`, `percent_upper`,
     `cv` and `quality`. Fill in the README and the table when adding a sampled source; the Data
     sources prose above the table is Greg's.
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
    `geom_text`/`cwr_label` sizes by 0.8. Anything else the phone version needs goes in
    the `phone = list(...)` argument, ggplot pieces added only to that version: a coarser
    scale (`scale_x_date(date_breaks = "2 years")`), a dropped legend, a wider expansion.
    Design for half the width from the start: one idea per chart, few categories, short
    labels, no annotation placed by a fixed x position near the right edge (put explanations
    in the subtitle instead), no dense small multiples or twelve-series lines. Always Read
    both PNGs in `figures/`, the `-phone` one scaled to about 320 px wide, before calling a
    chart done.
11a. **A gap written in data units is half as wide on a phone.** The same x scale is drawn
    across half the width, so a label nudged clear of its point on a desktop can touch it
    there. Write the nudge as `nudge * cwr_gap()` inside the aes and pass `phone_gap` to
    `cwr_figure()` (2 keeps the gap the same distance on the page). A vertical nudge on a
    category axis needs none of this - a row is about as deep on both versions - so lifting a
    label instead is the other fix; Greg's case (2026-09-22) was the year labels on the
    commuting post's work-at-home chart, where lifting them read as too high.

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

When a chart would be made of the same picture several times over - one district at a time, one
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
"Where commuters from Kitchener work", "How commuters get to work, by district", "The ten
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
