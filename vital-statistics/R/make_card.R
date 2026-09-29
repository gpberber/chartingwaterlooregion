# make_card.R
# Draws the Vital Statistics page's link image (the picture a link to the page
# shows on social media and in messages), also used in the post announcing the
# page: the page's seven main sections, set in the house blues and reds, over a
# faded chart from the page itself.
#
# Run from the project root, after 02_clean_data.R, whenever the look should
# change (the chart behind the words is the page's own data, so it moves on a
# little with each refresh, but the card does not need redrawing for that):
#   Rscript vital-statistics/R/make_card.R
#
# Writes vital-statistics/images/card.png, 1200 x 630 pixels: the size the
# big link previews use (Facebook, LinkedIn, Bluesky, X).

here::i_am("vital-statistics/R/make_card.R")
library(here)
source(here("R", "theme_cwr.R"))   # the house colours, cwr_label()

# ---- The chart behind the words ------------------------------------------------
# The unemployment rate for the Region, Ontario and Canada, as the page draws
# it (blue, silver, red), faded so it reads as texture rather than as data. It
# fills the whole card: no axes, no labels, only faint horizontal gridlines,
# which is enough to say "chart" at a glance.
unemployment <- read_csv(here("vital-statistics", "data", "unemployment_rate.csv"),
                         show_col_types = FALSE) |>
  clean_names() |>
  mutate(geo = factor(geo, levels = c("Canada", "Ontario", "Region")))

chart <- ggplot(unemployment, aes(x = date, y = percent, colour = geo)) +
  geom_hline(yintercept = seq(4, 12, by = 2), colour = "grey90", linewidth = 0.5) +
  geom_line(linewidth = 1.6, alpha = 0.3) +
  scale_colour_manual(values = c(Canada = habsred, Ontario = cowboysilver, Region = dodgerblue),
                      guide = "none") +
  scale_x_date(expand = expansion(0)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# ---- The words -------------------------------------------------------------------
# The page's seven main sections (its ## headings), placed by hand (Greg,
# 2026-09-28: no word cloud), in the open space above and below the chart's
# lines so they mostly clear them. Each word's left edge is at `x` and its
# middle at `y`, both as a share of the card's width and height.
#
# The lines run in a band across the middle of the card, measured from the
# data as a share of its height: about 0.22 to 0.47 on the left, rising to
# 0.36 to 0.60 on the right. So five words sit above the band and two below
# it, nudged by eye (Greg, 2026-09-28): Population down into the valley the
# lines make on the left, Crime down towards the dip on the right, Building
# permits up, and the three top words at staggered heights, not in a row. At 9.5 mm a word is about 0.09 of the card high, and the longest,
# "Business activity", about 0.39 of it wide (systemfonts::string_width()).
# If the chart behind changes, check the band again.
#
# Colours are the house blues and reds, full and 50% tints, alternating so no
# two neighbours match, with Population in the house silver (Greg,
# 2026-09-28). cwr_label() is the house text inside a chart: a
# white halo keeps each word readable where it does cross a line or gridline.
words <- tribble(
  ~word,               ~x,   ~y,   ~colour,
  "Labour market",     0.05, 0.830, dodgerblue,
  "Housing",           0.62, 0.785, habsred,
  "Business activity", 0.18, 0.655, as.character(habsred50),
  "Crime",             0.71, 0.595, as.character(dodgerblue50),
  "Population",        0.13, 0.480, cowboysilver,
  "Building permits",  0.58, 0.260, dodgerblue,
  "Rental market",     0.10, 0.10, as.character(dodgerblue50)
)

text_layer <- ggplot(words, aes(x = x, y = y, label = word, colour = colour)) +
  cwr_label(family = "Inter", fontface = "bold", size = 9.5, hjust = 0, vjust = 0.5,
            bg.r = 0.08) +
  scale_colour_identity() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA))

# ---- Together ---------------------------------------------------------------------
# patchwork lays the transparent text layer over the whole chart
card <- chart + inset_element(text_layer, left = 0, bottom = 0, right = 1, top = 1)

dir.create(here("vital-statistics", "images"), showWarnings = FALSE)
ragg::agg_png(here("vital-statistics", "images", "card.png"),
              width = 1200, height = 630, res = 150, background = "white")
print(card)
invisible(dev.off())
message("Wrote vital-statistics/images/card.png")
