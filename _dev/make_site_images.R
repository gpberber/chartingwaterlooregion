# make_site_images.R
# Generates the listing placeholder thumbnail: the picture the home page shows
# beside a post that has no images/thumbnail.png of its own.
#   Rscript _dev/make_site_images.R
#
# This script used to write images/social-card.png as well, from the same
# three-bar glyph. The card is now drawn from the map mark by
# _dev/logo/make_logo.R, alongside the logo and the icons, so that the name, the
# strapline and the mark can only ever come from one place. Do not put a card
# back in here: two scripts writing the same file means whichever ran last wins.

suppressPackageStartupMessages(source(here::here("R", "theme_cwr.R")))

bars <- tibble(
  x = 1:3,
  y = c(2, 3.4, 5),
  fill = c(cowboysilver50, habsred, dodgerblue)
)

glyph <- ggplot(bars, aes(x = x, y = y, fill = fill)) +
  geom_col(width = 0.7) +
  scale_fill_identity() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Placeholder thumbnail for posts without an image (4:3)
ggsave(
  here("images", "thumbnail-placeholder.png"),
  glyph + theme(plot.margin = margin(40, 60, 40, 60)),
  width = 4, height = 3, dpi = 150, bg = "white", device = ragg::agg_png
)
cat("images written\n")
