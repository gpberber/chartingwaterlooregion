# make_site_images.R
# Generates the raster images the site needs: the social-sharing card and the
# listing placeholder thumbnail. Re-run if the palette changes.
#   Rscript _dev/make_site_images.R

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

# Social card (1200 x 630) with the site name
card <- glyph +
  theme(plot.margin = margin(60, 700, 60, 80)) +
  labs(title = NULL)

card <- patchwork::wrap_elements(card) +
  patchwork::plot_annotation(
    title = "Charting Waterloo Region",
    subtitle = "Charts and plain-language analysis about life in Waterloo Region, built from open data.",
    theme = theme(
      plot.title = element_markdown(size = 34, face = "bold", colour = dodgerblue, hjust = 0),
      plot.subtitle = element_markdown(size = 16, colour = "grey30", hjust = 0),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(40, 40, 40, 40)
    )
  )

ggsave(
  here("images", "social-card.png"),
  card, width = 12, height = 6.3, dpi = 100, bg = "white", device = ragg::agg_png
)
cat("images written\n")
