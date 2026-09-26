# packages.R
# ---------------------------------------------------------------------------
# Every R package used on the site, in one place, so a reader can install them
# all with two lines:
#
#   source("R/packages.R")
#   install_missing()
#
# When a post adds a new library() call, add the package here too (the
# /review-post skill checks this). Exact versions used for each post are
# printed in that post's Reproducibility section.
#
# A package no post or shared script uses any more comes out again, so a reader
# is never asked to install something for nothing: readxl, openxlsx, pdftools,
# changepoint, segmented and prophet went that way on 2026-09-26, with the
# phone-queues post they belonged to. Tools only this repository's author runs
# are not listed either - rmapshaper, used by _dev/logo/make_logo.R to build the
# logo, is the only one.
# ---------------------------------------------------------------------------

cwr_packages <- c(
  # House stack, loaded by R/theme_cwr.R
  "tidyverse", "scales", "ggtext", "shadowtext", "patchwork", "gt", "gtExtras", "here",
  "janitor", "conflicted", "prismatic", "systemfonts", "ragg", "png",
  "ggiraph", "gdtools", "htmltools",   # interactive charts, cwr_interactive()

  # Rendering and publishing
  "quarto", "rmarkdown", "knitr", "sessioninfo", "gh", "httr2",
  "zip",   # R/data_bundle.R builds each post's download zip with it
  "xml2", "jsonlite", "yaml", "magick",   # R/seo_post_render.R, run after every render

  # Data sources and file formats
  "cansim", "arrow", "writexl",
  "cmhc",   # CMHC's Housing Market Information Portal (Vital Statistics page)

  # Spatial and maps
  "sf", "units", "farver",   # farver: cwr_text_on_fill() in R/maps.R

  # Time series and text
  "broom", "slider", "tidytext",

  # Shiny posts
  "shiny", "rsconnect",

  # Used by the crime dataset: 00_setup.R and the memory checks in 02_clean_data.R
  "pacman", "lobstr"
)

# Install any package in the list that is not already installed.
install_missing <- function(packages = cwr_packages) {
  installed <- rownames(installed.packages())
  to_install <- setdiff(packages, installed)
  if (length(to_install) == 0) {
    message("All ", length(packages), " packages are already installed.")
    return(invisible(NULL))
  }
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
  invisible(to_install)
}
