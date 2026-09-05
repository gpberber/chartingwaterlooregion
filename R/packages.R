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
# ---------------------------------------------------------------------------

cwr_packages <- c(
  # House stack, loaded by R/theme_cwr.R
  "tidyverse", "scales", "ggtext", "patchwork", "gt", "gtExtras", "here",
  "janitor", "conflicted", "prismatic", "systemfonts", "ragg",

  # Rendering and publishing
  "quarto", "rmarkdown", "knitr", "sessioninfo", "gh", "httr2",

  # Data sources and file formats
  "cansim", "arrow", "readxl", "openxlsx", "pdftools", "rvest",

  # Spatial and maps
  "sf", "units", "geosphere", "leaflet", "tidygeocoder",

  # Modelling and time series (phone queues post)
  "changepoint", "segmented", "prophet", "broom", "slider", "tidytext",

  # Shiny posts
  "shiny", "rsconnect"
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
