# helpers.R
# Functions and definitions shared by this dataset's pipeline and by the posts
# that use it: lookup vectors, recoding maps, small helper functions.
# Sourced by 02_clean_data.R and by load.R, so posts get them for free.

library(tidyverse)

# Example: a named vector for recoding raw category labels to display labels
# category_labels <- c("raw_a" = "Label A", "raw_b" = "Label B")
