# census_ci.R
# Approximate 95% confidence intervals for shares worked out from census
# long-form counts. Every long-form post uses these (cwr-charts rule 9b), in its
# R/02_clean_data.R:
#   source(here("R", "census_ci.R"))
# Cleaning scripts do not load the chart theme, so this file stands on its own.
#
# Why they are needed: the long form went to one household in four, so every
# count is an estimate. Statistics Canada publishes a 95% confidence interval
# for each count in many census tables (a "Statistics" dimension with "Count",
# "95% confidence interval lower bound, Count" and "... upper bound"), but none
# for a share worked out from two of them. The share's interval is approximated
# in two steps.
#
# 1. A published 95% interval is the estimate plus or minus 1.96 standard
#    errors, so the standard error is its width divided by 2 x 1.96. This
#    relies on the bounds being close to symmetric about the count, which they
#    are except for very small counts. (cwr_se_from_bounds())
# 2. The share p = x / y. When the part x is counted inside the total y, the
#    two errors move together and partly cancel. The US Census Bureau's formula
#    for a proportion (American Community Survey methodology) allows for that
#    with a minus sign:
#        se(p) = sqrt(se_x^2 - p^2 * se_y^2) / y
#    When the term under the root comes out negative, which a noisy total can
#    cause, the ratio version with a plus sign is used instead. It gives a
#    wider, so cautious, interval - too wide for a share near 100%, where it
#    is most likely to be needed. (cwr_share_se())
#
# What the interval does not cover: people who did not answer, answers
# Statistics Canada imputed, and the random rounding of every count to a
# multiple of 5. It is weakest for very small and very large shares and for
# small places, where the true interval is lopsided. A share of zero has no
# interval at all. Exact intervals would need a custom tabulation from
# Statistics Canada or the microdata in a Research Data Centre; for a blog this
# approximation is standard practice.
#
# A table with no published bounds (commuting flows, 98-10-0459, is one) gets no
# interval: nothing here can be used on it, and none is invented. Its charts
# carry the stock "no intervals" note instead (cwr_ci_notes in R/theme_cwr.R).
#
# Worked example: posts/commuting/R/02_clean_data.R.

library(dplyr)

# Above this coefficient of variation - the standard error as a share of the
# estimate - an estimate is too unreliable to use on its own. A third is the
# cut-off Statistics Canada's own quality ratings are built around. Such a
# share is kept and marked on the chart, not dropped, unless Greg decides
# otherwise: a small number can be realistic (township transit use) even when
# the sample cannot pin it down.
cwr_cv_limit <- 0.33

# A standard error from a published 95% interval
cwr_se_from_bounds <- function(lower, upper) (upper - lower) / (2 * 1.96)

# The standard error of the share x / y (a proportion, not a percentage), from
# the standard errors of x and y. See step 2 above. The root is taken after
# choosing the formula so a negative term never reaches sqrt().
cwr_share_se <- function(x, se_x, y, se_y) {
  p <- x / y
  radicand <- se_x^2 - p^2 * se_y^2
  sqrt(if_else(radicand >= 0, radicand, se_x^2 + p^2 * se_y^2)) / y
}

# Adds the interval and its reliability to a table that has `percent` (0 to
# 100) and the share's standard error `se_p` (as a proportion, from
# cwr_share_se()), and drops `se_p`:
#   percent_lower, percent_upper  the 95% interval, cut off at 0 and 100
#   cv                            coefficient of variation; NA for a zero share
#   unreliable                    TRUE when cv is over cwr_cv_limit
cwr_add_share_ci <- function(data) {
  data |>
    mutate(
      percent_lower = pmax(percent - 1.96 * se_p * 100, 0),
      percent_upper = pmin(percent + 1.96 * se_p * 100, 100),
      cv = if_else(percent > 0, se_p * 100 / percent, NA_real_),
      unreliable = !is.na(cv) & cv > cwr_cv_limit
    ) |>
    select(-se_p)
}
