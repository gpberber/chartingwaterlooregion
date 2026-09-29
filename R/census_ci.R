# census_ci.R
# Approximate 95% confidence intervals for shares worked out from census
# long-form counts, built the way Statistics Canada builds its own. Every
# long-form post uses these (cwr-charts rule 9b), in its R/02_clean_data.R:
#   source(here("R", "census_ci.R"))
# Cleaning scripts do not load the chart theme, so this file stands on its own.
#
# What the published figures are: the long form went to one private household
# in four. Each responding household carries a weight - about 4 to start, then
# adjusted for households that sent nothing back and matched to the full census
# counts of age, gender, household size and so on in areas of 5,000 to 15,000
# people - and a published long-form "count" is the sum of those weights. So it
# is an estimate for everyone in private households, not a count of the people
# who answered. People in collective dwellings (nursing and seniors' homes,
# student residences, group homes) are not in it at all. (2021 Census Sampling
# and Weighting Technical Report, 98-306-X, chapters 2-4.)
#
# Why intervals are needed: Statistics Canada publishes a 95% confidence
# interval for each count in many census tables (a "Statistics" dimension with
# "Count", "95% confidence interval lower bound, Count" and "... upper bound"),
# but none for a share worked out from two of them. The share's interval is
# built in three steps, each matching Statistics Canada's own method (98-306-X
# chapter 7; 2021 Census Data Quality Guidelines, 98-26-0006, section 6):
#
# 1. The count's variance, backed out of its published interval
#    (cwr_var_from_bounds()). Statistics Canada does not use the textbook
#    "estimate plus or minus 1.96 standard errors". Its intervals for counts
#    are "modified Wilson" intervals, which lean slightly away from zero, and
#    they use Student's t with 32 degrees of freedom - one per replicate sample
#    used to estimate the variance - so the multiplier is 2.04, not 1.96. The
#    interval's half-width h satisfies
#        h^2 = t^2 V + (t^4 / 4) (V / Y)^2
#    for a count Y with variance V, which is solved for V here. A zero count
#    has no interval ("..." bounds); every replicate estimate of it is zero
#    too, so its variance is taken as 0.
# 2. The share's standard error (cwr_share_se()). The share p = x / y, where
#    the part x is counted inside the total y, so the two errors move together
#    and partly cancel. The US Census Bureau's formula for a proportion
#    (American Community Survey methodology) allows for that with a minus sign:
#        se(p) = sqrt(se_x^2 - p^2 * se_y^2) / y
#    When the term under the root comes out negative, which a noisy total can
#    cause, the ratio version with a plus sign is used instead. It gives a
#    wider, so cautious, interval - too wide for a share near 100%, where it
#    is most likely to be needed.
# 3. The share's interval (cwr_add_share_ci()), by Statistics Canada's
#    modified Wilson method for proportions, again with t on 32 degrees of
#    freedom. It stays between 0 and 100% and is lopsided near either end,
#    where a plain plus-or-minus interval would run past zero and cover too
#    little on the far side. It needs the "effective sample size", the number
#    of simple random draws that would give the same variance: p(1 - p) / V.
#    (Statistics Canada caps that at the actual sample size, which is not
#    published, so the cap is not applied; it only matters for shares whose
#    variance is smaller than a simple random sample's.)
#
# What the interval covers: sampling error, and the variability from
# households that sent nothing back (the replicate weights go through the same
# non-response adjustment). What it does not cover: bias if those households
# differ from the ones that answered, answers filled in for questions people
# skipped, people the census missed or counted twice, misreported answers, and
# the random rounding of every count to a multiple of 5. It is least exact for
# very small shares and small places. Exact intervals would need Statistics
# Canada's own replicate weights, in a custom tabulation or a Research Data
# Centre; for a blog this approximation is standard practice.
#
# Reliability: each share's coefficient of variation (CV, its standard error
# as a share of its value) is rated on Statistics Canada's survey scale. 16.6%
# to 33.3% is E, "use with caution"; over 33.3% is F, "too unreliable to be
# published". The census itself does not print these letters, but this site
# never uses an E or F figure (Greg, 2026-09-22, as for Statistics Canada's own
# E and F flags), so cwr_add_share_ci() blanks the share, its interval and its
# count to NA and records the letter in `quality`. The chart shows the row
# with "Not reported" and a note; never a zero, never a bridged gap.
#
# A table with no published bounds (commuting flows, 98-10-0459, is one) gets no
# interval: nothing here can be used on it, and none is invented. Its charts
# carry the stock "no intervals" note instead, written out in the post's chunk
# (cwr-charts rule 1d).
#
# Worked example: posts/commuting/R/02_clean_data.R.

library(dplyr)

# Student's t for 95% intervals on 32 degrees of freedom, as Statistics Canada
# uses for every published long-form interval (98-306-X, sections 6.2 and 7.3)
cwr_ci_t <- qt(0.975, df = 32)

# Statistics Canada's CV cut-offs: at or above the first a figure is E, "use
# with caution"; above the second it is F, "too unreliable to be published"
cwr_cv_caution <- 0.166
cwr_cv_unreliable <- 0.333

# A count's variance from its published 95% interval (step 1 above). `estimate`
# is the count; the bounds are the published ones.
cwr_var_from_bounds <- function(estimate, lower, upper) {
  h <- (upper - lower) / 2
  t <- cwr_ci_t
  # h^2 = t^2 V + a V^2, with a = t^4 / (4 Y^2): a quadratic in V, solved for
  # its positive root. A zero count has variance 0.
  a <- t^4 / (4 * estimate^2)
  if_else(
    estimate > 0,
    (-t^2 + sqrt(t^4 + 4 * a * h^2)) / (2 * a),
    0
  )
}

# The standard error of the share x / y (a proportion, not a percentage), from
# the standard errors of x and y. See step 2 above. The root is taken after
# choosing the formula so a negative term never reaches sqrt().
cwr_share_se <- function(x, se_x, y, se_y) {
  p <- x / y
  radicand <- se_x^2 - p^2 * se_y^2
  sqrt(if_else(radicand >= 0, radicand, se_x^2 + p^2 * se_y^2)) / y
}

# Adds the interval and its reliability to a table that has `percent` (0 to
# 100), the count behind it in `count_col`, and the share's standard error
# `se_p` (as a proportion, from cwr_share_se()), and drops `se_p`:
#   percent_lower, percent_upper  the 95% interval (step 3 above); NA for a
#                                 share of zero, which has none
#   cv                            coefficient of variation; NA for zero
#   quality                       "E" or "F" on Statistics Canada's survey
#                                 scale, NA when the share is fine to use
# A share rated E or F has its percent, interval and count blanked to NA.
cwr_add_share_ci <- function(data, count_col) {
  t <- cwr_ci_t
  data |>
    mutate(
      p = percent / 100,
      # Effective sample size; infinite (so no interval) for a zero share
      n_eff = if_else(se_p > 0, p * (1 - p) / se_p^2, NA_real_),
      centre = (p + t^2 / (2 * n_eff)) / (1 + t^2 / n_eff),
      half = t / (1 + t^2 / n_eff) * sqrt(p * (1 - p) / n_eff + t^2 / (4 * n_eff^2)),
      percent_lower = (centre - half) * 100,
      percent_upper = (centre + half) * 100,
      cv = if_else(percent > 0, se_p * 100 / percent, NA_real_),
      quality = case_when(
        cv > cwr_cv_unreliable ~ "F",
        cv >= cwr_cv_caution ~ "E"
      ),
      # E and F are never used: blank the share, its interval and its count
      across(c(percent, percent_lower, percent_upper, {{ count_col }}),
             \(x) if_else(is.na(quality), x, NA_real_))
    ) |>
    select(-c(p, n_eff, centre, half, se_p))
}
