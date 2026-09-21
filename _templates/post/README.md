# {{title}}

Post: https://chartingwaterlooregion.ca/posts/{{slug}}/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| | | | | | |

<!-- Sample: "None (full count)" for administrative data, a census short-form question or a
     full register; otherwise name the sample, e.g. "Census long form, 25% sample of households"
     or the survey and its sample size, from the survey's own documentation. This column is
     printed in the post's Data sources section, so readers see it. -->

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|

<!-- One row per data-quality issue in the data the post uses: each flag and quality footnote
     in data/quality_flags.csv (written by cwr_quality_flags() in R/02_clean_data.R), and any
     caveat another source's own documentation gives. Closely related flags share a row - one
     table graded acceptable for one municipality and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     Then end the Issue cell with a hidden code for every flag the row covers, for example
       ... (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 -->
     A symbol is table:symbol (17-10-0155-01:E), a footnote is table:note plus its number
     (98-10-0459:note1), several are comma-separated. Readers never see the code. The post
     will not render while any flag in data/quality_flags.csv lacks a row with its code, and a
     row left out of the post with a reason still counts.

     A post using census long-form data (or any sample) also gets a methodology row, printed,
     with its second column empty. Stock wording, adjusted to the post's tables:
       | **Sample estimates:** these figures come from the census long-form questionnaire, which
       went to one household in four, so every figure is an estimate. Where Statistics Canada
       publishes a 95% confidence interval for a count, the interval for a share worked out from
       it is approximated with the US Census Bureau's formula for a proportion; a share whose
       sampling error is more than a third of its value is marked as unreliable. These intervals
       cover sampling error only, not non-response or rounding, and are least exact for very
       small shares and small places. (tables ...) | |
     Add "No intervals are published for <table>, so close rankings there may be sampling error"
     for a table without them. It needs no flag code.

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("{{slug}}") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("{{slug}}")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/{{slug}}/R/01_get_data.R")            # fills data-raw/
source("posts/{{slug}}/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/{{slug}}` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/{{slug}}/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- Sampling: <!-- Which sources are samples (see the Sample column above) and what that means for
  the figures; whether the source publishes confidence intervals; how R/census_ci.R turned them
  into intervals for the shares and which data files carry them; and what each chart does with
  them - draws them, states the widest in a note, marks unreliable shares - or, for a table with
  none, that its rankings carry the stock "no intervals" note. "None of the data are sampled" if
  so. -->
