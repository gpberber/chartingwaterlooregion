# {{title}}

Post: https://chartingwaterlooregion.ca/posts/{{slug}}/

## Key terms

| Term | Definition |
|---|---|
| | |

<!-- Only terms that appear in the main body of a chart - segment and legend names, panel
     headings, axis and direct labels - and that a reader needs defined to read it: never
     statistical terms (confidence interval, coefficient of variation), never census geography
     terms (census subdivision, census division), never a term used only in a title, note or the
     prose. Greg asks for any term he wants added. Quote the source's own definition and name the
     source in brackets at the end - the table's metadata, footnotes or category names, the census
     dictionary or reference guides, or other official documentation ("(2021 Census
     Dictionary)"). A term with no official definition is Greg's to define: Claude lists it with
     the Definition cell empty, to be filled in or deleted. The post's "Key terms" section prints every row that has a
     definition, and the render warns about any that does not. Every chart note, label and
     sentence about who or what is counted is checked against these rows (cwr-charts rule 1d). -->

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| | | | | | |

<!-- Sample: "None (full count)" for administrative data, a census short-form question or a
     full register; otherwise name the sample, e.g. "Census long form, 25% sample of private households"
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
     table graded acceptable for one district and good for another is one issue. Say which
     figures it touches and what it means for them, in a sentence or two, and end with the
     table number in brackets.

     Then end the Issue cell with a hidden code for every flag the row covers, for example
       ... (table 18-10-0004-01) <!-- flags: 18-10-0004-01:note36 -->
     A symbol is table:symbol (17-10-0155-01:E), a footnote is table:note plus its number
     (98-10-0459:note1), several are comma-separated. Readers never see the code. The post
     will not render while any flag in data/quality_flags.csv lacks a row with its code, and a
     row left out of the post with a reason still counts.

     A post using census long-form data gets three methodology rows, printed, with their second
     column empty and no flag code (the charts carry no long-form note; these rows are the
     disclosure). Stock wording, adjusted to the post's tables and charts
     (posts/commuting/README.md is the worked example):
       | **Long-form census:** every chart in this post is drawn from the 2021 census long-form
       questionnaire (tables ...). Its figures are estimates for people in private households,
       from the 2021 census long-form questionnaire sent to 25% of households; people living in
       collective dwellings, such as nursing and seniors' homes and student residences, are not
       included. Each responding household stands for about four, its weight adjusted by
       Statistics Canada for households that did not respond and matched to the full census
       counts of age, household size and other characteristics; answers left blank are filled
       in from similar households. (tables ...) | |
       | **Sample estimates:** because the figures are estimates, each share worked out from table
       ... has a 95% confidence interval, built from the intervals Statistics Canada publishes for
       its counts by Statistics Canada's own method. The intervals cover sampling error and the
       variability from households that did not respond; they do not cover any bias if those
       households differ from the ones that did, answers filled in for blank questions, people
       the census missed or counted twice, misreported answers, or rounding. A share whose
       sampling error is 16.6% of its value or more - rated "use with caution" (E) or "too
       unreliable to be published" (F) on Statistics Canada's scale - is not reported: ... (tables ...) | |
       | **Response rates:** ...% to ...% of the households sent the long form in each area
       returned nothing usable (Statistics Canada advises caution only at 50% or more), and ...%
       to ...% of the answers to the ... questions were missing or filled in. (Census Profile
       2021; table ...) | |
     For a table without published intervals add "Table ... publishes no intervals, so close
     rankings drawn from it may differ only by sampling error" to the Sample estimates row.

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
  into intervals for the shares and which data files carry them; which shares are not shown as E
  or F; what each chart does with the intervals - draws them or states the widest in a note - or,
  for a table with none, that its rankings carry the stock "no intervals" note; and where the
  response rates are kept (data/census_quality.csv). "None of the data are sampled" if so. -->
