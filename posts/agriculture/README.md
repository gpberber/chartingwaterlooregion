# Farm animals in the townships

Post: https://chartingwaterlooregion.ca/posts/agriculture/

## Data sources

| File in `data-raw/` | What it is | Source (link) | Licence | Sample | Accessed |
|---|---|---|---|---|---|
| `table_32100370.csv` | Table 32-10-0370-01, cattle inventory on farms, 2021 Census of Agriculture, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210037001) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_32100371.csv` | Table 32-10-0371-01, sheep inventory on farms, 2021 Census of Agriculture, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210037101) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_32100372.csv` | Table 32-10-0372-01, pig inventory on farms, 2021 Census of Agriculture, cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210037201) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_32100424.csv` | Table 32-10-0424-01, cattle inventory on farms, 2011 and 2016 Censuses of Agriculture (inactive), cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210042401) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_32100425.csv` | Table 32-10-0425-01, sheep inventory on farms, 2011 and 2016 Censuses of Agriculture (inactive), cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210042501) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |
| `table_32100426.csv` | Table 32-10-0426-01, pig inventory on farms, 2011 and 2016 Censuses of Agriculture (inactive), cut to Waterloo Region | [Statistics Canada](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210042601) | [Statistics Canada Open Licence](https://www.statcan.gc.ca/en/reference/licence) | None (Census of Agriculture, every farm) | 2026-09-18 |

<!-- Sample: "None (full count)" for administrative data, a census short-form question or a
     full register; otherwise name the sample, e.g. "Census long form, 25% sample of households"
     or the survey and its sample size, from the survey's own documentation. This column is
     printed in the post's Data sources section, so readers see it. -->

Files that cannot be re-downloaded by script, or are over 25 MB, are attached to a GitHub Release
(tag shown in the table) and fetched by `R/01_get_data.R`.

## Reliability

| Issue | Left out of the post because |
|---|---|
| **Figures not shown:** Statistics Canada does not publish the 2021 pig count for Wilmot or the 2021 sheep count for North Dumfries, rating them too unreliable to publish (F), and suppressed the 2016 sheep count for North Dumfries to protect confidentiality (x). It publishes 2021 pig counts for North Dumfries and Woolwich but flags them "use with caution" (E), and this site does not use figures flagged that way. Those rows are left empty in the charts. (tables 32-10-0371-01, 32-10-0372-01, 32-10-0425-01) <!-- flags: 32-10-0372-01:E, 32-10-0372-01:F, 32-10-0371-01:F, 32-10-0425-01:x --> | |
| **Quality grades, 2021:** Statistics Canada grades each 2021 township figure from A (excellent) to D (acceptable). Cattle: Wellesley excellent (A), Woolwich very good (B), Wilmot good (C), North Dumfries acceptable (D). Sheep: Wellesley and Woolwich very good (B), Wilmot acceptable (D). Pigs: Wellesley acceptable (D). The 2011 and 2016 figures carry no grades. (tables 32-10-0370-01, 32-10-0371-01, 32-10-0372-01) <!-- flags: 32-10-0370-01:A, 32-10-0370-01:B, 32-10-0370-01:C, 32-10-0370-01:D, 32-10-0371-01:B, 32-10-0371-01:D, 32-10-0372-01:D --> | |
| **Comparing one census with another:** farms are assigned to places, and the boundaries of those places refined, between one Census of Agriculture and the next; and in 2021 the definition of a census farm changed. Statistics Canada advises caution comparing 2021 with earlier censuses, and 2011 with 2016. (tables 32-10-0370-01, 32-10-0371-01, 32-10-0372-01, 32-10-0424-01, 32-10-0425-01, 32-10-0426-01) <!-- flags: 32-10-0370-01:note1, 32-10-0370-01:note2, 32-10-0370-01:note4, 32-10-0371-01:note1, 32-10-0371-01:note2, 32-10-0371-01:note4, 32-10-0372-01:note1, 32-10-0372-01:note2, 32-10-0372-01:note4, 32-10-0424-01:note21, 32-10-0425-01:note21, 32-10-0426-01:note21 --> | |
| **Combined areas, 2011 and 2016:** to protect confidentiality, figures for an area with very few farms may be combined with a neighbouring census consolidated subdivision or census division. (tables 32-10-0424-01, 32-10-0425-01, 32-10-0426-01) <!-- flags: 32-10-0424-01:note2, 32-10-0425-01:note2, 32-10-0426-01:note2 --> | |
| **Territories:** Yukon and Northwest Territories figures are left out of the national totals. (table 32-10-0425-01) <!-- flags: 32-10-0425-01:note18, 32-10-0425-01:note19 --> | Does not apply: the post uses four Ontario townships and no national totals. |

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

     The post's "Reliability" table prints every row whose second column is empty. Never
     delete a row: to leave an issue out of the post, write why in the second column. An empty
     table prints "No data-reliability issues to note for this post." -->

## Data dictionary

<!-- Generated. Leave this heading in place and do not write the table by hand: running
     cwr_dictionary_readme("agriculture") from R/data_bundle.R replaces everything between this
     heading and the next one with a table built from the data plus data/dictionary.csv.
     Building the download bundle does it too, so /publish keeps it current. -->

Not generated yet. Fill in `data/tables.csv` and `data/dictionary.csv`, then run:

```r
source(here::here("R", "data_bundle.R"))
cwr_dictionary_readme("agriculture")
```

## Reproduce

From the project root in R:

```r
source("R/packages.R"); install_missing()          # once
source("posts/agriculture/R/01_get_data.R")            # fills data-raw/
source("posts/agriculture/R/02_clean_data.R")          # fills data/
```

Then `quarto render posts/agriculture` from a terminal.

## Notes

- Background reading for this post is kept outside the repository (`../chartingwaterlooregion-background/agriculture/`).
- No API keys are needed. <!-- If one is, say which and how to get it. -->
- Livestock is published for census consolidated subdivisions (CCS), groups of neighbouring
  census subdivisions, not for single districts. Each of the four townships is a CCS on its own, under
  its own census subdivision code, in all three censuses, so the township figures are the townships exactly (see
  the welcome post's README). The figures are each table's total head count on census day: "Total
  cattle" (2021) or "Total cattle and calves" (2011, 2016), "Total pigs", and "Total sheep" (2021)
  or "Total sheep and lambs" (2011, 2016).
- Sampling: none of the data are sampled. The Census of Agriculture counts every farm.
