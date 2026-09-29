# Charting Waterloo Region

Learning about life in Waterloo Region, one chart at a time.

**Site:** https://chartingwaterlooregion.ca

This repository holds the code and data behind every published post, so anyone can check a figure
or rebuild a chart. Each post's page links back to its folder here.

## What is here

| Path | Contents |
|---|---|
| `posts/<slug>/` | One folder per post: `index.qmd` (the post), `R/` (data scripts), `data/` (small tidy data), `README.md` (sources and licences) |
| `vital-statistics/` | The standing page of regularly updated charts, built the same way |
| `R/theme_cwr.R` | House chart style: packages, colours and the ggplot2 theme used on every chart. Sourcing it also brings in `chart_helpers.R`, `figures.R` (saves each chart at desktop and phone size), `right_axis.R`, `interactive.R` and `post_sections.R` |
| `R/data_quality.R` | Finds the quality flags and footnotes on the figures a post uses, and blanks the ones this site never publishes |
| `R/census_ci.R` | Confidence intervals for shares worked out from the census long form |
| `R/data_bundle.R` | Builds each post's downloadable data zip and its data dictionary |
| `R/data_helpers.R` | Download and upload large data files via GitHub Releases |
| `R/maps.R` | Map helpers: base maps and label placement |
| `R/seo_post_render.R` | Runs after every render and tidies the built site for search engines |
| `R/packages.R` | Every R package used on the site, with an installer |
| `_freeze/` | Cached render results, so the site builds without re-running the analysis |
| `fonts/` | The Inter typeface, served by the site itself rather than from a font CDN |

Some posts draw on a shared dataset folder, `datasets/<slug>/`, holding the download and cleaning
scripts and tidy data that several posts use. One appears here when a published post depends on it.

## Reproduce a post

In short:

```r
source("R/packages.R"); install_missing()
source("posts/<slug>/R/01_get_data.R")
source("posts/<slug>/R/02_clean_data.R")
```

then `quarto render posts/<slug>`.

Raw data is not stored in this repository. Each post's `01_get_data.R` downloads it from the
original source or from a [GitHub Release](https://github.com/gpberber/chartingwaterlooregion/releases)
attached to this repo. No API keys are needed.

If you only want the numbers, each post ends with a link to a zip of its tables in CSV and Parquet
form, with an Excel workbook, a data dictionary and a README naming every source and licence.

## Licence

- **Code** (R scripts, Quarto config, styling, templates): [MIT](LICENSE).
- **Text, charts, and images**: [CC BY 4.0](LICENSE-CONTENT.md). Reuse freely with credit to Charting Waterloo Region.
- **Data files** keep the licence of their original source, listed in each post's README.
- **The Inter typeface** in `fonts/` is redistributed under the [SIL Open Font Licence 1.1](fonts/Inter-LICENSE.txt), which is why that licence file sits beside it. The site serves these files itself rather than calling a font CDN, so no reader's browser has to talk to a third party to read a page.
