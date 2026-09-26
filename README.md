# Charting Waterloo Region

Learning about life in Waterloo Region, one chart at a time.

**Site:** https://chartingwaterlooregion.ca

**How this is built and published:** [WORKFLOW.md](WORKFLOW.md). Section 4 covers saving, committing,
pushing and publishing - what each one does, and what not to do.

**Choosing a chart:** [CHARTS.md](CHARTS.md) - every chart template, what it is called, and what it is
good for.

## What is here

| Path | Contents |
|---|---|
| `posts/<slug>/` | One folder per post: `index.qmd` (the post), `R/` (data scripts), `data/` (small tidy data), `README.md` (sources and licences) |
| `datasets/<slug>/` | Shared data used by several posts: download and cleaning scripts, `load.R` for posts, tidy `data/`, `README.md` (sources, licences, versions) |
| `R/theme_cwr.R` | House chart style: colours, the ggplot2 theme used on every chart, and the helpers that save each chart at desktop and phone size |
| `R/data_quality.R` | Finds the quality flags and footnotes on the figures a post uses, and blanks the ones this site never publishes |
| `R/census_ci.R` | Confidence intervals for shares worked out from the census long form |
| `R/data_bundle.R` | Builds each post's downloadable data zip and its data dictionary |
| `R/data_helpers.R` | Download and upload large data files via GitHub Releases |
| `R/maps.R` | Map helpers: base maps and label placement |
| `R/seo_post_render.R` | Runs after every render and tidies the built site for search engines |
| `R/packages.R` | Every R package used on the site, with an installer |
| `_freeze/` | Cached render results, so the site builds without re-running the analysis |
| `fonts/` | The Inter typeface, served by the site itself rather than from a font CDN |

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

## Licence

- **Code** (R scripts, Quarto config, styling, templates): [MIT](LICENSE).
- **Text, charts, and images**: [CC BY 4.0](LICENSE-CONTENT.md). Reuse freely with credit to Charting Waterloo Region.
- **Data files** keep the licence of their original source, listed in each post's README.
- **The Inter typeface** in `fonts/` is redistributed under the [SIL Open Font Licence 1.1](fonts/Inter-LICENSE.txt), which is why that licence file sits beside it. The site serves these files itself rather than calling a font CDN, so no reader's browser has to talk to a third party to read a page.
