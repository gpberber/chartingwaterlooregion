# Charting Waterloo Region

Charts and plain-language analysis about life in Waterloo Region, built from open data.

**Site:** https://gpberber.github.io/chartingwaterlooregion

## What is here

| Path | Contents |
|---|---|
| `posts/<slug>/` | One folder per post: `index.qmd` (the post), `R/` (data scripts), `data/` (small tidy data), `README.md` (sources and licences) |
| `datasets/<slug>/` | Shared data used by several posts: download and cleaning scripts, `load.R` for posts, tidy `data/`, `README.md` (sources, licences, versions) |
| `R/theme_cwr.R` | House chart style: colours and the ggplot2 theme used on every chart |
| `R/data_helpers.R` | Download and upload large data files via GitHub Releases |
| `R/packages.R` | Every R package used on the site, with an installer |
| `_freeze/` | Cached render results, so the site builds without re-running the analysis |

## Reproduce a post

See https://gpberber.github.io/chartingwaterlooregion/reproduce.html, or in short:

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
