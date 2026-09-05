# Charting Waterloo Region

Charts and plain-language analysis about life in Waterloo Region, built from open data.

**Site:** https://gpberber.github.io/chartingwaterlooregion

## What is here

| Path | Contents |
|---|---|
| `posts/<slug>/` | One folder per post: `index.qmd` (the post), `R/` (data scripts), `data/` (small tidy data), `README.md` (sources and licences) |
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

<!-- TODO: choose a licence. Common choice: MIT for code, CC BY 4.0 for text and charts. -->
Licence to be confirmed. Reuse with attribution to Charting Waterloo Region is welcome.
Data files keep the licence of their original source, listed in each post's README.
