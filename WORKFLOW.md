# Charting Waterloo Region — Publish Workflow

## Overview

This is a Quarto website published to Posit Connect Cloud. Posts are written in `.qmd` files and rendered locally before publishing. The `_freeze/` directory caches rendered outputs so Posit Connect Cloud does not need to re-execute R code.

## Adding or updating a post

### 1. Write the post

Each post lives at `posts/<post-name>/reports/analysis.qmd`. The YAML front matter controls visibility:

``` yaml
draft: true   # hidden from the blog listing
draft: false  # visible on the published site
```

All other posts in `posts/` with `draft: true` are excluded from the listing automatically.

### 2. Render locally

Run from the project root:

``` r
quarto::quarto_render("posts/<post-name>/reports/analysis.qmd")
```

Or render the full site:

``` r
quarto::quarto_render()
```

This executes the R code and writes cached outputs to `_freeze/`. Always render before publishing — the published site uses these cached outputs.

### 3. Publish

``` bash
quarto publish posit-connect-cloud --no-prompt
```

This re-renders the full site (using freeze cache where unchanged), uploads everything, and deploys to the URL stored in `_publish.yml`.

## Key configuration

| File | Purpose |
|----|----|
| `_quarto.yml` | Site config; `execute: freeze: auto` enables output caching |
| `_publish.yml` | Stores the Posit Connect Cloud deployment target |
| `_freeze/` | Cached render outputs — must be committed to git |
| `data/` | Raw data — gitignored, only needed locally for rendering |

## How `freeze: auto` works

- On first render of a document, outputs are written to `_freeze/`.
- On subsequent renders, a document is only re-executed if its source has changed.
- At publish time, unchanged documents use cached outputs — data files do not need to be present on the server.
- Commit `_freeze/` to git so the cache travels with the source.

## Post structure conventions

```         
posts/
  <post-name>/
    R/               # exploratory scripts and source .Rmd files
    background/      # background documents pertaining to post's subject matter
    data/            # raw data (gitignored)
    images/          # thumbnail and figures
    reports/
      analysis.qmd   # the published post
```

The `here::i_am()` call at the top of each `analysis.qmd` must point to its own path:

``` r
here::i_am("posts/<post-name>/reports/analysis.qmd")
```
