---
name: shiny-post
description: Build, deploy, and embed a Shiny app in a post - app lives in posts/<slug>/app/, is deployed to shinyapps.io with rsconnect, and embedded with a responsive iframe. Use when a post needs an interactive app or when working on an existing app folder.
argument-hint: <slug>
---

# Shiny app in a post

Argument: `$ARGUMENTS` = the post slug. The app lives in `posts/<slug>/app/` and is excluded
from Quarto rendering by `_quarto.yml`. It is deployed separately and embedded by URL.

## Where things go

```
posts/<slug>/
  app/
    app.R            single-file app (ui + server); split into ui.R/server.R only if it grows large
    data/            small tidy data the app reads (copy from ../data/ in 02_clean_data.R)
    www/             css, images the app serves
  index.qmd          the post; embeds the app with the block below
```

The app reads only files inside `app/` (use `file.path("data", "x.csv")`, not `here()`): the
deployed app has no access to the rest of the repo. Keep app data small; it is uploaded on every deploy.
Apply the house style: `source("theme_cwr.R")` from a copy placed in `app/` (copy `R/theme_cwr.R`
there in `02_clean_data.R` so it stays in sync), or at minimum use the palette hex values.

## Build and test locally

```bash
Rscript -e "shiny::runApp('posts/<slug>/app', port = 4300, launch.browser = FALSE)"
```
Run in the background, then open `http://localhost:4300` in the Browser pane and exercise every input.
Stop the process when done.

## Deploy to shinyapps.io

The user already has a shinyapps.io account (apps under `deadicated-to-data`). Credentials are
stored by `rsconnect::setAccountInfo()` once, outside the repo; never write them into files.

```r
rsconnect::deployApp(
  appDir = here::here("posts", "<slug>", "app"),
  appName = "cwr-<slug>",
  forceUpdate = TRUE
)
```

`rsconnect/` folders it creates are gitignored. Note the app URL it prints.

## Embed in the post

```markdown
::: {.shiny-embed}
<iframe src="https://deadicated-to-data.shinyapps.io/cwr-<slug>/" title="<what the app shows>" loading="lazy"></iframe>
:::
::: {.shiny-embed-link}
[Open the app full screen](https://deadicated-to-data.shinyapps.io/cwr-<slug>/){target="_blank"} · Source: [`posts/<slug>/app/`](https://github.com/gpberber/chartingwaterlooregion/tree/master/posts/<slug>/app)
:::
```

The `.shiny-embed` class (custom.scss) keeps a 4:3 box that scales with the page; override the
ratio inline with `style="aspect-ratio: 16/9"` for wide apps. Add a static screenshot of the app
as `images/thumbnail.png` so the listing card and social previews have an image.

## Notes

- Free shinyapps.io tier: 25 active hours per month across apps; a sleeping app takes a few seconds
  to wake, which is fine for a blog.
- For a tiny app with no heavy packages, Quarto's `shinylive` extension can run it in the browser
  with no server; not worth it for anything using sf, arrow, or large data.
- Every package the app uses must be listed in `R/packages.R` too.
