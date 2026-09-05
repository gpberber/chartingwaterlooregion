# Charting Waterloo Region

A Quarto blog of data-analysis posts about Waterloo Region, written in R, published to GitHub
Pages at https://gpberber.github.io/chartingwaterlooregion. The author is a competent self-taught
R user, new to blogging and to git. Explain git and publishing steps in plain words.

## Layout

- `posts/<slug>/index.qmd` is a post. Its data pipeline is `R/01_get_data.R` (fills `data-raw/`,
  gitignored) then `R/02_clean_data.R` (fills `data/`, committed when each file is under 25 MB).
  `images/thumbnail.png` is the listing image. `README.md` lists sources and licences.
  Optional `app/` holds a Shiny app (deployed to shinyapps.io, never rendered by Quarto).
- `R/theme_cwr.R` is the house chart style (colours, `theme_cwr()`, helpers). Every post sources it.
  `R/data_helpers.R` moves big files to and from GitHub Releases. `R/packages.R` lists every package.
- `_templates/post/` is the scaffold `/new-post` copies. `_dev/` holds tooling (safety script,
  hooks, snippet converter). Neither is rendered.
- `_freeze/` is Quarto's cache of rendered results and is committed; never edit it by hand.
  `_site/` is the built site and is ignored.
- Background reading (PDFs, articles) lives outside the repo at
  `../chartingwaterlooregion-background/<slug>/`.

## Commands (run from the project root, Bash tool)

- Render one post, drafts visible: `quarto render posts/<slug> --profile draft`
- Preview server for the Browser pane: `preview_start` with `quarto-preview` (port 4200)
- Full public render: `quarto render`
- Publish: `quarto publish gh-pages --no-render --no-prompt` (only via `/publish`)
- Run R: `Rscript path/to/script.R` (never RStudio addins). Long R snippets go in a scratchpad
  script, not `-e`.

## Skills

`/new-post`, `/preview`, `/review-post`, `/publish`, `/share-data`, `/shiny-post`, `/save`, and
`cwr-charts` (loaded automatically when writing charts). Use them rather than improvising the workflow.

## Rules

- Charts: source `R/theme_cwr.R`, pick a template from the `cwr-charts` skill, one chart or table
  per chunk, every figure has `fig-cap` and `fig-alt`, caption through `cwr_caption()`. Tables are gt only.
- Data: raw never in git; files over 25 MB go to a GitHub Release via `/share-data`. Every post
  must be reproducible from its `R/` scripts plus the release.
- Secrets: none in the repo. Read keys with `Sys.getenv()` from `~/.Renviron`. The pre-commit hook
  (`_dev/check_repo_safety.sh`) blocks big files and key-like strings; never bypass it.
- Git: single `master` branch, commit via `/save`, never force-push, never rewrite history.
- Licence: code is MIT (`LICENSE`), text and charts are CC BY 4.0 (`LICENSE-CONTENT.md`), set
  site-wide by `license: "CC BY"` in `posts/_metadata.yml`. Data is never relicensed: every post
  README must name each source's own licence, and material obtained by request (not from an
  open-data portal) needs its terms checked before it goes into a release.
- Drafts: new posts start `draft: true`; `draft-mode: gone` keeps them off the public site.
  Only `/publish` flips a post live, after `/review-post`.
- Do not change `_quarto.yml` theme or `custom.scss` palette without asking; the palette must stay
  in sync with `R/theme_cwr.R`.
- Style: tidyverse, `|>`, purrr and stringr over base, `here()` for every path, snake_case,
  `janitor::clean_names()` after every read, thorough comments for a self-taught reader.
