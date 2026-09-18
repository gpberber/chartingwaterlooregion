# Charting Waterloo Region

A Quarto blog of data-analysis posts about Waterloo Region, written in R, published to GitHub
Pages at https://chartingwaterlooregion.ca. The author is a competent self-taught
R user, new to blogging and to git. Explain git and publishing steps in plain words.

## Layout

- `posts/<slug>/index.qmd` is a post. Its data pipeline is `R/01_get_data.R` (fills `data-raw/`,
  gitignored) then `R/02_clean_data.R` (fills `data/`, committed when each file is under 25 MB).
  `images/thumbnail.png` is the listing image. `README.md` lists sources and licences.
  Optional `app/` holds a Shiny app (deployed to shinyapps.io, never rendered by Quarto).
- `datasets/<slug>/` is the shared data layer for data that feeds several posts: `R/01_get_data.R`,
  `R/02_clean_data.R`, `R/helpers.R`, `R/load.R` (posts source this one line), `data-raw/`, `data/`,
  `README.md`. Two-layer rule: a dataset is tidy, complete, and general (every year, every region);
  a post filters and summarises for its story. A derived table two posts need moves into the dataset.
  `datasets/crime/` is the worked example.
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

`/new-post`, `/new-dataset`, `/preview`, `/review-post`, `/publish`, `/share-data`, `/shiny-post`, `/save`, and
`cwr-charts` (loaded automatically when writing charts). Use them rather than improvising the workflow.

## Rules

- Charts: never a pie or a donut - the About page makes that a promise to readers, so parts of a
  whole go in a bar chart. Source `R/theme_cwr.R`, pick a template from the `cwr-charts` skill, one
  chart or table per chunk. Every chart goes through `cwr_figure()` (desktop and phone PNGs in the post's
  `figures/`, committed) with `alt` filled in; source line through `cwr_caption()`. Figure
  numbers follow the post type: Deep dives get "Figure N" and `@fig-` references, Snapshots get
  no label. Captions are optional and usually omitted. Tables are gt only.
- Chart text calls Waterloo Region "Region": use `cwr_region` for every axis, legend, direct
  label, tooltip and note (cleaning scripts type "Region"). Line labels are positioned with
  `cwr_line_labels()`, never hand-typed coordinates. Hover charts use `cwr_interactive()`.
- Census: long-form variables (commuting, labour, education, housing, immigration and more, plus
  average and aggregate income; median income is 100% data) are a 25% sample, so charts show shares or rates, never counts - `cwr-charts` rule 9a. If
  Greg asks for counts from one, flag it before building. Any chart from sample data (census long
  form, surveys) carries the stock note from `cwr_caption(sample = )`, with confidence intervals
  drawn or stated when the source publishes them - rule 9b. The sample is also recorded in the
  README's Data sources table (Sample column) and Notes, and in `data/tables.csv` (`sample`).
- Data: raw never in git; files over 25 MB go to a GitHub Release via `/share-data`. Every post
  must be reproducible from its `R/` scripts plus the release.
- Data download: every post lists the tables it uses in `data/tables.csv` and documents every
  column in `data/dictionary.csv`. `R/data_bundle.R` turns those into a zip (CSV, Parquet, Excel,
  dictionary, README) attached to release `data-<slug>-v<n>`; `/publish` builds it and the post's
  Reproducibility box links to it. Bundles are per post only, never for a dataset on its own.
- The data dictionary never appears in the post. It lives in two generated places only: the
  `## Data dictionary` section of `posts/<slug>/README.md` and `data_dictionary.csv` in the zip,
  both written by `cwr_dictionary_readme()` / `cwr_data_bundle()`. Never hand-write either.
- Secrets: none in the repo. Read keys with `Sys.getenv()` from `~/.Renviron`. The pre-commit hook
  (`_dev/check_repo_safety.sh`) blocks big files and key-like strings; never bypass it.
- Git: single `master` branch, commit via `/save`, never force-push, never rewrite history.
- Licence: code is MIT (`LICENSE`), text and charts are CC BY 4.0 (`LICENSE-CONTENT.md`), set
  site-wide by the `license:` block in `posts/_metadata.yml` (heading "Reusing this post", set by
  `language: section-title-reuse` in `_quarto.yml`). Data is never relicensed: every post
  README must name each source's own licence, and material obtained by request (not from an
  open-data portal) needs its terms checked before it goes into a release. WRPS occurrence data
  carries a required disclaimer: posts using it show `wrps_disclaimer` (datasets/crime/R/helpers.R).
- The writing is Greg's, not Claude's. "Create a post" means scaffold it and stop: no opening
  paragraph, no headings of your own, no analysis. "Add a chart" means add the chart chunk and
  nothing else - no prose around it, and never reword, fill in, or delete the template's stock
  sections and placeholder comments. A post renders fine with every stock section untouched and
  `data/tables.csv` and `data/dictionary.csv` still empty, so nothing forces the extras; leave
  them alone and say in one line what is still outstanding. Write prose only when asked for it.
  A chart's finding title and subtitle are prose too, and Greg's: when drafting a chart, give it a
  brief, purely descriptive working title of the data shown ("Where commuters from Kitchener
  work") - no finding, no claim - so he can tell the charts apart, and leave the subtitle as the
  template's placeholder line. He replaces the working title with the finding. The `cwr_caption()` source line, the `alt` text and the code comments are
  Claude's to fill in - they are machinery and accessibility, not voice.
- Post types: the first entry in `categories` is `Snapshot` (a few simple charts, read in under
  5 minutes) or `Deep dive` (longer, more rigorous); then 2 to 3 topic categories.
- Drafts: new posts start `draft: true`; `draft-mode: gone` keeps them off the public site.
  Only `/publish` flips a post live, after `/review-post`.
- Do not change `_quarto.yml` theme or `custom.scss` palette without asking; the palette must stay
  in sync with `R/theme_cwr.R`.
- Style: tidyverse, `|>`, purrr and stringr over base, `here()` for every path, snake_case,
  `janitor::clean_names()` after every read, thorough comments for a self-taught reader.
