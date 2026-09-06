# Charting Waterloo Region: workflow guide

How the blog works and how to do everything from writing a post to publishing it. Written for
the author, who is comfortable in R but new to blogging and to git. Claude Code does most of the
mechanical steps through the skills listed below; the manual commands are given too so nothing
depends on Claude being available.

## 1. How the site works

- **Quarto website.** Every post is a `.qmd` file with R code that reads data, draws charts, and
  writes tables. `quarto render` runs the code and builds HTML into `_site/`.
- **Freeze.** Results of running the R code are cached in `_freeze/` and committed to git. A post
  is only re-executed when its source changes, so a full site render is fast and readers can
  rebuild the site without any data.
- **GitHub is the backup and the source.** The repository at
  https://github.com/gpberber/chartingwaterlooregion holds everything except raw data and
  background reading. Commit and push often; that is the save button.
- **GitHub Pages is the host.** `quarto publish gh-pages` copies `_site/` to the `gh-pages`
  branch, and GitHub serves it at https://gpberber.github.io/chartingwaterlooregion. Nothing
  runs on a server; rendering always happens on this machine.
- **Drafts never leak.** A post with `draft: true` is left out of the public site entirely.
  Preview drafts locally with the `draft` profile.

## 2. Folder map

```
chartingwaterlooregion/
  _quarto.yml            site settings (navbar, theme, licence heading, what gets rendered)
  _quarto-draft.yml      profile that makes drafts visible for local preview
  custom.scss            site colours and fonts (mirrors R/theme_cwr.R)
  index.qmd              home page (the post listing)
  about.qmd  reproduce.qmd  404.qmd
  CLAUDE.md              conventions Claude follows in this project
  WORKFLOW.md            this guide
  LICENSE  LICENSE-CONTENT.md
  R/
    theme_cwr.R          house chart style: colours, theme_cwr(), cwr_caption(), cwr_session_info()
    data_helpers.R       move big files to and from GitHub Releases
    packages.R           every package the site uses; install_missing()
  posts/
    _metadata.yml        defaults for every post (author, licence, echo: false, figure sizes)
    <slug>/              one folder per post (layout in section 3)
  datasets/
    <slug>/              shared data used by several posts (section 5)
  _templates/            scaffolds copied by /new-post and /new-dataset
  _dev/                  tooling: safety check, git hook, snippet converter, r.snippets
  _freeze/               cached render results (committed; never edit by hand)
  _site/                 built site (ignored; rebuilt by quarto render)
  .claude/skills/        the Claude Code skills
../chartingwaterlooregion-background/<slug>/   background reading, outside the repo
```

## 3. Create and publish a new post

### 3.1 Scaffold it

In Claude Code:

```
/new-post housing-starts "Housing starts in Waterloo Region"
```

The slug becomes the URL (`.../posts/housing-starts/`), so keep it short, lowercase, hyphenated.
Claude copies the template and creates:

```
posts/housing-starts/
  index.qmd          the post, starts with draft: true
  README.md          data sources, licences, how to reproduce
  R/01_get_data.R    fetches raw inputs into data-raw/
  R/02_clean_data.R  turns data-raw/ into small tidy files in data/
  data-raw/          raw inputs (gitignored, never committed)
  data/              tidy files the post reads (committed when under 25 MB)
  images/            thumbnail.png plus any static images
../chartingwaterlooregion-background/housing-starts/   PDFs, articles, notes
```

Claude asks for a one-sentence description (shown on the listing card) and two to four
categories. Reuse existing categories where they fit; they are listed on the home page.

If the post draws on a shared dataset (section 5), say so when asked; Claude wires the
`load.R` line into the setup chunk and you skip most of 3.2.

### 3.2 Get and clean the data

1. Write `R/01_get_data.R` so it recreates `data-raw/` from scratch: download from the open-data
   portal, pull Statistics Canada tables with `cansim`, or fetch from a GitHub Release (3.6).
   Files you downloaded by hand from a portal still need a script that fetches them again.
2. Write `R/02_clean_data.R` to produce the tidy files the post needs in `data/`. Call
   `janitor::clean_names()` right after every read. Keep each output under 25 MB.
3. Run both from the project root:
   ```r
   source("posts/housing-starts/R/01_get_data.R")
   source("posts/housing-starts/R/02_clean_data.R")
   ```
4. Fill the source table in `README.md`: file, what it is, link, the source's own licence
   (for example Open Government Licence – Kitchener, Statistics Canada Open Licence), date accessed.
5. Any new package goes into `R/packages.R` as well as the script.

Claude can write both scripts if you describe the source; it follows the same conventions.

### 3.3 Write the post

The template gives the structure: an opening paragraph with the question and the answer,
sections with plain-language headings, one chart or table per section, a Data and methods
section, and a collapsed Reproducibility box at the end.

- **Charts.** Ask Claude for the chart you want ("a ranked lollipop of housing starts per 1,000
  residents for the Big 12, Waterloo Region bolded"). The `cwr-charts` skill loads
  automatically: it picks a template from the snippet library, applies the house colours and
  theme, renders the chart to a PNG, looks at it, and tunes label positions before putting the
  code in the post. Every chart chunk needs a `fig-cap`, a `fig-alt`, and a caption made with
  `cwr_caption("Source name")`.
- **Tables.** `gt` only, one per chunk, with `tab_source_note()`.
- **Numbers in the prose** should come from the data, not be typed by hand where a chunk can
  compute them; the review step checks they match.
- **Thumbnail.** Save the best chart as `images/thumbnail.png` (4:3 works best for the listing).
- **Setup chunk.** The template already has `here::i_am(...)` and
  `source(here::here("R", "theme_cwr.R"))`. Add post-specific `library()` calls under it.

### 3.4 Preview

```
/preview housing-starts
```

Claude renders the post with drafts visible, fixes any R or Quarto errors, opens it in the
browser pane, and screenshots it at desktop and phone widths. Or by hand:

```bash
quarto preview --profile draft
```

then open http://localhost:4200/posts/housing-starts/. The preview re-renders on every save.

### 3.5 Review

```
/review-post housing-starts
```

Produces a findings list: prose numbers versus data, headings, chart style, captions and
alt text, tables, `here()` paths, packages listed, README sources and licences, thumbnail,
categories, reproducibility box, and a run of the safety check. Fix what it finds
(Claude offers to), then re-run until clean.

### 3.6 Publish

```
/publish housing-starts
```

Claude confirms the post should go public, flips `draft: false`, renders the full site,
runs the safety check, commits, pushes, deploys with `quarto publish gh-pages`, then opens
the live URL and confirms it loads. The site updates within a minute or two. By hand:

```bash
quarto render
git add -A
git commit -m "Publish: Housing starts in Waterloo Region"
git push origin master
quarto publish gh-pages --no-render --no-prompt
```

### 3.7 Update a published post

Edit `index.qmd`, run `/preview`, then `/publish` with no argument. Quarto adds a "Modified"
date automatically. If the data changed, re-run the scripts in `R/` first and mention the
refresh date in the post.

## 4. Save work at any time

```
/save fixed typos in housing starts
```

Claude stages everything, runs the safety check, commits with a clear message, and pushes.
Do this at the end of every working session even when nothing is ready to publish; drafts
are invisible on the public site but safe on GitHub. By hand: `git add -A`,
`git commit -m "message"`, `git push origin master`.

The safety check (`_dev/check_repo_safety.sh`, also installed as a git pre-commit hook)
blocks any commit that includes a file over 25 MB, a `.Renviron` or `.env` file, text that
looks like an API key, or a background-reading folder. If it blocks, read its message: the
fix is usually `/share-data` for a big file or `git rm --cached` for a file that should be
ignored. Never work around it.

## 5. Shared datasets (data used by several posts)

When a source will feed more than one post, it goes in `datasets/<slug>/` rather than inside
a post. Rule: a dataset is **tidy, complete, and general** (every year, every region, no
story-specific filtering); a post **filters and summarises** for its own argument. When two
posts need the same derived table, that table moves into the dataset.

### 5.1 Create one

```
/new-dataset housing "Housing starts, prices and rents"
```

Creates:

```
datasets/housing/
  README.md          sources, pipeline table, file list, version log
  R/01_get_data.R    fetches raw inputs into data-raw/
  R/02_clean_data.R  raw to tidy; writes data/
  R/helpers.R        lookups and functions shared by the pipeline and by posts
  R/load.R           one source() call that gives a post every table by name
  data-raw/          gitignored
  data/              tidy files; committed when under 25 MB, else in a release
```

Write the two scripts as in 3.2, then list the objects in `load.R`, one line each with a
comment saying what the table is. Prefer Parquet (`arrow::write_parquet()`) for any table
over a few tens of MB; it is far smaller than `.rds` and posts can read only the columns they
need with `arrow::open_dataset()`.

### 5.2 Use it in a post

One line in the post's setup chunk:

```r
source(here::here("datasets", "housing", "R", "load.R"))
```

The post README notes which dataset it uses and the date or version of the data.

### 5.3 Refresh it

When the source publishes a new year: run `01_get_data.R` and `02_clean_data.R`, add a row to
the README version table, bump the release version if any file lives in a release, then
re-render and re-publish the posts that use it (their Modified date updates).

`datasets/crime/` is the worked example, with eleven Statistics Canada tables, Ontario
Financial Information Returns, and the WRPS occurrence data.

### 5.4 Promote a post's pipeline to a dataset

If a second post wants data that lives inside an earlier post: move that post's `R/` scripts,
`data-raw/`, and `data/` into `datasets/<slug>/`, rewrite the paths, write `load.R`, and point
both posts at it. Claude does this on request; `/new-dataset` describes the steps.

## 6. Large data and GitHub Releases

Git is for code and small files. Anything over 25 MB, and any raw file that cannot be
re-downloaded by script, goes to a GitHub Release: a tagged snapshot of the repository that
carries attached files of up to 2 GB each. Readers download them without an account.

```
/share-data housing-starts data-raw            # a post's raw inputs
/share-data housing data --dataset             # a dataset's big cleaned files
```

Claude uploads the files with `R/data_helpers.R`, adds the matching download call to the
`01_get_data.R` script, adds big cleaned files to `.gitignore`, and updates the README.
Tags follow `data-raw-<slug>-v1` and `data-<slug>-v1` (with `dataset-` inserted for
datasets). When files change, use the next version so old renders stay reproducible.

Uploading needs a GitHub token stored once:

```r
usethis::create_github_token()   # opens GitHub; choose the "repo" scope
gitcreds::gitcreds_set()         # paste the token when asked
```

Downloading needs nothing.

## 7. Shiny apps in a post

The app lives in `posts/<slug>/app/app.R`, is deployed to shinyapps.io with `rsconnect`,
and is embedded in the post with a responsive iframe. `/shiny-post <slug>` walks through
building, testing locally, deploying, and embedding. The app reads only files inside `app/`.

## 8. Changing the look of the site

- Colours and fonts: `custom.scss` (site) and `R/theme_cwr.R` (charts). The two palettes are
  the same three colours; change both or neither.
- Navbar, footer, licence heading, what gets rendered: `_quarto.yml`.
- About page: `about.qmd`. Reader instructions: `reproduce.qmd`.
- Defaults for every post (author name, figure sizes, licence text): `posts/_metadata.yml`.

After any of these, `quarto render` then `/publish` with no argument.

## 9. Licence and disclaimers

- Code is MIT (`LICENSE`); text and charts are CC BY 4.0 (`LICENSE-CONTENT.md`). Every post ends
  with a "Reusing this post" box saying so; the text lives in `posts/_metadata.yml`.
- Data is never relicensed. Each README names the source's own licence.
- WRPS occurrence data requires a disclaimer on any publication. Posts that use it include
  `wrps_disclaimer` (from `datasets/crime/R/helpers.R`) in their Data and methods section;
  `/review-post` checks for it.

## 10. Reproducibility checklist (what a reader needs)

A post is reproducible when someone can clone the repo, run `source("R/packages.R");
install_missing()`, run the post's (or dataset's) `01_get_data.R` and `02_clean_data.R`, and
`quarto render posts/<slug>` to get the same page. That holds when:

- every path goes through `here()`;
- raw data comes back from a script or a release, never from a file only you have;
- the README lists every source with a link and licence;
- no key is needed, or the README says which and how to get one (keys live in `~/.Renviron`,
  read with `Sys.getenv()`, never in code);
- the Reproducibility box at the end of the post prints package versions
  (`cwr_session_info()` does this without exposing local paths).

## 11. Manual command reference

| Task | Command (from the project root) |
|---|---|
| Render one post, drafts visible | `quarto render posts/<slug> --profile draft` |
| Live preview with drafts | `quarto preview --profile draft` |
| Full public render | `quarto render` |
| Publish | `quarto publish gh-pages --no-render --no-prompt` |
| Run an R script | `Rscript posts/<slug>/R/02_clean_data.R` |
| See what changed | `git status` |
| Save to GitHub | `git add -A` then `git commit -m "message"` then `git push origin master` |
| Undo edits to one file since last commit | `git checkout -- path/to/file` |
| Safety check by hand | `bash _dev/check_repo_safety.sh` (after `git add -A`) |
| Rebuild chart templates after editing snippets | `Rscript _dev/build_chart_references.R` |

## 12. Troubleshooting

- **Render fails with a missing file.** The post's `data/` is empty on this machine: run its
  `01_get_data.R` and `02_clean_data.R`, or the dataset's.
- **A chart looks stale after changing data.** Freeze cached the old result. Touch the post
  (any edit to `index.qmd`) and render again; or delete `_freeze/posts/<slug>/` to force a
  full re-run.
- **Safety check blocks a commit.** Read the line; big file → `/share-data`; wrong folder
  staged → `git rm --cached <path>` and add it to `.gitignore`.
- **Publish says the gh-pages branch is missing.** It exists; run `git fetch origin` and try
  again. First-time setup only: Settings → Pages on GitHub must point at the `gh-pages` branch.
- **Push rejected because the remote has changes.** `git pull --rebase origin master`, then
  push again. Ask Claude if it reports a conflict.
- **Charts render in the wrong font.** Install Inter from https://rsms.me/inter/ once;
  `theme_cwr()` picks it up automatically and falls back to the default sans font otherwise.
- **A folder will not delete.** OneDrive holds a lock briefly after files move out of it; try
  again later. Git does not track empty folders, so it does no harm.
- **`quarto preview` port already in use.** Another preview is running; stop it, or add
  `--port 4201`.
- **Token missing when uploading data.** Run the two `usethis` / `gitcreds` lines in section 6.

## 13. Skills at a glance

| Skill | Use it when |
|---|---|
| `/new-post slug "Title"` | starting a post |
| `/new-dataset slug "Title"` | a source will feed several posts |
| `cwr-charts` (automatic) | writing or restyling any chart |
| `/preview [slug]` | seeing a draft in the browser |
| `/review-post slug` | before publishing |
| `/publish [slug]` | taking a post live, or republishing after edits |
| `/share-data slug [data-raw\|data] [--dataset]` | a file is too big for git or cannot be re-downloaded |
| `/shiny-post slug` | building or embedding a Shiny app |
| `/save [note]` | end of a session, or any time you want a backup |
