---
name: new-post
description: Scaffold a new blog post folder from the template - use when starting a post. Creates posts/<slug>/ with index.qmd, README, R scripts, data folders, and the background-reading folder outside the repo.
argument-hint: <slug> "Post title"
---

# New post

Arguments: `$ARGUMENTS` = a slug and a title, e.g. `/new-post housing-starts "Housing starts in Waterloo Region"`.
If either is missing, ask for it. The slug is lowercase, hyphenated, and becomes the URL:
`https://chartingwaterlooregion.ca/posts/<slug>/`.

## Steps

1. Refuse if `posts/<slug>/` already exists.
2. Copy `_templates/post/` to `posts/<slug>/` (index.qmd, README.md, R/01_get_data.R, R/02_clean_data.R).
3. Create empty folders `posts/<slug>/data-raw/`, `posts/<slug>/data/`, `posts/<slug>/images/`
   (add a `.gitkeep` to `data/` and `images/` so git keeps them; `data-raw/` is gitignored).
4. Create the background-reading folder outside the repo:
   `../chartingwaterlooregion-background/<slug>/` (PDFs and articles go there, never in the repo).
5. Fill the placeholders in the copied files: `{{slug}}`, `{{title}}`, `{{date}}` (today, ISO),
   the `here::i_am()` path.
6. Leave `draft: true` in the YAML. It stays true until `/publish` flips it.
7. Ask whether the post draws on a shared dataset (list the folders in `datasets/`). If yes, add
   `source(here::here("datasets", "<dataset>", "R", "load.R"))` to the setup chunk, note the dataset
   version in the README, and keep the post's own `R/` scripts for story-specific summaries only.
8. Ask whether the post is a **Snapshot** (a few simple charts and basic analysis, read in under
   5 minutes) or a **Deep dive** (longer, more rigorous). That word goes first in `categories`,
   followed by 2 to 3 topic categories (existing ones are listed on the home page; reuse where
   possible). Ask for a one-sentence `description` for the listing card; fill them in.
9. Do not write any of the post. The scaffold's headings, placeholder comments and stock sections
   are left exactly as copied; the prose is Greg's. Adding a chart later means adding that chunk
   alone - and the post's first chart goes IN the template's `first` chunk, replacing its
   placeholder code and renaming its label and id, never in a new chunk beside it (see the
   `cwr-charts` skill, "Scope"). Later charts are new chunks after it. Report what was created and the next steps: put raw data in `data-raw/` (or write
   `R/01_get_data.R` to fetch it - for Ontario municipal census populations 1996-2021, `gt::towny` is
   already installed; see `WORKFLOW.md` section 3.2 for how to use it and its traps), write
   `R/02_clean_data.R` to produce small tidy files in `data/`, running `cwr_quality_flags()` on the
   rows it keeps from each source (see the template; `01_get_data.R` saves each Statistics Canada
   table's footnotes for it) and telling Greg about every flag and footnote it reports before any
   chart uses that data,
   draft the README's `## Key terms` table once the charts exist: only terms used in a chart's
   title, subtitle or main body (never statistical or census geography terms, never a term found
   only in a note or in the prose), each with the source's own definition
   (table notes or categories, census dictionary, reference guides; source in brackets, empty
   Definition cell for a term with no official definition). Before writing any chart note, check
   the source's definition of who is counted. Then
   fill `data/tables.csv` (including its `sample` column: the sample, or "None (full count)") and
   `data/dictionary.csv` for every table the post reads (these drive the
   reader download bundle and the generated `## Data dictionary` section of the post's README, which
   is where the dictionary appears - never in the post itself; Claude can draft the descriptions
   from the cleaning script), then write the post using the `cwr-charts` skill. Preview with `/preview <slug>`.

## Rules to remind the user of

- Raw data never goes in git. If it cannot be re-downloaded by script, it goes to a GitHub
  Release with `/share-data`.
- Any file over 25 MB in `data/` also goes to a release.
- No keys in code; `Sys.getenv("NAME")` reads them from `~/.Renviron`.
- Thumbnail: copy the phone version of the post's best chart (`figures/fig-<name>-phone.png`,
  made by `cwr_figure()`) to `images/thumbnail.png`; its larger text reads at listing-card size.
