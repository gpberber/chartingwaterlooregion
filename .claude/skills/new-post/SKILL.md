---
name: new-post
description: Scaffold a new blog post folder from the template - use when starting a post. Creates posts/<slug>/ with index.qmd, README, R scripts, data folders, and the background-reading folder outside the repo.
argument-hint: <slug> "Post title"
---

# New post

Arguments: `$ARGUMENTS` = a slug and a title, e.g. `/new-post housing-starts "Housing starts in Waterloo Region"`.
If either is missing, ask for it. The slug is lowercase, hyphenated, and becomes the URL:
`https://gpberber.github.io/chartingwaterlooregion/posts/<slug>/`.

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
8. Ask the user for 2 to 4 categories (existing ones are listed on the home page; reuse where possible)
   and a one-sentence `description` for the listing card; fill them in.
9. Report what was created and the three next steps: put raw data in `data-raw/` (or write
   `R/01_get_data.R` to fetch it), write `R/02_clean_data.R` to produce small tidy files in `data/`,
   then write the post using the `cwr-charts` skill. Preview with `/preview <slug>`.

## Rules to remind the user of

- Raw data never goes in git. If it cannot be re-downloaded by script, it goes to a GitHub
  Release with `/share-data`.
- Any file over 25 MB in `data/` also goes to a release.
- No keys in code; `Sys.getenv("NAME")` reads them from `~/.Renviron`.
- Thumbnail: save the best chart of the post as `images/thumbnail.png` (4:3 works best).
