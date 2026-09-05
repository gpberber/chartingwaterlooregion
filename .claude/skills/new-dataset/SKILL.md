---
name: new-dataset
description: Scaffold a shared dataset folder under datasets/<slug>/ that several posts can draw on - get-data and clean scripts, a load.R for posts, README, data folders. Use when a data source will feed more than one post, or when a post's data pipeline should be promoted to a dataset.
argument-hint: <slug> "Dataset title"
---

# New dataset

Arguments: `$ARGUMENTS` = a slug and a title, e.g. `/new-dataset housing "Housing starts and prices"`.
Datasets are the shared data layer: tidy, complete, general-purpose tables that posts filter
and summarise for their own story. See `datasets/crime/` for the worked example.

## Steps

1. Refuse if `datasets/<slug>/` already exists.
2. Copy `_templates/dataset/` to `datasets/<slug>/` (README.md, R/01_get_data.R, R/02_clean_data.R,
   R/helpers.R, R/load.R). Create `data-raw/` (gitignored) and `data/` with a `.gitkeep`.
3. Fill the placeholders `{{slug}}`, `{{title}}`, `{{date}}`, and `{{slug_snake}}` (the slug with
   hyphens replaced by underscores, used for R object names in `load.R`).
4. Ask the user what the sources are and fill the README source table (name, URL, licence).
5. Explain the two-layer rule in one paragraph: the dataset holds every year and every region with
   no story-specific filtering; posts do the filtering. A derived table needed by two posts moves
   into `02_clean_data.R`.
6. Report next steps: write `01_get_data.R`, then `02_clean_data.R`, then list the objects in
   `load.R`. Big clean files (over 25 MB) go to a release with `/share-data <slug> data --dataset`.

## Promoting a post pipeline to a dataset

When a post's `R/` scripts should become shared:
- Move `01_get_data.R` and `02_clean_data.R` into `datasets/<slug>/R/`, move `data-raw/` and
  `data/` alongside, and rewrite paths from `"posts", "<post>"` to `"datasets", "<slug>"`.
- Write `load.R` with the objects posts need; the post's setup chunk sources it.
- Any file over 25 MB in `data/` gets an explicit `.gitignore` line and a release.
- Update the post README to point at the dataset README for sources.

## Conventions

- Prefer Parquet (`arrow::write_parquet()`) for tables over a few tens of MB so posts can read
  only the columns and rows they need with `open_dataset()`.
- Release tags: `data-raw-dataset-<slug>-v<n>` and `data-dataset-<slug>-v<n>`. Bump the version
  when the data changes; note it in the README version table. Posts record which version they used.
- Nothing obtained by request rather than from an open-data portal goes into a release until its
  terms are confirmed; keep it local and say so in the README.
