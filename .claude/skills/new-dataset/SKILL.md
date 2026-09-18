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
- **Carry the quality flags through to `data/`.** The dataset keeps every row, so it cannot say
  which flags matter; a post filters to its own rows and runs `cwr_quality_flags()`
  (`R/data_quality.R`) on them, and that needs each figure's flag and each table's footnotes.
  - Footnotes: `01_get_data.R` saves every table's `get_cansim_table_notes()` to `data-raw/`
    (wrapped in `tryCatch()`, so a table Statistics Canada has withdrawn warns instead of
    stopping the run), and the build copies them into `data/`.
  - Flags, in one of two ways. **Long tables** (one row per figure) keep the `status` and `symbol`
    columns. **Tables pivoted wide by statistic** cannot: a flag belongs to one figure, and the
    pivot puts several figures on a row. Those write every flagged figure to a separate long file
    instead, keyed the way posts filter the dataset. `datasets/crime/R/03_quality_flags.R` is the
    worked example: it reads only the flagged rows of each raw file, writes
    `data/quality_flags.parquet` and `data/quality_notes.csv`, and is sourced at the end of
    `02_clean_data.R`.
  - `load.R` does not load them (a post's cleaning script reads them, not its charts); say where
    they are in a comment there and in the README's script and file tables.
  - A post using a pivoted dataset joins the flags onto the rows it keeps (by the dataset's keys)
    and passes them to `cwr_quality_flags(value_cols = )`, so figures flagged E are blanked like
    any others. Greg never uses a figure flagged E.
- **When Statistics Canada withdraws a table**, remove it everywhere: its download in
  `01_get_data.R`, its read and cleaning in `02_clean_data.R`, its entry in the quality-flags
  sources and footnote list, its line in `load.R`, its output in `data/` (`git rm`, so it stays in
  the history), any notebook line that reads that output, and its rows in the README's Sources and
  Files tables, with a line in the Versions table saying so. Leave the local raw copy in
  `data-raw/` alone: it is not in git and may be the only copy left.
- Nothing obtained by request rather than from an open-data portal goes into a release until its
  terms are confirmed; keep it local and say so in the README.
