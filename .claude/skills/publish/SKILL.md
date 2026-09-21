---
name: publish
description: Publish the site to GitHub Pages - flips a post from draft, runs pre-flight checks (full render, safety scan, clean git state), commits, pushes, and deploys with quarto publish gh-pages, then verifies the live page. Use when the user wants a post to go live.
argument-hint: [slug]
---

# Publish

Argument: `$ARGUMENTS` = the slug of the post going live (optional; no argument republishes the
site as it is, e.g. after fixing a typo or changing the design).

Explain each step in one plain sentence as you go; the user is new to git and publishing.

## Pre-flight

1. If a slug is given: confirm with the user that `posts/<slug>` should go public, then set
   `draft: false` in its YAML. Suggest running `/review-post <slug>` first if it has not been done.
   **Dates.** A post's `date:` is its publication date, so set it to today when flipping
   `draft: false` - whatever date the scaffold put there is the day drafting started, not the day
   it goes live - and make sure the post has no `date-modified:` line, so no "Modified" date shows
   on a post nobody has revised yet. When republishing a post that is **already live** and whose
   `index.qmd` changed, leave `date:` alone and set `date-modified:` to today in its YAML instead;
   ask first if the change is cosmetic, since it puts a "Modified" date on the post for readers.
   (`posts/_metadata.yml` deliberately sets no site-wide `date-modified`.)
2. Build and upload the data bundle for the post (skip when publishing with no slug and no data
   changed). Read `data_bundle_version` from the post's setup chunk; if the tables changed since
   the last bundle, bump that number in the post first so old links keep pointing at old data.
   Then run, via a scratchpad script with `Rscript`:
   ```r
   source(here::here("R", "data_bundle.R"))
   cwr_dictionary_check("<slug>")               # stops if any column is undocumented
   cwr_data_bundle("<slug>", version = <n>)     # zip with CSV, Parquet, Excel, dictionary, README
   ```
   It attaches `<slug>-data-v<n>.zip` to release `data-<slug>-v<n>` and prints the URL the post
   links to. It also rewrites the `## Data dictionary` section of `posts/<slug>/README.md` from
   the same data, so that section shows up as a change to commit in step 6; that is expected.
   The dictionary is never printed in the post itself - README and zip only. Requires `data/tables.csv` and `data/dictionary.csv` to be filled in (see
   `R/data_bundle.R`); if they are not, stop and ask the user to complete them (Claude can
   draft descriptions from the cleaning script for the user to check).
3. Stop any running preview server (it renders with the draft profile and writes draft posts,
   and a listing that includes them, into `_site/`). **First render each post going live on its
   own** - `quarto render posts/<slug>` - and likewise any live post whose `README.md` changed since
   it was last rendered (`git diff --name-only` since the last publish). A full render reuses each
   post's saved results in `_freeze/`, which only refresh when the `.qmd` changes, so without this a
   README edit would never reach the post: its Sources and Reliability tables would go out stale,
   and the check that every data-quality flag has a Reliability row would never run. Rendering a
   single post always re-runs its code, so both are current. It must finish without errors; a
   stop from `cwr_reliability_table()` means a flag in `data/quality_flags.csv` has no row, and
   is fixed in the README, never worked around. Then `quarto render` (no profile) from the project
   root. It must finish without errors. Check that `_site/posts/<slug>/index.html`
   exists, that `_site/index.html` does not mention any draft slug, and that no draft post
   appears in `_site/posts/`; a public render does not delete a draft's folder left behind by
   an earlier draft render, so `rm -rf _site/posts/<draft-slug>` for any that remain. Never
   start the preview server between this render and step 8. Also confirm `_site/CNAME` exists and
   contains `chartingwaterlooregion.ca`; without it the publish would strip the custom domain off
   the `gh-pages` branch and the site would drop back to the github.io address.
   Also check that `_includes/analytics.html` no longer contains `YOURCODE`: that is the GoatCounter
   placeholder, and publishing with it means the site counts nothing. If it is still there, stop and
   ask the user for the code from their goatcounter.com dashboard address.

   Check that the search tidying ran (`R/seo_post_render.R`, run by Quarto after every render):
   the render output ends with a `seo_post_render.R: ... pages tagged` line, and
   `grep -c 'rel="canonical"' _site/posts/<slug>/index.html` prints 1. If not, run
   `Rscript R/seo_post_render.R` and read its error. Never publish without it: the sitemap would
   go back to listing `index.html` addresses.

   Then check for links whose target is percent-encoded R code:
   ```bash
   grep -rn 'href="[^"]*%60\|src="[^"]*%60' _site --include=*.html
   ```
   Nothing found is a pass. A hit means a `` `r ... ` `` expression ended up in a link or image
   target and pandoc encoded it, so the link points at the literal text of the code and goes
   nowhere. `%60` is a backtick, which never belongs in a URL. This happens on its own: RStudio's
   visual editor rewrites the whole document through pandoc every time it saves, and a target
   that is not a valid URL comes back encoded. It shipped once before anyone noticed, because a
   broken link still renders as a link and Quarto reports no error.
   **Do not fix this in the rendered `_site/` copy** - that is a build output and the next render
   would undo it. Fix the `.qmd`, re-render, and run the check again. The fix is to move the R out
   of the target so it sits in an ordinary inline span: have R return the finished markdown link,
   the way `cwr_bundle_link()` in `R/data_bundle.R` does for the data download.
4. `git add -A` then `bash _dev/check_repo_safety.sh`. Stop on any BLOCKED line and fix it
   (usually: a big file that needs `/share-data`, or a file that belongs in `.gitignore`).
5. `git status --short`: list what will be committed in plain words (which posts, whether `_freeze`
   changed, config changes). If nothing changed and the site is already published, say so and stop.

## Commit and push

6. Commit with a message like `Publish: <post title>` or `Site: <what changed>`. End the message with
   a `Co-Authored-By:` line naming **the model that did the work**, not a fixed name - for example
   `Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>`.
7. `git push origin master`. If the push is rejected because the remote has new commits, run
   `git pull --rebase origin master`, then push again. Never force-push.

## Deploy

8. `quarto publish gh-pages --no-render --no-prompt`. This pushes the already-rendered `_site/` to
   the `gh-pages` branch, which GitHub Pages serves. First time only: it creates the branch, and
   the user may need to set Settings → Pages → Source to the `gh-pages` branch on GitHub.
9. Wait about a minute, then open `https://chartingwaterlooregion.ca/posts/<slug>/`
   (or the home page) in the Browser pane. Confirm the page loads, the listing shows the post with
   its thumbnail, and charts display. Screenshot it for the user.

## After

10. Report: the live URL, the data bundle URL, what was committed, and anything the user should do by hand
   (share the link, enable Pages the first time).
11. If the live site does not update within a few minutes: check the gh-pages branch exists
    (`git branch -r`), and that GitHub Pages is enabled on it. Do not re-run publish repeatedly.
