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
2. `quarto render` (no profile) from the project root. It must finish without errors. Check that
   `_site/posts/<slug>/index.html` exists and that no draft post appears in `_site/posts/`.
3. `git add -A` then `bash _dev/check_repo_safety.sh`. Stop on any BLOCKED line and fix it
   (usually: a big file that needs `/share-data`, or a file that belongs in `.gitignore`).
4. `git status --short`: list what will be committed in plain words (which posts, whether `_freeze`
   changed, config changes). If nothing changed and the site is already published, say so and stop.

## Commit and push

5. Commit with a message like `Publish: <post title>` or `Site: <what changed>`. End the message with
   `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.
6. `git push origin master`. If the push is rejected because the remote has new commits, run
   `git pull --rebase origin master`, then push again. Never force-push.

## Deploy

7. `quarto publish gh-pages --no-render --no-prompt`. This pushes the already-rendered `_site/` to
   the `gh-pages` branch, which GitHub Pages serves. First time only: it creates the branch, and
   the user may need to set Settings → Pages → Source to the `gh-pages` branch on GitHub.
8. Wait about a minute, then open `https://gpberber.github.io/chartingwaterlooregion/posts/<slug>/`
   (or the home page) in the Browser pane. Confirm the page loads, the listing shows the post with
   its thumbnail, and charts display. Screenshot it for the user.

## After

9. Report: the live URL, what was committed, and anything the user should do by hand
   (share the link, enable Pages the first time).
10. If the live site does not update within a few minutes: check the gh-pages branch exists
    (`git branch -r`), and that GitHub Pages is enabled on it. Do not re-run publish repeatedly.
