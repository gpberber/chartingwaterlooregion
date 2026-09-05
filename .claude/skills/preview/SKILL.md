---
name: preview
description: Render a post (or the whole site) including drafts and open it in the Browser pane to check layout, charts, and mobile width. Use after writing or changing a post, or when the user asks to see how something looks.
argument-hint: [slug]
---

# Preview

Argument: `$ARGUMENTS` = a post slug (optional). No argument means the whole site.

## Steps

1. **Render first, so errors surface in the terminal, not the browser.**
   - One post: `quarto render posts/<slug> --profile draft`
   - Whole site: `quarto render --profile draft`
   Run from the project root with the Bash tool. The `draft` profile makes `draft: true` posts
   visible; the public site never uses it.
2. If rendering fails, read the error, open the chunk it names, fix the cause, and re-render.
   Common causes: a data file missing from `data/` (run the post's `R/01_get_data.R` or
   `R/02_clean_data.R`), a package not loaded, a `here()` path wrong after a move.
3. Start the preview server with the Browser pane: `preview_start` with name `quarto-preview`
   (defined in `.claude/launch.json`; it runs `quarto preview --profile draft` on port 4200).
   If it is already running, reuse it.
4. Navigate to the page: `http://localhost:4200/posts/<slug>/` (or `/` for the listing).
5. Take a screenshot at desktop width and one at the `mobile` preset. Look for: charts that
   overflow, labels clipped at panel edges, thumbnails missing on the listing, captions
   missing, headings out of order, TOC clutter.
6. Report what you saw with the screenshot(s) sent via `SendUserFile`, plus a short list of
   anything that needs fixing. Fix chart-level issues with the `cwr-charts` iteration loop.
7. Reset the viewport to `desktop` when done.

## Notes

- `quarto preview` watches files and re-renders on save; leave it running while iterating.
- Rendering writes to `_freeze/` (committed) and `_site/` (ignored). Never edit either by hand.
- A post that renders under the draft profile but is absent from a plain `quarto render` is
  behaving correctly: it is still a draft.
