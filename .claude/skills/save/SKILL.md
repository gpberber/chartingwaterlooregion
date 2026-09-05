---
name: save
description: Save work to git and GitHub in one step - explains what changed in plain words, runs the safety check, commits with a clear message, and pushes. Use whenever the user says save, commit, back up, or push, or at the end of a working session.
argument-hint: [short note about what changed]
---

# Save (commit and push)

The user is new to git. Treat this as the "save to cloud" button: one command, no branches,
no jargon beyond commit and push.

## Steps

1. `git status --short`. If nothing changed, say so and stop.
2. `git add -A`, then `bash _dev/check_repo_safety.sh`. If it prints BLOCKED, explain the line in
   plain words, fix the cause (usually `git rm --cached <file>` plus a `.gitignore` entry, or
   `/share-data` for a big file), and re-run until it passes. Never commit around a block.
3. Summarise the staged changes in two or three plain sentences: which posts, whether the design
   or config changed, whether `_freeze` (rendered results) changed. Skip file-by-file lists.
4. Commit. Message: first line under 70 characters describing the change (use `$ARGUMENTS` if
   given), a blank line, then one or two lines of detail if useful, then
   `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.
5. `git push origin master`. If rejected because the remote moved, `git pull --rebase origin master`
   then push again. If a conflict appears, stop and explain which file conflicts and what the two
   versions are; do not resolve it silently.
6. Report in one line: saved and pushed, with the commit's first line.

## Never

- `git push --force`, `git reset --hard`, or deleting branches.
- Committing `_site/`, `data-raw/`, background folders, `.Renviron`, or anything the safety check blocks.
- Editing `_freeze/` by hand; it changes only through `quarto render`.
