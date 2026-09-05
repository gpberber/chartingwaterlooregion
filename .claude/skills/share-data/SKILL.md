---
name: share-data
description: Move data files that are too big for git (over 25 MB) or raw inputs to a GitHub Release through the GitHub API, and wire the post's download script so readers can fetch them. Use when a post has large data or when the safety check blocks a big file.
argument-hint: <slug> [data-raw|data]
---

# Share data through a GitHub Release

Arguments: `$ARGUMENTS` = post slug and optionally the kind (`data-raw` for raw inputs, `data` for
big cleaned files). Default kind is `data-raw`.

Uses `R/data_helpers.R` (`cwr_data_upload()`, `cwr_data_download()`, `cwr_data_list()`), which
call the GitHub REST API through the `gh` and `httr2` packages. Tags follow `data-raw-<slug>-v<n>` / `data-<slug>-v<n>`.

## Steps

1. List the files in `posts/<slug>/<kind>/` with sizes. Decide with the user which files go to the
   release (raw inputs that cannot be re-downloaded by script; any file over 25 MB).
2. Check for a token: uploading needs a GitHub token with `repo` scope. `gh::gh_token()` reads `GITHUB_PAT`
   from the environment or the git credential store (`gitcreds::gitcreds_get()`). Test with
   `Rscript -e 'gh::gh_token() |> nchar() > 0'`. If there is no token, tell the user to run
   `usethis::create_github_token()` in R, create the token in the browser, then
   `gitcreds::gitcreds_set()` and paste it. Never ask the user to paste the token into the chat.
3. Decide the version: new post → v1. Files changed for a published post → next version, so old
   renders remain reproducible.
4. Upload with a short Rscript:
   ```r
   source(here::here("R", "data_helpers.R"))
   cwr_data_upload("<slug>", kind = "<kind>", files = c("file1.csv", "file2.rds"), version = <n>)
   ```
5. Wire the download: in `posts/<slug>/R/01_get_data.R`, add (or update) the block
   ```r
   source(here::here("R", "data_helpers.R"))
   cwr_data_download("<slug>", kind = "<kind>", version = <n>)
   ```
   For `data-raw` nothing else is needed (folder is gitignored). For `data`, add each big file's
   path to `.gitignore` under the "Large clean files" comment and, if it was tracked,
   `git rm --cached` it.
6. Update the post README data table: file, what it is, source, licence, "GitHub Release <tag>".
7. Verify: in the scratchpad, run the download into a temp folder
   with `cwr_data_list("<slug>", kind = "<kind>")` and confirm file names and sizes match, then
   `cwr_data_download()` into a copy of the post folder in the scratchpad.
8. Run `bash _dev/check_repo_safety.sh` after staging to confirm nothing big remains.
