# data_helpers.R
# ---------------------------------------------------------------------------
# Move data files that are too big for git (over 25 MB), or raw inputs that
# cannot be re-downloaded by script, to and from GitHub Releases. A GitHub
# Release is a tagged snapshot of the repository that can carry attached files
# ("assets") of up to 2 GB each. Readers download them without any account;
# uploading needs a GitHub token.
#
# Talks to the GitHub REST API directly through the gh and httr2 packages.
# (The piggyback package does the same job but its release lookup did not work
# with this account's token, so the few calls needed are written out here.)
#
# Conventions
#   Files belong either to a post (posts/<slug>/) or to a shared dataset
#   (datasets/<slug>/) that several posts draw on. One release tag per folder
#   and data kind:
#     data-raw-<slug>-v1            raw inputs for posts/<slug>/data-raw/
#     data-<slug>-v1                big cleaned files for posts/<slug>/data/
#     data-raw-dataset-<slug>-v1    raw inputs for datasets/<slug>/data-raw/
#     data-dataset-<slug>-v1        big cleaned files for datasets/<slug>/data/
#   Bump the version (v2, v3, ...) when the files change so old renders stay
#   reproducible against the old tag.
#
# Usage from a post's or dataset's R/01_get_data.R (no token needed):
#   source(here::here("R", "data_helpers.R"))
#   cwr_data_download("kitchener-phone-queues", kind = "data-raw")
#   cwr_data_download("crime", kind = "data", root = "datasets")
#
# Usage when publishing (the /share-data skill runs this; needs a token):
#   cwr_data_upload("crime", kind = "data", root = "datasets",
#                   files = c("criminal_incidents.parquet"))
#
# Token: run usethis::create_github_token() once (scope: repo), then
# gitcreds::gitcreds_set() to store it. gh::gh_token() finds it automatically.
# ---------------------------------------------------------------------------

library(here)
library(purrr)
library(stringr)

cwr_repo <- "gpberber/chartingwaterlooregion"

# Build the release tag from slug, data kind, version, and which folder tree
# the files live in ("posts" or "datasets").
cwr_data_tag <- function(slug, kind = c("data-raw", "data"), version = 1,
                         root = c("posts", "datasets")) {
  kind <- match.arg(kind)
  root <- match.arg(root)
  tag_slug <- if (root == "datasets") paste0("dataset-", slug) else slug
  paste0(kind, "-", tag_slug, "-v", version)
}

# All releases on the repo as a list (empty list if none).
cwr_releases <- function() {
  gh::gh("GET /repos/{repo}/releases", repo = cwr_repo, .limit = Inf)
}

# One release by tag, or NULL if it does not exist. Looked up directly by tag
# because the list endpoint can lag a few minutes behind a newly created release.
cwr_release <- function(tag) {
  tryCatch(
    gh::gh("GET /repos/{repo}/releases/tags/{tag}", repo = cwr_repo, tag = tag),
    http_error_404 = \(e) NULL
  )
}

# Newest version number that exists for a slug and kind (NULL if none).
# Probes v1, v2, ... by direct tag lookup until one is missing.
cwr_data_latest_version <- function(slug, kind = c("data-raw", "data"),
                                    root = c("posts", "datasets"), max_version = 20) {
  kind <- match.arg(kind)
  root <- match.arg(root)
  found <- NULL
  for (v in seq_len(max_version)) {
    if (is.null(cwr_release(cwr_data_tag(slug, kind, v, root)))) break
    found <- v
  }
  found
}

# List the files attached to a release: name, size in MB, download URL.
cwr_data_list <- function(slug, kind = c("data-raw", "data"), version = NULL,
                          root = c("posts", "datasets")) {
  kind <- match.arg(kind)
  root <- match.arg(root)
  if (is.null(version)) version <- cwr_data_latest_version(slug, kind, root)
  tag <- cwr_data_tag(slug, kind, version, root)
  release <- cwr_release(tag)
  if (is.null(release)) stop("No release with tag '", tag, "'.")
  tibble::tibble(
    file_name = map_chr(release$assets, "name"),
    size_mb = round(map_dbl(release$assets, "size") / 1024^2, 1),
    url = map_chr(release$assets, "browser_download_url")
  )
}

# Download every file attached to a release into the post's data folder.
# version = NULL means the newest version. Works without a token on a public repo.
# subdir: optional folder inside data-raw/ or data/ to download into, for
# releases whose files live in a subfolder (e.g. "raw_occurrence_data_files").
cwr_data_download <- function(slug, kind = c("data-raw", "data"), version = NULL,
                              root = c("posts", "datasets"), subdir = NULL) {
  kind <- match.arg(kind)
  root <- match.arg(root)
  if (is.null(version)) version <- cwr_data_latest_version(slug, kind, root)
  if (is.null(version)) {
    stop("No release found for ", root, "/", slug, " and kind '", kind, "'.")
  }
  assets <- cwr_data_list(slug, kind, version, root)
  dest <- if (is.null(subdir)) here(root, slug, kind) else here(root, slug, kind, subdir)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  message("Downloading ", nrow(assets), " file(s) from ", cwr_data_tag(slug, kind, version, root), " into ", dest)
  walk2(assets$url, assets$file_name, \(url, name) {
    message("  ", name)
    httr2::request(url) |>
      httr2::req_perform(path = file.path(dest, name))
  })
  invisible(assets)
}

# Upload files from the post's data folder to a release, creating the release
# if needed. Existing assets with the same name are replaced.
cwr_data_upload <- function(slug, files, kind = c("data-raw", "data"), version = NULL,
                            root = c("posts", "datasets")) {
  kind <- match.arg(kind)
  root <- match.arg(root)
  if (is.null(version)) {
    latest <- cwr_data_latest_version(slug, kind, root)
    version <- if (is.null(latest)) 1 else latest
  }
  tag <- cwr_data_tag(slug, kind, version, root)
  paths <- here(root, slug, kind, files)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) stop("Files not found: ", paste(missing, collapse = ", "))

  cwr_release_upload(
    tag = tag,
    paths = paths,
    title = paste0("Data for ", root, "/", slug, " (", kind, ", v", version, ")"),
    body = paste0(
      "Data files for ", root, "/", slug, " in this repository. ",
      "Download from R with cwr_data_download('", slug, "', kind = '", kind,
      "', version = ", version, ", root = '", root, "') after sourcing R/data_helpers.R."
    )
  )
}

# Low-level: attach files to a release (creating it with the given title and
# body if it does not exist yet). Used by cwr_data_upload() and by the data
# bundle builder in R/data_bundle.R.
cwr_release_upload <- function(tag, paths, title = tag, body = "") {
  release <- cwr_release(tag)
  if (is.null(release)) {
    message("Creating release ", tag)
    release <- gh::gh(
      "POST /repos/{repo}/releases",
      repo = cwr_repo,
      tag_name = tag,
      name = title,
      body = body
    )
  }

  # The upload endpoint is a different host from the API; strip the {?name,label} template
  upload_url <- str_remove(release$upload_url, "\\{.*\\}$")
  existing <- map_chr(release$assets, "name")

  # Asset names are plain file names: GitHub turns "/" into "." otherwise. Files
  # from a subfolder are downloaded back into it with cwr_data_download(subdir = ...).
  walk2(paths, basename(paths), \(path, name) {
    if (name %in% existing) {
      message("  replacing ", name)
      asset_id <- release$assets[[which(existing == name)]]$id
      gh::gh("DELETE /repos/{repo}/releases/assets/{id}", repo = cwr_repo, id = asset_id)
    } else {
      message("  uploading ", name, " (", round(file.size(path) / 1024^2, 1), " MB)")
    }
    httr2::request(upload_url) |>
      httr2::req_url_query(name = name) |>
      httr2::req_headers(
        Authorization = paste("token", gh::gh_token()),
        Accept = "application/vnd.github+json",
        `Content-Type` = "application/octet-stream"
      ) |>
      httr2::req_body_file(path) |>
      httr2::req_perform()
  })
  message("Done: ", tag)
  invisible(tag)
}
