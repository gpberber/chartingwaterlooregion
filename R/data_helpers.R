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
#   One release tag per post and data kind:
#     data-raw-<slug>-v1   raw inputs (what goes in posts/<slug>/data-raw/)
#     data-<slug>-v1       cleaned files too big to commit (posts/<slug>/data/)
#   Bump the version (v2, v3, ...) when the files change so old renders stay
#   reproducible against the old tag.
#
# Usage from a post's R/01_get_data.R (no token needed):
#   source(here::here("R", "data_helpers.R"))
#   cwr_data_download("kitchener-school-collisions", kind = "data-raw")
#
# Usage when publishing (the /share-data skill runs this; needs a token):
#   cwr_data_upload("crime", kind = "data", files = c("criminal_incidents.rds"))
#
# Token: run usethis::create_github_token() once (scope: repo), then
# gitcreds::gitcreds_set() to store it. gh::gh_token() finds it automatically.
# ---------------------------------------------------------------------------

library(here)
library(purrr)
library(stringr)

cwr_repo <- "gpberber/chartingwaterlooregion"

# Build the release tag from post slug, data kind and version.
cwr_data_tag <- function(slug, kind = c("data-raw", "data"), version = 1) {
  kind <- match.arg(kind)
  paste0(kind, "-", slug, "-v", version)
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
cwr_data_latest_version <- function(slug, kind = c("data-raw", "data"), max_version = 20) {
  kind <- match.arg(kind)
  found <- NULL
  for (v in seq_len(max_version)) {
    if (is.null(cwr_release(cwr_data_tag(slug, kind, v)))) break
    found <- v
  }
  found
}

# List the files attached to a release: name, size in MB, download URL.
cwr_data_list <- function(slug, kind = c("data-raw", "data"), version = NULL) {
  kind <- match.arg(kind)
  if (is.null(version)) version <- cwr_data_latest_version(slug, kind)
  tag <- cwr_data_tag(slug, kind, version)
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
cwr_data_download <- function(slug, kind = c("data-raw", "data"), version = NULL) {
  kind <- match.arg(kind)
  if (is.null(version)) version <- cwr_data_latest_version(slug, kind)
  if (is.null(version)) {
    stop("No release found for post '", slug, "' and kind '", kind, "'.")
  }
  assets <- cwr_data_list(slug, kind, version)
  dest <- here("posts", slug, kind)
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  message("Downloading ", nrow(assets), " file(s) from ", cwr_data_tag(slug, kind, version), " into ", dest)
  walk2(assets$url, assets$file_name, \(url, name) {
    message("  ", name)
    httr2::request(url) |>
      httr2::req_perform(path = file.path(dest, name))
  })
  invisible(assets)
}

# Upload files from the post's data folder to a release, creating the release
# if needed. Existing assets with the same name are replaced.
cwr_data_upload <- function(slug, files, kind = c("data-raw", "data"), version = NULL) {
  kind <- match.arg(kind)
  if (is.null(version)) {
    latest <- cwr_data_latest_version(slug, kind)
    version <- if (is.null(latest)) 1 else latest
  }
  tag <- cwr_data_tag(slug, kind, version)
  paths <- here("posts", slug, kind, files)
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) stop("Files not found: ", paste(missing, collapse = ", "))

  release <- cwr_release(tag)
  if (is.null(release)) {
    message("Creating release ", tag)
    release <- gh::gh(
      "POST /repos/{repo}/releases",
      repo = cwr_repo,
      tag_name = tag,
      name = paste0("Data for post '", slug, "' (", kind, ", v", version, ")"),
      body = paste0(
        "Data files for https://gpberber.github.io/chartingwaterlooregion/posts/", slug, "/. ",
        "Download from R with cwr_data_download('", slug, "', kind = '", kind,
        "', version = ", version, ") after sourcing R/data_helpers.R."
      )
    )
  }

  # The upload endpoint is a different host from the API; strip the {?name,label} template
  upload_url <- str_remove(release$upload_url, "\\{.*\\}$")
  existing <- map_chr(release$assets, "name")

  walk2(paths, files, \(path, name) {
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
