# seo_post_render.R
# ---------------------------------------------------------------------------
# Search-engine tidying for the built site. Quarto runs this after every render
# (`post-render:` in _quarto.yml), with the project root as the working
# directory. It edits only the built copy in _site/ - never a .qmd, never
# _freeze/ - so it can run any number of times and always lands on the same
# result: everything it adds sits between <!-- cwr-seo --> markers and is
# replaced, not added again, on the next run.
#
# What it does, and why (search engines read all of this; readers see none of it):
#
# 1. Canonical links. Every page says which address is the real one. A post is
#    reachable as /posts/<slug>/ and /posts/<slug>/index.html, and without a
#    canonical link Google has to guess which to rank and splits the credit.
#    The sitemap Quarto writes uses the index.html form, so it is rewritten to
#    the short form too, to match.
# 2. Structured data (JSON-LD, the schema.org vocabulary). A hidden block that
#    tells search engines and AI tools, in a fixed format, that a post is an
#    article, who wrote it, when, and which picture goes with it. The home page
#    gets one describing the site.
# 3. A Dataset block on every post with a data download, so Google Dataset
#    Search lists the post's tables with a link back to the post. Built from
#    the post's data/tables.csv, data/dictionary.csv and README sources table.
# 4. A landscape sharing picture. Link previews (LinkedIn, Facebook, messaging
#    apps, Google Discover) want 1200 x 630; a post's listing thumbnail is
#    usually another shape and gets cropped. The post's first chart (or the one
#    named by `share-figure:` in its YAML) is fitted on a white 1200 x 630
#    canvas as figures/share.png, and the page's preview tags point at it.
#    `share-figure: none` keeps the thumbnail instead.
# 5. Lazy loading. Every picture in the page body after the first loads only as
#    a reader scrolls near it. The first is left alone because it is usually on
#    screen straight away, and delaying it would slow the page down.
# 6. The 404 page is marked noindex so it never turns up in search results.
#
# Nothing here needs a post to change. To run it by hand after a render:
#   Rscript R/seo_post_render.R
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

site_dir <- here("_site")
config <- yaml::read_yaml(here("_quarto.yml"))
site_url <- str_remove(config$website[["site-url"]], "/$")
site_name <- config$website$title
site_description <- config$website$description

# The author and publisher are the same person. The About page is his page.
author <- list(
  `@type` = "Person",
  name = "Greg Berberich",
  url = paste0(site_url, "/about.html")
)

# Everything this script adds to a page goes between these two markers.
marker_start <- "<!-- cwr-seo -->"
marker_end <- "<!-- /cwr-seo -->"

# ---- Helpers -----------------------------------------------------------------

# The content of a <meta> tag in the page, by its name or property attribute.
meta_content <- function(page, key) {
  node <- xml2::xml_find_first(page, str_glue('//meta[@name="{key}" or @property="{key}"]'))
  value <- xml2::xml_attr(node, "content")
  if (is.na(value)) NULL else value
}

# Turns an R list into a JSON-LD <script> tag. "</" is escaped so no string in
# the data (a title, a description) can close the script tag early.
json_ld <- function(data) {
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")
  str_c('<script type="application/ld+json">', str_replace_all(json, fixed("</"), "<\\/"), "</script>")
}

# The address a page should be known by: /posts/welcome/index.html becomes
# /posts/welcome/, and the home page is the bare domain with a slash.
canonical_url <- function(path) {
  short <- str_remove(path, "(^|/)index\\.html$")
  if (short == "") paste0(site_url, "/") else if (str_ends(path, "index.html")) {
    paste0(site_url, "/", short, "/")
  } else {
    paste0(site_url, "/", short)
  }
}

# Reads one markdown table out of a README, under the given ## heading, as a
# tibble of the raw cell text (links left in). NULL if the heading is missing.
readme_table <- function(readme, heading) {
  if (!file.exists(readme)) return(NULL)
  lines <- read_lines(readme)
  start <- which(str_trim(lines) == paste("##", heading))
  if (length(start) == 0) return(NULL)
  after <- lines[(start[[1]] + 1):length(lines)]
  next_heading <- which(str_starts(after, "## "))
  if (length(next_heading) > 0) after <- after[seq_len(next_heading[[1]] - 1)]
  rows <- after[str_starts(str_trim(after), "\\|")]
  if (length(rows) < 3) return(NULL)
  cells <- rows |>
    str_trim() |>
    str_remove("^\\|") |>
    str_remove("\\|$") |>
    str_split("\\|") |>
    map(str_trim)
  header <- cells[[1]]
  cells[-(1:2)] |>   # drop the header and the |---| line
    map(\(row) set_names(as.list(row[seq_along(header)]), header)) |>
    bind_rows()
}

# Every link target in some markdown text.
link_urls <- function(text) {
  str_match_all(text, "\\]\\((https?://[^)\\s]+)\\)") |>
    map(\(m) m[, 2]) |>
    unlist() |>
    unique()
}

# The 1200 x 630 sharing picture: the chart scaled to fit inside a margin, on
# white. The chart PNGs already carry their own title and source line, so
# nothing else is drawn on the canvas. Rebuilt only when the chart is newer.
make_share_image <- function(chart_png, share_png) {
  if (file.exists(share_png) && file.mtime(share_png) >= file.mtime(chart_png)) return(invisible())
  magick::image_read(chart_png) |>
    magick::image_trim() |>                   # drop the chart's own white margin first
    magick::image_scale("1136x566") |>        # then 32 px clear on every side
    magick::image_extent("1200x630", gravity = "center", color = "white") |>
    magick::image_write(share_png, format = "png")
  invisible()
}

# ---- Structured data for a post or a standing page ------------------------------

# Posts live in posts/<slug>/; a standing page such as vital-statistics/ sits at
# the top of the project. Both are a folder holding an index.qmd, a README and a
# data/ folder, so both are described here - a post as a BlogPosting (written
# once, on a date) and a page as a WebPage (kept up to date). Without this a
# standing page going live would carry a canonical link and nothing else.
page_folder <- function(slug) {
  post_dir <- here("posts", slug)
  if (dir.exists(post_dir)) post_dir else here(slug)
}

page_json_ld <- function(page, html, url, slug, is_post) {
  folder <- page_folder(slug)
  qmd <- file.path(folder, "index.qmd")
  yaml <- if (file.exists(qmd)) rmarkdown::yaml_front_matter(qmd) else list()

  # The title without the " – Charting Waterloo Region" Quarto adds for the tab
  title <- meta_content(page, "og:title") |> str_remove(fixed(paste0(" – ", site_name)))
  description <- meta_content(page, "description")
  published <- meta_content(page, "dcterms.date")
  modified <- as.character(yaml$`date-modified` %||% "")
  modified <- if (str_detect(modified, "^\\d{4}-\\d{2}-\\d{2}")) modified else NULL
  categories <- xml2::xml_find_all(page, '//*[contains(@class, "quarto-category")]') |>
    xml2::xml_text() |>
    str_trim() |>
    unique() |>
    setdiff(c("Snapshot", "Deep dive"))   # the post type is not a topic
  image <- meta_content(page, "og:image")

  # A post is an article and takes `headline`; a standing page is a page and
  # takes `name`. A page also has no publication date to give - it is kept up
  # to date rather than written on a day - so it carries a date only when its
  # YAML sets `date-modified:`, and says nothing when it does not.
  article <- compact(list(
    `@type` = if (is_post) "BlogPosting" else "WebPage",
    headline = if (is_post) title else NULL,
    name = if (is_post) NULL else title,
    description = description,
    datePublished = published,
    dateModified = modified %||% published,
    author = author,
    publisher = author,
    image = image,
    url = url,
    mainEntityOfPage = url,
    keywords = if (length(categories) > 0) str_c(categories, collapse = ", ") else NULL,
    inLanguage = "en-CA",
    isAccessibleForFree = TRUE,
    license = "https://creativecommons.org/licenses/by/4.0/"
  ))

  graph <- list(article)

  # A Dataset block when the page links to its data bundle on GitHub Releases
  bundle <- str_match(html, "https://github\\.com/[^\"]+/releases/download/[^\"]+\\.zip")[, 1]
  tables_csv <- file.path(folder, "data", "tables.csv")
  if (!is.na(bundle) && file.exists(tables_csv)) {
    tables <- read_csv(tables_csv, col_types = cols(.default = col_character())) |>
      janitor::clean_names()
    dictionary_csv <- file.path(folder, "data", "dictionary.csv")
    dictionary <- if (file.exists(dictionary_csv)) {
      read_csv(dictionary_csv, col_types = cols(.default = col_character())) |>
        janitor::clean_names() |>
        filter(table %in% tables$table)
    } else {
      tibble(column = character(), description = character(), units = character())
    }
    sources <- readme_table(file.path(folder, "README.md"), "Data sources")
    licence_col <- names(sources)[str_detect(str_to_lower(names(sources)), "licen")][1]
    source_col <- names(sources)[str_detect(str_to_lower(names(sources)), "^source")][1]
    licences <- if (!is.null(sources) && !is.na(licence_col)) link_urls(sources[[licence_col]]) else character()
    if (length(licences) == 0) licences <- unique(na.omit(tables$licence))
    based_on <- if (!is.null(sources) && !is.na(source_col)) link_urls(sources[[source_col]]) else character()

    dataset_description <- str_c(
      "The tables behind the Charting Waterloo Region ", if (is_post) "post" else "page",
      " “", title, "”, cleaned and ",
      "ready to use, in CSV, Parquet and Excel with a data dictionary. ",
      str_c(tables$table, ": ", tables$description, collapse = " ")
    ) |> str_trunc(5000)

    dataset <- compact(list(
      `@type` = "Dataset",
      name = str_c("Data for “", title, "” (Charting Waterloo Region)"),
      description = dataset_description,
      url = url,
      creator = author,
      datePublished = published,
      license = if (length(licences) == 1) licences else if (length(licences) > 1) licences else NULL,
      isBasedOn = if (length(based_on) > 0) based_on else NULL,
      isAccessibleForFree = TRUE,
      keywords = if (length(categories) > 0) categories else NULL,
      spatialCoverage = list(`@type` = "Place", name = "Waterloo Region, Ontario, Canada"),
      variableMeasured = if (nrow(dictionary) > 0) {
        pmap(dictionary, \(...) {
          row <- list(...)
          compact(list(
            `@type` = "PropertyValue",
            name = row$column,
            description = row$description,
            unitText = if (!is.na(row$units %||% NA)) row$units else NULL
          ))
        })
      } else NULL,
      distribution = list(list(
        `@type` = "DataDownload",
        encodingFormat = "application/zip",
        contentUrl = bundle
      ))
    ))
    graph <- append(graph, list(dataset))
  }

  list(`@context` = "https://schema.org", `@graph` = graph)
}

# ---- Each page -------------------------------------------------------------------

pages <- list.files(site_dir, pattern = "\\.html$", recursive = TRUE) |>
  purrr::discard(\(path) str_starts(path, "site_libs/"))

counts <- c(pages = 0, share = 0, dataset = 0)

# Every page that should be in the sitemap, as url = when it was last built.
#
# The date comes from Quarto's own sitemap wherever it has one, because that is
# when the page was last rendered. Reading it back rather than taking the
# file's timestamp matters: this script rewrites every page it tags, so a
# timestamp would move each time the script ran and tell a search engine a page
# had changed when nothing had. A page Quarto has no entry for - the first time
# one is published - falls back to when it was built.
sitemap <- file.path(site_dir, "sitemap.xml")
quarto_dates <- c()
if (file.exists(sitemap)) {
  xml <- read_file(sitemap)
  locs <- str_match_all(xml, "<loc>([^<]*)</loc>\\s*<lastmod>([^<]*)</lastmod>")[[1]]
  if (nrow(locs) > 0) {
    urls <- str_remove(locs[, 2], "index\\.html$")
    quarto_dates <- tapply(locs[, 3], urls, max)   # newest entry wins
  }
}

listed <- c()

walk(pages, function(path) {
  file <- file.path(site_dir, path)
  html <- read_file(file)

  # Drafts leave a tiny placeholder file behind in a public render (see the
  # publish skill); it has no <head> worth tagging, and /publish deletes it.
  if (!str_detect(html, fixed("</head>")) || file.size(file) < 1000) return()

  # Take out whatever an earlier run added, so this run starts clean
  html <- str_remove_all(html, str_c(marker_start, "[\\s\\S]*?", marker_end, "\\n?"))

  page <- xml2::read_html(html)
  url <- canonical_url(path)

  # Two shapes get described in full: a post at posts/<slug>/index.html, and a
  # standing page in a folder of its own at <slug>/index.html (vital-statistics
  # today). The home page is index.html with no folder, so it matches neither
  # and gets its own WebSite block further down.
  is_post <- str_detect(path, "^posts/[^/]+/index\\.html$")
  is_standing_page <- str_detect(path, "^[^/]+/index\\.html$") && !str_starts(path, "posts/")
  slug <- if (is_post) {
    str_match(path, "^posts/([^/]+)/")[, 2]
  } else if (is_standing_page) {
    str_match(path, "^([^/]+)/")[, 2]
  } else {
    NA_character_
  }
  added <- character()

  # A draft rendered with the draft profile carries Quarto's draft banner. It
  # is kept out of the sitemap below, so a preview never writes an unfinished
  # page into the list handed to search engines.
  is_draft <- str_detect(html, fixed("quarto-draft-alert"))

  if (path == "404.html") {
    added <- '<meta name="robots" content="noindex">'
  } else {
    added <- str_c('<link rel="canonical" href="', url, '">')
    if (!is_draft) {
      known <- unname(quarto_dates[url])
      built <- format(as.POSIXct(file.mtime(file), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
      listed <<- c(listed, set_names(if (is.na(known)) built else known, url))
    }
  }

  # 4. The sharing picture, before the structured data so both use it
  if (is_post || is_standing_page) {
    built_dir <- str_remove(path, "index\\.html$")     # "posts/welcome/" or "vital-statistics/"
    qmd <- file.path(page_folder(slug), "index.qmd")
    choice <- if (file.exists(qmd)) rmarkdown::yaml_front_matter(qmd)$`share-figure` else NULL
    if (!identical(choice, "none")) {
      # A page drawn entirely with cwr_interactive() has no PNG to use, and
      # keeps whatever its `image:` names - the site card, usually.
      charts <- str_match_all(html, 'src="figures/(fig-[^"]+?)\\.png"')[[1]][, 2]
      charts <- charts[!str_ends(charts, "-phone")]
      figure <- choice %||% charts[1]
      chart_png <- file.path(site_dir, built_dir, "figures", paste0(figure, ".png"))
      if (length(figure) == 1 && !is.na(figure) && file.exists(chart_png)) {
        share_png <- file.path(site_dir, built_dir, "figures", "share.png")
        make_share_image(chart_png, share_png)
        share_url <- str_c(site_url, "/", built_dir, "figures/share.png")
        # Quarto writes og: tags with property= and twitter: tags with name=.
        # The lookbehinds match the text before each value without capturing it.
        html <- html |>
          str_replace('(?<=<meta property="og:image" content=")[^"]*', share_url) |>
          str_replace('(?<=<meta name="twitter:image" content=")[^"]*', share_url) |>
          str_replace('(?<=<meta property="og:image:width" content=")[^"]*', "1200") |>
          str_replace('(?<=<meta property="og:image:height" content=")[^"]*', "630") |>
          str_replace('(?<=<meta name="twitter:image-width" content=")[^"]*', "1200") |>
          str_replace('(?<=<meta name="twitter:image-height" content=")[^"]*', "630")
        page <- xml2::read_html(html)
        counts[["share"]] <<- counts[["share"]] + 1
      } else if (!is.null(choice)) {
        warning("share-figure: ", choice, " in ", built_dir, " has no PNG in figures/; ",
                "the page keeps its thumbnail as its sharing picture.", call. = FALSE)
      }
    }
  }

  # 2 and 3. Structured data
  if (is_post || is_standing_page) {
    data <- page_json_ld(page, html, url, slug, is_post)
    if (length(data$`@graph`) > 1) counts[["dataset"]] <<- counts[["dataset"]] + 1
    added <- c(added, json_ld(data))
  } else if (path == "index.html") {
    added <- c(added, json_ld(list(
      `@context` = "https://schema.org",
      `@type` = "WebSite",
      name = site_name,
      description = site_description,
      url = url,
      inLanguage = "en-CA",
      author = author,
      publisher = author
    )))
  }

  block <- str_c(marker_start, "\n", str_c(added, collapse = "\n"), "\n", marker_end, "\n")
  html <- str_replace(html, fixed("</head>"), str_c(block, "</head>"))

  # 5. Lazy loading for every <img> in the body after the first. An <img> that
  # already says how to load is left as it is, which also makes this safe to
  # run twice.
  main_start <- str_locate(html, "<main")[1, "start"]
  if (!is.na(main_start)) {
    body <- str_sub(html, main_start)
    tags <- str_locate_all(body, "<img\\b[^>]*>")[[1]]
    if (nrow(tags) > 1) {
      # From the last tag back to the second, so earlier positions stay valid
      for (i in rev(seq_len(nrow(tags))[-1])) {
        tag <- str_sub(body, tags[i, "start"], tags[i, "end"])
        if (!str_detect(tag, "\\bloading=")) {
          str_sub(body, tags[i, "start"], tags[i, "start"] + 3) <- '<img loading="lazy" decoding="async"'
        }
      }
      html <- str_c(str_sub(html, 1, main_start - 1), body)
    }
  }

  write_file(html, file)
  counts[["pages"]] <<- counts[["pages"]] + 1
})

# 1, continued. The sitemap is written out fresh from the pages tagged above,
# so it holds exactly the addresses the canonical links name - each once, with
# no draft and no 404 page.
#
# It used to be a search and replace over Quarto's own file, turning
# ".../index.html" into ".../". That left Quarto unable to recognise its own
# entries afterwards, and a local sitemap grew to 273 entries for 10 pages, the
# home page 149 times over, with a draft among them. A full render rewrites the
# file from scratch, so the published sitemap was never wrong - but it only
# takes one publish that skips the full render for it to be. Writing the list
# here rather than editing it takes that possibility away: whatever Quarto
# leaves behind, what ships is this.
if (length(listed) > 0) {
  entries <- listed[!duplicated(names(listed))]
  entries <- entries[order(names(entries))]
  str_c(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n',
    str_c(
      "  <url>\n    <loc>", names(entries), "</loc>\n    <lastmod>", entries,
      "</lastmod>\n  </url>",
      collapse = "\n"
    ),
    "\n</urlset>\n"
  ) |>
    write_file(sitemap)
}

message(str_glue(
  "seo_post_render.R: {counts[['pages']]} pages tagged, ",
  "{counts[['share']]} sharing pictures, {counts[['dataset']]} dataset blocks, ",
  "{length(unique(names(listed)))} in the sitemap."
))
