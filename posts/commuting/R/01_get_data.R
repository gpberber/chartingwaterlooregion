# 01_get_data.R
# Fetches every raw input for the "Commuting in Waterloo Region" post into data-raw/.
# data-raw/ is gitignored, so this script is how a reader (or future you)
# recreates it. Run from the project root: source("posts/commuting/R/01_get_data.R")

here::i_am("posts/commuting/R/01_get_data.R")

library(tidyverse)
library(here)
library(cansim)   # Statistics Canada table downloads

raw_dir <- here("posts", "commuting", "data-raw")
dir.create(raw_dir, showWarnings = FALSE)

# ---- Commuting, 2021 census -----------------------------------------------
# Table 98-10-0462, "Commuting destination by main mode of commuting, age and
# gender". The dimension that matters is commuting destination, which sorts
# every worker by how far they go: within their own municipality, to another
# municipality in the same census division, to another census division in the
# province, or to another province.
#
# The whole table covers every census subdivision in Canada and is 67 MB
# zipped, so it is cut down as it arrives to Waterloo Region's eight
# geographies: the Region itself (census division 3530) and its seven
# municipalities (census subdivisions 3530xxx), whose codes all start "3530".
# (Statistics Canada's coordinate service could fetch just the cells needed
# instead; a plain download is easier to check.)
get_cansim("98-10-0462") |>
  filter(str_starts(GeoUID, "3530")) |>
  write_csv(file.path(raw_dir, "table_98100462.csv"))

# ---- Commuting flows, 2021 census ------------------------------------------
# Table 98-10-0459, "Commuting flow from geography of residence to geography of
# work": for every municipality people live in, how many work in every
# municipality in Canada. It counts workers with a usual place of work, so
# people who work from home or have no fixed workplace are not in it.
#
# Every place is crossed with every other, so the table is 26 million rows -
# 292 MB zipped, 2.2 GB unzipped - almost all of them zero. It is downloaded to
# a folder in the computer's temporary directory rather than to data-raw/,
# which sits in OneDrive and would otherwise sync a file that is only needed
# while this script runs, and only the rows with one end of the commute in
# Waterloo Region are kept. tempdir() is a fresh folder every time R starts, so its
# parent is used instead: that way a second run finds the file already there.
download_dir <- file.path(dirname(tempdir()), "cwr_downloads")
dir.create(download_dir, showWarnings = FALSE)
flows_zip <- file.path(download_dir, "98100459-eng.zip")
if (!file.exists(flows_zip)) {
  # The default 60-second timeout is too short for a file this size
  options(timeout = 1200)
  download.file(
    "https://www150.statcan.gc.ca/n1/tbl/csv/98100459-eng.zip",
    destfile = flows_zip,
    mode = "wb"
  )
}

# The metadata file, which travels in the same zip, lists every place in both
# dimensions with a member id and - for places of residence - the census
# subdivision code. The two dimensions list the same 5,161 places with the same
# member ids, so this is what turns a place-of-work member id into a code, and
# what says which member ids are the Region's seven municipalities. The file
# stacks several tables of different widths one after another, separated by
# blank lines, so the one wanted is cut out as lines before it is parsed.
metadata_lines <- read_lines(unz(flows_zip, "98100459_MetaData.csv"))
members_start <- str_which(metadata_lines, "^\"Dimension ID\",\"Member Name\"")
members_end <- members_start +
  str_which(metadata_lines[-seq_len(members_start)], "^$")[1] - 1

members <- metadata_lines[members_start:members_end] |>
  I() |>
  read_csv(col_types = cols(.default = col_character()))

write_csv(members, file.path(raw_dir, "table_98100459_members.csv"))

# The seven municipalities' member ids. Every census subdivision code begins
# with its census division code, and Waterloo Region is division 3530.
region_members <- members |>
  filter(`Dimension ID` == "1", str_starts(str_remove_all(`Classification Code`, "[^0-9]"), "3530")) |>
  pull(`Member ID`)

# read_csv_chunked() reads the file a million rows at a time and keeps only the
# rows the callback lets through, so the whole table never has to fit in
# memory. unz() reads the one file wanted straight out of the zip.
#
# Both directions are kept: the rows where somebody lives in the Region, and
# the rows where somebody works in it. The post needs both - where the Region's
# own commuters go, and where the people who work here come from - and one pass
# over 26 million rows is enough for both.
#
# Place of residence is in DGUID, the "dissemination geography unique
# identifier": a prefix saying which geography scheme it is ("2021A0005" means
# a 2021 census subdivision) followed by the census subdivision code.
#
# Place of work arrives as a name ("Guelph (CY), Ont.") rather than a code, so
# it is picked out by member id instead: `Coordinate` is "residence.work", and
# the number after the dot is the place of work's member id.
read_csv_chunked(
  unz(flows_zip, "98100459.csv"),
  callback = DataFrameCallback$new(\(chunk, pos) {
    filter(
      chunk,
      str_starts(DGUID, "2021A00053530") | str_extract(Coordinate, "[0-9]+$") %in% region_members
    )
  }),
  chunk_size = 1e6,
  col_types = cols(.default = col_character())
) |>
  write_csv(file.path(raw_dir, "table_98100459_region.csv"))

# ---- Municipal boundaries --------------------------------------------------
# Statistics Canada's 2021 census subdivision cartographic boundary file, the
# same one the welcome post uses. It covers every municipality in Canada
# (about 150 MB), so like the flows it is downloaded to the temporary folder and
# only the pieces this post draws are kept: the Region's seven municipalities,
# plus every place outside the Region that its residents commute to, so the
# map can mark where those places are.
boundary_zip <- file.path(download_dir, "lcsd000b21a_e.zip")
if (!file.exists(boundary_zip)) {
  options(timeout = 1200)
  download.file(
    "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lcsd000b21a_e.zip",
    destfile = boundary_zip,
    mode = "wb"
  )
}
boundary_dir <- file.path(download_dir, "csd_boundaries")
unzip(boundary_zip, exdir = boundary_dir)

# Every place of work the Region's own commuters travel to, read back from the
# file just written. Its member ids are matched to codes the same way
# 02_clean_data.R does it.
work_codes <- read_csv(
  file.path(raw_dir, "table_98100459_region.csv"),
  col_types = cols(.default = col_character())
) |>
  # Only the rows for people who live here, and only the places at least one of
  # them travels to: the table lists every place of work, including thousands
  # nobody goes to
  filter(str_starts(DGUID, "2021A00053530"), `Gender (3):Total - Gender[1]` != "0") |>
  mutate(member_id = str_extract(Coordinate, "[0-9]+$")) |>
  distinct(member_id) |>
  inner_join(members |> filter(`Dimension ID` == "1"), join_by(member_id == `Member ID`)) |>
  # The code is published in square brackets, "[3523008]"; keep the digits
  mutate(code = str_remove_all(`Classification Code`, "[^0-9]")) |>
  pull(code)

# st_read()'s `query` is passed to GDAL, which filters while it reads, so the
# other 5,000 polygons are never loaded. A geopackage is a single-file spatial
# format, which keeps data-raw/ tidy.
sf::st_read(
  file.path(boundary_dir, "lcsd000b21a_e.shp"),
  query = paste0(
    "SELECT * FROM lcsd000b21a_e WHERE CSDUID IN ('",
    paste(unique(work_codes), collapse = "','"),
    "') OR CSDUID LIKE '3530%'"
  ),
  quiet = TRUE
) |>
  sf::write_sf(file.path(raw_dir, "csd_boundaries.gpkg"), delete_dsn = TRUE)

# ---- Footnotes -------------------------------------------------------------
# Each Statistics Canada table carries footnotes, some of them about the
# quality or comparability of the figures. They are saved beside the tables so
# that 02_clean_data.R can report the ones that apply to the rows this post
# keeps (cwr_quality_flags() in R/data_quality.R) without going back online.
c("98-10-0462", "98-10-0459") |>
  walk(\(table_number) {
    get_cansim_table_notes(table_number) |>
      write_csv(file.path(
        raw_dir,
        str_c("table_", str_remove_all(table_number, "-"), "_notes.csv")
      ))
  })
