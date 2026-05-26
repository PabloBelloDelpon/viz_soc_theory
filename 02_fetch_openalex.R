###--- Fetch citation data from OpenAlex
###
### Strategy:
###   1. Fetch every Work authored by each of the 25 theorists. We need the
###      OpenAlex work IDs of their books and articles in order to find
###      citations TO them.
###   2. Fetch every Work whose primary source is one of our 4 journals.
###      OpenAlex returns each work's referenced_works list.
###   3. (Cleaning, in 03_data_cleaning.R) For each journal article, check
###      which of its referenced_works are by which theorist.
###
### Why this shape:
###   - One big query per journal beats one query per (theorist x journal).
###   - Reference data is already on each Work record, so the citation graph
###     is reconstructable in memory without extra calls.
###   - We save raw RDS snapshots so a re-run of cleaning/figures doesn't
###     hit the API again.
###
### Reads:  input_data/openalex_authors.csv  (hand-verified Author IDs)
###         input_data/openalex_sources.csv  (Journal Source IDs from 02a)
### Writes: openalex_data/theorist_works.RDS  (works BY the 25 theorists)
###         openalex_data/journal_works.RDS   (works in the 4 journals)

library(tidyverse)
library(openalexR)

options(openalexR.mailto = "pablo.bello@duke.edu")
dir.create("openalex_data", showWarnings = FALSE)

authors_oa  <- read_csv("input_data/openalex_authors.csv", show_col_types = FALSE)
sources_oa  <- read_csv("input_data/openalex_sources.csv", show_col_types = FALSE)
authors_tbl <- readRDS("input_data/authors_tbl.RDS")

stopifnot(all(!is.na(authors_oa$openalex_id)))
stopifnot(all(!is.na(sources_oa$openalex_id)))


# ----- 1. Theorists' own works ---------------------------------------------
# openalex_authors.csv is a LONG table: one row per (Theorist, openalex_id).
# Historical authors typically have multiple IDs because OpenAlex splits
# translated/transliterated works across clusters. We fetch each ID
# separately and concatenate.

cat("Fetching theorist works (", nrow(authors_oa), " (theorist, ID) rows) ...\n", sep = "")

fetch_one_id <- function(theorist, openalex_id) {
  cat("  ", theorist, "(", openalex_id, ") ... ", sep = "")
  res <- oa_fetch(
    entity = "works",
    authorships.author.id = openalex_id,
    verbose = FALSE
  )
  cat(nrow(res), "works\n")
  if (nrow(res) == 0) return(NULL)
  res |> mutate(Theorist = theorist, theorist_openalex_id = openalex_id)
}

theorist_works <- authors_oa |>
  pmap_dfr(\(Theorist, openalex_id, ...) fetch_one_id(Theorist, openalex_id))

# Some works may be duplicated across IDs (the same work attributed to two
# Author clusters). Dedupe per (Theorist, work_id).
theorist_works <- theorist_works |>
  distinct(Theorist, id, .keep_all = TRUE)

cat("Fetched ", nrow(theorist_works), " unique (Theorist x work) pairs across ",
    n_distinct(theorist_works$id), " distinct works.\n", sep = "")

# Sanity table: are any theorists left with shockingly few works?
cat("\nWorks per theorist (sanity check):\n")
theorist_works |>
  count(Theorist, name = "n_works") |>
  arrange(n_works) |>
  print(n = Inf)

saveRDS(theorist_works, "openalex_data/theorist_works.RDS")
cat("\nSaved", nrow(theorist_works), "rows to openalex_data/theorist_works.RDS\n\n")


# ----- 2. All works in the 4 journals --------------------------------------
# OpenAlex pipe-separates OR values in filters. We fetch per-journal in
# separate calls because: (a) progress is more legible, (b) failures don't
# nuke everything, (c) AJS + ASR + SF + ST is ~30K works and per-journal
# files are easier to re-fetch selectively if anything goes wrong.

cat("Fetching journal works (one source at a time) ...\n")

fetch_one_journal <- function(journal_abb, source_id) {
  cat("  ", journal_abb, "(", source_id, ") ... ", sep = "")
  res <- oa_fetch(
    entity = "works",
    primary_location.source.id = source_id,
    verbose = FALSE
  )
  cat(nrow(res), "works\n")
  res |> mutate(journal_abb = journal_abb, journal_source_id = source_id)
}

journal_works <- sources_oa |>
  pmap_dfr(\(journal_abb, openalex_id, ...) fetch_one_journal(journal_abb, openalex_id))

saveRDS(journal_works, "openalex_data/journal_works.RDS")
cat("Saved", nrow(journal_works), "journal works to openalex_data/journal_works.RDS\n\n")


# ----- 3. Quick coverage summary -------------------------------------------
# How much of our journal corpus has a usable reference list? This is the
# core methodological caveat to report in the manuscript.

cat("Reference-list coverage by journal x decade:\n")
journal_works |>
  mutate(
    decade = (publication_year %/% 10) * 10,
    has_refs = lengths(referenced_works) > 0
  ) |>
  group_by(journal_abb, decade) |>
  summarise(
    n_articles = n(),
    pct_with_refs = round(mean(has_refs) * 100, 1),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = journal_abb, values_from = c(n_articles, pct_with_refs)) |>
  arrange(decade) |>
  print(n = Inf)
