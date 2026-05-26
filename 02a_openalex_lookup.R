###--- Discover OpenAlex IDs for the 25 theorists and 4 journals
###
### v2 (after discovering that historical theorists have works split across
### many OpenAlex Author IDs): surfaces top 25 candidates per theorist and
### enriches each with sample work titles so the user can hand-pick MULTIPLE
### IDs per theorist.
###
### Outputs:
###   input_data/openalex_sources.csv             (journals, by ISSN)
###   input_data/openalex_author_candidates.csv   (top 25 per theorist, with
###                                                sample-work titles)
###   input_data/openalex_authors_TEMPLATE.csv    (long format: Theorist,
###                                                openalex_id, one row per
###                                                included ID)
###
### Manual step after running:
###   Open openalex_author_candidates.csv.
###   For each theorist: pick ALL rows that genuinely refer to them.
###   For historical authors with translations (Marx, Weber, Durkheim,
###   Du Bois) expect 2-5 IDs each. For contemporary authors (Bourdieu,
###   Blau, R. Collins) expect 1 ID.
###   Copy the verified rows into input_data/openalex_authors.csv with
###   columns Theorist, openalex_id, note.

library(tidyverse)
library(openalexR)

options(openalexR.mailto = "pablo.bello@duke.edu")

authors_tbl  <- readRDS("input_data/authors_tbl.RDS")


# ----- 1. Journal Sources by ISSN ------------------------------------------
# (Unchanged from v1; kept here so the whole lookup is one script.)

journal_issn <- tribble(
  ~journal_abb, ~query_issn,
  "AJS",        "0002-9602",
  "ASR",        "0003-1224",
  "ST",         "0735-2751",
  "SF",         "0037-7732"
)

pluck1 <- function(tbl, col, default = NA) {
  if (is.null(tbl) || nrow(tbl) == 0) return(default)
  if (!col %in% names(tbl))            return(default)
  v <- tbl[[col]][[1]]
  if (length(v) == 0)                  return(default)
  v
}

cat("Looking up journal Sources by ISSN ...\n")
sources_found <- journal_issn |>
  mutate(
    src = map(query_issn,
              \(x) oa_fetch(entity = "sources", issn = x, verbose = FALSE))
  ) |>
  mutate(
    openalex_id            = map_chr(src, pluck1, "id",           default = NA_character_),
    display_name           = map_chr(src, pluck1, "display_name", default = NA_character_),
    works_count            = map_int(src, \(x) as.integer(pluck1(x, "works_count", default = NA_integer_))),
    host_organization_name = map_chr(src, \(x) {
      for (col in c("host_organization_name", "host_organization",
                    "host_organization_lineage_names")) {
        v <- pluck1(x, col, default = NA_character_)
        if (!is.na(v)) return(as.character(v))
      }
      NA_character_
    })
  ) |>
  select(journal_abb, query_issn, openalex_id, display_name,
         works_count, host_organization_name)

print(sources_found)
write_csv(sources_found, "input_data/openalex_sources.csv")
cat("Wrote input_data/openalex_sources.csv\n\n")


# ----- 2. Author candidates: top 25 per theorist ---------------------------
# Pull a wider net than v1 (5 -> 25) so that secondary clusters for
# historical authors are visible. Annotate each candidate with:
#   - works_count, cited_by_count          (size of the cluster)
#   - last_known_institution               (contemporary affiliation, if any)
#   - top_concepts                         (subject matter)
#   - sample_titles                        (3 top-cited work titles)

cat("Looking up author candidates (25 per theorist, with sample works) ...\n")

# Safe helpers ---------------------------------------------------------------

pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

extract_institution <- function(x) {
  if (is.null(x)) return(NA_character_)
  try_paths <- list(
    \() x[[1]]$institution$display_name,
    \() x[[1]]$display_name,
    \() x$institution$display_name[1],
    \() x$display_name[1]
  )
  for (f in try_paths) {
    v <- tryCatch(f(), error = function(e) NULL)
    if (!is.null(v) && length(v) > 0 && !is.na(v[[1]])) return(as.character(v[[1]]))
  }
  NA_character_
}

extract_concepts <- function(x) {
  if (is.null(x)) return(NA_character_)
  for (col in c("display_name", "name")) {
    v <- tryCatch(x[[col]], error = function(e) NULL)
    if (!is.null(v) && length(v) > 0) {
      return(paste(head(as.character(v), 3), collapse = "; "))
    }
  }
  NA_character_
}

# Sample-work titles: for a given Author ID, fetch top 3 most-cited works
# and return their concatenated titles (truncated). This is the most
# information-dense disambiguator we can get without manual link-clicking.
sample_titles <- function(author_id, k = 3) {
  works <- tryCatch(
    oa_fetch(
      entity = "works",
      authorships.author.id = author_id,
      options = list(sort = "cited_by_count:desc"),
      per_page = k,
      verbose = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(works) || nrow(works) == 0) return(NA_character_)
  titles <- works$title %||% works$display_name %||% character(0)
  if (length(titles) == 0) return(NA_character_)
  paste(head(titles, k), collapse = " | ") |> substr(1, 250)
}


# Fetch candidates -----------------------------------------------------------

fetch_candidates <- function(name, k = 25) {
  res <- tryCatch(
    oa_fetch(entity = "authors", search = name, per_page = k, verbose = FALSE),
    error = function(e) { message("  ", name, ": ", conditionMessage(e)); NULL }
  )
  if (is.null(res) || nrow(res) == 0) return(NULL)
  res |> head(k) |> mutate(query = name, rank = row_number())
}

candidates <- map_dfr(authors_tbl$Theorist, function(name) {
  cat("  ", name, "...\n")
  fetch_candidates(name, k = 25)
})

# Slim and enrich ------------------------------------------------------------

concepts_col <- pick_col(candidates, c("topics", "x_concepts"))
affil_col    <- pick_col(candidates, c("affiliations",
                                       "last_known_institutions",
                                       "last_known_institution"))

cat("\nFetching sample-work titles for each candidate (this is slow) ...\n")

candidates_slim <- candidates |>
  transmute(
    query,
    rank,
    openalex_id            = id,
    display_name,
    works_count,
    cited_by_count,
    orcid                  = if (pick_col(candidates, "orcid") |> is.na() |> isTRUE()) NA_character_ else orcid,
    last_known_institution = if (!is.na(affil_col))    map_chr(.data[[affil_col]],    extract_institution) else NA_character_,
    top_concepts           = if (!is.na(concepts_col)) map_chr(.data[[concepts_col]], extract_concepts)    else NA_character_
  )

# Only fetch sample titles for non-trivial candidates (works_count >= 3).
# Cuts ~25% of the API calls.
needs_sample <- candidates_slim$works_count >= 3 & !is.na(candidates_slim$openalex_id)

candidates_slim$sample_titles <- NA_character_
candidates_slim$sample_titles[needs_sample] <-
  map_chr(candidates_slim$openalex_id[needs_sample], sample_titles)

print(candidates_slim, n = 50)
write_csv(candidates_slim, "input_data/openalex_author_candidates.csv")
cat("\nWrote input_data/openalex_author_candidates.csv\n")


# ----- 3. Template for the multi-ID author file ---------------------------
# openalex_authors.csv is now a LONG table: one row per (Theorist, ID).
# The fetch script will combine works across IDs per theorist.

template <- authors_tbl |>
  select(Theorist) |>
  mutate(openalex_id = NA_character_,
         note        = NA_character_)

write_csv(template, "input_data/openalex_authors_TEMPLATE.csv")
cat("Wrote template:  input_data/openalex_authors_TEMPLATE.csv\n")
cat("Format: long. One row per (Theorist, openalex_id). Multiple rows per\n")
cat("theorist OK -- works are merged downstream.\n")
