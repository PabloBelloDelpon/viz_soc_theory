###--- Build the citation graph from OpenAlex snapshots
###
### Reads:    openalex_data/theorist_works.RDS  (works BY the 25 theorists)
###           openalex_data/journal_works.RDS   (works IN the 4 journals)
###           input_data/authors_tbl.RDS        (theorist metadata)
###
### Writes:   final_data/citations_long.RDS     (one row per article x cited
###                                              theorist; the analysis unit
###                                              for both the line chart and
###                                              the co-citation network)
###           final_data/citation_counts.RDS    (pre-aggregated theorist x
###                                              year x journal counts)

suppressMessages(library(tidyverse))

theorist_works <- readRDS("openalex_data/theorist_works.RDS")
journal_works  <- readRDS("openalex_data/journal_works.RDS")
authors_tbl    <- readRDS("input_data/authors_tbl.RDS")

dir.create("final_data", showWarnings = FALSE)


# ----- 1. Build a (work_id -> theorist) lookup -----------------------------
# A theorist's "id space" is the set of OpenAlex work IDs they authored.
# Multi-authored works are attributed to every theorist on the byline (if
# two of our 25 theorists co-authored a paper, citations to that paper
# count for both).

work_to_theorist <- theorist_works |>
  distinct(work_id = id, Theorist)

cat("Theorist work-ID lookup: ",
    nrow(work_to_theorist), " (work, theorist) pairs across ",
    n_distinct(work_to_theorist$work_id), " unique works.\n", sep = "")


# ----- 2. Explode journal articles -> their references ---------------------
# Each row of journal_works has a list-column referenced_works (OpenAlex work
# IDs of the cited papers). We unnest to one row per (citing article x cited
# work), then join in our theorist lookup.

cat("Exploding referenced_works ...\n")

article_refs <- journal_works |>
  select(article_id          = id,
         publication_year,
         journal_abb,
         referenced_works) |>
  unnest_longer(referenced_works, values_to = "cited_work_id") |>
  filter(!is.na(cited_work_id))

cat("  ", nrow(article_refs), " (article x reference) edges total.\n", sep = "")


# ----- 3. Join references against theorist works ---------------------------
# An (article, reference) edge survives the join only if the cited work is
# by one of our 25 theorists. The result is the citation long table.

citations_long <- article_refs |>
  inner_join(work_to_theorist, by = c("cited_work_id" = "work_id")) |>
  left_join(authors_tbl, by = "Theorist")

cat("After join: ", nrow(citations_long), " (article x cited theorist) rows.\n",
    "  Articles in the long table: ", n_distinct(citations_long$article_id), "\n",
    "  Theorists with >=1 citation: ", n_distinct(citations_long$Theorist), "\n",
    sep = "")


# ----- 4. Filter out implausible citations ---------------------------------
# Drop citing articles dated before a theorist could plausibly have been
# cited. Catches OpenAlex date noise (e.g., a misdated 1900 article that
# "cites" Bourdieu).

n_before <- nrow(citations_long)
citations_long <- citations_long |>
  filter(is.na(earliest_cite_year) | publication_year >= earliest_cite_year)
cat("Dropped ", n_before - nrow(citations_long), " citations before plausible window.\n",
    sep = "")


# ----- 5. Deduplicate ------------------------------------------------------
# An article shouldn't double-count a theorist even if it cites multiple of
# their works. (If Marx's _Capital_ and _Manifesto_ are both cited in one
# AJS article, that's still "1 AJS article cited Marx in year Y", not 2.)

citations_long <- citations_long |>
  distinct(article_id, Theorist, .keep_all = TRUE)

cat("After dedup: ", nrow(citations_long), " unique (article x theorist) rows.\n",
    sep = "")


# ----- 6. Pre-aggregated counts --------------------------------------------

citation_counts <- citations_long |>
  count(Theorist, cohort, publication_year, journal_abb,
        name = "n_articles_citing")


# ----- 7. Save -------------------------------------------------------------

saveRDS(citations_long,   "final_data/citations_long.RDS")
saveRDS(citation_counts,  "final_data/citation_counts.RDS")
cat("\nWrote final_data/citations_long.RDS  (", nrow(citations_long),  " rows)\n",
    "Wrote final_data/citation_counts.RDS (", nrow(citation_counts), " rows)\n",
    sep = "")


# ----- 8. Sanity check: top theorist x decade ------------------------------

cat("\nTop theorists cited in each decade (sanity check):\n")
citations_long |>
  mutate(decade = (publication_year %/% 10) * 10) |>
  count(decade, Theorist) |>
  group_by(decade) |>
  slice_max(n, n = 3) |>
  print(n = Inf)
