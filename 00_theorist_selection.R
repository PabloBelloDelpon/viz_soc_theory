###--- Theorist selection from canonical theory textbooks
###
### Builds an externally validated theorist sample by counting how many
### canonical sociology theory textbooks/readers include each author.
###
### Sources (4):
###   - ritzer:               Ritzer, Modern Sociological Theory
###   - calhoun-classical:    Calhoun et al., Classical Sociological Theory reader
###   - calhoun-contemporary: Calhoun et al., Contemporary Sociological Theory reader
###   - Joas:                 Joas & Knöbl, Social Theory: Twenty Introductory Lectures
###
### Note on Joas: it lists ~440 names (closer to a full name index than a
### curated list of treated theorists). The other three sources list ~30
### each. Consequence: appearing in Joas alone is weakly informative;
### the ≥2-source rule mostly works by requiring presence outside Joas.
###
### Author-name normalization handles two known issues:
###   1. Diacritic inconsistency  (Schütz/Schutz, Habermas with/without ü)
###   2. Middle-initial variation (George C. vs George Caspar Homans,
###      Richard vs Richard M. Emerson)
###
### Output: /input_data/theorist_selection_summary.csv (long table with
### source-count per theorist), printed candidate lists at thresholds.

library(tidyverse)
library(stringi)

###--- Load
theorists_raw <- read_csv("input_data/theorists.csv", show_col_types = FALSE)

stopifnot(nrow(theorists_raw) > 0)
cat("Loaded", nrow(theorists_raw), "rows across",
    length(unique(theorists_raw$Origin)), "sources.\n\n")

###--- Manual name-merge map: variants that refer to the SAME person.
### Only merges entries we've verified by eye. Same-last-name pairs
### that are different people (Adler, Becker, Cohen, Collins, Hall,
### Marshall, McCarthy, Mullins, Putnam, Schelling, Smith, Taylor,
### Thomas, Turner, Wagner) are deliberately kept separate.
name_merge <- c(
  "Alfred Schutz"            = "Alfred Schütz",
  "Jurgen Habermas"          = "Jürgen Habermas",
  "George Caspar Homans"     = "George C. Homans",
  "Richard Emerson"          = "Richard M. Emerson"
)

theorists <- theorists_raw |>
  mutate(Theorist = recode(Theorist, !!!name_merge))

###--- Sanity check: any name still differing only by diacritics?
diacritic_check <- theorists |>
  mutate(stripped = stri_trans_general(Theorist, "Latin-ASCII") |> tolower()) |>
  group_by(stripped) |>
  summarise(variants = list(sort(unique(Theorist))), .groups = "drop") |>
  filter(lengths(variants) > 1)

if (nrow(diacritic_check) > 0) {
  cat("WARNING - unresolved diacritic/case variants still present:\n")
  print(diacritic_check)
  cat("\n")
} else {
  cat("Diacritic check: passed.\n\n")
}

###--- Long table: theorist × source (one row per appearance)
appearances <- theorists |>
  distinct(Theorist, Origin) |>
  arrange(Theorist, Origin)

###--- Summary: how many sources each theorist appears in
summary_tbl <- appearances |>
  group_by(Theorist) |>
  summarise(
    n_sources = n(),
    sources   = paste(sort(Origin), collapse = "; "),
    .groups   = "drop"
  ) |>
  arrange(desc(n_sources), Theorist)

###--- Write outputs
write_csv(summary_tbl, "input_data/theorist_selection_summary.csv")
cat("Wrote input_data/theorist_selection_summary.csv\n\n")

###--- Threshold reports ----------------------------------------------------

report_threshold <- function(k) {
  picked <- summary_tbl |> filter(n_sources >= k)
  cat(sprintf("--- Theorists appearing in >=%d sources (n = %d) ---\n",
              k, nrow(picked)))
  print(picked, n = Inf)
  cat("\n")
  invisible(picked)
}

sel_4 <- report_threshold(4)
sel_3 <- report_threshold(3)
sel_2 <- report_threshold(2)

###--- Alternative criterion: appears in any of the 3 curated sources
### (not Joas). Useful because Joas is closer to an index.
curated <- appearances |>
  filter(Origin != "Joas") |>
  group_by(Theorist) |>
  summarise(
    n_curated_sources = n(),
    curated_sources   = paste(sort(Origin), collapse = "; "),
    .groups = "drop"
  ) |>
  arrange(desc(n_curated_sources), Theorist)

cat("--- Theorists in >=2 of the three curated sources (excluding Joas alone) ---\n")
print(curated |> filter(n_curated_sources >= 2), n = Inf)
cat("\n")

cat("--- Theorists in >=1 curated source (excluding Joas-only) ---\n")
print(curated, n = Inf)
cat("\n")

###--- Cross-tab: which threshold gives a workable shortlist?
cat("--- Theorist count by threshold ---\n")
summary_tbl |>
  count(n_sources, name = "n_theorists") |>
  arrange(desc(n_sources)) |>
  mutate(cumulative = cumsum(n_theorists)) |>
  print()

###--- LOCKED-IN SELECTION ------------------------------------------------
### Rule: appears in >=2 of the 3 curated readers (Ritzer, Calhoun-classical,
### Calhoun-contemporary). Joas is used only as supplementary evidence (its
### index lists ~440 names, so inclusion there is weakly informative).
###
### Rationale to defend in the manuscript: the rule is symmetric across the
### three most widely adopted English-language sociology theory teaching
### sources, neither circularly defined by citation patterns in our outcome
### journals nor a one-source curation. It produces a 25-theorist sample
### spanning classical, canonization, and contemporary generations,
### including Du Bois, Patricia Hill Collins, Dorothy E. Smith, and
### Kimberlé Crenshaw -- who are missing from Joas & Knöbl.

selected <- curated |>
  filter(n_curated_sources >= 2) |>
  arrange(Theorist)

write_csv(selected, "input_data/selected_theorists.csv")
cat(sprintf("\nWrote input_data/selected_theorists.csv with %d theorists.\n",
            nrow(selected)))
