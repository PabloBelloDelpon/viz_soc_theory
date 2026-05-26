###--- Validation: compare OpenAlex (new) counts vs Scholar (old) counts
###
### Sanity-checks the new OpenAlex pipeline by reading the archived Scholar
### scrapes in /outdated/output_data/ and computing citation counts per
### theorist x decade x journal, then placing them next to the new counts
### from /final_data/citations_long.RDS.
###
### Caveats:
###   - The 11 theorists in the old data is a subset of the 25 in the new.
###     Only overlap rows are compared.
###   - The old data used Sociology of Education; the new data uses Social
###     Forces. Counts under "SoE" in old data have no new counterpart.
###   - Scholar conflates surname collisions (Collins, Smith) and full-name
###     spellings. The old Collins counts in particular bundle Randall and
###     (back then unsampled) Patricia Hill Collins.
###
### Writes: final_data/old_vs_new_counts.csv

suppressMessages({
  library(tidyverse)
  library(fs)
})

# ----- 1. Read old Scholar scrapes -----------------------------------------

old_dir <- "outdated/output_data"
old_files <- dir_ls(old_dir, glob = "*.RDS")
stopifnot(length(old_files) > 0)

# Filenames are "{journal_abb}_{author_last_name}_{site}.RDS"
# Author last names in the old data: Bourdieu, Coleman, Du Bois, Durkheim,
# Giddens, Habermas, Marx, Merton, Parsons, Simmel, Weber
old_meta <- tibble(path = old_files) |>
  mutate(
    fname     = basename(path) |> str_remove("\\.RDS$"),
    journal_abb = str_extract(fname, "^[^_]+"),
    rest        = str_remove(fname, "^[^_]+_"),
    author_last = str_extract(rest, "^[^_]+"),
    site        = str_remove(rest, "^[^_]+_")
  )

cat("Reading", length(old_files), "old Scholar files ...\n")
old_data <- old_meta |>
  mutate(data = map(path, readRDS)) |>
  unnest(data) |>
  mutate(years = as.integer(years)) |>
  drop_na(years)


# ----- 2. Map old last-names -> new Theorist strings -----------------------

old_to_new <- tribble(
  ~author_last, ~Theorist,
  "Bourdieu",   "Pierre Bourdieu",
  "Coleman",    "James S. Coleman",
  "Du Bois",    "W.E.B. Du Bois",
  "Durkheim",   "Emile Durkheim",
  "Giddens",    "Anthony Giddens",
  "Habermas",   "Jürgen Habermas",
  "Marx",       "Karl Marx",
  "Merton",     "Robert K. Merton",
  "Parsons",    "Talcott Parsons",
  "Simmel",     "Georg Simmel",
  "Weber",      "Max Weber"
)

old_data <- old_data |>
  left_join(old_to_new, by = "author_last") |>
  filter(!is.na(Theorist))


# ----- 3. Count old citations by decade x journal x theorist ---------------

old_counts <- old_data |>
  filter(journal_abb %in% c("AJS", "ASR", "ST")) |>   # exclude SoE
  mutate(decade = (years %/% 10) * 10) |>
  distinct(Theorist, journal_abb, decade, titles) |>  # one row per article
  count(Theorist, journal_abb, decade, name = "n_old")


# ----- 4. Build the same shape from the new OpenAlex pipeline ---------------

new_long <- readRDS("final_data/citations_long.RDS")

new_counts <- new_long |>
  filter(journal_abb %in% c("AJS", "ASR", "ST")) |>
  mutate(decade = (publication_year %/% 10) * 10) |>
  count(Theorist, journal_abb, decade, name = "n_new")


# ----- 5. Join and inspect --------------------------------------------------

compare <- full_join(old_counts, new_counts,
                     by = c("Theorist", "journal_abb", "decade")) |>
  replace_na(list(n_old = 0L, n_new = 0L)) |>
  mutate(
    diff      = n_new - n_old,
    pct_new   = if_else(n_old > 0, round(n_new / n_old * 100, 0L), NA_integer_)
  ) |>
  arrange(Theorist, journal_abb, decade)

write_csv(compare, "final_data/old_vs_new_counts.csv")
cat("\nWrote final_data/old_vs_new_counts.csv (",
    nrow(compare), " rows)\n", sep = "")


# ----- 6. Per-theorist summary ---------------------------------------------

cat("\nPer-theorist totals (AJS+ASR+ST combined, all decades):\n")
compare |>
  group_by(Theorist) |>
  summarise(
    old_total   = sum(n_old),
    new_total   = sum(n_new),
    pct_of_old  = if_else(old_total > 0,
                          round(new_total / old_total * 100, 0L),
                          NA_integer_)
  ) |>
  arrange(pct_of_old) |>
  print(n = Inf)

cat("\nIf 'pct_of_old' is far below 100 for a theorist, OpenAlex is missing\n")
cat("citations relative to Scholar. The likely cause is an incomplete\n")
cat("Author-ID set (the theorist's works are split across more IDs than\n")
cat("we've included in input_data/openalex_authors.csv).\n")
