###--- Build canonical author and journal tables for the OpenAlex pipeline
###
### Reads:
###   input_data/selected_theorists.csv  (locked in by 00_theorist_selection.R)
###
### Writes:
###   input_data/authors_tbl.RDS   - 25-theorist metadata for downstream joins
###   input_data/journals_tbl.RDS  - 4-journal panel
###
### What changed from the Scholar version:
###   - No more journal_site / year_start / year_end fields (OpenAlex doesn't
###     care which platform hosts a journal).
###   - No more search_keyword field (OpenAlex uses disambiguated Author IDs,
###     so the Collins / Smith problems vanish at the source).
###   - The author x journal cross-join (formerly journals_authors.RDS, the
###     "scrape queue") is gone. OpenAlex queries are per-theorist; we filter
###     citing works to our 4 journals on the response side.

library(tidyverse)


# ----- 1. Authors -----------------------------------------------------------

selected <- read_csv("input_data/selected_theorists.csv", show_col_types = FALSE)
stopifnot(nrow(selected) == 25)

# Per-theorist metadata. cohort follows the original paper's three-bucket
# scheme. year_born / year_death drive sanity checks; earliest_cite_year is
# the floor for plausible citations (e.g., a 1920 article citing "Bourdieu"
# is almost certainly a false positive).
authors_tbl <- tribble(
  ~Theorist,                ~cohort,         ~year_born, ~year_death, ~earliest_cite_year,
  "Alexis de Tocqueville",  "classical",     1805,       1859,        1835,
  "Anthony Giddens",        "contemporary",  1938,       NA,          1971,
  "C. Wright Mills",        "canonization",  1916,       1962,        1948,
  "Dorothy E. Smith",       "contemporary",  1926,       2022,        1974,
  "Emile Durkheim",         "classical",     1858,       1917,        1893,
  "Erving Goffman",         "contemporary",  1922,       1982,        1956,
  "Georg Simmel",           "classical",     1858,       1918,        1900,
  "George C. Homans",       "canonization",  1910,       1989,        1950,
  "George Herbert Mead",    "classical",     1863,       1931,        1934,
  "Immanuel Wallerstein",   "contemporary",  1930,       2019,        1974,
  "James S. Coleman",       "contemporary",  1926,       1995,        1955,
  "Jeffrey C. Alexander",   "contemporary",  1947,       NA,          1982,
  "Jürgen Habermas",        "contemporary",  1929,       NA,          1962,
  "Karl Marx",              "classical",     1818,       1883,        1848,
  "Kimberlé Crenshaw",      "contemporary",  1959,       NA,          1989,
  "Max Weber",              "classical",     1864,       1920,        1904,
  "Michel Foucault",        "contemporary",  1926,       1984,        1961,
  "Norbert Elias",          "canonization",  1897,       1990,        1939,
  "Patricia Hill Collins",  "contemporary",  1948,       NA,          1990,
  "Peter M. Blau",          "canonization",  1918,       2002,        1955,
  "Pierre Bourdieu",        "contemporary",  1930,       2002,        1962,
  "Randall Collins",        "contemporary",  1941,       NA,          1971,
  "Robert K. Merton",       "canonization",  1910,       2003,        1936,
  "Talcott Parsons",        "canonization",  1902,       1979,        1927,
  "W.E.B. Du Bois",         "classical",     1868,       1963,        1899
)

missing <- setdiff(selected$Theorist, authors_tbl$Theorist)
extra   <- setdiff(authors_tbl$Theorist, selected$Theorist)
if (length(missing) > 0) stop("authors_tbl missing: ", paste(missing, collapse = ", "))
if (length(extra) > 0)   stop("authors_tbl has extras: ", paste(extra,   collapse = ", "))


# ----- 2. Journals ----------------------------------------------------------

# Four-journal panel (aligns with the Du Bois version). OpenAlex Source IDs
# are added in a separate hand-verified CSV (input_data/openalex_sources.csv)
# produced by the lookup script.
journals_tbl <- tribble(
  ~journal_abb, ~journal_name,                   ~founded,
  "AJS",        "American Journal of Sociology", 1895,
  "ASR",        "American Sociological Review",  1936,
  "ST",         "Sociological Theory",           1983,
  "SF",         "Social Forces",                 1922
)


# ----- 3. Save --------------------------------------------------------------

saveRDS(authors_tbl,  "input_data/authors_tbl.RDS")
saveRDS(journals_tbl, "input_data/journals_tbl.RDS")
cat("Wrote input_data/authors_tbl.RDS  (", nrow(authors_tbl),  " theorists)\n", sep = "")
cat("Wrote input_data/journals_tbl.RDS (", nrow(journals_tbl), " journals)\n",   sep = "")
