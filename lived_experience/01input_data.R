###--- Create input data


###--- Libraries
library(tidyverse)

###--- Journals 

journal_tbl <- 
  tribble(~journal_abb, ~journal_name, ~journal_type,
          "ASR","American Sociological Review","general",
          "SoE","Sociology of Education","subfield",
          "AJS","American Journal of Sociology","general",
          "ST","Sociological Theory","general")
# others <- c("Sociological Methods and Research", "Social Problems", "Rationality and Society")

###--- Journal sites
journal_tbl2 <- 
  tribble(~journal_abb, ~journal_site, ~journal_year_start,~journal_year_end,
          "AJS","journals.uchicago.edu",1895,2022,
          "ASR","journals.sagepub.com",2004,2022,
          "ASR","jstor.org",1936,2003,
          "SoE","journals.sagepub.com",2004,2022,
          "SoE","jstor.org",1963,2003,
          "ST","journals.sagepub.com",1997,2022,
          "ST","jstor.org",1983,1996)

###--- Join them 
journal_tbl <- 
  journal_tbl |> 
  left_join(journal_tbl2)


###--- Keyword
journal_tbl <- 
  journal_tbl |> 
  mutate(keyword = "lived experience") |> 
  rowwise() |> 
  mutate(output = paste0(c(journal_abb,keyword,journal_site),collapse = "_"))

saveRDS(journal_tbl, "lived_experience/journal_tbl.RDS")

