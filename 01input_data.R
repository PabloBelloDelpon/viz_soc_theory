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


###--- Authors
  authors_tbl <- 
    tribble(~author_first_name, ~author_last_name,~author_type, ~author_year_born, ~author_year_death,
            "Max","Weber","classic",1864,1920,
            "Emile","Durkheim","classic",1858,1917,
            "Georg","Simmel","classic",1858,1918,
            "Talcott","Parsons","canon",1902,1979,
            "Robert K.","Merton","canon",1910,2003,
            "James S.","Coleman","modern",1926,1995,
            "Pierre","Bourdieu","modern",1930,2002,
            "Jürgen","Habermas","modern",1929,NA,
            "Anthony","Giddens","modern",1938,NA,
            "Karl","Marx","classic",1818,1883,
            "W.E.B","Du Bois","classic",1868,1963)

  
###--- Put journals and authors together
  data_tbl <- 
    expand.grid("journal_name" = unique(journal_tbl$journal_name),"author_last_name" = authors_tbl$author_last_name) |> 
    as_tibble() |> 
    left_join(journal_tbl) |>
    left_join(authors_tbl) |> 
    rowwise() |> 
    mutate(output = paste0(c(journal_abb,author_last_name,journal_site),collapse = "_"))
  
  saveRDS(data_tbl,"input_data/journals_authors.RDS")
