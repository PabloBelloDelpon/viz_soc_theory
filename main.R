suppressMessages(library(tidyverse))
source("scrape_scholar_function.R")

###--- Data
jou_auth <- readRDS("journals_authors.RDS")

###---
safe_scrape_scholar <- possibly(scrape_scholar, otherwise = tibble(),quiet = FALSE)

for(j in 1:nrow(jou_auth)){
  
  ###--- Params
  journal  <- jou_auth[j,] |> pull(journal)
  journal_abb <- names(journal)
  journal <- journal[[1]]
  keyword <- jou_auth[j,] |> pull(author)
  year_start <- NA
  year_end <- NA
  lang <- "en"
  
  output <- paste0(c(journal_abb,keyword),collapse = "_")
  output <- paste0("data/",output,".RDS")
  
  if(file.exists(output) == FALSE) {
    
    print(paste("Starting collection for",keyword,"in",journal_abb))
    res <- scrape_scholar(journal = journal, keyword = keyword, lang = lang)
    saveRDS(res,output)
  }
}




  
