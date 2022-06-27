
suppressMessages(library(tidyverse))
source("scrape_scholar_function.R")

###--- Data
jou_auth <- readRDS("journals_authors.RDS")

###--- Split work between computers
j_indeces <- split(1:nrow(jou_auth),f = c(1:4))
info <- Sys.info()
if(info["sysname"] == "Darwin") {j_indeces <- j_indeces[[1]]}
if(info["nodename"] == "WIN02672") {j_indeces <- j_indeces[[2]]}
if(info["nodename"] == "ISV-0587-W64") {j_indeces <- j_indeces[[3]]}
if(info["nodename"] == "WIN03872") {j_indeces <- j_indeces[[4]]}

Sys.info()
###---
safe_scrape_scholar <- possibly(scrape_scholar, otherwise = tibble(),quiet = FALSE)

for(j in j_indeces){
  
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




  
