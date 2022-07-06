
suppressMessages(library(tidyverse))
source("scrape_quick.R")

###--- Data
jou_auth <- readRDS("journals_authors.RDS")
jou_auth <- jou_auth |> mutate(journal_abb = names(journal))
years <- c(1970:2020)
output <- "quick_scrape.RDS"


###--- 
out <- readRDS(output)
out <- out |> count(journal_abb = journal,author = theorist) |> filter(n == 51) |> select(-n)

jou_auth <- jou_auth |> anti_join(out)



out <- list()
for(j in 1:nrow(jou_auth)){
  
  ###--- Params
  journal  <- jou_auth[j,] |> pull(journal)
  journal_abb <- names(journal)
  journal <- journal[[1]]
  keyword <- jou_auth[j,] |> pull(author)
  lang <- "en"
  
  print(paste("Starting collection for",keyword,"in",journal_abb))
  
  res <- list() 
  for(i in 1:length(years)) {
    
    
    year_start <- years[i]
    year_end <- years[i]
    
    
    hits <- scrape_hits(journal = journal, 
                        keyword = keyword, 
                        lang = lang,
                        year_start = year_start, 
                        year_end = year_end)
    
    res[[i]] <- tibble(hits, year = year_start, journal = journal_abb, theorist = keyword)
    print(paste( "Year:", year_start, "/ Hits:", hits))
    
  }
  
  out[[j]] <- bind_rows(res)
  
}



out_new <- bind_rows(out)

out <- 
  readRDS(output) |> 
  bind_rows(out_new)
  

saveRDS(out,output)



# out |>  count(journal,theorist)
# 
# out |> 
#   ggplot(aes(year,hits, group = journal, color = journal)) +
#   geom_line() +
#   theme_minimal()

