
###--- Libraries
suppressMessages(library(tidyverse))
library(here)
source("scrape_scholar_function.R")


###--- Files & folders
output_folder <- "output_data"

###--- Data
jou_auth <- readRDS(here("input_data", "journals_authors.RDS"))


###--- Done
done <- list.files(output_folder)
done <- str_remove(done,".RDS")
done <- str_split(done,"_")
done <- lapply(done, function(x) {
  journal_abb <- x[1]
  author <- x[2]
  tibble(journal_abb,author)
}) |> 
  bind_rows()
       

###--- remove already collected rows
jou_auth <- 
  jou_auth |> 
  mutate(journal_abb = names(journal)) |> 
  anti_join(done)

###--- remove poetics
jou_auth <- jou_auth |> filter(journal_abb != "Poetics")


###---
safe_scrape_scholar <- possibly(scrape_scholar, otherwise = "error",quiet = FALSE)


###--- Open browser
rD <- rsDriver(browser= "firefox", port= 4544L, verbose= FALSE)
remDr <- rD[["client"]]

###---


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
  temp_output <- paste0("temp/",output)
  if(dir.exists(temp_output) == FALSE){dir.create(temp_output)}
  output <- here(output_folder,paste0(output,".RDS"))
  
  
  ###--- Check if some pages have been collected already
  pages <- list.files(temp_output)
  pages <- as.numeric(str_extract(pages,regex("[0-9]+")))
  last_page <- ifelse(length(pages) == 0,0,max(pages))

  print(paste("Starting collection for",keyword,"in",journal_abb))
  print(paste("Pages collected:", last_page))
  total_pages <- c()
  res <- safe_scrape_scholar(journal = journal, 
                             keyword = keyword, 
                             lang = lang,
                             page = last_page,
                             temp_output = temp_output)
  

  if(res == "done") {
    
    all_pages <- list.files(temp_output,full.names = TRUE)
    
    tbl <- 
      all_pages |> 
      map_dfr(readRDS) |> 
      as_tibble()
    
    saveRDS(tbl,output)
    file.remove(all_pages)
    unlink(temp_output, recursive = TRUE)     
  }
}


remDr$close() # Close the client
rD$server$stop() # Close the server



