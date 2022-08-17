
###--- Libraries
suppressMessages(library(tidyverse))
library(here)
source("scrape_scholar_function.R")
source("helper_functions.R")

###--- Files & folders
output_folder <- "output_data2"

###--- Data
jou_auth <- readRDS(here("input_data", "journals_authors.RDS"))

###--- Done
done <- list.files(output_folder)
done <- str_remove(done,".RDS")
done <- str_split(done,"_")
done <- lapply(done, function(x) {
  journal_abb <- x[1]
  author_last_name <- x[2]
  journal_site <- x[3]
  tibble(journal_abb,author_last_name,journal_site)
}) |> 
  bind_rows()
       

###--- remove already collected rows
jou_auth <- 
  jou_auth |> 
  anti_join(done)


###--- Open browser
rD <- rsDriver(browser= "firefox", port= 4547L, verbose= FALSE)
remDr <- rD[["client"]]

###---



for(j in 1:nrow(jou_auth)){
  
  ###--- Params
  journal  <- jou_auth[[j,"journal_name"]]
  journal_abb  <- jou_auth[[j,"journal_abb"]]
  keyword <- jou_auth[[j,"author_last_name"]]
  year_start <- jou_auth[[j,"journal_year_start"]]
  year_end <- jou_auth[[j,"journal_year_end"]]
  site <- jou_auth[[j,"journal_site"]]
  lang <- "en"

  ###--- Create output folder
  output <- jou_auth[[j,"output"]]
  temp_output <- paste0("temp/",output)
  if(dir.exists(temp_output) == FALSE){dir.create(temp_output)}
  output <- here(output_folder,paste0(output,".RDS"))
  
  ###--- Check if some pages have been collected already
  pages <- list.files(temp_output)
  pages <- as.numeric(str_extract(pages,regex("[0-9]+")))
  last_page <- ifelse(length(pages) == 0,0,max(pages))

  print(paste("Starting collection for",keyword,"in",journal_abb, "site:",site))
  print(paste("Pages collected:", last_page))
  total_pages <- c()
  res <- safe_scrape_scholar(journal = journal, 
                             keyword = keyword, 
                             lang = lang,
                             year_start = year_start,
                             year_end = year_end,
                             page = last_page,
                             temp_output = temp_output,
                             site = site)
  

  if(res == "done") {
    make_output_file(temp_folder = temp_output,
                     output_file = output)     
  }
  
  else if(res == "error") {
        stop()
  }
}

remDr$close() # Close the client
rD$server$stop() # Close the server




