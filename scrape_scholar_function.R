
###--- Libraries
library(httr)
library(rvest)
library(xml2)
library(selectr)
library(stringr)
library(jsonlite)
library(RSelenium)
source("helper_functions.R")



scrape_scholar <- function(keyword,journal,lang,year_start = NA,year_rend = NA,page,temp_output) {
  
  
  consecutive_0 <- 0 # To count consecutive pages with 0 articles on them
  
  ###--- Construct the URL
  journal <- paste0(journal,collapse = "+") # URL bersion of journal name
  if(journal == "rationality+&+society") {journal <- "rationality+and+society"} # An exception
  header <- "https://scholar.google.com/scholar?start=" 
  query <- paste0(header,"&q=+-author:",keyword,"+source:%22",journal,"%22+",keyword,"&hl=",lang,"&as_sdt=0,5") 
  if (!is.na(year_start) & !is.na(year_end)){query <- paste0(query, "&as_ylo=",year_start,"&as_yhi=",year_end)}
  first_page <- paste0(header,0,query)
  
 
  ###--- RSelenium. Navigate to the first page of query
  remDr$navigate(first_page)
  Sys.sleep(2)
  html <- remDr$getPageSource()[[1]]
  wp <- read_html(html)
  
  ###--- Check if there is a captcha
  check_captcha(wp)
  
  ###--- Number of hits (expected number of articles, but Google gets it wrong all the time)
  hits <- check_n_hits(wp)
  
  ###--- Construct URL for next pages
  next_page <- page + 1
  start_from <- 0:floor(hits/10)*10
  total_pages <<- length(start_from)
  start_from <- start_from[next_page:length(start_from)] # Remove already collected pages
  urls <- paste0(header,start_from,query)
  

  ###--- Iterate over URLs
  for(i in 1:length(urls)){

    url <- urls[i]

    ###--- Selenium. Navigate to page and scrape html
    remDr$navigate(url)
    Sys.sleep(3)
    html <- remDr$getPageSource()[[1]]
    wp <- read_html(html)
    
    ###--- Check if there is a captcha
    check_captcha(wp)
    
    ###--- Process the html
    tbl <- process_wp(wp)
    print(paste(nrow(tbl), "articles collected"))

    ###--- If I found any articles in the page, save them
    if(nrow(tbl) > 0){
      save_to <- paste0(temp_output,"/page_",next_page,".RDS")
      saveRDS(tbl,save_to)
      next_page <- next_page + 1
      consecutive_0 <- 0
    }
    
    ###--- Otherwise check visually in the browser if the collection is finished
    else if(nrow(tbl) == 0) {
      
      consecutive_0 <- consecutive_0 + 1
      print(paste("Consecutive zeros:" , consecutive_0))
      
      if(consecutive_0 == 3) {
        end <- fun() # Ask me if the collection is finished
        
        if(end == TRUE) return("done")
        else if(end == FALSE) return("error")
        
      }
    }
    
    ###--- Go to sleep between pages
    sleep <- floor(runif(1,min = 5,max = 20))
    print(paste("Finished", i, "/",length(urls), "- Sleeping for",sleep,"seconds"))
    Sys.sleep(sleep)
    
  }
  
  return("done")
  
}
