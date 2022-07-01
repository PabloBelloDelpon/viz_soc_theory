
library(httr)
library(rvest)
library(xml2)
library(selectr)
library(stringr)
library(jsonlite)
library(RSelenium)




scrape_scholar <- function(keyword,journal,lang,year_start = NA,year_rend = NA,page) {
  
  journal <- paste0(journal,collapse = "+")
  if(journal == "rationality+&+society") {journal <- "rationality+and+society"}
  
  ###--- Construct the URL
  header <- "https://scholar.google.com/scholar?start="
  query <- paste0(header,"&q=+-author:",keyword,"+source:%22",journal,"%22+",keyword,"&hl=",lang,"&as_sdt=0,5")
  if (!is.na(year_start) & !is.na(year_end)){query <- paste0(query, "&as_ylo=",year_start,"&as_yhi=",year_end)}
  first_page <- paste0(header,0,query)
  
 
  ###--- RSelenium
  remDr$navigate(first_page)
  Sys.sleep(5)
  html <- remDr$getPageSource()[[1]]
  wp <- read_html(html)
  
  
  ###--- Check if there is a captcha
  is_captcha <- 
    html_text(wp) |> 
    str_detect("captcha")
  if(is_captcha == TRUE){ 
    readline("There is a captcha, check the browser!")
    
    remDr$navigate(first_page)
    Sys.sleep(5)
    html <- remDr$getPageSource()[[1]]
    wp <- read_html(html)
    
  }
  
  ###---
  
  
  # Number of hits 
  hits <- 
    wp |> 
    html_element('form[method=post] + div div > div:contains("results")') |>
    html_text() |>
    str_split("results") 
  
  hits <- 
    unlist(hits)[1] |> 
    str_extract_all("(\\d+)") |>
    unlist() |> 
    paste(collapse = "") |> 
    as.integer()
  
  
  # construct url for next pages
  next_page <- page + 1
  start_from <- 0:floor(hits/10)*10
  start_from <- start_from[next_page:length(start_from)]
  urls <- paste0(header,start_from,query)
  

  # Iterate over URLs

  for(i in 1:length(urls)){

    url <- urls[i]

    ###--- Selenium
    remDr$navigate(url)
    Sys.sleep(5)
    html <- remDr$getPageSource()[[1]]
    wp <- read_html(html)
    
    
    ###--- Check if there is a captcha

    is_captcha <- 
      html_text(wp) |> 
      str_detect("captcha")
    if(is_captcha == TRUE){ 
      readline("There is a captcha, check the browser!")
      
      remDr$navigate(url)
      Sys.sleep(5)
      html <- remDr$getPageSource()[[1]]
      wp <- read_html(html)
      
      }
    
    ###---
    
    
    
    # Extract raw data
    titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
    authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
    
    abstract <- wp |> html_nodes('.gs_rs') |> html_text()
    cited <- wp |> html_nodes('.gs_fl > a:nth-child(3)') |> html_text() |> str_remove("Cited by ") |> as.integer()
    
    # Process data
    authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
    years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
    
    
    leftovers <- authors_years %>% 
      str_remove_all(authors[str_length(authors) > 0]) %>% 
      str_remove_all(years[str_length(years) > 0])
    
    
    if(length(leftovers) == length(titles)){
      journals <- str_split(leftovers, "-") %>% 
        map_chr(2) %>% 
        str_extract_all("[:alpha:]*") %>% 
        map(function(x) x[x != ""]) %>% 
        map(~paste(., collapse = " ")) %>% 
        unlist()
    }
    else {
      journals <- rep(NA,10)
      }
    
    # Make data frame
    tbl <- data.frame(titles = titles, authors = authors, 
                      years = years, journals = journals, 
                      cited = cited,
                      abstract = abstract,
                      stringsAsFactors = FALSE)
    
    
    
    print(paste(nrow(tbl), "articles collected"))
    
    if(nrow(tbl) > 0){
      save_to <- paste0("temp/page_",next_page,".RDS")
      saveRDS(tbl,save_to)
      next_page <- next_page + 1
    }
    
    sleep <- floor(runif(1,min = 5,max = 20))
    print(paste("Finished", i, "/",length(urls), "- Sleeping for",sleep,"seconds"))
    Sys.sleep(sleep)
    
  }
}
