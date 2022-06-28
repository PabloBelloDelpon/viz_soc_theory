
library(httr)
library(rvest)
library(xml2)
library(selectr)
library(stringr)
library(jsonlite)



scrape_hits <- function(keyword,journal,lang,year_start = NA,year_end = NA) {
  
  journal <- paste0(journal,collapse = "+")
  
  ###--- Construct the URL
  header <- "https://scholar.google.com/scholar?start="
  query <- paste0(header,"&q=+-author:",keyword,"+source:%22",journal,"%22+",keyword,"&hl=",lang,"&as_sdt=0,5")
  if (!is.na(year_start) & !is.na(year_end)){query <- paste0(query, "&as_ylo=",year_start,"&as_yhi=",year_end)}
  first_page <- paste0(header,0,query)
  
 
  wp <- 
    httr::GET(first_page) |>
    read_html()
  
  
  ###--- Check if there is a captcha
  
  is_captcha <- 
    html_text(wp) |> 
    str_detect("captcha")
  if(is_captcha == TRUE){ 
    readline(blue("There is a captcha, check the browser!"))
    
    remDr$navigate(url)
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
  
  sleep <- floor(runif(1,min = 1,max = 10))
  Sys.sleep(sleep)
  
  return(hits)
}
