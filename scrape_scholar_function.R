
library(httr)
library(rvest)
library(xml2)
library(selectr)
library(stringr)
library(jsonlite)

my_user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"
user_agents <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.169 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.121 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/74.0.3729.157 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/96.0.4664.110 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.102 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.159 Safari/537.36"
)




scrape_scholar <- function(keyword,journal,lang,year_start = NA,year_rend = NA) {
  
  journal <- paste0(journal,collapse = "+")
  
  ###--- Construct the URL
  header <- "https://scholar.google.com/scholar?start="
  query <- paste0(header,"&q=+-author:",keyword,"+source:%22",journal,"%22+",keyword,"&hl=",lang,"&as_sdt=0,5")
  if (!is.na(year_start) & !is.na(year_end)){query <- paste0(query, "&as_ylo=",year_start,"&as_yhi=",year_end)}
  first_page <- paste0(header,0,query)
  
  ###---
  # download.file(first_page, destfile = "scrapedpage.html", quiet=TRUE)
  # wp <- read_html("scrapedpage.html")
  ###---
  
  wp <- 
    httr::GET(first_page, 
            set_cookies(`_SMIDA` = "7cf9ea4bfadb60bbd0950e2f8f4c279d",
                        `__utma` = "29983421.138599299.1413649536.1413649536.1413649536.1",
                        `__utmb` = "29983421.5.10.1413649536",
                        `__utmc` = "29983421",
                        `__utmt` = "1",
                        `__utmz` = "29983421.1413649536.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)"),
            user_agent(user_agents[sample(1:length(user_agents),1)])) |>
    read_html()
  
  
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
  start_from <- 0:floor(hits/10)*10
  urls <- paste0(header,start_from,query)
  
  
  # Iterate over scholar pages
  res <- list()
  not_collected <- c()
  
  for(i in 1:length(urls)){

    
    url <- urls[i]
    
    ###---
    # download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    # wp <- read_html(url)
    # ###---
    
   wp <- 
     httr::GET(url, 
               set_cookies(`_SMIDA` = "7cf9ea4bfadb60bbd0950e2f8f4c279d",
                           `__utma` = "29983421.138599299.1413649536.1413649536.1413649536.1",
                           `__utmb` = "29983421.5.10.1413649536",
                           `__utmc` = "29983421",
                           `__utmt` = "1",
                           `__utmz` = "29983421.1413649536.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)"),
               user_agent(user_agents[sample(1:length(user_agents),1)])) |>
     read_html()
    
    # Extract raw data
    titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
    authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
    
    abstract <- wp |> html_nodes('.gs_rs') |> html_text()
    cited <- wp |> html_nodes('.gs_fl > a:nth-child(3)') |> html_text() |> str_remove("Cited by ") |> as.integer()
    
    # Process data
    authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
    years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
    
    
    leftovers <- authors_years %>% 
      str_remove_all(authors) %>% 
      str_remove_all(years)
    
    
    journals <- str_split(leftovers, "-") %>% 
      map_chr(2) %>% 
      str_extract_all("[:alpha:]*") %>% 
      map(function(x) x[x != ""]) %>% 
      map(~paste(., collapse = " ")) %>% 
      unlist()
    
    # Make data frame
    tbl <- data.frame(titles = titles, authors = authors, 
                      years = years, journals = journals, 
                      cited = cited,
                      abstract = abstract,
                      stringsAsFactors = FALSE)
    res[[i]] <- tbl
    
    print(paste(nrow(tbl), "articles collected"))
    
    if(nrow(tbl) == 0) {
      
      if(i %in% not_collected == FALSE){
        e <- i - 1
      }
      
      not_collected[length(not_collected) + 1] <- i
      print(paste("Google caught me, go to sleep at",Sys.time()))
      Sys.sleep(60*60*24)
      i <- e
    }
    
    sleep <- floor(runif(1,min = 10,max = 14))
    print(paste("Finished", i, "/",length(urls), "- Sleeping for",sleep,"seconds"))
    Sys.sleep(sleep)
    
  }

  
 res <- res[which(sapply(res,function(x)is.data.frame(x)) == TRUE)]
 res <- 
    bind_rows(res) |> 
    as_tibble() |> 
   mutate(expected_hits = hits)
 
 out <- list("data" = res,"meta" = not_collected)
  
  return(out)
}
