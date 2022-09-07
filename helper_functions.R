

###--- 
fun <- function() {
x = readline(prompt="Is the collection finished? Answer 'yes / no' ")
switch(x, yes = TRUE, no = FALSE)
}



###--- Check the number of hits in scholar

check_n_hits <- function(wp){
  
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
  
  return(hits)
}



###--- Check if there is a captcha 

check_captcha <- function(wp,fp){
  
  is_captcha <- 
    html_text(wp) |> 
    str_detect("captcha")
  
  if(is_captcha == TRUE){ 
    readline("There is a captcha, check the browser!")
    
    remDr$navigate(fp)
    Sys.sleep(5)
    html <- remDr$getPageSource()[[1]]
    wp <- read_html(html)
  }
  
}


###--- Process data (html) 

process_wp <- function(wp){
  
  titles <- rvest::html_text(rvest::html_nodes(wp, '.gs_rt'))
  authors_years <- rvest::html_text(rvest::html_nodes(wp, '.gs_a'))
  abstract <- wp |> html_nodes('.gs_rs') |> html_text()
  cited <- wp |> html_nodes('.gs_fl > a:nth-child(3)') |> html_text() |> str_remove("Cited by ") |> as.integer()
  authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
  years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
  
  
  leftovers <- authors_years %>% 
    str_remove_all(authors[str_length(authors) > 0]) %>% 
    str_remove_all(years[str_length(years) > 0])
  
  leftovers <- ifelse(str_length(leftovers) == 0,"-",leftovers)
  
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
  
  ###--- Make data frame
  tbl <- tibble(titles = titles, 
                authors = authors, 
                years = years, 
                journals = journals, 
                cited = cited,
                abstract = abstract)
  return(tbl)
  
}


###--- Put together the pages (temp files) into one when collection is finished
make_output_file <- function(temp_folder,output_file) {
  
  all_pages <- list.files(temp_folder,full.names = TRUE)
  
  tbl <- 
    all_pages |> 
    map_dfr(readRDS) |> 
    as_tibble()
  
  saveRDS(tbl,output_file)
  file.remove(all_pages)
  unlink(temp_folder, recursive = TRUE)   
  
}
