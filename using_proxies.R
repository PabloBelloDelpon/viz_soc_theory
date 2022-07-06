
library(getProxy)
library(rvest)
library(httr)




###--- Params
journal <- c("American","Journal","of","Sociology")
journal <- paste0(journal,collapse = "+")
keyword <- "Randall Collins"
lang <- "en"
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0"


###--- Construct the URL
header <- "https://scholar.google.com/scholar?start="
query <- paste0(header,"&q=+-author:",keyword,"+source:%22",journal,"%22+",keyword,"&hl=",lang,"&as_sdt=0,5")
first_page <- paste0(header,0,query)





###--- 
proxy <- getProxy::getProxy()
httr::set_config(httr::use_proxy("34.228.74.208:8080"))
wp <- read_html(GET(first_page,
                    add_headers(`Connection` = "keep-alive", 
                                `User-Agent` = user_agent)))
text <- html_text(wp)


