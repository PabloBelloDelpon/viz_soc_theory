
suppressMessages(library(tidyverse))
library(zoo)

###--- Import data
files <- list.files("data",full.names = TRUE)

data <- lapply(files,function(x){
  j <- str_remove(x,"data/")
  j <- str_remove(j,".RDS")
  j <- str_split(j,"_")
  journal <- sapply(j,function(i) i[1])
  author <- sapply(j,function(i) i[2])
  
  x <- readRDS(x)
  if(class(x) == "list"){
    x <- x[["data"]]
  }
  
  x <- x |> mutate(journal_abb = journal,
                   theorist = author)
  
  return(x)
}) |> 
  bind_rows()


###--- Clean up data
data <- 
  data |> 
  mutate(years = as.integer(years)) |> 
  drop_na(years) |> 
  filter(years > 1940 & years < 2022)

###--- Describe

data |> 
  count(theorist,journal_abb) |> 
  arrange(journal_abb)


###--- Bourdieu
# bourdieu <- 
#   data |> 
#   filter(theorist == "Bourdieu")
# 
# bourdieu2 <- 
#   bourdieu |> 
#   count(theorist,journal_abb,years)
# 
# bourdieu2 |> 
#   mutate(n = rollmean(x = n,k = 3,fill = NA)) |> 
#   ggplot(aes(years,n,group = journal_abb, color = journal_abb)) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = unique(bourdieu2$theorist),
#        color = "",
#        x = "",
#        y = "Number of papers citing")



###--- Bourdieu and Parsons

tbl <- 
  data |> 
  filter(journal_abb %in% c("R&S","AJS","ASR","SF","ST")) |> 
  filter(theorist %in% c("Parsons","Bourdieu"))



tbl2 <- 
  tbl |> 
  drop_na(cited) |>  # there are some weird cases for Bourdieu in 2002, revise this 
  distinct(abstract,.keep_all = TRUE) |> 
  count(journal_abb,theorist,years)


tbl2 |> count(journal_abb,theorist)

c <- paste0(unique(tbl2$journal_abb),collapse = ", ")
c <- paste0("Journals: ",c)


tbl2 |> 
  group_by(theorist,years) |> 
  summarise(n = sum(n)) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ggplot(aes(years,n,group = theorist, color = theorist)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Papers citing Bourdieu and Parsons",
       caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")

###--- Break down by journal

tbl2 |> 
  ungroup() |> 
  filter(journal_abb %in% c("AJS","SF","ST")) |>
  mutate(id = paste(journal_abb,theorist)) |> 
  group_by(theorist,journal_abb) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ggplot(aes(years,n,group = id, color = theorist)) +
  geom_line(aes(linetype = journal_abb)) +
  theme_minimal() +
  labs(title = "Papers citing Bourdieu and Parsons",
       #caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")


