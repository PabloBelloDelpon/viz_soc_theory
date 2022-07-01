
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
  filter(years > 1950 & years < 2022)

###--- Describe

data |> count(theorist,journal_abb) |> arrange(journal_abb)


###--- Viz

bourdieu <- 
  data |> 
  filter(theorist == "Bourdieu")

bourdieu2 <- 
  bourdieu |> 
  count(theorist,journal_abb,years)

bourdieu2 |> 
  mutate(n = rollmean(x = n,k = 3,fill = NA)) |> 
  ggplot(aes(years,n,group = journal_abb, color = journal_abb)) +
  geom_line() +
  theme_minimal() +
  labs(title = unique(bourdieu2$theorist),
       color = "",
       x = "",
       y = "Number of papers citing")



###---

tbl <- 
  data |> 
  filter(journal_abb %in% c("R&S","SF")) |> 
  filter(theorist %in% c("Parsons","Bourdieu"))



tbl2 <- 
  tbl |> 
  count(journal_abb,theorist,years)

tbl2 |> 
  filter(journal_abb == "SF") |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ggplot(aes(years,n,group = theorist, color = theorist)) +
  geom_line() +
  theme_minimal() +
  labs(title = unique(tbl2$journal_abb),
       color = "",
       x = "",
       y = "Number of papers citing")

###---



tbl <- 
  tbl |>
  filter(!row_number() %in% c(20)) |>
  filter(years < 2023) |> 
  mutate(years = as.integer(years))


tbl |>
  count(years) |> 
  arrange(years) |> 
  ggplot(aes(years,n, group = 1)) +
  geom_line() +
  theme_minimal()