###--- Libraries
suppressMessages(library(tidyverse))


###--- Files and folders
output_file <- "viz_soc_theory_data.RDS"




###--- Import data
files <- list.files("output_data",full.names = TRUE)

data <- lapply(files,function(x){
  j <- str_remove(x,"output_data/")
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
  filter(years > 1940 & years < 2022) |> 
  select(- expected_hits) |> 
  mutate(journals = str_to_lower(journals),
         journals = str_squish(journals))



###---  
data <- 
  data |> 
  group_by(theorist) |> 
  distinct(titles,.keep_all = TRUE) |> 
  ungroup()



###--- CLean up Sociology of Education
soe <- 
  data |>
  filter(journal_abb == "SoE") |> 
  filter(str_detect(journals,regex("^sociology of education$")) == TRUE)

data <- data |> filter(journal_abb != "SoE") 
data <- data |> bind_rows(soe)


###--- Clean up Sociological Theory
st <- 
  data |>
  filter(journal_abb == "ST") |> 
  filter(str_detect(journals,regex("^sociological theory$")) == TRUE)

st |> count(therist)

data <- data |> filter(journal_abb != "ST") 
data <- data |> bind_rows(st)


###---

saveRDS(data, output_file)

###---

journals <- data |> distinct(journal_abb) |> pull()

tbl <- 
  data |> 
  count(journal_abb, journals) |> 
  arrange(journal_abb,desc(n)) |> 
  group_by(journal_abb) |> 
  group_split()


View(tbl[[8]])

# View(data |>
#        filter(journal_abb == "SF") |> 
#        count(theorist, journals) )
#   



###--- Describe
data |> 
  count(theorist,journal_abb) |> 
  arrange(journal_abb)




data |> 
  group_by(titles) |>
  mutate(n = n(),
         n_t = n_distinct(theorist)) |> 
  arrange(desc(n))
  count(theorist) |> 
  arrange(desc(n))
