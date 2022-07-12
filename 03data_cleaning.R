###--- Libraries
suppressMessages(library(tidyverse))


###--- Files and folders
output_file <- "viz_soc_theory_data.RDS"
journal_names_file <- "input_data/journal_names.RDS"


###--- Import data
journal_names <- readRDS(journal_names_file) |> mutate(journal = str_to_title(journal))

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

st |> count(theorist)

data <- data |> filter(journal_abb != "ST") 
data <- data |> bind_rows(st)


###--- 
classical <- c("Weber", "Durkheim")
modern <- c("Bourdieu","Parsons","Goffman")
analytical_soc <- c("Merton","Boudon","Coleman")
general <- c("AJS","ARS","ASR","ESR","SF")
field <- c("R&S","SoE","ST")


data <- 
  data |> 
  mutate(theorist_type = case_when(theorist %in% classical == TRUE ~ "classical",
                                   theorist %in% modern == TRUE ~ "modern",
                                   theorist %in% analytical_soc == TRUE ~ "analytical sociology"
  ),
  journal_type = ifelse(journal_abb %in% general,"general","field")) |> 
  mutate(theorist_type = str_to_title(theorist_type),
         theorist_type = as_factor(theorist_type))





#################
###--- Issues with Bourdieu AJS 2002 (year in which he died)

data <- data |> mutate(journals = na_if(journals,""))

data_bourdieu_2002 <- 
  data |> 
  filter(theorist == "Bourdieu", years == 2002, journal_abb == "AJS")

data <- data |> anti_join(data_bourdieu_2002)
data_bourdieu_2002 <- data_bourdieu_2002 |> drop_na(journals)
data <- data |> bind_rows(data_bourdieu_2002) 



#################

data <- 
  data |> 
  left_join(journal_names |> rename(journal_abb = abb)) 

###--- REMOVE NON-CITED PUBS
data <- data |> drop_na(cited)

###--- REMOVE [ CITATION ]
data <- data |> filter(str_detect(titles, "CITATION") == FALSE)


saveRDS(data, output_file)




