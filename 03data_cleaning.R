###--- Libraries
suppressMessages(library(tidyverse))


###--- Files and folders
output_file <- "viz_soc_theory_data2.RDS"
input_data <- "output_data2"
original_data <- "input_data/journals_authors.RDS"

###--- Import data
files <- fs::dir_info(input_data) |> pull(path)
n <- str_remove(files,"output_data2/") # Name the files 
n <- str_remove(n,".RDS")
names(files) <- n

###--- Load files (into a single tibble)
data <- 
  files |> 
  map_dfr(readRDS,.id = "output") 


###--- Merge with previos data
orig_tbl <- readRDS(original_data) 

data <- 
  data |> 
  left_join(orig_tbl)


###--- Clean up data
data <- 
  data |> 
  mutate(years = as.integer(years)) |> 
  drop_na(years) |> 
  mutate(journals = str_to_lower(journals),
         journals = str_squish(journals))


data |> 
  count(journals) |> 
  arrange(desc(n))

###---  Remove Dupes 
data <- 
  data |> 
  group_by(output) |> 
  distinct(titles,.keep_all = TRUE) |> 
  ungroup()




###--- Remove citations to authors 
###--- Save 
saveRDS(data, output_file)




