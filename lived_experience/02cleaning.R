###--- Libraries
suppressMessages(library(tidyverse))


###--- Files and folders
output_file <- "lived_experience/lived_experience_data.RDS"
input_data <- "lived_experience/output"
original_data <- "lived_experience/journal_tbl.RDS"

###--- Import data
files <- fs::dir_info(input_data) |> pull(path)
n <- str_remove(files,"lived_experience/output/") # Name the files 
n <- str_remove(n,".RDS")
names(files) <- n


###--- Load files (into a single tibble)
data <- 
  files |> 
  map_dfr(readRDS,.id = "output") |> 
  mutate(output = str_split(output,"_")) |> 
  mutate(output = purrr::map(output, setNames, c("journal_abb","keyword","journal_site"))) |> 
  unnest_wider(col = output) |> 
  mutate(keyword = if_else(keyword == "cultural schemas", "cultural schema",keyword)) ###--- Rcombine cultural schema and schemas

  
###--- Merge with input data
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



###--- Save 
saveRDS(data, output_file)




