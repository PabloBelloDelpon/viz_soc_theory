###--- Libraries
suppressMessages(library(tidyverse))
library(sysfonts)
library(showtext)
source("helpers_data_viz.R")


###--- Files and folders 
input_file <- "lived_experience/lived_experience_data.RDS"
output_folder <- "lived_experience/data_viz"


###--- Read the data
data <- readRDS(input_file)
data  <- 
  data |> 
  filter(! journal_abb %in% c("SoE"))

###--- Create article id
title_id <- data |> 
  distinct(titles, journal_abb) |> 
  arrange(desc(titles)) |> 
  mutate(paper_id = row_number())


###--- Graphic settings
plot_set()

###--- Caption with journal abbreviations
caption <- paste0(unique(data$journal_abb),collapse = ", ")
caption <- paste0("Journals: ",caption)


###--- 
data |> 
  mutate(decade = floor(years /10) * 10) |> 
  count(decade) |> 
  filter(decade < max(decade)) |> 
  ggplot(aes(decade,n, group = 1)) +
  geom_line() +
  geom_point() +
  labs(y = "Number of articles",
       x = "",
       title = "Popularity of \"lived experience\" by decade",
       caption = caption)

ggsave(paste0(output_folder,"/plot_1.png"),dpi=320)  
