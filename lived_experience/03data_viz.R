###--- Libraries
suppressMessages(library(tidyverse))
library(sysfonts)
library(showtext)
library(ggrepel)
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
title_id <- 
  data |> 
  distinct(titles, journal_abb) |> 
  arrange(desc(titles)) |> 
  mutate(paper_id = row_number())

###--- Check the data
data |> 
  distinct(output,.keep_all = TRUE) |> 
  count(keyword)


###--- Graphic settings
plot_set()


###--- Caption with journal abbreviations
caption <- paste0(unique(data$journal_abb),collapse = ", ")
caption <- paste0("Journals: ",caption)


###--- 
data |> 
  mutate(decade = floor(years /10) * 10) |> 
  mutate(decade = lubridate::ymd(decade, truncated = 2L)) |> 
  count(keyword,decade) |> 
  filter(decade < max(decade)) |> 
  mutate(label = if_else(decade == max(decade), keyword, NA_character_)) |> 
  ggplot(aes(decade,n, group = keyword, color = keyword)) +
  geom_line() +
  geom_point() +
  # geom_label_repel(aes(label = label),
  #                  nudge_x = 1,
  #                  nudge_y = 
  #                  na.rm = TRUE) +
  xlim(c(as.Date("1950-01-01"),as.Date("2010-01-01"))) +
  labs(y = "Number of articles",
       x = "",
       title = "Concept popularity by decade",
       caption = caption,
       color = "") 

ggsave(paste0(output_folder,"/plot_2.png"),dpi=320)  



###--- 
data |> 
  mutate(decade = floor(years /10) * 10) |> 
  count(keyword,journal_name,decade) |> 
  filter(decade < 2020) |> 
  mutate(label = if_else(decade == max(decade), keyword, NA_character_)) |> 
  ggplot(aes(decade,n, group = keyword, color = keyword)) +
  geom_line() +
  geom_point() +
  # geom_label_repel(aes(label = label),
  #                  nudge_x = 1,
  #                  nudge_y = 
  #                  na.rm = TRUE) +
  xlim(c(1930,2010)) +
  labs(y = "Number of articles",
       x = "",
       title = "Concept popularity by decade",
       caption = caption) +
  facet_wrap(~ journal_name,nrow = 3)

ggsave(paste0(output_folder,"/plot_3.png"),dpi=320)  




###---
View(data |> 
  filter(keyword == "habitus") |> 
  arrange(years) |> 
  head())
