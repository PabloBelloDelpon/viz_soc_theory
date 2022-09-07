###--- Libraries
suppressMessages(library(tidyverse))
library(zoo)
library(MetBrewer)
library(sysfonts)
library(showtext)
library(ggnewscale)
library(cowplot)
source("helpers_data_viz.R")


###--- Files and folders 
input_file <- "final_data/viz_soc_theory_data.RDS"
output_folder <- "data_viz"


###--- Read the data
data <- readRDS(input_file)

###--- Create article id
title_id <- data |> distinct(titles, journal_abb) |> arrange(desc(titles)) |> mutate(paper_id = row_number())

###--- Check the data
data |> 
  distinct(output,.keep_all = TRUE) |> 
  count(author_last_name)


###--- Graphic settings
plot_set()

###--- Colors for theorists
colors <- met.brewer(name = "Cross",n = length(unique(data$author_last_name)))
scale_1 <- scale_color_manual(values = colors)


###--- Caption with journal abbreviations
caption <- paste0(unique(data$journal_abb),collapse = ", ")
caption <- paste0("Journals: ",caption)



###--- Count theorist - year observations 
data_counts <- 
  data |> 
  count(author_type,journal_name,author_last_name,years)


###--- Transform the data
data_plot1 <-
  data_counts |> 
  ungroup() |> 
  mutate(id = paste(journal_name,author_last_name)) |> 
  group_by(author_last_name,journal_name) |> 
  mutate(n = rollmean(x = n,k = 5,fill = NA))


############################ PLOT 1 ############################ 
###--- By Journal
(plot1 <- 
  data_plot1 |> 
  ggplot(aes(years,n,group = id, color = author_last_name)) +
  geom_line() +
  labs(title = "Popularity of (some) sociological theorists by journal",
       caption = paste(caption,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles") +
  facet_wrap(~ journal_name,scales = "free_y") +
  scale_1)


ggsave(paste0(output_folder,"/plot_1.png"),dpi=320)  
#showtext_auto(FALSE)

############################ PLOT 2 ############################ 
###--- By Journal  (single Journal)

###--- Pick a Journal
journal <- unique(data_plot1$journal_name)[4]


(plot2 <- 
   data_plot1 |> 
   filter(journal_name == journal) |> 
   ggplot(aes(years,n,color = author_last_name)) +
   geom_line() +
   labs(title = journal,
        caption = paste(caption,"\nSliding window of 4 years"),
        color = "",
        x = "",
        y = "# Articles") +
   facet_wrap(~ author_last_name,scales = "free_y") +
   scale_1 +
   theme(legend.position = "none",
         text = element_text(size = 10)
         ))

ggsave(paste0(output_folder,"/plot_2.png"),dpi=320)  



############################ PLOT 3 ############################ 
###--- By type of Journal and type of theorist

data_plot2 <- 
  data_plot1 |> 
  group_by(author_type) |> 
  group_split() 

scale_2 <- setNames(colors[1:length(unique(data$author_last_name))], unique(data$author_last_name))


plots <- lapply(data_plot2, function(x){
  x |> 
    ggplot(aes(years,n,group = id, color = author_last_name)) +
    geom_line() +
    labs(title = unique(x$author_type),
         #caption = paste(c,"\nSliding window of 4 years"),
         color = "",
         x = "",
         y = "# Articles") +
    scale_color_manual(values = scale_2[which(names(scale_2) %in% unique(x$author_last_name))]) +
    facet_wrap(~ journal_name,scales = "free_y",ncol = 3)
  })

plot_3 <- plot_grid(plotlist = plots,nrow = 3) 

ggsave(paste0(output_folder,"/plot_3.png"),dpi=320,width = 27,height = 40,units = "cm")  



############################ PLOT 4 ############################ 

tbl5 <-
  data_counts |> 
  mutate(author_type = as_factor(author_type)) |> 
  group_by(author_last_name,years) |> 
  summarise(author_type = unique(author_type),
            n = sum(n),
            .groups = "drop") |> 
  group_by(author_last_name) |> 
  mutate(n = rollmean(x = n,k = 5,fill = NA)) |> 
  ungroup() 
  
tbl5_split <- split(tbl5, tbl5$author_type)

colors <- scales::hue_pal()(n = length(unique(tbl5$author_last_name)))
colors <- list(
  "classic" = colors[1:3],
  "modern" = colors[4:5],
  "canon" = colors[6:7])

ggplot(mapping = aes(x = years, y = n)) +
  purrr::imap(tbl5_split, function(x, y) {
    # Get order
    order <- which(levels(tbl5$author_type) == y)
    list(
      geom_line(data = x, aes(color = author_last_name)),
      scale_color_manual(values = colors[[y]], guide = guide_legend(order = order, title = y)),
      new_scale("color")
    )
  }) +
  facet_wrap(~ author_type) +
  labs(title = "The Evolution of Sociological Theory",
       caption = paste(caption,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")

ggsave(paste0(output_folder,"/plot_4.png"),dpi=320)  


###--- Measure segmentation 
# How much do the set of papers the cite author X overlap with the set of paper that cite Y?
# More segmentation means less overlap. Segmentation has decreased which means that, from the population 
# of papers that cite at least one of our list of authors, they increasingly cite a variety of them instead of just 1. 

seg_tbl <-
  data |>
  select(output,titles,years) |>
  left_join(title_id) |>
  select(-titles) |>
  group_by(years) |>
  summarise(n = n(), n_unique = n_distinct(paper_id), seg = n_unique/n)



seg_tbl |>
  filter(n_unique > 20) |>
  mutate(div = rollmean(x = seg,k = 5,fill = NA)) |>
  ggplot(aes(years,seg, group = 1)) +
  geom_line(aes(color = n_unique))



# ###--- 
# 
# diversity_tbl <- 
#   data |> 
#   select(output,titles,years) |> 
#   left_join(title_id) |> 
#   select(-titles) |> 
#   group_by(paper_id) |>
#   summarise(n = n(), years = unique(years)) |> 
#   ungroup() |> 
#   group_by(years) |> 
#   summarise(n_papers_year = n(), av_authors = mean(n))
# 
# diversity_tbl |> 
#   ggplot(aes(years,av_authors)) +
#   geom_point(alpha = .2)
# 
#   summarise(n = n(), n_unique = n_distinct(paper_id), div = n_unique/n)


# 
# diversity_tbl |> 
#   filter(n_unique > 30) |> 
#   mutate(div = rollmean(x = div,k = 5,fill = NA)) |> 
#   ggplot(aes(years,div, group = 1)) +
#   geom_line(aes(color = n_unique))





