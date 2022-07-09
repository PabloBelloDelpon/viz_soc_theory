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
input_file <- "viz_soc_theory_data.RDS"
output_folder <- "data_viz"
journal_names_file <- "input_data/journal_names.RDS"


###--- Read the data
data <- readRDS(input_file)
journal_names <- readRDS(journal_names_file)

###--- Check the data

data <- data |> drop_na(cited)

data |> 
  distinct(theorist,journal_abb) |> 
  count(theorist)



###--- Graphic settings

plot_set()

###--- Colors for theorists
colors <- met.brewer(name = "Cross",n = length(unique(data$theorist)))
scale_1 <- scale_color_manual(values = colors)


###--- Caption with journal abbreviations
caption <- paste0(unique(data$journal_abb),collapse = ", ")
caption <- paste0("Journals: ",caption)



###--- Count theorist - year observations 
data_counts <- 
  data |> 
  count(journal_type, theorist_type, journal_abb,theorist,years)



###--- By Journal

###--- Transform the data
data_plot1 <-
  data_counts |> 
  left_join(journal_names |> rename(journal_abb = abb)) |> 
  ungroup() |> 
  mutate(id = paste(journal_abb,theorist)) |> 
  group_by(theorist,journal_abb) |> 
  mutate(n = rollmean(x = n,k = 5,fill = NA))


colors

###--- Plot
(plot1 <- 
  data_plot1 |> 
  ggplot(aes(years,n,group = id, color = theorist)) +
  geom_line() +
  labs(title = "Popularity of (some) sociological theorists by type of journal",
       caption = paste(caption,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles") +
  facet_wrap(~ journal,scales = "free_y") +
  scale_1)


# ggsave(paste0(output_folder,"/plot_1.png"),dpi=320)  
# showtext_auto(FALSE)



###--- By type of Journal and type of theorist

data_plot2 <- 
  data_plot1 |> 
  group_by(theorist_type) |> 
  group_split() 

scale_2 <- setNames(colors[1:length(unique(data$theorist))], unique(data$theorist))


plots <- lapply(data_plot2, function(x){
  x |> 
    ggplot(aes(years,n,group = id, color = theorist)) +
    geom_line() +
    labs(title = unique(x$theorist_type),
         #caption = paste(c,"\nSliding window of 4 years"),
         color = "",
         x = "",
         y = "# Articles") +
    scale_color_manual(values = scale_2[which(names(scale_2) %in% unique(x$theorist))]) +
    facet_wrap(~ journal,scales = "free_y")
  })

plot_grid(plotlist = plots,nrow = 3) 

ggsave(paste0(output_folder,"/plot_2.png"),dpi=320,width = 27,height = 40,units = "cm")  

tbl5 <-
  data_counts |> 
  group_by(theorist,years) |> 
  summarise(theorist_type = unique(theorist_type),
            n = sum(n),
            .groups = "drop") |> 
  group_by(theorist) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ungroup() 
  


tbl5_split <- split(tbl5, tbl5$theorist_type)

colors <- scales::hue_pal()(n = length(unique(tbl5$theorist)))
colors <- list(
  "Analytical Sociology" = colors[1:3],
  "Classical" = colors[4:5],
  "Modern" = colors[6:8])

ggplot(mapping = aes(x = years, y = n)) +
  purrr::imap(tbl5_split, function(x, y) {
    # Get order
    order <- which(levels(tbl5$theorist_type) == y)
    list(
      geom_line(data = x, aes(color = theorist)),
      scale_color_manual(values = colors[[y]], guide = guide_legend(order = order, title = y)),
      new_scale("color")
    )
  }) +
  facet_wrap(~ theorist_type) +
  labs(title = "The Evolution of Sociological Theory",
       caption = paste(caption,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")

ggsave(paste0(output_folder,"/plot_3.png"),dpi=320)  




