
library(igraph)
library(tidyverse)
library(GGally)
library(cowplot)
library(sysfonts)
library(showtext)
library(ggraph)
source("helpers_data_viz.R")

###--- Files and folders 
input_file <- "final_data/viz_soc_theory_data.RDS"
output_folder <- "data_viz"


###--- Read the data
data <- readRDS(input_file)
title_id <- data |> distinct(titles, journal_abb) |> arrange(desc(titles)) |> mutate(paper_id = row_number())

data_2 <- 
  data |> 
  left_join(title_id) |> 
  group_by(paper_id) |> 
  summarise(theorist = list(author_last_name), 
            years = unique(years),
            journal_name = unique(journal_name),
            n_theorist = n())  



data_2 |> 
  group_by(journal_name) |> 
  summarise(mean_n = mean(n_theorist), sd(n_theorist)) |> 
  arrange(desc(mean_n))


data_2 |> 
  ggplot(aes(n_theorist)) +
  geom_bar() +
 facet_wrap(~ journal_name,scales = "free_y")


# ######################  PLOT 1 ######################  
# ###----  By Journal 
# 
# edge_list <- 
#   data_2 |> 
#   ungroup() |> 
#   select(paper_id,theorist,journal) |> 
#   unnest(theorist) |>
#   group_by(journal) |> 
#   group_split()
# 
# layout(matrix(1:length(edge_list), ncol = 3))
# 
# for(i in 1:length(edge_list)) {
#   
#   
#   mat <- as.matrix(edge_list[[i]] |> select(paper_id,theorist))
#   
#   g <- igraph::graph_from_edgelist(mat)
#   V(g)$type <- bipartite_mapping(g)$type
#   projected_g <- bipartite_projection(g, multiplicity = TRUE)
#   
#   plot(projected_g$proj2,
#        edge.width = E(projected_g$proj2)$weight/max(E(projected_g$proj2)$weight)*5,
#        )
#   
#   title(unique(edge_list[[i]]$journal),col.main="Black")
# }




###---- By Decade
edge_list <- 
  data_2 |> 
  ungroup() |>
  mutate(decade = years - years %% 10) |> 
  filter(decade < 2020 & decade > 1920) |> 
  select(paper_id,theorist,decade) |> 
  unnest(theorist) |>
  group_by(decade) |> 
  group_split() 

names(edge_list) <- sapply(edge_list, function(x) nrow(x))

######################  PLOT 2 ######################  

layout(matrix(1:4,
              #1:length(edge_list), 
              nrow = 2,
              byrow = TRUE))

for(i in 1:4) {
  
  x <- edge_list[[i]]
  
  mat <- as.matrix(x |> select(paper_id,theorist))
  
  g <- igraph::graph_from_edgelist(mat)
  V(g)$type <- bipartite_mapping(g)$type
  projected_g <- bipartite_projection(g, multiplicity = TRUE)
  
  
  V(projected_g$proj2)$color<-"white"
  if ("Bourdieu" %in% names(V(projected_g$proj2)) == TRUE) V(projected_g$proj2)["Bourdieu"]$color<-"red"
  
  plot(projected_g$proj2,
       edge.width = E(projected_g$proj2)$weight/max(E(projected_g$proj2)$weight)*5,
  )
  
  title(unique(edge_list[[i]]$decade),col.main="Black")
  
}

dev.off()


######################  PLOT 3 ######################  
###---- By Decade
network_plot_set()

plots <- list()

for(i in 1:length(edge_list)) {
  
  x <- edge_list[[i]]
  mat <- as.matrix(x |> select(paper_id,theorist))
  
  g <- igraph::graph_from_edgelist(mat)
  V(g)$type <- bipartite_mapping(g)$type
  projected_g <- bipartite_projection(g, multiplicity = TRUE)
  
  
  V(projected_g$proj2)$color<- FALSE
  if ("Bourdieu" %in% names(V(projected_g$proj2)) == TRUE) V(projected_g$proj2)["Bourdieu"]$color<- TRUE
  if ("Parsons" %in% names(V(projected_g$proj2)) == TRUE) V(projected_g$proj2)["Parsons"]$color<- TRUE
  
  plots[[i]] <- 
    ggraph(projected_g$proj2,layout = "fr") +
    geom_edge_link(aes(width = weight), color = "grey70", alpha = .5) +
    scale_edge_width(range = c(.5,4)) +
    geom_node_text(aes(label = name, color = color), size = 7, repel = FALSE) +
    labs(title = paste0(unique(edge_list[[i]]$decade),"'s\nN = ",names(edge_list)[i])) +
    scale_color_manual(values = c("black","red"))
  
}
names(plots) <- unique(data.table::rbindlist(edge_list)$decade)

(plots <- plot_grid(plotlist = plots))


title <- ggdraw() + 
  draw_label("Co-citation network of sociological theorists by decade",
             fontface = 'bold',
             hjust = .5,
             fontfamily = "futura",
             size = 50) +
  theme(
    plot.background = element_rect(fill = "white", color = "white")
    )


  plot_grid(title, plots,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(paste0(output_folder,"/plot_network_decades.png"),dpi = 320,scale = 4)


