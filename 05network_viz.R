
library(igraph)
library(tidyverse)

###--- Files and folders 
input_file <- "viz_soc_theory_data.RDS"


###--- Read the data
data <- readRDS(input_file)

data_2 <- 
  data |> 
  group_by(titles,authors) |> 
  summarise(theorist = list(theorist), years = unique(years),journal = unique(journal), n_theorist = n()) |> 
  mutate(pub_id = row_number())


data_2 |> 
  group_by(journal) |> 
  summarise(mean_n = mean(n_theorist), sd(n_theorist)) |> 
  arrange(desc(mean_n))


data_2 |> 
  ggplot(aes(n_theorist)) +
  geom_histogram() +
 facet_wrap(~ journal,scales = "free_y")



###----  By Journal 

edge_list <- 
  data_2 |> 
  ungroup() |> 
  select(pub_id,theorist,journal) |> 
  unnest(theorist) |>
  group_by(journal) |> 
  group_split()

layout(matrix(1:length(edge_list), ncol = 3))

for(i in 1:length(edge_list)) {
  
  
  mat <- as.matrix(edge_list[[i]] |> select(pub_id,theorist))
  
  g <- igraph::graph_from_edgelist(mat)
  V(g)$type <- bipartite_mapping(g)$type
  projected_g <- bipartite_projection(g, multiplicity = TRUE)
  
  plot(projected_g$proj2,
       edge.width = E(projected_g$proj2)$weight/max(E(projected_g$proj2)$weight)*5,
       )
  
  title(unique(edge_list[[i]]$journal),col.main="Black")
}


###---- By Decade

edge_list <- 
  data_2 |> 
  ungroup() |>
  mutate(decade = years - years %% 10) |> 
  select(pub_id,theorist,decade) |> 
  unnest(theorist) |>
  group_by(decade) |> 
  group_split()

layout(matrix(1:length(edge_list), nrow = 3,byrow = TRUE))

for(i in 1:length(edge_list)) {
  
  
  mat <- as.matrix(edge_list[[i]] |> select(pub_id,theorist))
  
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

title("evo",col.main="Black")


layout(matrix(c(1,2,3,4,1,5,3,6),ncol=2),heights=c(1,3,1,3))
