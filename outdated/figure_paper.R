plot_set()

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



colors <- 
  tbl5 |> 
  distinct(author_last_name,.keep_all = TRUE) |> 
  mutate(color = scales::hue_pal()(n = n())) |> 
  select(author_type,color) |> 
  group_split(author_type) |> 
  setNames(levels(unique(tbl5$author_type))) |> 
  lapply(. %>% pull(color))

tbl5_split <- split(tbl5, tbl5$author_type)

# colors <- scales::hue_pal()(n = length(unique(tbl5$author_last_name)))
# colors <- list(
#   "classic" = colors[1:nrow(tbl5 |> 
#                                   filter(author_type == "classic") |> 
#                                            distinct(author_last_name))],
#   "modern" = colors[4:7],
#   "canon" = colors[6:9])

p1 <- 
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
  facet_wrap(~ author_type,nrow = 3) +
  labs(#title = "The Evolution of Sociological Theory",
    #caption = paste(caption,"\nSliding window of 4 years"),
    color = "",
    x = "",
    y = "# Articles") +
  scale_x_continuous(breaks = c(1900,1920,1940,1960,1980,2000))

 


###---

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


###---

n1 <- plots[[3]] 
n2 <- plots[[6]]
n3 <- plots[[9]]

n1
ggsave(paste0(output_folder,"/network_50.svg"))  

p1


###--- Put them together
library(patchwork)

p1 / (n1 | n2 | n3) 
ggsave(paste0(output_folder,"/plot_paper.png"),scale = 2)  






