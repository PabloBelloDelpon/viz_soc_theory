###--- Libraries
suppressMessages(library(tidyverse))
library(zoo)
library(MetBrewer)
library(sysfonts)
library(showtext)
library(ggnewscale)


###--- Files and folders 
input_file <- "viz_soc_theory_data.RDS"
output_folder <- "data_viz"
journal_names_file <- "input_data/journal_names.RDS"


###--- Read the data
data <- readRDS(input_file)
journal_names <- readRDS(journal_names_file)


###---
theorist_selection <- c("Weber", "Parsons","Goffman","Bourdieu")
journal_selection <- c("AJS","ASR","ST","SoE")
classical <- c("Weber", "Durkheim")
contemp <- c("Bourdieu","Parsons","Goffman")
analytical_soc <- c("Merton","Boudon","Coleman")
general <- c("AJS","ARS","ASR","ESR","SF")
field <- c("R&S","SoE","ST")


tbl <- 
  data |> 
  mutate(theorist_type = case_when(theorist %in% classical == TRUE ~ "classical",
                                   theorist %in% contemp == TRUE ~ "modern",
                                   theorist %in% analytical_soc == TRUE ~ "analytical sociology"
                                   ),
         journal_type = ifelse(journal_abb %in% general,"general","field"))

  
  

###---
tbl |> 
  distinct(theorist,journal_abb) |> 
  count(theorist)

###--- First plot. General 
tbl2 <- 
  tbl |> 
  #drop_na(cited) |>  # there are some weird cases for Bourdieu in 2002 (year in which he), revise this 
  distinct(abstract,.keep_all = TRUE) |> 
  count(journal_type, theorist_type, journal_abb,theorist,years)


tbl2 |> count(journal_abb,theorist)


###--- Plot settings
showtext_auto()
showtext_opts(dpi = 320)
font <- "futura"
font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/Futura Light font.ttf")

colors <- met.brewer(name = "Cross",n = length(unique(data$theorist)))
colors <- scale_color_manual(values = colors)

c <- paste0(unique(tbl2$journal_abb),collapse = ", ")
c <- paste0("Journals: ",c)

theme_set(
  theme_minimal() +
  theme(text = element_text(family = font,size = 17),
        strip.text = element_text(size = 17,face = "bold"),
        plot.title = element_text(hjust = .5),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.caption = element_text(hjust = 0, face= "italic")
  ))




###--- By type of theorist

tbl2 |> 
  group_by(theorist,years) |> 
  summarise(theorist_type = unique(theorist_type),
            n = sum(n),
            .groups = "drop") |> 
  group_by(theorist) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ungroup() |> 
  ggplot(aes(years,n,group = theorist, color = theorist)) +
  geom_line() +
  labs(title = "Theorist popularity",
       caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles") +
  facet_grid(~ theorist_type)




###--- By Journal

###--- Transform the data
tbl3 <-
  tbl2 |> 
  left_join(journal_names |> rename(journal_abb = abb)) |> 
  ungroup() |> 
  mutate(id = paste(journal_abb,theorist)) |> 
  group_by(theorist,journal_abb) |> 
  mutate(n = rollmean(x = n,k = 5,fill = NA))




###--- Plot
(plot2 <- 
  tbl3 |> 
  ggplot(aes(years,n,group = id, color = theorist)) +
  geom_line() +
  labs(title = "Popularity of (some) sociological theorists by type of journal",
       #caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles") +
  facet_wrap(~ journal,scales = "free_y") +
  colors)



# ggsave(paste0(output_folder,"/plot_1.png"),dpi=320)  
# showtext_auto(FALSE)




###--- By type of Journal and type of theorist

tbl4 <- 
  tbl3 |> 
  group_by(theorist_type) |> 
  group_split() 


plots <- lapply(tbl4, function(x){
  x |> 
    ggplot(aes(years,n,group = id, color = theorist)) +
    geom_line() +
    labs(title = unique(x$theorist_type),
         #caption = paste(c,"\nSliding window of 4 years"),
         color = "",
         x = "",
         y = "# Articles") +
    facet_wrap(~ journal,scales = "free_y")
  })

poster <- cowplot::plot_grid(plotlist = plots,nrow = 3)

ggsave(paste0(output_folder,"/plot_2.png"),dpi=320,width = 27,height = 40,units = "cm")  
showtext_auto(FALSE)





###################


tbl5 <-
  tbl2 |> 
  group_by(theorist,years) |> 
  summarise(theorist_type = unique(theorist_type),
            n = sum(n),
            .groups = "drop") |> 
  group_by(theorist) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ungroup() |> 
  mutate(theorist_type = str_to_title(theorist_type),
         theorist_type = as_factor(theorist_type))


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
       caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")

ggsave(paste0(output_folder,"/plot_3.png"),dpi=320)  




