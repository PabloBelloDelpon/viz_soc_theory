
suppressMessages(library(tidyverse))
library(zoo)
library(MetBrewer)
library(sysfonts)
library(showtext)

###--- Files and folders
journal_names_file <- "input_data/journal_names.RDS"
output_folder <- "data_viz"

###--- Import data
files <- list.files("output_data",full.names = TRUE)

data <- lapply(files,function(x){
  j <- str_remove(x,"output_data/")
  j <- str_remove(j,".RDS")
  j <- str_split(j,"_")
  journal <- sapply(j,function(i) i[1])
  author <- sapply(j,function(i) i[2])
  
  x <- readRDS(x)
  if(class(x) == "list"){
    x <- x[["data"]]
  }
  
  x <- x |> mutate(journal_abb = journal,
                   theorist = author)
  
  return(x)
}) |> 
  bind_rows()


###--- Clean up data
data <- 
  data |> 
  mutate(years = as.integer(years)) |> 
  drop_na(years) |> 
  filter(years > 1940 & years < 2022)

###--- Describe
data |> 
  count(theorist,journal_abb) |> 
  arrange(journal_abb)


###--- Bourdieu
# bourdieu <- 
#   data |> 
#   filter(theorist == "Bourdieu")
# 
# bourdieu2 <- 
#   bourdieu |> 
#   count(theorist,journal_abb,years)
# 
# bourdieu2 |> 
#   mutate(n = rollmean(x = n,k = 3,fill = NA)) |> 
#   ggplot(aes(years,n,group = journal_abb, color = journal_abb)) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = unique(bourdieu2$theorist),
#        color = "",
#        x = "",
#        y = "Number of papers citing")



###--- 
journal_names <- readRDS(journal_names_file)
theorist_selection <- c("Weber", "Parsons","Goffman","Bourdieu")
journal_selection <- c("AJS","ASR","ST","SoE")


tbl <- 
  data |> 
  filter(journal_abb %in% journal_selection) |> 
  filter(theorist %in% theorist_selection)


tbl |> 
  distinct(theorist,journal_abb) |> 
  count(theorist)


tbl2 <- 
  tbl |> 
  drop_na(cited) |>  # there are some weird cases for Bourdieu in 2002 (year in which he), revise this 
  distinct(abstract,.keep_all = TRUE) |> 
  count(journal_abb,theorist,years)


tbl2 |> count(journal_abb,theorist)

c <- paste0(unique(tbl2$journal_abb),collapse = ", ")
c <- paste0("Journals: ",c)


tbl2 |> 
  group_by(theorist,years) |> 
  summarise(n = sum(n)) |> 
  mutate(n = rollmean(x = n,k = 4,fill = NA)) |> 
  ggplot(aes(years,n,group = theorist, color = theorist)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Papers citing Bourdieu and Parsons",
       caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles")

###--- Break down by journal

###--- Transform the data
tbl3 <-
  tbl2 |> 
  left_join(journal_names |> rename(journal_abb = abb)) |> 
  ungroup() |> 
  mutate(id = paste(journal_abb,theorist)) |> 
  group_by(theorist,journal_abb) |> 
  mutate(n = rollmean(x = n,k = 5,fill = NA))


###--- Plot settings
showtext_auto()
showtext_opts(dpi = 320)
font <- "futura"
font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/Futura Light font.ttf")

colors <- met.brewer(name = "Cross",n = length(theorist_selection))
colors <- scale_color_manual(values = colors)


###--- Plot
plot2 <- 
  tbl3 |> 
  ggplot(aes(years,n,group = id, color = theorist)) +
  geom_line() +
  labs(title = "Popularity of (some) sociological theorists over time",
       #caption = paste(c,"\nSliding window of 4 years"),
       color = "",
       x = "",
       y = "# Articles") +
  facet_wrap(~ journal,scales = "free_y") +
  theme_minimal() +
  colors 

plot2 +
  theme(text = element_text(family = font,size = 17),
        strip.text = element_text(size = 17,face = "bold"),
        plot.title = element_text(hjust = .5),
        plot.background = element_rect(fill = "white", color = "white")
        )

ggsave(paste0(output_folder,"/plot_1.png"),dpi=320)  
showtext_auto(FALSE)


