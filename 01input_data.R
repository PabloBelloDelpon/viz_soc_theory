###--- create input data

###--- Journals
journals_etienne <- read.table("journal_list_etienne.txt",sep = ",")
journals_etienne <- as.vector(t(as.matrix(journals_etienne)))
journals_etienne <- journals_etienne[c(1,2,15,18,31,32)]

journals_others <- c("annual review of sociology","rationality & society",
                     "european sociological review")
journals <- c(journals_etienne,journals_others)
names(journals) <- c("AJS", "ASR", "Poetics","SF","ST","SoE","ARS","R&S","ESR") 
journals <- strsplit(journals," ")

###--- Authors
authors <- c("Bourdieu","Parsons","Boudon","Merton","Coleman","Goffman","Weber","Durkheim")

###--- Put journals and authors together

expand.grid("journal" = journals,"author" = authors) |> 
  as_tibble() |> 
  mutate(author = as.character(author)) |> 
  saveRDS("journals_authors.RDS")
