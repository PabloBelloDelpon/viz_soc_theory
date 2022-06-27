

###--- Viz

tbl <- readRDS("sociological_theory_bourdieu.RDS")




tbl <- 
  tbl |>
  filter(!row_number() %in% c(20)) |>
  filter(years < 2023) |> 
  mutate(years = as.integer(years))


tbl |>
  count(years) |> 
  arrange(years) |> 
  ggplot(aes(years,n, group = 1)) +
  geom_line() +
  theme_minimal()