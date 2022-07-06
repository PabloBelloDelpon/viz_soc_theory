
###--- Data Viz

###--- Helper functions
library(silgelib)

###--- Set a theme for plots


theme_figs <- function(font_family){
  #theme_roboto(base_size = 20)
  theme_minimal(base_size = 30) +
    theme(
      plot.title = element_text(size = rel(1.5), hjust = .5,vjust = 2),
      plot.subtitle = element_text(size = rel(.8), hjust = .5),
      axis.title.x = element_text(size = rel(1)),
      #axis.title.y = element_text(size = rel(1)),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = rel(1)),
      #axis.text.y.left = element_text(size = rel(1.2)),
      axis.text.y = element_blank(),
      #axis.text.y.right = element_text(size = rel(0.9)),
      legend.position = "none",
      panel.grid = element_blank(),
      text = element_text(family = font_family)
    )
}

