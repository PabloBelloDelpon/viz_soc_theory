###--- Plot settings
plot_set <- function(font = "futura", data){
  
  showtext_auto()
  showtext_opts(dpi = 320)
  
  font_add(family = font, regular = "/Users/pablobellodelpon/Library/Fonts/Futura Light font.ttf")
  

  theme_set(
    theme_minimal() +
      theme(text = element_text(family = font,size = 23),
            strip.text = element_text(size = 23,face = "bold"),
            plot.title = element_text(hjust = .5),
            plot.background = element_rect(fill = "white", color = "white"),
            plot.caption = element_text(hjust = 0, face= "italic")
      ))
  
  
}

