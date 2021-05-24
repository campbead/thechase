theme_campbead <- function(){
  font_main <- "Karla"
  font_numbers <- "Fira Mono"

  theme_minimal() %+replace%
    theme(

      #grid elements
      #panel.grid.major = element_blank(),    #strip major gridlines
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks = element_blank(),          #strip axis ticks

      plot.background = element_rect(
        fill = "ghostwhite"
      ),
      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = element_text(             #title
        family = font_main,                  #set font family
        size = 20,                           #set font size
        face = 'bold',                       #bold typeface
        margin=margin(
          t = 4, r = 0, b = 4, l = 0, unit = "pt"
        )
      ),

      plot.subtitle = element_text(          #subtitle
        family = font_main,                  #font family
        size = 14,                           #font size
        margin=margin(
          t = 4, r = 0, b = 4, l = 0, unit = "pt"
        )
      ),

      plot.caption = element_text(           #caption
        family = font_main,                  #font family
        size = 9,                            #font size
        hjust = 1                            #right align
        ),

      legend.title = element_text(
        family = font_main,
        size = 12,
        face = 'bold'
        ),

      legend.text = element_text(
        family = font_main,
        size = 9
        ),

      legend.key.size = unit(0.5, "cm"),

      legend.background = element_rect(fill = "#dfdfff",size = 0.5, color = "#acacff"),

      axis.title = element_text(             #axis titles
        family = font_main,            #font family
        face = 'bold',
        size = 12               #font size
      ),

      axis.text = element_text(              #axis text
        family = font_numbers,            #axis famuly
        size = 8                        #font size
        ),

      axis.text.x = element_text(            #margin for axis text
        margin=margin(t = 4, r = 0, b = 4, l = 0, unit = "pt"),
        angle = 30
        ),

      legend.position = "top",

      legend.box = "horizontal"
    )

}

theme_campbead_dark <- function(){
  theme_campbead() %+replace%
    theme(
      plot.background = element_rect(
        fill = "black"
        ),
      #panel.background = element_rect(
      #  color = "black",
      #  fill = "black"
      #  ),
      plot.title = element_text(
        color = "white"
      )
    )
}
