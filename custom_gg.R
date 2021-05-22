theme_campbead <- function(){
  font_main <- "Karla"
  font_numbers <- "Fira Mono"

#   style = list(fontFamily = "Fira Mono, Consolas,                                                         Monaco, monospace",
#                fontSize = "13.5px",
#                whiteSpace = "pre"),
#   ),
#
# columns = list(
#   STATE = colDef(
#     name = "REGION",
#     align = "left",
#     width = 120,
#     style = list(fontFamily = "Karla, Helvetica Neue, Helvetica, Arial, sans-serif",
#                  fontSize = "13.5px",

  theme_minimal() %+replace%
    theme(

      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks

      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = element_text(             #title
        family = font_main,            #set font family
        size = 20,                #set font size
        face = 'bold',
        margin=margin(
          t = 2, r = 0, b = 8, l = 0, unit = "pt"
          )
        ),            #bold typeface
        #hjust = 1,                #left align
        #vjust = 1),               #raise slightly

      plot.subtitle = element_text(          #subtitle
        family = font_main,            #font family
        size = 14,
        margin=margin(
          t = 2, r = 0, b = 8, l = 0, unit = "pt"
        )
      ),               #font size

      plot.caption = element_text(           #caption
        family = font_main,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      legend.title = element_text(
        family = font_main,
        size = 10),

      legend.text  = element_text(
        family = font_main,
        size = 9),

      axis.title = element_text(             #axis titles
        family = font_main,            #font family
        size = 10),               #font size

      axis.text = element_text(              #axis text
        family = font_numbers,            #axis famuly
        size = 9),                #font size

      axis.text.x = element_text(            #margin for axis text
        margin=margin(t = 4, r = 0, b = 4, l = 0, unit = "pt"))

      #since the legend often requires manual tweaking
      #based on plot content, don't define it here
    )
  }
