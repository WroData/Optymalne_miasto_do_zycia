library(shiny)
library(plotly)
library(extrafont)


ui <- fluidPage(
    headerPanel('Zaleznosc wielkosci miasta od jakosci zycia'),
    sidebarPanel(
        checkboxGroupInput("variable", "Dodatkowe fikuse:",
                           c("Wyroznij Wroc?aw" = "WRO",
                             "Wyroznij miasta Polskie" = "PL",
                             "Wyroznij stolice panstw" = "ST",
                             "Dodaj linie trendu" = "TR",
                             "Dodaj linie mediany krocz¹cej" = "MED",
                             "Dodaj linie œredniej krocz¹cej" = "SR",
                             "Dodaj wielkoœæ Warszawy po proponowanych reformach" = "WAW")
                           ),
        
        conditionalPanel( condition = "input.items.includes('MED')",
                          sliderInput(inputId = "num",
                                      label = "Mediana kroczaca obserwacji",
                                      value = 25, min = 1, max = 75)
                        ),
        conditionalPanel( condition = "input.items.includes('MED')",
                          sliderInput(inputId = "num_sr",
                                      label = "Œrednia kroczaca obserwacji",
                                      value = 25, min = 1, max = 75)
                        )
        
    ),
    mainPanel(
      plotOutput('plot1',  click = "plot_click", brush = brushOpts(id = "plot1_brush")),
      h4("Points near click"),
      verbatimTextOutput("click_info"),
      h4("Brushed points"),
      verbatimTextOutput("brush_info")
      
      )
      
         
            
    
)
