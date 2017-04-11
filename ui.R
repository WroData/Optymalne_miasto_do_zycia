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
                             "Dodaj linie mediany krocz?cej" = "MED",
                             "Dodaj wielko?? Warszawy po proponowanych reformach" = "WAW")),
        sliderInput(inputId = "num",
                    label = "Mediana kroczaca obserqacji",
                    value = 25, min = 1, max = 100)
        
    ),
    mainPanel(
        plotlyOutput('plot1')
    )
    
)