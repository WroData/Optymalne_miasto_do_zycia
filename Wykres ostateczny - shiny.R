library(shiny)
library(plotly)

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
    plotOutput('plot1')
  )
 
  )

server <- function(input, output){

  
  output$plot1 <-  renderPlot({ 
    dane <- cbind(temp2, runmed(temp2[,4], input$num, endrule = "median", algorithm = NULL, print.level = 0))
    names(dane) <- c(names(temp2), "aa")

    
    a <- ggplot(dane, aes(x=Population, y=-QL.numbeo)) +
          annotate("rect", xmin=min(Wielka_Warszawa), xmax=max(Wielka_Warszawa), ymin=-200, ymax=0, alpha=0.2, fill="blue") +
          annotate("text", x=mean(Wielka_Warszawa), y= -10 , label = "Wielka Warszawa", col="blue") +
          geom_point(aes(col=Czy.stolica, shape = Czy.Wroclaw, size = Czy.Polska)) +    
          geom_path(aes(x=Population, y=-aa)) +
          #geom_smooth(se=FALSE)+
          geom_smooth(method=lm, se=FALSE) +
          labs(title= c(names(temp[5]), "vs QL.mercer")) +
          scale_x_continuous(limits = c(0, 5000000), labels = space) +
          scale_y_continuous(labels = minus) +
          labs(title="Zale?no?? wielko?ci miasta od jako?ci ?ycia", 
               y= "Miejsce w rankingu jako?ci ?ycia w miastach przeprowadzonym przez Numbeo", 
               x="Populacja miasta",  col="Czy miasto jest stolic??", shape="# of gears") +
          scale_color_manual(values=kolory2,   name="Czy miasto jest      \nstolic??",
                               breaks=c("PRAWDA", "FA?SZ" ), labels=c("Tak", "Nie" )) +
          scale_shape_manual(values=c(16, 8),  name="Czy to Wroc?aw?",
                               breaks=c("PRAWDA", "FA?SZ" ), labels=c("Tak", "Nie" )) +
          scale_size_manual (values=c(2, 5),   name="Czy to Polska?         ",
                               breaks=c("PRAWDA", "FA?SZ" ), labels=c("Tak", "Nie" )) +
          theme( 
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks=element_blank(), 
            axis.text.x=element_text(size=wielkosc_czcionki, face="bold", hjust=0.4, family="Corbel", colour=kolor_czcionki),
            axis.text.y=element_text(size=wielkosc_czcionki, face="bold", hjust=0.4, family="Corbel", colour=kolor_czcionki),
            plot.title = element_text(size=wielkosc_czcionki*1.5, family="Corbel", hjust=0.5, face="bold"),
            plot.background   = element_rect(fill = kolot_tla, colour = NA),
            panel.background  = element_rect(fill = kolot_tla, colour = NA),
            legend.title = element_text(size=wielkosc_czcionki*1.2, face="bold", family="Corbel", colour=kolor_czcionki),
            legend.text = element_text(size=wielkosc_czcionki, face="bold", family="Corbel", colour=kolor_czcionki),
            legend.background = element_rect(fill = kolot_tla, size=.5,  linetype="solid", colour ="grey50"),
            legend.key = element_rect(  fill = kolot_tla, size = 0.5, colour = kolot_tla)
          )
    
    q <- ggplotly(a)
    q
  })
}

shinyApp(ui = ui, server = server)
