library(fpp)  
library(forecast) 

#setwd("C:/Users/User/Documents/GitHub/Optymalne_miasto_do_zycia")
#danee <- read.csv2("Dane.csv", header = T) 


skalar <- 100000

#temp <- danee[,c(1:6,9,7,8,10,12)]  
#temp$Population <- temp$Population / skalar
#temp <- temp[order(temp[,5]),]
#temp <- temp[!is.na(temp[,4]),]
#dane <- temp[, c(1, 2, 4, 5)] 
#saveRDS(dane, file="dane.Rds")

dane <- readRDS("dane.Rds")

Wielka_Warszawa <- c(3250000,3750000) / skalar


kolot_tla <- "grey90"
kolor_czcionki <- "grey30"
kolory2<-c('#fe9929','#993404') #skala kolor?w 
wielkosc_czcionki <- 10


### funkcja zmiany formatu liczb do dodania spacji co 1 000
space <- function(x, ...) { 
    format(x * skalar, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}
### funkcja zmiany formatu liczb do odwrotnosci (aby ranking by? logiczny)
minus <- function(x, ...) { 
    format(-x, ..., scientific = FALSE, trim = TRUE)
}



server <- function(input, output){
    
    
    output$plot1 <-  renderPlot({
        

        dane <- data.frame(
          cbind(dane, runmed(dane[,3], input$num, endrule = "median", algorithm = NULL, print.level = 0)))
        names(dane) <- c(names(dane[,1:4]), "aa")
        dane$bb <-  ma(dane$QL.numbeo, order = input$num_sr, centre = T)

        
        
        a <- ggplot(dane, aes(x=Population, y=-QL.numbeo)) +
            geom_point() +
            scale_x_continuous(limits = c(0, 5000000 / skalar), labels = space) +
            scale_y_continuous(labels = minus) +
            labs(title="Zależność wielkosści miasta od jakości życia", 
                 y= "\nMiejsce w rankingu jakości życia w miastach\nprzeprowadzonym przez Numbeo", 
                 x= "\nPopulacja miasta") +
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
        
        
        if("WAW" %in% input$variable ){
        a <- a +  annotate("rect", xmin=min(Wielka_Warszawa), xmax=max(Wielka_Warszawa), ymin=-180, ymax=0, alpha=0.2, fill="blue") +
             annotate("text", x=mean(Wielka_Warszawa), y= -10 , label = "Wielka Warszawa", col="blue")
        }
        
        if("TR" %in% input$variable){
          a <- a + geom_smooth(method=lm, se=FALSE)
        }
        if("MED" %in% input$variable){
          a <- a + geom_path(aes(x=Population, y=-aa))
        }
        if("SR" %in% input$variable){
          a <- a + geom_path(aes(x=Population, y=-bb), col="red")
        }
        
        if(  "WRO" %in% input$variable  & 
           !("PL"  %in% input$variable) &
           !("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(shape = Czy.Wroclaw)) +  
            scale_shape_manual(values=c(16, 8),  name="Czy to Wrocław?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) 
        }        
        
        if( (!("WRO" %in% input$variable) && 
              ("PL"  %in% input$variable) &&
             !("ST"  %in% input$variable)) ){
          a <- a + 
            geom_point(aes(size = Czy.Polska)) +   
            scale_size_manual (values=c(2, 5),   name="Czy to Polska?     ",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" ))
        }
        if( !("WRO" %in% input$variable) & 
            !("PL"  %in% input$variable) &
             ("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(col=Czy.stolica)) +
            scale_color_manual(values=kolory2,   name="Czy miasto jest    \nstolicą?",
                             breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) 
            
        }
        
        if(   ("WRO" %in% input$variable) & 
              ("PL"  %in% input$variable) &
              !("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(shape = Czy.Wroclaw, size = Czy.Polska)) +  
            scale_shape_manual(values=c(16, 8),  name="Czy to Wrocław?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" ))  + 
            scale_size_manual (values=c(2, 5),   name="Czy to Polska?     ",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" ))
          
        } 
        
        if(    ("WRO" %in% input$variable) & 
              !("PL"  %in% input$variable) &
               ("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(shape = Czy.Wroclaw, col=Czy.stolica)) +  
            scale_shape_manual(values=c(16, 8),  name="Czy to Wrocław?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" ))  + 
            scale_color_manual(values=kolory2,   name="Czy miasto jest     \nstolicą?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) 
        } 
        
        if(    !("WRO" %in% input$variable) & 
               ("PL"  %in% input$variable) &
               ("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(col=Czy.stolica, size = Czy.Polska)) +  
            scale_size_manual (values=c(2, 5),   name="Czy to Polska?      ",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) +
            scale_color_manual(values=kolory2,   name="Czy miasto jest     \nstolicą?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) 
        } 
        
        if(    ("WRO" %in% input$variable) & 
               ("PL"  %in% input$variable) &
               ("ST"  %in% input$variable)){
          a <- a + 
            geom_point(aes(shape = Czy.Wroclaw, col=Czy.stolica, size = Czy.Polska)) +  
            scale_shape_manual(values=c(16, 8),  name="Czy to Wrocław?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" ))  + 
            scale_size_manual (values=c(2, 5),   name="Czy to Polska?      ",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) +
            scale_color_manual(values=kolory2,   name="Czy miasto jest    \nstolicą?",
                               breaks=c("PRAWDA", "FAłSZ" ), labels=c("Tak", "Nie" )) 
        } 
          
   
        
        
        a

    })
    
    output$click_info <- renderPrint({
      nearPoints(dane, input$plot_click, addDist = TRUE, threshold = 70,
                 xvar ="Population", yvar = "QL.numbeo")
    })
    
    output$brush_info <- renderPrint({
      brushedPoints(dane, input$plot_brush, 
                    xvar = "Population", yvar = "QL.numbeo",
                    panelvar1 ="Population", panelvar2 = "QL.numbeo")
    })
}

