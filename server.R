

dane <- readRDS("dane.Rds")

### funkcja zmiany formatu liczb do dodania spacji co 1 000
space <- function(x, ...) { 
    format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)
}
### funkcja zmiany formatu liczb do odwrotnosci (aby ranking by? logiczny)
minus <- function(x, ...) { 
    format(-x, ..., scientific = FALSE, trim = TRUE)
}

server <- function(input, output){
    
    
    output$plot1 <-  renderPlotly({
        
        temp <- dane[,c(1:6,9,7,8,10,12)]  
        temp <- temp[order(temp[,5]),]
        temp2 <- temp[!is.na(temp[,4]),]
        temp2$med_ql.Numbeo<- runmed(temp2[,4], 25, endrule = "median", algorithm = NULL, print.level = 0)
        Wielka_Warszawa<-c(2000000,3000000)
        
        
        kolot_tla <- "grey90"
        kolor_czcionki <- "grey30"
        kolory2<-c('#fe9929','#993404') #skala kolor?w 
        wielkosc_czcionki <- 10
        
        
        dane <- cbind(temp2, runmed(temp2[,4], input$num, endrule = "median", algorithm = NULL, print.level = 0))
        names(dane) <- c(names(temp2), "aa")
        
        
        a <- ggplot(dane, aes(x=Population, y=-QL.numbeo)) +
            geom_point(aes(col=Czy.stolica, shape = Czy.Wroclaw, size = Czy.Polska)) +    
            geom_path(aes(x=Population, y=-aa)) +
            #geom_smooth(se=FALSE)+
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
        
        
        if("WAW" %in% input$variable){
        a <- a +  annotate("rect", xmin=min(Wielka_Warszawa), xmax=max(Wielka_Warszawa), ymin=-200, ymax=0, alpha=0.2, fill="blue") +
             annotate("text", x=mean(Wielka_Warszawa), y= -10 , label = "Wielka Warszawa", col="blue")
        }
        
        if("TR" %in% input$variable){
        a <- a + geom_smooth(method=lm, se=FALSE)
        }
        
        
        q <- ggplotly(a)
        q
    })
}