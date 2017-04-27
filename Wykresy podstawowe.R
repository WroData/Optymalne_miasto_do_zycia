
# setwd("D:/KK/OneDrive/Wroclaw w Liczbach/Gotowe projekty/Optymalna wielkosć miasta")

#dane <- read.csv2("dane.csv", header = T)  



png(filename="1.png", width = 16, height = 10, units = 'in', res = 500)

par(mfrow=c(2,3))
plot(x=dane$Population, y=dane$QL.mercer, xlim = c(0,10000000))
plot(x=dane$Area, y=dane$QL.mercer, xlim = c(0,1000))
plot(x=dane$Gestosc, y=dane$QL.mercer) 
plot(x=dane$Population, y=dane$QL.numbeo, xlim = c(0,10000000))
plot(x=dane$Area, y=dane$QL.numbeo, xlim = c(0,1000))
plot(x=dane$Gestosc, y=dane$QL.numbeo) 

dev.off()

library(ggplot2)
require(zoo)
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")

ggplot(dane, aes(x=Gestosc, y=QL.numbeo, col=Czy.Wroclaw)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) 



options(scipen=999) 

#wyliczenie mediany po populacji - sprawdzenie które optymalne

#png(filename="pop_mercer proba median.png", width = 30, height = 6, units = 'in', res = 500)

plot_list = list()
for (i in 1:117) {
  
  temp <- dane[,c(3:5,10) ]
  temp$Population <- temp$Population/1000
  temp <- temp[order(temp$Population),]
  temp2 <- temp[!is.na(temp[,1]),]
  temp2$pop_med_ql.Mercer<- runmed(temp2[,1], 1+i*2, endrule = "median", algorithm = NULL, print.level = 0)
 
   
p = ggplot(temp2, aes(x=Population, y=-QL.mercer, col=Czy.Wroclaw)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm) +
    geom_path(aes(x=Population, y=pop_med_ql.Mercer)) +
    labs(title= 1+i*2) +
    scale_x_continuous(limits = c(0, 5000))

plot_list[[i]] = p

}

pdf("plots.pdf")
for (i in 1:117) {
  print(plot_list[[i]])
}
dev.off()


# wybrane 25

plot_list = list()

i=1
temp <- dane[,c(1:6,9,7,8,10)]
temp$Czy.stolica <- factor(temp$Czy.stolica, levels=c(levels(temp$Czy.stolica), 'Wrocław'))
temp$Czy.stolica[temp$Czy.Wroclaw=="PRAWDA"] <- "Wrocław"
temp$Population <- temp$Population/1000
temp <- temp[order(temp[,i+4]),]
temp1 <- temp[!is.na(temp[,3]),]
temp1$med_ql.Mercer<- runmed(temp1[,3], 25, endrule = "median", algorithm = NULL, print.level = 0)
temp2 <- temp[!is.na(temp[,4]),]
temp2$med_ql.Numbeo<- runmed(temp2[,4], 25, endrule = "median", algorithm = NULL, print.level = 0)
p = ggplot(temp1, aes(x=Population, y=-QL.mercer)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Population, y=-med_ql.Mercer)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+1]] = p  
p = ggplot(temp2, aes(x=Population, y=-QL.numbeo)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Population, y=-med_ql.Numbeo)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+2]] = p

i=2
temp <- dane[,c(1:6,9,7,8,10)]
temp$Czy.stolica <- factor(temp$Czy.stolica, levels=c(levels(temp$Czy.stolica), 'Wrocław'))
temp$Czy.stolica[temp$Czy.Wroclaw=="PRAWDA"] <- "Wrocław"
temp$Population <- temp$Population/1000
temp <- temp[order(temp[,i+4]),]
temp1 <- temp[!is.na(temp[,3]),]
temp1$med_ql.Mercer<- runmed(temp1[,3], 25, endrule = "median", algorithm = NULL, print.level = 0)
temp2 <- temp[!is.na(temp[,4]),]
temp2$med_ql.Numbeo<- runmed(temp2[,4], 25, endrule = "median", algorithm = NULL, print.level = 0)
p = ggplot(temp1, aes(x=Area, y=-QL.mercer)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Area, y=-med_ql.Mercer)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+1]] = p  
p = ggplot(temp2, aes(x=Area, y=-QL.numbeo)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Area, y=-med_ql.Numbeo)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+2]] = p

i=3
temp <- dane[,c(1:6,9,7,8,10)]
temp$Czy.stolica <- factor(temp$Czy.stolica, levels=c(levels(temp$Czy.stolica), 'Wrocław'))
temp$Czy.stolica[temp$Czy.Wroclaw=="PRAWDA"] <- "Wrocław"
temp$Population <- temp$Population/1000
temp <- temp[order(temp[,i+4]),]
temp1 <- temp[!is.na(temp[,3]),]
temp1$med_ql.Mercer<- runmed(temp1[,3], 25, endrule = "median", algorithm = NULL, print.level = 0)
temp2 <- temp[!is.na(temp[,4]),]
temp2$med_ql.Numbeo<- runmed(temp2[,4], 25, endrule = "median", algorithm = NULL, print.level = 0)
p = ggplot(temp1, aes(x=Gestosc, y=-QL.mercer)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Gestosc, y=-med_ql.Mercer)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+1]] = p  
p = ggplot(temp2, aes(x=Gestosc, y=-QL.numbeo)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Gestosc, y=-med_ql.Numbeo)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))
plot_list[[(i-1)*2+2]] = p
  
  
 
multiplot(plotlist=plot_list,  cols=3)

png(filename="pop_mercer proba median.png", width = 30, height = 6, units = 'in', res = 500)
for (i in 1:5) {
  multiplot(plotlist=plot_list,  cols=3)
}
dev.off()






# wybrany wykres
i=1
temp <- dane[,c(1:6,9,7,8,10)]
temp$Czy.stolica <- factor(temp$Czy.stolica, levels=c(levels(temp$Czy.stolica), 'Wrocław'))
temp$Czy.stolica[temp$Czy.Wroclaw=="PRAWDA"] <- "Wrocław"
temp$Population <- temp$Population/1000 
temp <- temp[order(temp[,i+4]),]
temp2 <- temp[!is.na(temp[,4]),]
temp2$med_ql.Numbeo<- runmed(temp2[,4], 25, endrule = "median", algorithm = NULL, print.level = 0)

png(filename="wybrany.png", width = 10, height = 6, units = 'in', res = 500)

ggplot(temp2, aes(x=Population, y=-QL.numbeo)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Population, y=-med_ql.Numbeo)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))


dev.off()



# wybrany wykres - proba na wartościach rankingu, nie na rankingu
i=1
temp <- dane[,c(1:6,9,11,7,8,10)]
temp$Czy.stolica <- factor(temp$Czy.stolica, levels=c(levels(temp$Czy.stolica), 'Wrocław'))
temp$Czy.stolica[temp$Czy.Wroclaw=="PRAWDA"] <- "Wrocław"
temp$Population <- temp$Population/1000 
temp <- temp[order(temp[,i+4]),]
temp2 <- temp[!is.na(temp[,4]),]
temp2$med_ql.Numbeo<- runmed(temp2[,8], 25, endrule = "median", algorithm = NULL, print.level = 0)

png(filename="wybrany2.png", width = 10, height = 6, units = 'in', res = 500)

ggplot(temp2, aes(x=Population, y=No.numbero)) +
  geom_point(shape=1, aes(col=Czy.stolica, size = Czy.Wroclaw)) +    
  geom_path(aes(x=Population, y=med_ql.Numbeo)) +
  labs(title= c(names(temp[i+4]), "vs QL.mercer")) +
  scale_x_continuous(limits = c(0, 5000))


dev.off()










# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
