# Evaluacion de la 6ta Clase
#### PREGUNTA 1 ####
##  Realice un analisis descriptivo del data frame iris (Libreria datasets) 
##  por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
##  y en conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]
data(iris)

# Respuesta
rm(list=ls())

#setear el directorio de trabajo
setwd("C:/Users/user/Desktop/R_CTIC/Evaluacion_Clase6")
getwd()
dir()

library(datasets)
library(RSelenium)
library(carData)
library(car)
library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)


library(help="datasets")


data(iris)

str(iris)
head(iris)
tail(iris)
View(iris)

summary(iris)

# por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
# setosa
# versicolor
# virginica

iris_setosa <- iris[iris$Species=="setosa",]
iris_setosa_sepal<- iris_setosa[order(iris_setosa$Sepal.Length, iris_setosa$Sepal.Width),]
iris_setosa_petal<- iris_setosa[order(iris_setosa$Petal.Length, iris_setosa$Petal.Width),]

View(iris_setosa_sepal)

iris_versicolor <- iris[iris$Species=="versicolor",]
iris_versicolor_sepal<- iris_versicolor[order(iris_versicolor$Sepal.Length, iris_versicolor$Sepal.Width),]
iris_versicolor_petal<- iris_versicolor[order(iris_versicolor$Petal.Length, iris_versicolor$Petal.Width),]


iris_virginica <- iris[iris$Species=="virginica",]
iris_virginica_sepal<- iris_virginica[order(iris_virginica$Sepal.Length, iris_virginica$Sepal.Width),]
iris_virginica_petal<- iris_virginica[order(iris_virginica$Petal.Length, iris_virginica$Petal.Width),]

#### HISTOGRAMAS DE SETOSA ####

x11()
jpeg("HistogramaSetosa_01.jpeg")
HistSetosaSepal <- ggplot(data=iris_setosa_sepal, aes(x=iris_setosa_sepal$Sepal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Setosa - Sepal Length")+
  geom_vline(data=iris_setosa_sepal, aes(xintercept = mean(iris_setosa_sepal$Sepal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaSetosa_02.jpeg")
HistSetosaSepal2 <- ggplot(data=iris_setosa_sepal, aes(x=iris_setosa_sepal$Sepal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Setosa - Sepal Width")+
  geom_vline(data=iris_setosa_sepal, aes(xintercept = mean(iris_setosa_sepal$Sepal.Width)),linetype="dashed",color="grey")

dev.off()


x11()
jpeg("HistogramaSetosa_03.jpeg")
HistSetosaPetal1 <- ggplot(data=iris_setosa_petal, aes(x=iris_setosa_petal$Petal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Setosa - Petal Length")+
  geom_vline(data=iris_setosa_petal, aes(xintercept = mean(iris_setosa_petal$Petal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaSetosa_04.jpeg")
HistSetosaPetal2 <- ggplot(data=iris_setosa_petal, aes(x=iris_setosa_petal$Petal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Setosa - Petal Width")+
  geom_vline(data=iris_setosa_petal, aes(xintercept = mean(iris_setosa_petal$Petal.Width)),linetype="dashed",color="grey")

dev.off()

#### HISTOGRAMAS DE VERSICOLOR ####

x11()
jpeg("HistogramaVersicolor_01.jpeg")
HistVersicolorSepal1 <- ggplot(data=iris_versicolor_sepal, aes(x=iris_versicolor_sepal$Sepal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Versicolor Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Versicolor - Sepal Length")+
  geom_vline(data=iris_versicolor_sepal, aes(xintercept = mean(iris_versicolor_sepal$Sepal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaVersicolor_02.jpeg")
HistVersicolorSepal2 <- ggplot(data=iris_versicolor_sepal, aes(x=iris_versicolor_sepal$Sepal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Versicolor Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Versicolor - Sepal Width")+
  geom_vline(data=iris_versicolor_sepal, aes(xintercept = mean(iris_versicolor_sepal$Sepal.Width)),linetype="dashed",color="grey")

dev.off()


x11()
jpeg("HistogramaVersicolor_03.jpeg")
HistVersicolorPetal1 <- ggplot(data=iris_versicolor_petal, aes(x=iris_versicolor_petal$Petal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Versicolor - Petal Length")+
  geom_vline(data=iris_versicolor_petal, aes(xintercept = mean(iris_versicolor_petal$Petal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaVersicolor_04.jpeg")
HistVersicolorPetal2 <- ggplot(data=iris_versicolor_petal, aes(x=iris_versicolor_petal$Petal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Versicolor - Petal Width")+
  geom_vline(data=iris_versicolor_petal, aes(xintercept = mean(iris_versicolor_petal$Petal.Width)),linetype="dashed",color="grey")

dev.off()


#### HISTOGRAMAS DE VIRGINICA ####

x11()
jpeg("HistogramaVirginica_01.jpeg")
HistVirginicaSepal1 <- ggplot(data=iris_virginica_sepal, aes(x=iris_virginica_sepal$Sepal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Virginica - Sepal Length")+
  geom_vline(data=iris_virginica_sepal, aes(xintercept = mean(iris_virginica_sepal$Sepal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaVirginica_02.jpeg")
HistVirginicaSepal2 <- ggplot(data=iris_virginica_sepal, aes(x=iris_virginica_sepal$Sepal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Virginica - Sepal Width")+
  geom_vline(data=iris_virginica_sepal, aes(xintercept = mean(iris_virginica_sepal$Sepal.Width)),linetype="dashed",color="grey")

dev.off()


x11()
jpeg("HistogramaVirginica_03.jpeg")
HistVirginicaPetal1 <- ggplot(data=iris_virginica_petal, aes(x=iris_virginica_petal$Petal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Virginica - Petal Length")+
  geom_vline(data=iris_virginica_petal, aes(xintercept = mean(iris_virginica_petal$Petal.Length)),linetype="dashed",color="grey")

dev.off()

x11()
jpeg("HistogramaVirginica_04.jpeg")
HistVirginicaPetal2 <- ggplot(data=iris_virginica_petal, aes(x=iris_virginica_petal$Petal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de Frecuencia Virginica - Petal Width")+
  geom_vline(data=iris_virginica_petal, aes(xintercept = mean(iris_virginica_petal$Petal.Width)),linetype="dashed",color="grey")

dev.off()

#### VISUALIZACION DE 12 HISTOGRAMAS ####
jpeg("HistogramaFrecuenciasIris.jpeg")
HistogramaFrecuenciasIris<-grid.arrange(HistSetosaSepal + ggtitle("Setosa"),
             HistSetosaSepal2 + ggtitle("Setosa"),
             HistSetosaPetal1 + ggtitle("Setosa"),
             HistSetosaPetal2  + ggtitle("Setosa"),
             HistVersicolorSepal1 + ggtitle("Versicolor"),
             HistVersicolorSepal2 + ggtitle("Versicolor"),
             HistVersicolorPetal1 + ggtitle("Versicolor"),
             HistVersicolorPetal2  + ggtitle("Versicolor"),
             HistVirginicaSepal1 + ggtitle("Virginica"),
             HistVirginicaSepal2 + ggtitle("Virginica"),
             HistVirginicaPetal1 + ggtitle("Virginica"),
             HistVirginicaPetal2  + ggtitle("Virginica"),
             nrow = 3,
             top = textGrob("Histograma de Frecuencia IRIS", 
                            gp=gpar(fontsize=30))
)
dev.off()

#### DIAGRAMAS BOXPLOT SETOSA ####

x11()
jpeg("BoxplotSetosa_01.jpeg")
BoxplotSetosa_01<-
  ggplot(iris_setosa_sepal, aes(Species, iris_setosa_sepal$Sepal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Setosa Sepal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotSetosa_02.jpeg")
BoxplotSetosa_02<-
  ggplot(iris_setosa_sepal, aes(Species, iris_setosa_sepal$Sepal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Setosa Sepal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotSetosa_03.jpeg")
BoxplotSetosa_03<-
  ggplot(iris_setosa_petal, aes(Species, iris_setosa_petal$Petal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Setosa Petal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotSetosa_04.jpeg")
BoxplotSetosa_04<-
  ggplot(iris_setosa_petal, aes(Species, iris_setosa_petal$Petal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Setosa Petal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

#### DIAGRAMAS BOXPLOT VERSICOLOR ####

x11()
jpeg("BoxplotVersicolor_01.jpeg")
BoxplotVersicolor_01<-
  ggplot(iris_versicolor_sepal, aes(Species, iris_versicolor_sepal$Sepal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Versicolor Sepal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVersicolor_02.jpeg")
BoxplotVersicolor_02<-
  ggplot(iris_versicolor_sepal, aes(Species, iris_versicolor_sepal$Sepal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Versicolor Sepal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVersicolor_03.jpeg")
BoxplotVersicolor_03<-
  ggplot(iris_versicolor_petal, aes(Species, iris_versicolor_petal$Petal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Versicolor Petal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVersicolor_04.jpeg")
BoxplotVersicolor_04<-
  ggplot(iris_versicolor_petal, aes(Species, iris_versicolor_petal$Petal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Versicolor Petal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

#### DIAGRAMAS BOXPLOT VIRGINICA ####

x11()
jpeg("BoxplotVirginica_01.jpeg")
BoxplotVirginica_01<-
  ggplot(iris_virginica_sepal, aes(Species, iris_virginica_sepal$Sepal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Virginica Sepal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVirginica_02.jpeg")
BoxplotVirginica_02<-
  ggplot(iris_virginica_sepal, aes(Species, iris_virginica_sepal$Sepal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Virginica Sepal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVirginica_03.jpeg")
BoxplotVirginica_03<-
  ggplot(iris_virginica_petal, aes(Species, iris_virginica_petal$Petal.Length)) + 
  geom_boxplot()+
  labs(title = "Iris Virginica Petal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotVirginica_04.jpeg")
BoxplotVirginica_04<-
  ggplot(iris_virginica_petal, aes(Species, iris_virginica_petal$Petal.Width)) + 
  geom_boxplot()+
  labs(title = "Iris Virginica Petal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

#### VISUALIZACION DE 12 DIAGRAMAS BOXPLOT  ####
jpeg("BoxplotIris.jpeg")
BoxplotIris<-grid.arrange(BoxplotSetosa_01 + ggtitle("Setosa"),
                          BoxplotSetosa_02 + ggtitle("Setosa"),
                          BoxplotSetosa_03 + ggtitle("Setosa"),
                          BoxplotSetosa_04  + ggtitle("Setosa"),
                          BoxplotVersicolor_01 + ggtitle("Versicolor"),
                          BoxplotVersicolor_02 + ggtitle("Versicolor"),
                          BoxplotVersicolor_03 + ggtitle("Versicolor"),
                          BoxplotVersicolor_04  + ggtitle("Versicolor"),
                          BoxplotVirginica_01 + ggtitle("Virginica"),
                          BoxplotVirginica_02 + ggtitle("Virginica"),
                          BoxplotVirginica_03 + ggtitle("Virginica"),
                          BoxplotVirginica_04 + ggtitle("Virginica"),
                             nrow = 3,
                             top = textGrob("Diagramas BOXPLOT IRIS", 
                                            gp=gpar(fontsize=30))
)
dev.off()



#### DIAGRAMAS DE DISPERSION SETOSA ####

# Diagrama de Dispersion del data frame
head(iris_setosa_sepal, n=1)$Sepal.Length # Primer dia de la data
tail(iris_setosa_sepal, n=1)$Sepal.Length # Ultimo dia de la data

head(iris_setosa_petal, n=1)$Petal.Length # Primer dia de la data
tail(iris_setosa_petal, n=1)$Petal.Length # Ultimo dia de la data


x11()
jpeg("DispersionSetosa_01.jpeg")
DispersionSetosa01<-
  ggplot(data = iris_setosa_sepal, mapping = aes(x = iris_setosa_sepal$Sepal.Length, y = iris_setosa_sepal$Sepal.Width))+
  geom_point()+
  xlab(paste("Sepal Length [" , min(iris_setosa_sepal$Sepal.Length) , "-", max(iris_setosa_sepal$Sepal.Length),"]"))+
  ylab("Sepal Width")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE SETOSA")+
  labs(subtitle = "Sepal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(4,6,by = 0.1))+
  scale_y_continuous(breaks = seq(0,4.5,by = 0.1))                   
                     
dev.off()

x11()
jpeg("DispersionSetosa_02.jpeg")
DispersionSetosa02<-
  ggplot(data = iris_setosa_petal, mapping = aes(x = iris_setosa_petal$Petal.Length, y = iris_setosa_petal$Petal.Width))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris_setosa_petal$Petal.Length) , "-", max(iris_setosa_petal$Petal.Length),"]"))+
  ylab("Petal Width")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE SETOSA")+
  labs(subtitle = "Petal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(1,2,by = 0.1))+
  scale_y_continuous(breaks = seq(0,0.6,by = 0.1))                   

dev.off()


x11()
jpeg("DispersionSetosa_03.jpeg")
DispersionSetosa03<-
  ggplot(data = iris_setosa_sepal, mapping = aes(x = iris_setosa_sepal$Sepal.Width, y = iris_setosa_sepal$Sepal.Length))+
  geom_point()+
  xlab(paste("Sepal Width [" , min(iris_setosa_sepal$Sepal.Width) , "-", max(iris_setosa_sepal$Sepal.Width),"]"))+
  ylab("Sepal Length")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE SETOSA")+
  labs(subtitle = "Sepal Width vs Length",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(4,6,by = 0.1))+
  scale_x_continuous(breaks = seq(0,4.5,by = 0.1))                   

dev.off()

x11()
jpeg("DispersionSetosa_04.jpeg")
DispersionSetosa04<-
  ggplot(data = iris_setosa_petal, mapping = aes(x = iris_setosa_petal$Sepal.Width, y = iris_setosa_petal$Petal.Length))+
  geom_point()+
  xlab(paste("Petal Width [" , min(iris_setosa_petal$Sepal.Width) , "-", max(iris_setosa_petal$Sepal.Width),"]"))+
  ylab("Petal Length")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE SETOSA")+
  labs(subtitle = "Petal Width vs Length",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(1,2,by = 0.1))+
  scale_x_continuous(breaks = seq(0,0.6,by = 0.1))                   

dev.off()



#### DIAGRAMAS DE DISPERSION VERSICOLOR ####


# Diagrama de Dispersion del data frame
head(iris_versicolor_sepal, n=1)$Sepal.Length # Primer dia de la data
tail(iris_versicolor_sepal, n=1)$Sepal.Length # Ultimo dia de la data

head(iris_versicolor_petal, n=1)$Petal.Length # Primer dia de la data
tail(iris_versicolor_petal, n=1)$Petal.Length # Ultimo dia de la data


x11()
jpeg("DispersionVersicolor_01.jpeg")
DispersionVersicolor01<-
  ggplot(data = iris_versicolor_sepal, mapping = aes(x = iris_versicolor_sepal$Sepal.Length, y = iris_versicolor_sepal$Sepal.Width))+
  geom_point()+
  xlab(paste("Sepal Length [" , min(iris_versicolor_sepal$Sepal.Length) , "-", max(iris_versicolor_sepal$Sepal.Length),"]"))+
  ylab("Sepal Width")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE VERSICOLOR")+
  labs(subtitle = "Sepal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(4.5,7.5,by = 0.1))+
  scale_y_continuous(breaks = seq(2,4,by = 0.1))   

dev.off()

x11()
jpeg("DispersionVersicolor_02.jpeg")
DispersionVersicolor02<-
  ggplot(data = iris_versicolor_petal, mapping = aes(x = iris_versicolor_petal$Petal.Length, y = iris_versicolor_petal$Petal.Width))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris_versicolor_petal$Petal.Length) , "-", max(iris_versicolor_petal$Petal.Length),"]"))+
  ylab("Petal Width")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE VERSICOLOR")+
  labs(subtitle = "Petal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(3,5.5,by = 0.1))+
  scale_y_continuous(breaks = seq(1,2,by = 0.1))   

dev.off()


x11()
jpeg("DispersionVersicolor_03.jpeg")
DispersionVersicolor03<-
  ggplot(data = iris_versicolor_sepal, mapping = aes(x = iris_versicolor_sepal$Sepal.Width, y = iris_versicolor_sepal$Sepal.Length))+
  geom_point()+
  xlab(paste("Sepal Width [" , min(iris_versicolor_sepal$Sepal.Width) , "-", max(iris_versicolor_sepal$Sepal.Width),"]"))+
  ylab("Sepal Length")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE VERSICOLOR")+
  labs(subtitle = "Sepal Width vs Length",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(4.5,7.5,by = 0.1))+
  scale_x_continuous(breaks = seq(2,4,by = 0.1))   

dev.off()

x11()
jpeg("DispersionVersicolor_04.jpeg")
DispersionVersicolor04<-
  ggplot(data = iris_versicolor_petal, mapping = aes(x = iris_versicolor_petal$Petal.Width, y = iris_versicolor_petal$Petal.Length))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris_versicolor_petal$Petal.Width) , "-", max(iris_versicolor_petal$Petal.Width),"]"))+
  ylab("Petal Length")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE VERSICOLOR")+
  labs(subtitle = "Petal Width vs Length",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(3,5.5,by = 0.1))+
  scale_x_continuous(breaks = seq(1,2,by = 0.1))   

dev.off()

#### DIAGRAMAS DE DISPERSION VIRGINICA ####

# Diagrama de Dispersion del data frame
head(iris_virginica_sepal, n=1)$Sepal.Length # Primer dia de la data
tail(iris_virginica_sepal, n=1)$Sepal.Length # Ultimo dia de la data

head(iris_virginica_petal, n=1)$Petal.Length # Primer dia de la data
tail(iris_virginica_petal, n=1)$Petal.Length # Ultimo dia de la data


x11()
jpeg("DispersionVirginica_01.jpeg")
DispersionVirginica01<-
  ggplot(data = iris_virginica_sepal, mapping = aes(x = iris_virginica_sepal$Sepal.Length, y = iris_virginica_sepal$Sepal.Width))+
  geom_point()+
  xlab(paste("Sepal Length [" , min(iris_virginica_sepal$Sepal.Length) , "-", max(iris_virginica_sepal$Sepal.Length),"]"))+
  ylab("Sepal Width")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE VIRGINICA")+
  labs(subtitle = "Sepal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(4.5,8,by = 0.1))+
  scale_y_continuous(breaks = seq(2,4,by = 0.1))   

dev.off()

x11()
jpeg("DispersionVirginica_02.jpeg")
DispersionVirginica02<-
  ggplot(data = iris_virginica_petal, mapping = aes(x = iris_virginica_petal$Petal.Length, y = iris_virginica_petal$Petal.Width))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris_virginica_petal$Petal.Length) , "-", max(iris_virginica_petal$Petal.Length),"]"))+
  ylab("Petal Width")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE VIRGINICA")+
  labs(subtitle = "Petal Length vs Width",
       caption = "Evaluacion 6")+
  scale_x_continuous(breaks = seq(4,7,by = 0.1))+
  scale_y_continuous(breaks = seq(1.5,2.5,by = 0.1))   

dev.off()


x11()
jpeg("DispersionVirginica_03.jpeg")
DispersionVirginica03<-
  ggplot(data = iris_virginica_sepal, mapping = aes(x = iris_virginica_sepal$Sepal.Width, y = iris_virginica_sepal$Sepal.Length))+
  geom_point()+
  xlab(paste("Sepal Length [" , min(iris_virginica_sepal$Sepal.Width) , "-", max(iris_virginica_sepal$Sepal.Width),"]"))+
  ylab("Sepal Width")+
  ggtitle("DIAGRAMA DE DISPERSION SÉPALO ESPECIE VIRGINICA")+
  labs(subtitle = "Sepal Length vs Width",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(4.5,8,by = 0.1))+
  scale_x_continuous(breaks = seq(2,4,by = 0.1))   

dev.off()

x11()
jpeg("DispersionVirginica_04.jpeg")
DispersionVirginica04<-
  ggplot(data = iris_virginica_petal, mapping = aes(x = iris_virginica_petal$Petal.Width, y = iris_virginica_petal$Petal.Length))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris_virginica_petal$Petal.Width) , "-", max(iris_virginica_petal$Petal.Width),"]"))+
  ylab("Petal Width")+
  ggtitle("DIAGRAMA DE DISPERSION PÉTALO ESPECIE VIRGINICA")+
  labs(subtitle = "Petal Length vs Width",
       caption = "Evaluacion 6")+
  scale_y_continuous(breaks = seq(4,7,by = 0.1))+
  scale_x_continuous(breaks = seq(1.5,2.5,by = 0.1))   

dev.off()



#### VISUALIZACION DE 12 DIAGRAMAS DE DISPERSION  ####
jpeg("DispersionIris.jpeg")
DispersionIris<-grid.arrange(DispersionSetosa01 + ggtitle("Setosa"),
                                        DispersionSetosa02 + ggtitle("Setosa"),
                                        DispersionSetosa03 + ggtitle("Setosa"),
                                        DispersionSetosa04  + ggtitle("Setosa"),
                                        DispersionVersicolor01 + ggtitle("Versicolor"),
                                        DispersionVersicolor02 + ggtitle("Versicolor"),
                                        DispersionVersicolor03 + ggtitle("Versicolor"),
                                        DispersionVersicolor04  + ggtitle("Versicolor"),
                                        DispersionVirginica01 + ggtitle("Virginica"),
                                        DispersionVirginica02 + ggtitle("Virginica"),
                                        DispersionVirginica03 + ggtitle("Virginica"),
                                        DispersionVirginica04  + ggtitle("Virginica"),
                                        nrow = 3,
                                        top = textGrob("Diagramas de Dispersión IRIS", 
                                                       gp=gpar(fontsize=30))
)
dev.off()





##  En conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]

#### HISTOGRAMA PARA LAS 3 ESPECIES ####

View(iris)

x11()
jpeg("HistogramaSepalLength01.jpeg")
HistogramaSL <- ggplot(data=iris, aes(x=iris$Sepal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma 3 Especies - Sepal Length")+
  geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Length)),linetype="dashed",color="grey")+
  scale_x_continuous(breaks = seq(0,10,by = 0.2))+
  scale_y_continuous(breaks = seq(0,20,by = 1))

    # legend.position = c(0.1, 0.5),
    # plot.title = element_text(size=18,hjust = 1),
    # plot.subtitle = element_text(size=9,hjust = 1)

dev.off()

x11()
jpeg("HistogramaSepalWidth02.jpeg")
HistogramaSW <- ggplot(data=iris, aes(x=iris$Sepal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Sepal Width") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma 3 Especies - Sepal Width")+
  geom_vline(data=iris, aes(xintercept = mean(iris$Sepal.Width)),linetype="dashed",color="grey")+
  scale_x_continuous(breaks = seq(0,10,by = 0.2))+
  scale_y_continuous(breaks = seq(0,40,by = 1)) 

dev.off()

x11()
jpeg("HistogramaPetalLength03.jpeg")
HistogramaPL <- ggplot(data=iris, aes(x=iris$Petal.Length))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Length") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma 3 Especies - Petal Length")+
  geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Length)),linetype="dashed",color="grey")+
  scale_x_continuous(breaks = seq(0,10,by = 0.4))+
  scale_y_continuous(breaks = seq(0,20,by = 1))

dev.off()

x11()
jpeg("HistogramaPetalWidth04.jpeg")
HistogramaPW <- ggplot(data=iris, aes(x=iris$Petal.Width))+
  geom_histogram(binwidth=0.1, color="black", aes(fill=Species)) + 
  xlab("Petal Width") +  
  ylab("Frequencia") + 
  theme(legend.position=c(0.9, 0.9))+
  ggtitle("Histograma 3 Especies - Petal Width")+
  geom_vline(data=iris, aes(xintercept = mean(iris$Petal.Width)),linetype="dashed",color="grey")+
  scale_x_continuous(breaks = seq(0,10,by = 0.2))+
  scale_y_continuous(breaks = seq(0,40,by = 1)) 

dev.off()


#### VISUALIZACION DE HISTOGRAMAS 3 ESPECIES ####
jpeg("HistogramaFrecuenciasIris3Especies.jpeg")
HistogramaFrecuenciasIris3<-grid.arrange(HistogramaSL + ggtitle(""),
                                        HistogramaSW + ggtitle(""),
                                        HistogramaPL + ggtitle(""),
                                        HistogramaPW  + ggtitle(""),
                                        nrow = 2,
                                        top = textGrob("Histograma de Frecuencia IRIS 3 ESPECIES", 
                                                       gp=gpar(fontsize=20))
)

dev.off()


#### BOXPLOT PARA LAS 3 ESPECIES ####


x11()
jpeg("BoxplotSepalLength_01.jpeg")
BoxplotSL_01<-
  ggplot(iris, aes(Species, iris$Sepal.Length, fill=Species)) + 
  geom_boxplot()+
  labs(title = "Iris Sepal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotSepalWidth_02.jpeg")
BoxplotSW_02<-
  ggplot(iris, aes(Species, iris$Sepal.Width, fill=Species)) + 
  geom_boxplot()+
  labs(title = "Iris Sepal Width Box Plot")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()

x11()
jpeg("BoxplotPetalLength_03.jpeg")
BoxplotPL_03<-
  ggplot(iris, aes(Species, iris$Petal.Length, fill=Species)) + 
  geom_boxplot()+
  labs(title = "Iris Petal Length Box Plot")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.2))

dev.off()

x11()
jpeg("BoxplotPetalWidth_04.jpeg")
BoxplotPW_04<-
  ggplot(iris, aes(Species, iris$Petal.Width, fill=Species)) + 
  geom_boxplot()+
  labs(title = "Iris Petal Width Box Plot", x = "Species")+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.1))

dev.off()


#### VISUALIZACION DE BOXPLOT 3 ESPECIES ####
jpeg("Boxplot3Especies.jpeg")
Boxplot3 <-grid.arrange(BoxplotSL_01 + ggtitle(""),
                                  BoxplotSW_02 + ggtitle(""),
                                  BoxplotPL_03 + ggtitle(""),
                                  BoxplotPW_04  + ggtitle(""),
                                         nrow = 2,
                                         top = textGrob("BOXPLOT IRIS 3 ESPECIES", 
                                                        gp=gpar(fontsize=20))
)

dev.off()



#### DIAGRAMA DE DISPERSION PARA LAS 3 ESPECIES ####

x11()
jpeg("DispersionSepal_01.jpeg")
DispersionSepal<- 
  ggplot(data = iris, mapping = aes(x = iris$Sepal.Length, y = iris$Sepal.Width, colour=Species))+
  geom_point()+
  xlab(paste("Sepal Length [" , min(iris$Sepal.Length) , "-", max(iris$Sepal.Length),"]"))+
  ylab("Sepal Width")+
  ggtitle("DIAGRAMA DE DISPERSION LENGTH 3 ESPECIES")+
  labs(subtitle = "SEPAL",
       caption = "Evaluacion 6")+
  scale_x_continuous("Length", breaks= seq(0,10,by = 0.2))+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.2))

dev.off()

x11()
jpeg("DispersionPetal_02.jpeg")
DispersionPetal<- 
  ggplot(data = iris, mapping = aes(x = iris$Petal.Length, y = iris$Petal.Width, colour=Species))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris$Petal.Length) , "-", max(iris$Petal.Length),"]"))+
  ylab("Petal Width")+
  ggtitle("DIAGRAMA DE DISPERSION PETAL 3 ESPECIES")+
  labs(subtitle = "PETAL",
       caption = "Evaluacion 6")+
  scale_x_continuous("Length", breaks= seq(0,10,by = 0.2))+
  scale_y_continuous("Width", breaks= seq(0,10,by = 0.2))

dev.off()

x11()
jpeg("DispersionSepal_03.jpeg")
DispersionSepal2<- 
  ggplot(data = iris, mapping = aes(x = iris$Sepal.Width, y = iris$Sepal.Length, colour=Species))+
  geom_point()+
  xlab(paste("Sepal Width [" , min(iris$Sepal.Width) , "-", max(iris$Sepal.Width),"]"))+
  ylab("Sepal Length")+
  ggtitle("DIAGRAMA DE DISPERSION WIDTH 3 ESPECIES")+
  labs(subtitle = "SEPAL",
       caption = "Evaluacion 6")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.2))+
  scale_x_continuous("Width", breaks= seq(0,10,by = 0.2))

dev.off()

x11()
jpeg("DispersionPetal_04.jpeg")
DispersionPetal2<- 
  ggplot(data = iris, mapping = aes(x = iris$Petal.Width, y = iris$Petal.Length, colour=Species))+
  geom_point()+
  xlab(paste("Petal Length [" , min(iris$Petal.Width) , "-", max(iris$Petal.Width),"]"))+
  ylab("Petal Length")+
  ggtitle("DIAGRAMA DE DISPERSION PETAL 3 ESPECIES")+
  labs(subtitle = "PETAL",
       caption = "Evaluacion 6")+
  scale_y_continuous("Length", breaks= seq(0,10,by = 0.2))+
  scale_x_continuous("Width", breaks= seq(0,10,by = 0.2))

dev.off()







#### VISUALIZACION DE DIAGRAMA DISPERSION 3 ESPECIES ####
jpeg("Dispersion3Especies.jpeg")
Dispersion3 <-grid.arrange(DispersionSepal + ggtitle(""),
                           DispersionSepal2 + ggtitle(""),
                           DispersionPetal + ggtitle(""),
                           DispersionPetal2 + ggtitle(""),
                           nrow = 2,
                           top = textGrob("DIAGRAMA DISPERSION IRIS 3 ESPECIES", 
                                          gp=gpar(fontsize=20))
)

dev.off()


#### PREGUNTA 2 ####
##  Contruya un arbol de regresion para el data frame iris (Libreria datasets)  y genere las predicciones 
##  para el siguiente data frame :
# NuevaEspecie <- data.frame(
#   Sepal.Length = 6.5, Sepal.Width = 3.0,
#   Petal.Length = 5.2, Petal.Width = 2.0
# )

library(ggplot2)
library(xts)
library(zoo)
library(quantmod)
library(tseries)
library(forecast)
library(corrplot)
library(rpart)
library(rpart.plot)
library(modeest)
frequency(iris)

View(iris)
View(NuevaEspecie)

datos_iris <- iris
# TODAS LAS VARIABLES DEPENDIENTES VS INDEPENDIENTES

#Para nuestro caso Specie es dependiente
#CREAMOS EL MODELO

datos_iris$Species<- factor(datos_iris$Species)
View(datos_iris)

mod_iris <- rpart(Species ~ ., data=datos_iris) # el punto . quiere decir todas las variables independientes
class(mod_iris)
typeof(mod_iris)
mod_iris


#n= 150 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node

# 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#   2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *


rpart.plot(mod_iris)
prp(mod_iris)
dev.off()

# Todo el particionado lo esta haciendo utlizando la variable Petal.Length y Petal.Width
# los nuevos terminales son los que tienen asterisco *
# de acuerdo al grafico del arbol de decision vemos que prosigue solo si Petal.Length es mayor igual a 2.45
# Y si Petal.Width es menor a 1.8
iris_Level1 <- datos_iris[datos_iris$Petal.Length>= 2.45,]
# para el segundo nivel vemos que el resultado depende si Petal.Width sea menor a 1.8
iris_Level2 <- iris_Level1[iris_Level1$Petal.Width<1.75,]

# Respuesta a la prediccion de NuevaEspecie
NuevaEspecie <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)

# Utilizamos la funcion predict y nuestro modelo mod_iris
prediccion <- predict(mod_iris,newdata=NuevaEspecie,type = "prob")
#   setosa versicolor virginica
#        0 0.02173913 0.9782609
# Observamos que tenemos un 98 % de que la nueva especie sea VIRGINICA


#### PREGUNTA 3 ####
## Cree un arbol de clasificacion para predecir la variable "diabetes" en los siguientes dos escenarios
## Escenario 1 : no considere la variable "pedigree" 
## Escenario 2 : no considere la variable "glucose" 
## Para cada uno de los escenario, genere nuevos data frames para predecir sus nuevos resultados 
## (predecir la variable "diabetes" cuyos posibles resultados son pos-neg)
library(mlbench)
data("PimaIndiansDiabetes2")
help("PimaIndiansDiabetes2") # Pima Indians Diabetes Database
View(PimaIndiansDiabetes2)

datos_diabetes <- PimaIndiansDiabetes2

# Escenario 1 : no considere la variable "pedigree"

#CREAMOS EL MODELO

#ELIMINAMOS LA VARIABLE PEDIGREE
datos_diabetes_sp <- datos_diabetes[,-7]
#ELIMINAMOS LOS NA
datos_diabetes_sp <- na.omit(datos_diabetes_sp)
View(datos_diabetes_sp)

mod_diabetes_sp <- rpart(diabetes ~ ., data=datos_diabetes_sp) # el punto . quiere decir todas las variables independientes
class(mod_diabetes_sp)
typeof(mod_diabetes_sp)
mod_diabetes_sp

n= 392 

# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 392 130 neg (0.66836735 0.33163265)  
#   2) glucose< 127.5 241  36 neg (0.85062241 0.14937759)  
#     4) insulin< 143.5 181  16 neg (0.91160221 0.08839779) *
#     5) insulin>=143.5 60  20 neg (0.66666667 0.33333333)  
#       10) age< 28.5 32   2 neg (0.93750000 0.06250000) *
#       11) age>=28.5 28  10 pos (0.35714286 0.64285714) *
#   3) glucose>=127.5 151  57 pos (0.37748344 0.62251656)  
#     6) glucose< 165.5 105  52 pos (0.49523810 0.50476190)  
#       12) age< 23.5 19   1 neg (0.94736842 0.05263158) *
#       13) age>=23.5 86  34 pos (0.39534884 0.60465116)  
#         26) mass< 26.2 7   1 neg (0.85714286 0.14285714) *
#         27) mass>=26.2 79  28 pos (0.35443038 0.64556962)  
#           54) pregnant< 7.5 60  25 pos (0.41666667 0.58333333)  
#           108) pregnant>=3.5 27  11 neg (0.59259259 0.40740741)  
#             216) pressure< 76 16   4 neg (0.75000000 0.25000000) *
#             217) pressure>=76 11   4 pos (0.36363636 0.63636364) *
#           109) pregnant< 3.5 33   9 pos (0.27272727 0.72727273) *
#         55) pregnant>=7.5 19   3 pos (0.15789474 0.84210526) *
#     7) glucose>=165.5 46   5 pos (0.10869565 0.89130435) *

rpart.plot(mod_diabetes_sp)
prp(mod_diabetes_sp)
dev.off()


# Todo el particionado lo esta haciendo utlizando las variables glucose,age,mass,pregnant,pressure
# los nuevos terminales son los que tienen asterisco *
# de acuerdo al grafico del arbol de decision vemos que prosigue solo si glucose es mayor igual a 128

diabetes_level1 <- datos_diabetes_sp[datos_diabetes_sp$glucose>= 128,]
# para el segundo nivel vemos que el resultado depende si glucose sea menor a 166
diabetes_level2 <- diabetes_level1[diabetes_level1$glucose < 166,]
diabetes_level3 <- diabetes_level2[diabetes_level2$age>=24,]
diabetes_level4 <- diabetes_level3[diabetes_level3$mass>=26,]
diabetes_level5 <- diabetes_level4[diabetes_level4$pregnant<8,]
diabetes_level6 <- diabetes_level5[diabetes_level5$pregnant>=4,]
diabetes_level7 <- diabetes_level6[diabetes_level6$pressure<76,]

View(diabetes_level7)

# Si Petal.Width < 1.8, la preddiccion sera la moda (Versicolor)
mfv(diabetes_level7$diabetes)
# [1] neg
# Levels: neg pos


# Respuesta a la prediccion de NuevoDiabetes
NuevoDiabetes <- data.frame(
  pregnant = 10, glucose = 150, pressure=29, triceps= 30,
 insulin= 125, mass = 35.5, age=30) 


# Utilizamos la funcion predict y nuestro modelo mod_diabetes_sp
prediccion_diabetes_sp <- predict(mod_diabetes_sp,newdata=NuevoDiabetes,type = "prob")
#       neg       pos
#  0.1578947 0.8421053
# Observamos que tenemos un 84 % de que el diagnostico sea POSITIVO


# Escenario 2 : no considere la variable "glucose"

#CREAMOS EL MODELO

#ELIMINAMOS LA VARIABLE GLUCOSE
datos_diabetes_sg <- datos_diabetes[,-2]
#ELIMINAMOS LOS NA
datos_diabetes_sg <- na.omit(datos_diabetes_sg)
View(datos_diabetes_sg)

mod_diabetes_sg <- rpart(diabetes ~ ., data=datos_diabetes_sg) # el punto . quiere decir todas las variables independientes
class(mod_diabetes_sg)
typeof(mod_diabetes_sg)
mod_diabetes_sp

# n= 392 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 392 130 neg (0.66836735 0.33163265)  
#   2) glucose< 127.5 241  36 neg (0.85062241 0.14937759)  
#     4) insulin< 143.5 181  16 neg (0.91160221 0.08839779) *
#     5) insulin>=143.5 60  20 neg (0.66666667 0.33333333)  
#       10) age< 28.5 32   2 neg (0.93750000 0.06250000) *
#     11) age>=28.5 28  10 pos (0.35714286 0.64285714) *
#   3) glucose>=127.5 151  57 pos (0.37748344 0.62251656)  
#     6) glucose< 165.5 105  52 pos (0.49523810 0.50476190)  
#       12) age< 23.5 19   1 neg (0.94736842 0.05263158) *
#       13) age>=23.5 86  34 pos (0.39534884 0.60465116)  
#         26) mass< 26.2 7   1 neg (0.85714286 0.14285714) *
#         27) mass>=26.2 79  28 pos (0.35443038 0.64556962)  
#           54) pregnant< 7.5 60  25 pos (0.41666667 0.58333333)  
#             108) pregnant>=3.5 27  11 neg (0.59259259 0.40740741)  
#               216) pressure< 76 16   4 neg (0.75000000 0.25000000) *
#               217) pressure>=76 11   4 pos (0.36363636 0.63636364) *
#             109) pregnant< 3.5 33   9 pos (0.27272727 0.72727273) *
#           55) pregnant>=7.5 19   3 pos (0.15789474 0.84210526) *
#     7) glucose>=165.5 46   5 pos (0.10869565 0.89130435) *

rpart.plot(mod_diabetes_sg)
prp(mod_diabetes_sg)
dev.off()


# Todo el particionado lo esta haciendo utlizando las variables insuline,age,pressure,pedigree,pregnant,triceps
# los nuevos terminales son los que tienen asterisco *
# de acuerdo al grafico del arbol de decision vemos que prosigue solo si insulina es mayor igual a 121

diabetes_sg_level1 <- datos_diabetes_sg[datos_diabetes_sg$insulin>=121,]
# para el segundo nivel vemos que el resultado depende si age sea mayor o igual a  29
diabetes_sg_level2 <- diabetes_sg_level1[diabetes_sg_level1$age > 29,]
diabetes_sg_level3 <- diabetes_sg_level2[diabetes_sg_level2$pressure<77,]
diabetes_sg_level4 <- diabetes_sg_level3[diabetes_sg_level3$pedigree>=0.33,]
diabetes_sg_level5 <- diabetes_sg_level4[diabetes_sg_level4$pregnant<8,]
diabetes_sg_level6 <- diabetes_sg_level5[diabetes_sg_level5$pregnant>=4,]
diabetes_sg_level7 <- diabetes_sg_level6[diabetes_sg_level6$triceps>=24,]

View(diabetes_sg_level7)

# Respuesta a la prediccion de NuevoDiabetes2
NuevoDiabetes2 <- data.frame(
  insulin= 130, age=30,  pressure=50, pedigree=0.9,
  pregnant = 7, triceps= 23, mass = 35.5) 


# Utilizamos la funcion predict y nuestro modelo mod_diabetes_sp
prediccion_diabetes_sg <- predict(mod_diabetes_sg,newdata=NuevoDiabetes2,type = "prob")
#   neg  pos
#  0.25 0.75
# Observamos que tenemos un 75 % de que el diagnostico sea POSITIVO

#### PREGUNTA 4 ####
## Construta un arbol de regresion para el data frame Boston (libreria MASS) y genere las predicciones
## para los siguientes dos escenarios 

# Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
#             210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
#             406 ,422, 423 ,453 ,455 ,485, 496, 505)
# Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
# Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
# Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
#   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
#   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
#   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.

# RESPUESTA
library(MASS)
data(Boston)
help("Boston") # Housing Values in Suburbs of Boston

View(datos_boston)
datos_boston <- Boston

Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
            210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
            406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2

help(primes) #Functions for Identifying and Generating Prime Numbers
View(Escenario1) #48 entradas
View(Escenario2) #25 entradas

#   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
#   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
#   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.

# ESCENARIO 1

mod_boston <- rpart(medv ~ ., data=datos_boston) # el punto . quiere decir todas las variables independientes
class(mod_boston)
typeof(mod_boston)
mod_boston


# n= 506 
#
# node), split, n, deviance, yval
# * denotes terminal node
# 1) root 506 42716.3000 22.53281  
#   2) rm< 6.941 430 17317.3200 19.93372  
#     4) lstat>=14.4 175  3373.2510 14.95600  
#       8) crim>=6.99237 74  1085.9050 11.97838 *
#       9) crim< 6.99237 101  1150.5370 17.13762 *
#     5) lstat< 14.4 255  6632.2170 23.34980  
#       10) dis>=1.5511 248  3658.3930 22.93629  
#         20) rm< 6.543 193  1589.8140 21.65648 *
#         21) rm>=6.543 55   643.1691 27.42727 *
#       11) dis< 1.5511 7  1429.0200 38.00000 *
#     3) rm>=6.941 76  6059.4190 37.23816  
#       6) rm< 7.437 46  1899.6120 32.11304  
#         12) lstat>=9.65 7   432.9971 23.05714 *
#         13) lstat< 9.65 39   789.5123 33.73846 *
#       7) rm>=7.437 30  1098.8500 45.09667 *

rpart.plot(mod_boston)
prp(mod_boston)
dev.off()

# Todo el particionado lo esta haciendo utlizando la variable rm
# los nuevos terminales son los que tienen asterisco *
# de acuerdo al grafico del arbol de decision vemos que prosigue solo si lstat es menor a 14

# Nuestro modelo indica que hay un 38% de que medv sea 22 
# medv: valor medio de viviendas ocupadas por sus propietarios en \ $ 1000s.

datos_boston_level1 <- datos_boston[datos_boston$rm<6.9,]
datos_boston_level2 <- datos_boston_level1[datos_boston_level1$lstat<14,]
datos_boston_level3 <- datos_boston_level2[datos_boston_level2$dis>=1.6,]
datos_boston_level4 <- datos_boston_level3[datos_boston_level3$rm<6.5,]

#Hallamos la media de nuestro modelo
media_boston <- mean(datos_boston_level4$medv)
# [1] 21.73908

View(datos_boston)
View(datos_boston_level4) # 174 filas

#PREDICCIÓN UTILIZANDO LA PRUEBA ESCENARIO 1

prediccion_datos_boston_1 <- predict(mod_boston,newdata=Escenario1)
View(prediccion_datos_boston_1) # 48 filas

#Hallamos la media
media_escenario_1 <- mean(prediccion_datos_boston_1)
#22.42833

#Hallamos la desviación estandar
desv_escenario_1 <- sd(prediccion_datos_boston_1)
#[1] 6.989674

#Indicador : Coeficiente de Variacion
coefic_1 <- (desv_escenario_1/media_escenario_1)*100
# [1] 31.16449

plot(x = prediccion_datos_boston_1, y = Escenario1$medv,
     main = "Predicciones modelo vs valor real", 
     xlab = "Predicción", 
     ylab = "Valor real", col = "darkgrey", pch = 19
 )


dev.off()

#PREDICCIÓN UTILIZANDO LA PRUEBA ESCENARIO 2

prediccion_datos_boston_2 <- predict(mod_boston,newdata=Escenario2)
View(prediccion_datos_boston_2) # 25 filas
View(Escenario2)

#Hallamos la media
media_escenario_2 <- mean(prediccion_datos_boston_2)
#24.56362

#Hallamos la desviación estandar
desv_escenario_2 <- sd(prediccion_datos_boston_2)
#[1] 10.80389

#Indicador : Coeficiente de Variacion
coefic_2 <- (desv_escenario_2/media_escenario_2)*100
# [1] 43.98332

plot(x = prediccion_datos_boston_2, y = Escenario2$medv,
     main = "Predicciones modelo vs valor real", 
     xlab = "Predicción", 
     ylab = "Valor real",
     col = "darkgrey", pch = 19
)


dev.off()

# CONCLUSION: Tomando como referencia el menor Coeficiente de Variación coefic_1: 31.16449, 
# muestra que la primera prueba:  Escenario1 presenta menor dispersion respecto a su Media Aritmetica


