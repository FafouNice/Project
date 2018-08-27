
setwd("B:/ASSU-INT-AFI-ACT/Transversal/Stages/2018 Mémoire Tabammout Afaf")
#REG LINEAIRE MULTIPLE
install.packages("ggcorrplot")


install.packages("XML")
install.packages("reshape2")
install.packages("plyr")
install.packages("ggplot2")
library(microbenchmark)

library(FSelectorRcpp)

library(RWeka)
library(Amelia)
library(scales)

library(XML)
library(reshape2)
library(plyr)
library(ggplot2)
source('http://klein.uk/R/Viz/pyramids.R')
require(RODBC)
library(sqldf)


table1=sqldf(' SELECT    Sexe,Age,count(Age) as Nombre from Base1 group by Age')

table1$Intervalles<- cut(table1$Age,seq(0,100,10))
table2= subset(table1, select = c(Age,Intervalles,Sexe,Nombre))
table3=sqldf(' SELECT Sexe,Intervalles, Nombre   from table1 group by  Intervalles ')

install.packages("devtools")
devtools::install_github("ramnathv/rCharts")
library(rCharts)

max(table1$Nombre)
sum(table1$Nombre)
pyramidFH <- ggplot(table1, aes(x = Intervalles, y = Nombre, fill = Sexe)) + 
  geom_bar(data = subset(table1, Sexe == "F"), stat = "identity") + 
  geom_bar(data = subset(table1, Sexe == "H"), stat = "identity") +
 
scale_y_continuous(labels = paste0(as.character(c(seq(2, 0, -1), seq(1, 2, 1))), "m")) + 
  coord_flip()
pyramidFH
nPyramid(dat = table1, colors = c('blue', 'silver'))

#Puis commençons à tracer notre graphique avec les dames en rose & les messieurs en bleu (quelle originalité !).
#Comme le graphique est pivoté à l'horizontale les axes sont inversés & les réglages de l'axe de y sont sur l'ordonnée.

gg <-  ggplot(Base1) +
  aes(x=cut.age,fill=Sexe) +
  geom_bar(data = subset(Base1,Base1$Sexe=="F"),aes(y=..count..*(-1))) + # les valeurs deviennent négatives
  geom_bar(data = subset(Base1,Base1$Sexe=="H")) +
  + scale_y_discrete(limits=c(-1000,0,1000,2000),labels=c(1000,0,1000,2000))+
  coord_flip()

plot(gg)





Base= read.csv(file.choose(),sep=";")
pie(table(Base$STATUS_AT_THE_CLOSING_DATE))

nrow(Base)
head(Base)
str(Base)
names(Base)
summary(Base)
#  gestion des Na dans les colonnes primes 
Base$Primes.2011[is.na(Base$Primes.2011)] <- 0
Base$Primes.2012[is.na(Base$Primes.2012)] <- 0
Base$Primes.2013[is.na(Base$Primes.2013)] <- 0
Base$Primes.2014[is.na(Base$Primes.2014)] <- 0
Base$Primes.2015[is.na(Base$Primes.2015)] <- 0

#Création d'une variable Prime totale

Base$Prime.Totale= Base$Primes.2011+Base$Primes.2012+Base$Primes.2013+Base$Primes.2014+Base$Primes.2015

# Le map des données maquantes 

missmap(Base, col=c("blue", "red"), legend=FALSE)

#Recodage des variables: Pour faire une analyse des variables quantitatives et qualitatives

# Nous allons tout d'abord distinguer les variables qualitatives et quantitatives 

factorconvert <- function(f){as.numeric(levels(f))[f]}

#  Séparation des  variables quantitaives et qualitatives

ind.quant <- sapply(Base, function(x) is.numeric(x) | is.integer(x))
ind.qual <- sapply(Base, function(x) is.factor(x))

# variables quantitatives
Base.quant <- Base[ ,ind.quant]


# variables qualitatives
Base.qual <- Base[ ,ind.qual]


# ANALYSE STATISTIQUE  des variables qualitatives: Analyse graphique

#  analyse de la variable Sexe

summary(Base$Sexe)

set.seed(1234)

Plot1=ggplot(data=Base, aes(x=Sexe,fill=Sexe)) +
  geom_bar(stat="count")
Plot1+expand_limits(y=c(0,500000))

Plot1 + scale_y_continuous(labels = scientific)


# Changer la couleur des traits par groupe

library(plyr)
mu <- ddply(Base, "Base$Sexe", summarise, grp.mean=mean(Base$Age))
head(mu)
p=ggplot(Base, aes(x=Age, color=Sexe)) +
  geom_density(alpha=0.4)
p + theme(legend.position="top")



colors<-c("gray","lightgreen","darkred")

# Histogramme avec courbe de distribution
ggplot(Base, aes(x=Age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 
# Couleur par groupes
ggplot(Base, aes(x=Age, color=Sexe, fill=Sexe)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.2) 


pie(table(Base$STATUS_AT_THE_CLOSING_DATE),col=colors,main="Répartition de la consommation de tabac",
    labels=round(table(Base$STATUS_AT_THE_CLOSING_DATE)/nrow(Base),digits=3))
legend("bottomleft", xpd = TRUE, legend = unique(Base$STATUS_AT_THE_CLOSING_DATE),fill=colors)


ggplot(data = Base, aes(x = Age, fill =Base$Sexe)) + 
  geom_bar(data = subset(Base,Base$Sexe == "H")) + 
  geom_bar(data = subset(Base, Base$Sexe == "F"), 
           mapping = aes(y = - ..count.. ),
           position = "identity") +
  scale_y_continuous(labels = abs) +
  coord_flip()




