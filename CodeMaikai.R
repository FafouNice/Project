
#Variables de scission
frequence<-1              #Number of periods per year for time based tranformation of the data based.2 for half-yearly, 1 for yearly, 0.5 for one every 2years
date_debut<-"2002-01-01"  #beginning of the period of study. Format is yyyy-mm-dd and with quote marks
duree<-16                 #Duration of the observed period. In years : 16 meaning 16 years of study including first year 
decal<-0               #Number of month between the date of the observation and the date of the corresponding economical predictors value
trainingEnd<-"2015-01-01" #Date of the end of the training period's end.Please indicate a begining of the month date. 


#Packages
library(robustHD)      #standardization
library(zoo)           #Time sequences (yearmon(),...)
library(rpart)         # CART
library(caret)         #Wrapper for the 
library(e1071)         #Svm 
library(randomForest)  #random forest
library(FNN)           #KNN
library(lubridate)     #Pour la fonction year()
library(pROC)
library(gbm)
library(dplyr)         # for data manipulation
library(DMwR)          # for smote implementation
library(purrr)         # for functional programming (map)
library(xgboost)

# setwd("B:/ASSU-INT-AFI-ACT/Transversal/Stages/2017 Stage_OEM/03-MÈmoire/donnees")
setwd("C:/Users/x168206/Desktop/donnees")

data71<-read.csv(file="71-final.csv", sep=";")

#On retire les observations pour lesquelles les valeurs sont mal ou non renseignÈes. On retire de plus les fins de contrats pour raisons autres que rachat et les rachats qui n'ont pas de date.  

data71<-data71[which(data71$CONTRACT_EFFECTIVE_DATE != "" & (data71$OPTION == "O" | data71$OPTION == "N") & ((data71$LIBANN == "Rachat total" & data71$DANNULAT !="") | data71$LIBANN=="")),]



#### Stats descriptives (Optionnel)####

data<-data71
#Graph de rÈpartition des MR par tranches
barplot(table(data$CatMR),col="red",xlab="Montant",main="RÈpartition rÈserves mathÈmatiques",names.arg=c("0-10k","10-20k","20-30k","30-40k","40-50k","50-60k","60-70k","70-80k","80-90k","90-100k","100k+"))


#Convertion des dates au bon format
data$CONTRACT_EFFECTIVE_DATE<-as.Date(data$CONTRACT_EFFECTIVE_DATE,format="%d/%m/%Y")
data$INSURED_BIRTHDATE<-as.Date(data$INSURED_BIRTHDATE,format="%d/%m/%Y")

#Age ‡ la souscription
data$AgeSouscription<-round((data$CONTRACT_EFFECTIVE_DATE-data$INSURED_BIRTHDATE)/365.25)

#AnciennetÈ lors du rachat
data$AnciennetÈRachat<-round((data$DANNULAT-data$CONTRACT_EFFECTIVE_DATE)/365,25)
data$AnciennetÈRachat<-round(data$AnciennetÈRachat)
#Attention la partie couleur (col) est ‡ adaptÈ au graphique
barplot(table(data$AnciennetÈRachat),main="AnciennetÈ lors du rachat",xlab="En annÈes",col=c("grey","red",rep("grey",15)))

#Age ‡ la souscription
y<-1
moy<-c(2002:2017)
for(i in c(2002:2017)){
  moy[y]<-mean(data[which(year(data$CONTRACT_EFFECTIVE_DATE)==i),"AgeSouscription"],na.rm=TRUE)
  y<-y+1
}

barplot(table(data$AgeSouscription),main="Age at subscription",xlab="Age(years)")


#Nouveaux contrats par annÈe
for(i in c(2002:2017)){
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==i),"AnnÈeSouscription"]<-i
}
#Les couleurs sont ‡ adapter au graph
barplot(table(data$AnnÈeSouscription),horiz=TRUE,las=1,col=c(rep("grey",6),"red","red",rep("grey",6)),main="Number of new contracts per year",xlab="Number of new contracts")

#Convertions importantes
data$LIBANN<-as.character(data$LIBANN)
data$DANNULAT<-as.Date(data$DANNULAT,format="%d/%m/%Y")

#Rachats par annÈes
for(i in c(2002:2017)){
  data[which(year(data$DANNULAT)==i & data$LIBANN=="Rachat total"),"AnneeRachat"]<-i
}
#couleurs ‡ adapter
barplot(table(data$AnneeRachat),horiz=TRUE,las=1,col=c(rep("grey",6),"red","red","grey","red",rep("grey",3)),main="Number of Lapses per year",xlab="Number of lapses")


#Age lors du rachat
y<-1
tauxRachat<-c(2004:2016)
for (i in 2004:2016){
  tauxRachat[y]<-nrow(data[which(data$LIBANN=="Rachat total" & year(data$DATEEFFETRACHAT)==i),])/nrow(data[which(year(data$CONTRACT_EFFECTIVE_DATE)<=i & year(data$DATEEFFETRACHAT)>=i),])
  y<-y+1
}
plot(c(2004:2016),tauxRachat,type="l",main="Taux de rachat observÈ (Epargne)",xlab="AnnÈe")


#Age au rachat
data$AgeRachat<-round((data$DANNULAT-data$INSURED_BIRTHDATE)/365.25)
barplot(table(data$AgeRachat),main="Age when lapse",xlab="Age")

#COntrats actif par annÈe
con<-c(rep(0,16))
y<-1
for(i in c(2002:2017)){
  con[y]<-nrow(data[which(year(data$DANNULAT)>i & year(data$CONTRACT_EFFECTIVE_DATE)<=i),])
  y<-y+1
}
con<-as.data.frame(con)
rownames(con)<-as.character(c(2002:2017))
barplot(con,names.arg=c(2002:2017),main="Active contracts per year",xlab="year")


#Age moyen lors du rachat
testAgemoyen<-c(rep(0,16))
y<-1
for(i in c(2002:2016)){
  testAgemoyen[y]<-mean(i-year(data[which(year(data$DANNULAT)>i & year(data$CONTRACT_EFFECTIVE_DATE)<=i),"INSURED_BIRTHDATE"]),na.rm=TRUE)
  y<-y+1
}

View(t(as.data.frame(testAgemoyen,row.names=as.character(c(2002:2017)))))

#AnciennetÈ au 31/12/2017
anciennetÈ<-as.numeric((as.Date("2017-12-31")-as.Date(data[which(data[,11]==""),6],format="%d/%m/%Y"))/365.25)
hist(anciennetÈ,col="red",main="AnciennetÈ des contrats actifs au 31/12/2017")

#Triangles des rachats pour l'outil Excel
expo<-as.data.frame(matrix(0,ncol=14,nrow=14),row.names=as.character(c(2004:2017)))
colnames(expo)<-c(2004:2017)
for (i in 2004:2017){
  for(y in i:2017){
    expo[as.character(i),as.character(y)]<-nrow(data[which(year(data$CONTRACT_EFFECTIVE_DATE) == i & (year(data$DANNULAT)>y | is.na(data$DANNULAT))),])
  }
}

rachat<-as.data.frame(matrix(0,ncol=14,nrow=14),row.names=as.character(c(2004:2017)))
colnames(rachat)<-c(2004:2017)
for (i in 2004:2017){
  for(y in i:2017){
    rachat[as.character(i),as.character(y)]<-nrow(data[which(year(data$CONTRACT_EFFECTIVE_DATE) == i & year(data$DANNULAT)==y ),])
  }
}



#### Construction des modËles ####
#RecupÈration des indices Èconomiques (Grille ‡ mettre ‡ jour si on veut projeter au del‡ de 2044-2017)

essai<-read.csv(file='IndiceEco4.csv',sep=";",header=TRUE)
essai$DateDebut<-as.Date(essai$DateDebut,"%d/%m/%Y") #On transforme la colonne mois pour qu'elle soit reconnaissable comme une date en mois par R


#Fonction d'ajout des variables Èconomiques
AjoutEco<-function(tableauEco,donnÈe,decalage){
  donnÈe$Val<-as.Date(as.yearmon(donnÈe$DateCalcul)-decalage/12) #creation d'une variable correspondant au mois dÈcalÈ dans le meme format que la base de donnÈes externes
  Test<-merge(donnÈe,tableauEco,by.x="Val",by.y="DateDebut")                                  #Attention "DateDebut" est le nom de la colonne dans le fichier IndiceEco. Si le fichier change il faut changer ici aussi. Fusion des bases avec comme clÈs les nouvelles variables crÈÈes dans la foncion suivante
  Test$Age <- as.numeric(round((Test$DateCalcul-Test$INSURED_BIRTHDATE)/365.25))              #Age en d√©but de p√©riode
  Test$DureeEcoulee <- ifelse(Test$DateCalcul>Test$CONTRACT_EFFECTIVE_DATE,as.numeric(round((Test$DateCalcul-Test$CONTRACT_EFFECTIVE_DATE)/365.25)),0)  #Dur√©e de vie du contrat en d√©ut de p√©riode
  return(Test)
} 

dupliFreqMaroc <- function(data,freq,DateDebut,Duree,decalage){
  
  #Convertions nÈcessaires au bon fonctionnement du code
  data$INSURED_BIRTHDATE<-as.Date(data$INSURED_BIRTHDATE,format="%d/%m/%Y")             #Chgt de format pour les dates
  data$CONTRACT_EFFECTIVE_DATE<-as.Date(data$CONTRACT_EFFECTIVE_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
  data$DANNULAT<-as.Date(data$DANNULAT,format="%d/%m/%Y")
  data$DATEEFFETRACHAT<-as.Date(data$DATEEFFETRACHAT,format="%d/%m/%Y")
  data$DANNULAT.1<-as.Date(data$DANNULAT.1,format="%d/%m/%Y")
  data$PRESTATION.SERVIE<-as.numeric(data$PRESTATION.SERVIE)  
  data$GenTaux<-0
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2002),"GenTaux"]<-1
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2003),"GenTaux"]<-2
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2004 | year(data$CONTRACT_EFFECTIVE_DATE)==2005 | year(data$CONTRACT_EFFECTIVE_DATE)==2006 ),"GenTaux"]<-3
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2007),"GenTaux"]<-4
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2008),"GenTaux"]<-5
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2009 | year(data$CONTRACT_EFFECTIVE_DATE)==2010 | year(data$CONTRACT_EFFECTIVE_DATE)==2011),"GenTaux"]<-6
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2012 | year(data$CONTRACT_EFFECTIVE_DATE)==2013 | year(data$CONTRACT_EFFECTIVE_DATE)==2014),"GenTaux"]<-7
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2015),"GenTaux"]<-8
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2016),"GenTaux"]<-9
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2017),"GenTaux"]<-10
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2002 | year(data$CONTRACT_EFFECTIVE_DATE)==2003 | year(data$CONTRACT_EFFECTIVE_DATE)==2005 ) & data$OPTION == "O"),"TR"]<-4.85
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2002 | year(data$CONTRACT_EFFECTIVE_DATE)==2003 | year(data$CONTRACT_EFFECTIVE_DATE)==2005 ) & data$OPTION == "N"),"TR"]<-5.00
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2007 | year(data$CONTRACT_EFFECTIVE_DATE)==2008 | year(data$CONTRACT_EFFECTIVE_DATE)==2008 | year(data$CONTRACT_EFFECTIVE_DATE)==2011) & data$OPTION == "O"),"TR"]<-4.05
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2007 | year(data$CONTRACT_EFFECTIVE_DATE)==2008 | year(data$CONTRACT_EFFECTIVE_DATE)==2008 | year(data$CONTRACT_EFFECTIVE_DATE)==2011) & data$OPTION == "N"),"TR"]<-4.20
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2004 & data$OPTION == "O"), "TR"] <- 4.75
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2004 & data$OPTION == "N"), "TR"] <- 4.90
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2006 & data$OPTION == "O"), "TR"] <- 4.45
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2006 & data$OPTION == "N"), "TR"] <- 4.60
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2010 & data$OPTION == "O"), "TR"] <- 4.35
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2010 & data$OPTION == "N"), "TR"] <- 4.50
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2012 | year(data$CONTRACT_EFFECTIVE_DATE)==2013 | year(data$CONTRACT_EFFECTIVE_DATE)==2014 | year(data$CONTRACT_EFFECTIVE_DATE)==2015) & data$OPTION == "O"),"TR"]<-3.85
  data[which((year(data$CONTRACT_EFFECTIVE_DATE)==2012 | year(data$CONTRACT_EFFECTIVE_DATE)==2013 | year(data$CONTRACT_EFFECTIVE_DATE)==2014 | year(data$CONTRACT_EFFECTIVE_DATE)==2015) & data$OPTION == "N"),"TR"]<-4.00
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2016 & data$OPTION == "O"), "TR"] <- 3.65
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2016 & data$OPTION == "N"), "TR"] <- 3.80
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2017 & data$OPTION == "O"), "TR"] <- 3.45
  data[which(year(data$CONTRACT_EFFECTIVE_DATE)==2017 & data$OPTION == "N"), "TR"] <- 3.60
  data[which(data$OPTION == "O" ),"OPT"]<-1
  data[which(data$OPTION == "N" ),"OPT"]<-0
  data$OPT<-factor(data$OPT,ordered=FALSE)
  data$GenTaux<-factor(data$GenTaux,ordered = FALSE)
  
  #Decoupage de la base selon la frÈquence et sur la durÈe choisie
  for (i in 1:(freq*Duree)){
    d<-as.Date(DateDebut)
    
    #Cette Ètape crÈe une nouvelle base si elle n'existe pas(premiËre itÈration)
    if(!(exists("baseDec"))){
      inter<-data[which(((as.Date(as.yearmon(d) + (i-1)/freq))<= data$DANNULAT | is.na(data$DANNULAT)) & data$CONTRACT_EFFECTIVE_DATE<(as.Date(as.yearmon(d) + i/freq))),]
      if(nrow(inter)!=0){
        inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
        inter$Lapse<-0
        inter$Lapse[which(inter$LIBANN=="Rachat total" & inter$DANNULAT<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$DANNULAT)]<-1
        inter<-AjoutEco(essai,inter,decalage)
        baseDec<-inter
      }
    }
    
    #Pour le reste de la boucle
    else    {
      inter<-data[which(((as.Date(as.yearmon(d) + (i-1)/freq))<= data$DANNULAT | is.na(data$DANNULAT))  & data$CONTRACT_EFFECTIVE_DATE <(as.Date(as.yearmon(d) + i/freq))),]
      if(nrow(inter)!=0){
        inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
        inter$Lapse<-0
        inter$Lapse[which(inter$LIBANN=="Rachat total" & inter$DANNULAT<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$DANNULAT)]<-1
        inter<-AjoutEco(essai,inter,decalage)
        baseDec<-rbind(baseDec,inter)
      }
    }
  }
  return(baseDec)
}

#On renseigne ici : la base, la frÈquence d'Ètude par an (1 pour 1 annÈe,2 pour un semestre...), la date de dÈbut et le nombre d'annÈes que l'on veut couvrir. 
test71<-dupliFreqMaroc(data71,frequence,date_debut,duree,decal)


#On transforme les variables quantitatives en binaire
gen.f<-as.factor(test71$GenTaux)
dummies<-model.matrix(~gen.f)
test71<-cbind(test71,dummies)


Type.Client<-as.factor(test71$TYPE_CLIENT)
dummies<-model.matrix(~Type.Client)
test71<-cbind(test71,dummies)

Freq.Prem<-as.factor(test71$PREMIUM_FREQ)
dummies<-model.matrix(~Freq.Prem)
test71<-cbind(test71,dummies)

#on crÈe la variable diffÈrence de taux
test71$TR<-test71$TR/100
test71$Diff<- test71$TR-test71$X10.ans


#CrÈation de nos Èchantillons d'apprentissage et de validation
training <- test71[which(test71$DateCalcul<as.Date(trainingEnd)),] #Apprentissage
validation <- test71[which(test71$DateCalcul>=as.Date(trainingEnd)),] #Validation 


#on garde une base intact au cas ou le random forest ne marche pas avec la base modifiÈe
trainingSep<-training#[!(training$CONTRACT_ID %in% validation$CONTRACT_ID),]

#on Èlimine les variables ne rentrant pas dans le cadre de notre Ètude
trainingSep<-trainingSep[,c(14,22:29,31:39,41:43,45:49)]

#On sÈpare prÈdicteurs et variable prÈdite
trainPredict<-trainingSep[,-2]#prÈdicteurs
trainResponse<-trainingSep[,2]#Lapse

#On transforme la variable Lapse pour pouvoir faire de la classification
trainResponse<-as.factor(make.names(trainResponse))
validation$Lapse<-as.factor(make.names(validation$Lapse))

validation$OPT<-as.numeric(validation$OPT)

#Parametres

#Avec tuning
#gbmGrid<-expand.grid(eta=c(0.02,0.05,0.2),nrounds=c(1000,2500,5000,10000),max_depth=c(3,6,9),colsample_bytree=c(0.5,0.7,1),gamma=0,min_child_weight=c(1,3,5,10),subsample=c(0.5,0.7,1))

#Mon modËle GBM 
gbmGrid<-expand.grid(eta=0.02,nrounds=10000,max_depth=4,colsample_bytree=0.5,gamma=5,min_child_weight=3,subsample=1)


#Si il n'y a pas de tuning ‡ faire
ctrl<-trainControl(method = "none",
                   summaryFunction = twoClassSummary,  #Pour notre problËme binaire
                   classProbs = TRUE,               #Afin de renvoyer les probabilitÈs d'appartenance ‡ chaque classe
                   allowParallel = TRUE)

#Si il y a du tuning ‡ faire
# ctrl<-trainControl(method = "repeatedcv",
#                    number = 3,
#                    repeats = 3,
#                    summaryFunction = twoClassSummary,
#                    classProbs = TRUE,
#                    allowParallel = TRUE)

#ModËle Xgboost
mod_fitXGBOOSTAn <- train(x=as.matrix(trainPredict),y=trainResponse,
                          method = "xgbTree",  #le type de modËle : cf page de caret pour la liste
                          verbose = FALSE,
                          metric = "ROC",    #Metrique, on peut utiliser AIC ou BIC par exemple mais aussi Accuracy bien que peu adaptÈe ‡ notre problËme
                          trControl = ctrl,
                          tuneGrid= gbmGrid)


#ModËle RandomForest

training2<-training[complete.cases(training[,-49]), ]
training2<-training[,c(14,22:29,31:39,41:49)]

mod_fitRF <- train(x=as.matrix(training2[,-c(2,27)]),y=as.factor(make.names(training2[,2])),
                   method = "rf",   
                   verbose = FALSE,
                   metric = "ROC",      #Metrique, on peut utiliser AIC ou BIC par exemple mais aussi Accuracy bien que peu adaptÈe ‡ notre problËme
                   trControl = ctrl)


#ModËle CART
mod_fitCART <- train(x=as.matrix(trainPredict),y=trainResponse,
                     method = "rpart",
                     # verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid=expand.grid(cp=0.004))


#### Sampling ####
#Un fois notre modËle identifiÈ, on peut essayÈ de voir l'impact du sampling

# gbmGrid<-expand.grid(eta=0.02,nrounds=10000,max_depth=4,colsample_bytree=0.5,gamma=0,min_child_weight=1,subsample=1)

# #ctrl<-trainControl(method = "none",
# summaryFunction = twoClassSummary,
# classProbs = TRUE,
# allowParallel = TRUE)


# ctrl$sampling<-"up"
# 
# mod_fitUp<- train(x=trainPredict,y=trainResponse,
#                   method = "xgbTree",
#                   verbose = FALSE,
#                   metric = "ROC",
#                   trControl = ctrl,
#                   tuneGrid= gbmGrid)
# 
# ctrl$sampling<-"down"
# 
# mod_fitDown<- train(x=trainPredict,y=trainResponse,
#                     method = "xgbTree",
#                     verbose = FALSE,
#                     metric = "ROC",
#                     trControl = ctrl,
#                     tuneGrid= gbmGrid)
# 
# ctrl$sampling<-"smote"
# 
# mod_fitSmote<- train(x=trainPredict,y=trainResponse,
#                      method = "xgbTree",
#                      verbose = FALSE,
#                      metric = "ROC",
#                      trControl = ctrl,
#                      tuneGrid= gbmGrid)
# 
# gbmGrid<-expand.grid(interaction.depth =3,
#                      n.trees = 1000,
#                      shrinkage = 0.01,
#                      n.minobsinnode=50)
# 
# ctrl<-trainControl(method = "none",
#                    #number = 3,
#                    #repeats = 3,
#                    summaryFunction = twoClassSummary,
#                    classProbs = TRUE,
#                    allowParallel = TRUE)
# 
# mod_fit <- train(x=trainPredict,y=trainResponse,
#                  method = "gbm",
#                  verbose = FALSE,
#                  metric = "ROC",
#                  trControl = ctrl,
#                  tuneGrid= gbmGrid)


#### Construction de la courbe ROC####

#Fonction permettant de calculer la courbe
test_roc <- function(model, data) {
  
  roc(data$Lapse,
      predict(model, data, type = "prob")[, "X1"])
  
}

#Liste des modËles pour lesquels on veut la courbe
model_list <- list(
  xgboost = mod_fitXGBOOSTAn
  # RandomForest= mod_fitRF
  # CART = mod_fitCART
  # up = mod_fitUp,
  # down = mod_fitDown
)

#Pour l'echantillon de validation, on doit le reconstruire
Training<-cbind(trainPredict,trainResponse)
colnames(Training)[colnames(Training)=="trainResponse"] <- "Lapse"

#Calcul des courbes
model_list_roc <- model_list %>%
  map(test_roc, data = validation)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

#Affichage des AUC par modËle
model_list_roc[[1]]$auc


results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all models
# Couleurs
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7","black","green","blue")

#TracÈ
ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  #scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


testPlot<-varImp(mod_fitXGBOOSTAn)
varImpPlot(varImp(mod_fitXGBOOSTAn))

#Matrice des valeurs de sensibilitÈ et spÈcificitÈ pour 
MatrixSens2<-cbind(model_list_roc$xgboost$sensitivities,model_list_roc$xgboost$specificities,model_list_roc$xgboost$thresholds,apply(cbind(model_list_roc$xgboost$sensitivities,model_list_roc$xgboost$specificities),1,sum))

MatrixSens2<-as.data.frame(MatrixSens2)
seuil<-MatrixSens2[which(MatrixSens2$V4==max(MatrixSens2$V4)),3] #RÈcuperation du seuil

confusionMatrix(table(predict(mod_fitXGBOOSTAn,validation,type="prob")[,2]>seuil,validation$Lapse == "X1"))


extract<-cbind(predict(mod_fitXGBOOSTAn,validation[which(year(validation$DateCalcul)==2016),],type="prob")[,2],validation[which(year(validation$DateCalcul)==2016),])
write.table(extract,file="Test2016.csv",sep=";")

extract<-cbind(predict(mod_fitXGBOOSTAn,validation[which(year(validation$DateCalcul)==2017),],type="prob")[,2],validation[which(year(validation$DateCalcul)==2017),])
write.table(extract,file="Test2017.csv",sep=";")

extract<-cbind(predict(mod_fitXGBOOSTAn,validation,type="prob")[,2],validation)
write.table(extract,file="TestExtract.csv",sep=";")


103879155+2813

42319/(42319+10387)

11968-9933

(8903+9933)/((2229+8763)+11968)
9933/11968
10


confusionMatrix(table(predict(mod_fitXGBOOSTAn,validation,type="prob")[,2]>0.1451196,validation$Lapse == "X1"))

validation$DateCalcul<-as.Date
confusionMatrix(table(predict(mod_fitXGBOOSTAn,validation[which(year(validation$DateCalcul)==2014),],type="prob")[,2]>0.2214917,validation[which(year(validation$DateCalcul)==2014),"Lapse"] == "X1"))
confusionMatrix(table(predict(mod_fitXGBOOSTAn,validation[which(year(validation$DateCalcul)==2015),],type="prob")[,2]>0.2214917,validation[which(year(validation$DateCalcul)==2015),"Lapse"] == "X1"))
confusionMatrix(table(predict(mod_fitXGBOOSTAn,validation[which(year(validation$DateCalcul)==2016),],type="prob")[,2]>0.2214917,validation[which(year(validation$DateCalcul)==2016),"Lapse"] == "X1"))

2258/(2258+648)
0.1993135
confusionMatrix(table(predict(xgb_train_3,validation,type="prob")[,2]>0.1993135,validation$Lapse == "X1"))
6438/(1734+6438)

summary(predict(mod_fitXGBOOSTAn,validation,type="raw"))

pred<-predict(mod_fitXGBOOSTAn,validation,type="prob")
predi<-ifelse(pred>0.2871453,"X1",0)
confusionMatrix(predi,validation$Lapse)
validation$Lapse<-make.names(as.factor(validation$Lapse))

pred<-predict(mod_fitXGBOOSTAn,newdata=validation)
confusionMatrix(pred,validation$Lapse)
trainMRL<-trainingSep[,-c(8)]
trainMRL<-standardize(trainMRL)
trainMRL<-cbind(trainMRL,trainingSep[,c(8)])
colnames(trainMRL)[colnames(trainMRL)=="trainingSep[, c(8)]"] <- "Lapse"
validationMRL<-validation[,c(1:5,9:14)]
validationMRL<-standardize(validationMRL)
validationMRL<-cbind(validationMRL,validation[,c(7,8,15:28)])


trainMRLPredict <- trainMRL[which(trainMRL$Annee < 2014),]
trainMRLValid <- trainMRL[which(trainMRL$Annee > 2013),]
library("flexmix")

trainMRLDown<-downSample(trainMRL[,-28],as.factor(trainMRL[,28]),yname = "Lapse")
trainMRLDown$Lapse<-as.numeric(trainMRLDown$Lapse)-1
#colnames(trainMRLDown)[colnames(trainMRLDown)=="trainingSep$Lapse"] <- "Lapse"
mod_fit3<-stepFlexmix(cbind(Lapse~ MR + TauxCroissancePIB  + Age + DureeEcoulee  , data = trainMRLDown,k=c(2,3,4,5,7,10),nrep=4,
                            model =FLXMRglm(family="binomial"),control=list(verbose=1,tolerance=0.000000001))
                      
                      mod_fit15<-flexmix(cbind(Lapse,1-Lapse)~Annee + MR + VPRIM + VSECONDE + MTINTER + MTPB + TauxCroissancePIB + Taux.Chomage + Taux.interets  + Age + DureeEcoulee + gen.f2 + gen.f3 + gen.f4 + gen.f5 + gen.f6 + Type.ClientAUTRES + Type.ClientMVIE + Type.ClientSGMB + Type.ClientPKFON + Freq.PremL + Freq.PremM + Freq.PremT, data = trainMRL,k=20 , control=list(verbose=1,tolerance=0.000001,nrep = 4,minprior = 0.01),model =FLXMRglm(family="binomial") )
                      #MR + VPRIM + VSECONDE + MTINTER + MTPB + TauxCroissancePIB + Taux.Chomage + Taux.interets  + Age + DureeEcoulee + gen.f2 + gen.f3 + gen.f4 + gen.f5 + gen.f6 + Type.ClientAUTRES + Type.ClientMVIE + Type.ClientSGMB + Type.ClientPKFON + Freq.PremL + Freq.PremM + Freq.PremT
                      sapply(mod_fit15, BIC)
                      summary(mod_fit15)
                      mod_opti<-getModel(mod_fit)
                      
                      data("NPreg")
                      View(NPreg)
                      mod_fit3@models
                      pred<-predict(mod_fit15,newdata=trainMRL,aggregate=TRUE)
                      predi<-ifelse(pred[[1]]<0.5,0,1)
                      table(predi,trainMRL$Lapse)
                      pred<-predict(mod_fit15,newdata=validationMRL,aggregate=TRUE)
                      predi<-ifelse(pred[[1]]<0.5,0,1)
                      table(predi,validationMRL$Lapse)
                      
                      
                      table(trainMRL$Lapse,mod_fit@cluster)
                      validation$Lapse<-as.factor(validation$Lapse)
                      trainingSep$Lapse<-as.factor(trainingSep$Lapse)
                      CM<-confusionMatrix(predi,validation$Lapse)
                      
                      mod_fitLR<-glm(Lapse~ MR +X10.ans+MASI+Age+DureeEcoulee, data = trainingSep,family=binomial(link="logit"))
                      
                      predLR<-predict(mod_fitLR,newdata=validation,type="response")
                      prediLR<-as.factor(ifelse(predLR<0.5,0,1))
                      confusionMatrix(as.factor(prediLR),validation$Lapse)
                      
                      
                      #### 
                      
                      tab<-data.frame()
                      pred<-predict(mod_fit,newdata=trainMRL,aggregate=TRUE)
                      for (i in seq(from=0, to =1,by = 0.005)){
                        predi<-ifelse(pred[[1]]<i,0,1)
                        inter<-table(predi,trainMRL$Lapse)
                        if(nrow(inter)==1 && rownames(inter)=="1" ){ tab<-rbind(tab,c(0,0,i,1))}
                        else if(nrow(inter)==1 && rownames(inter)=="0" ){ tab<-rbind(tab,c(1,1,i,0))}
                        else{
                          tab<-rbind(tab,c(inter[1,1]/(inter[1,1]+inter[2,1]),inter[1,2]/(inter[1,2]+inter[2,2]),i,1-(inter[1,2]/(inter[1,2]+inter[2,2]))))
                        }
                      }
                      
                      pred<-predict(mod_fit,newdata=validation,aggregate=TRUE)
                      predi<-ifelse(pred[[1]]<0.33,0,1)
                      table(predi,validation$Lapse)
                      
                      plot(tab[,2],tab[,1])
                      
                      
                      Models[[1]]<- train(x=trainPredict,y=trainResponse,
                                          method = "xgbTree",
                                          verbose = FALSE,
                                          metric = "ROC",
                                          trControl = ctrl,
                                          tuneGrid= gbmGrid)
                      model_list_roc <- Models %>%
                        map(test_roc, data = validation)
                      
                      testAUC <- model_list_roc %>%
                        map(auc)
                      
                      