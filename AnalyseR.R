
library(robustHD)      #standardisation
library(zoo)           #decoupage temporel (yearmon(),...)
library(rpart)         # CART
library(caret)         #Cart et Gradient Boosting
library(e1071)         #Svm 
library(randomForest)  #random forest
library(FNN)           #KNN
library(lubridate)     #Pour la fonction year()
library(pROC)
library(gbm)
library(dplyr) # for data manipulation
# for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(doParallel)

library(parallel)
library(doParallel)
library(psych)
setwd("B:/ASSU-INT-AFI-ACT/Transversal/Stages/2017 Stage_OEM/03-M�moire/donnees")

#Import de la base
dataTest<-read.csv(file='Base_Maroc_Prev.csv',sep=";",dec=".",stringsAsFactors = F)

dataTest$Lapse<-0
dataTest[which(dataTest$STATUS_AT_THE_CLOSING_DATE == "Lapse"),40]<-1
dataTest<-dataTest[dataTest$STATUS_AT_THE_CLOSING_DATE != "" ,]                                     #Pas d'infos sur le chgt de status
dataTest$INSURED_BIRTH_DATE<-as.Date(dataTest$INSURED_BIRTH_DATE,format="%d/%m/%Y")             #Chgt de format pour les dates
dataTest$CONTRACT_EFFECTIVE_DATE<-as.Date(dataTest$CONTRACT_EFFECTIVE_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
dataTest$THEORETICAL_ENDING_DATE<-as.Date(dataTest$THEORETICAL_ENDING_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
dataTest$Primes.2011<-as.numeric(dataTest$Primes.2011)
dataTest$Primes.2012<-as.numeric(dataTest$Primes.2012)
dataTest$Primes.2013<-as.numeric(dataTest$Primes.2013)
dataTest$Primes.2014<-as.numeric(dataTest$Primes.2014)
dataTest$Primes.2015<-as.numeric(dataTest$Primes.2015)
dataTest$Prime<-apply(cbind(dataTest$Primes.2011,dataTest$Primes.2012,dataTest$Primes.2013,dataTest$Primes.2014,dataTest$Primes.2015),1,mean,na.rm=TRUE)

dataTest<-dataTest[,-c(1:11,13:14,16:17,19:39)]
dataTest<-dataTest[-c(533115,533131,290739),]
dataTest$DURACTION<-as.numeric(dataTest$DURACTION)
dataTest$Prime[is.nan(dataTest$Prime)]<-0
echant.flag <- createDataPartition(y=dataTest$Lapse,p=0.66,list=FALSE)
training <- dataTest[echant.flag,]
validation <- dataTest[-echant.flag,]
training$Lapse<-make.names(as.factor(training$Lapse))
validation$Lapse<-make.names(as.factor(validation$Lapse))
#data2<-read.csv(file = 'BQ_EPA_ADHESION_72_final.csv',sep=";",header=TRUE,dec=",")
trainPredict<-training[,-c(3,4)]
trainResponse<-training[,4]

ctrl<-trainControl(method = "none",
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE,
                   allowParallel = TRUE)
gbmGrid<-expand.grid(interaction.depth =  3,
                     n.trees =1000,
                     shrinkage = 0.2,
                     n.minobsinnode=100)


mod_fit_test2<-train(x=trainPredict,y=trainResponse,
                     method = "gbm",
                     verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid= gbmGrid)

ctrl$sampling<-"up"
mod_up_fit2 <- train(x=trainPredict,y=trainResponse,
                     method = "gbm",
                     verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid= gbmGrid)

ctrl$sampling<-"down"
mod_down_fit2 <- train(x=trainPredict,y=trainResponse,
                       method = "gbm",
                       verbose = FALSE,
                       metric = "ROC",
                       trControl = ctrl,
                       tuneGrid= gbmGrid)

test_roc <- function(model, data) {
  
  roc(data$Lapse,
      predict(model, data, type = "prob")[, "X1"])
  
}

model_list <- list(original = mod_fit_test2,
                   up = mod_up_fit2,
                   down = mod_down_fit2)

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



results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
#table(data$Product_ID)
#table(data$PRODUCT_name)
#table(data$Type_Client)
#table(data$PREMIUM_FREQ)
#EffDateMissing<-data[which(data$CONTRACT_EFFECTIVE_DATE==""),]
#table(data$OPTION)
#DannulatDateMissing<-data[which(data$DANNULAT==""),]
#ChangingdatMissing<-data[which(data$STATUS_AT_THE_CLOSING_DATE==""),]
#EffRachatMissing<-data[which(data$DATEEFFETRACHAT==""),]
#prestaMissing<-data[which(is.na(data$PRESTATION.SERVIE)),]


#summary(data2)
#table(data2$PRODUCT_ID)
#table(data2$PRODUCT_NAME)
#table(data2$TYPE_CLIENT)
#table(data2$PREMIUM_FREQ)
#EffDateMissing<-data[which(data2$CONTRACT_EFFECTIVE_DATE==""),]
#table(data2$OPTION)
#DannulatDateMissing<-data2[which(data2$DANNULAT==""),]
#ChangingdatMissing<-data2[which(data2$STATUS_AT_THE_CLOSING_DATE==""),]
#EffRachatMissing<-data2[which(data2$DATEEFFETRACHAT==""),]
#prestaMissing<-data2[which(is.na(data2$PRESTATION.SERVIE)),]
#table(data2$LIBANN)
#table(data2$Type.Prest)
#barplot(table(data2$CONTRACT_EFFECTIVE_DATE),main="Effective date")
#barplot(table(data2$INSURED_BIRTH_DATE),main="insured birthdate")
#summary(data2)



#### Import des donn�es externes ####
essai<-read.csv(file='IndiceEco4.csv',sep=";",header=TRUE)
essai$DateDebut<-as.Date(essai$DateDebut,"%d/%m/%Y") #On transforme la colonne mois pour qu'elle soit reconnaissable comme une date en mois par R



AjoutEco<-function(tableauEco,donn�e,decalage){
  donn�e$Val<-as.Date(as.yearmon(donn�e$DateCalcul)-decalage/12)  #creation d'une variable correspondant au mois décalé dans le même format que la base de données externes
  Test<-merge(donn�e,tableauEco,by.x="Val",by.y="DateDebut")                                        #Fusion des bases avec comme clé les nouvelles variables créées précedemment
  Test$Age <- as.numeric(round((Test$DateCalcul-Test$INSURED_BIRTH_DATE)/365.25))              #Age en début de période
  Test$DureeEcoulee <- as.numeric(round((Test$DateCalcul-Test$CONTRACT_EFFECTIVE_DATE)/30.5))  #Durée de vie du contrat en déut de période
  #Test<-Test[,-c(1,2,3,6,7,8,9,10,11,12)]                                                   #Suppression des variables intermédiaires et des variables non intégrées dans le modèle
  return(Test)
} 

dupliFreq <- function(data,freq,DateDebut,Duree){
  
  #data<-data[,-c(1,2,3,4,5,6,7,8,10,13,14)]                                              #Suppression variables inutilisées
  data<-data[data$STATUS_AT_THE_CLOSING_DATE != "" ,]                                     #Pas d'infos sur le chgt de status
  data$INSURED_BIRTH_DATE<-as.Date(data$INSURED_BIRTH_DATE,format="%d/%m/%Y")             #Chgt de format pour les dates
  data$CONTRACT_EFFECTIVE_DATE<-as.Date(data$CONTRACT_EFFECTIVE_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
  data$THEORETICAL_ENDING_DATE<-as.Date(data$THEORETICAL_ENDING_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
  data$STATUS_CHANGE_DATE<-as.Date(data$STATUS_CHANGE_DATE,format="%d/%m/%Y")             #Chgt de format pour les dates
  data$Primes.2011<-as.numeric(data$Primes.2011)
  data$Primes.2012<-as.numeric(data$Primes.2012)
  data$Primes.2013<-as.numeric(data$Primes.2013)
  data$Primes.2014<-as.numeric(data$Primes.2014)
  data$Primes.2015<-as.numeric(data$Primes.2015)
  #dataTest<-dataTest[,-c(1,2,3,4,5,6,7,8,10,13,14,17,18,19,20,21,22,23)]
  data$Prime<-apply(cbind(data$Primes.2011,data$Primes.2012,data$Primes.2013,data$Primes.2014,data$Primes.2015),1,mean,na.rm=TRUE)
  #Decoupage de la base selon la fréquence
  for (i in 1:(freq*Duree)){
    d<-as.Date(DateDebut)
    
    if(!(exists("baseDec"))){
      inter<-data[which((as.Date(as.yearmon(d) + (i-1)/freq))<= data$STATUS_CHANGE_DATE & data$CONTRACT_EFFECTIVE_DATE<(as.Date(as.yearmon(d) + i/freq))),]
      if(nrow(inter)!=0){
      inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
      inter$Lapse<-0
      inter$Lapse[which(inter$STATUS_AT_THE_CLOSING_DATE=="Lapse" & inter$STATUS_CHANGE_DATE<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$STATUS_CHANGE_DATE)]<-1
      inter<-AjoutEco(essai,inter,3)
      baseDec<-inter
      }
    }
    else    {
      inter<-data[which((as.Date(as.yearmon(d) + (i-1)/freq) )<= data$STATUS_CHANGE_DATE & data$CONTRACT_EFFECTIVE_DATE <(as.Date(as.yearmon(d) + i/freq))),]
      if(nrow(inter)!=0){
      inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
      inter$Lapse<-0
      inter$Lapse[which(inter$STATUS_AT_THE_CLOSING_DATE=="Lapse" & inter$STATUS_CHANGE_DATE<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$STATUS_CHANGE_DATE)]<-1
      inter<-AjoutEco(essai,inter,3)
      baseDec<-rbind(baseDec,inter)
      }
    }
  }
  
  return(baseDec)
}
testPRev<-dupliFreq(dataTest,1,"2004-01-01",20)
#rm(list=c("data","essai"))

# dupliFreqMaroc <- function(data,freq,DateDebut,Duree){
#   
#   #data<-data[,-c(1,2,3,4,5,6,7,8,10,13,14)]                                              #Suppression variables inutilisées
#   data<-data[data$Type.annulation !="",]                                     #Pas d'infos sur le chgt de status
#   data$INSURED_BIRTHDATE<-as.Date(data$INSURED_BIRTHDATE,format="%d/%m/%Y")             #Chgt de format pour les dates
#   data$CONTRACT_EFFECTIVE_DATE<-as.Date(data$CONTRACT_EFFECTIVE_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
#   #data$THEORETICAL_ENDING_DATE<-as.Date(data$THEORETICAL_ENDING_DATE,format="%d/%m/%Y")   #Chgt de format pour les dates
#   data$DANNULAT<-as.Date(data$DANNULAT,format="%d/%m/%Y")
#   data$MR<-as.numeric(data$MR)                                          #Si on ne remet pas les primes en numérique on aura une aura dans la méthode randomForest
#   data$PRESTATION.SERVIE<-as.numeric(data$PRESTATION.SERVIE)  
#   
#   
#   #Decoupage de la base selon la fréquence
#   for (i in 1:(freq*Duree)){
#     d<-as.Date(DateDebut)
#     
#     if(i==1){
#       inter<-data[which((as.Date(as.yearmon(d) + (i-1)/freq))<= data$DANNULAT & data$CONTRACT_EFFECTIVE_DATE<(as.Date(as.yearmon(d) + i/freq))),]
#       inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
#       inter$Lapse<-0
#       inter$Lapse[which(inter$Type.annulation=="Rachat total" & inter$DANNULAT<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$DANNULAT)]<-1
#       #inter<-AjoutEco(essai,inter,3)
#       baseDec<-inter
#     }
#     else    {
#       inter<-data[which((as.Date(as.yearmon(d) + (i-1)/freq) )<= data$DANNULAT & data$CONTRACT_EFFECTIVE_DATE <(as.Date(as.yearmon(d) + i/freq))),]
#       inter$DateCalcul <- (as.Date(as.yearmon(d) + (i-1)/freq))
#       inter$Lapse<-0
#       inter$Lapse[which(inter$Type.annulation=="Rachat total" & inter$DANNULAT<(as.Date(as.yearmon(d) + i/freq)) & (as.Date(as.yearmon(d) + (i-1)/freq))<= inter$DANNULAT)]<-1
#       #inter<-AjoutEco(essai,inter,3)
#       baseDec<-rbind(baseDec,inter)
#     }
#   }
#   
#   return(baseDec)
# }
# test<-dupliFreqMaroc(data,4,"2003-01-01",10)
# 
# test<-test[,-c(1,2,3,4,5,6,7,8,9,11,14,17,18,19)]
# test<-test[,-4]
# 
# mean(test$OUTSTANDING_SUM_INSURED)
# test$OUTSTANDING_SUM_INSURED<-as.numeric(as.character(test$OUTSTANDING_SUM_INSURED))
# summary(test$OUTSTANDING_SUM_INSURED)
# 
# levels(data$Type.annulation)
# table(data$Type.annulation)
# levels(data$TYPE_CLIENT)
# table(data$TYPE_CLIENT)
# summary(data)
#### Creation des echantillons de training et de test ####

test2<-testPRev[,c(13,16,41:50)]


test2[which(is.na(test2$Prime)==TRUE),4]<-0
test2$DURACTION<-as.numeric(test2$DURACTION)
#train.flag <- createDataPartition(y=dataTest$Lapse,p=0.66,list=FALSE)
training <- test2[which(test2$DateCalcul<as.Date("2014-01-01")),]
validation <- test2[which(test2$DateCalcul>=as.Date("2014-01-01")),]
training$Lapse<-make.names(as.factor(training$Lapse))
validation$Lapse<-make.names(as.factor(validation$Lapse))
#rm(list=c("train.flag","test"))
#### Mod�le SVM ####
#Estimation du mod�le


# fitsvm<-svm(trainingY~.,data=cbind(trainingY,trainingX),type="C")
# pred.svm<-predict(fitsvm,newdata=validation,type="class")
# confusionMatrix(pred.svm, validation$Lapse)


#### Essai CART ####
# training <- test2[which(test$DateCalcul<as.Date("2014-01-01")),]
# validation <- test2[which(test$DateCalcul>=as.Date("2014-01-01")),]
# 
# modfit <- train(Species~.,method="rpart",data=training)
# fitCARTEco <- rpart(Lapse~INITIAL_SUM_INSURED + DURACTION +Prime +Taux.Croissance.PIB+Taux.croissance.PIB Indice.Boursier + Taux.Chomage + IndicePrix + ConfianceConso + Age + DureeEcoulee,method="class", data=training,parms=list(prior=c(0.05,0.95))) 
# pred.cartEco<-predict(fitCARTEco,newdata=validation,type="class")
# confusionMatrix(pred.cartEco, validation$Lapse)


#### Random forest ####
# test2<-test[,-c(1:12,14,15,17:41)]
# training <- test2[which(test$DateCalcul<as.Date("2014-01-01")),]
# validation <- test2[which(test$DateCalcul>=as.Date("2014-01-01")),]
# 
# training$Lapse <- as.factor(training$Lapse)
# training<-training[,-3]
# training<-na.omit(training)
# trainingX<-training[,-11]
# trainingY<-training[,11]
# validation$Lapse <- as.factor(validation$Lapse)
# validation<-na.omit(validation)
# trainingX<-trainingX[,-c(1,2,5,6,7,8,10)]
# training<-training[,-c(1,2)]
# validation<-validation[,-c(1,2)]
# training<-training[,-c(4)]
# validation<-validation[,-c(4)]
# 
# validation<-validation[,-c(1,2,5,6,7,8,10)]
# propLapse<-nrow(training[which(training$Lapse==1),])/nrow(training)

# gc()
# fitRFEco <- randomForest(Lapse ~ INITIAL_SUM_INSURED + DURACTION +Prime +Taux.croissance.PIB+ Indice.Boursier + Taux.Chomage + IndicePrix + ConfianceConso + Age + DureeEcoulee,data=training,ntree=100,nodesize = 25,cutoff=c(0.99,0.01))
# pred.RFEco<-predict(fitRFEco,validation)
# confusionMatrix(pred.RFEco, validation$Lapse)
# 
# 
# test2<-testPRev[,-c(1:12,14,15,17:41,44:48)]
# test2[which(is.na(test2$Prime)==TRUE),3]<-0
# #train.flag <- createDataPartition(y=dataTest$Lapse,p=0.66,list=FALSE)
# training <- test2[which(test2$DateCalcul<as.Date("2014-01-01")),]
# validation <- test2[which(test2$DateCalcul>=as.Date("2014-01-01")),]
# training$Lapse<-make.names(as.factor(training$Lapse))
# validation$Lapse<-make.names(as.factor(validation$Lapse))

trainPredict<-training[,-c(3,5)]
trainResponse<-training[,c(3)]
trainResponse<-make.names(trainResponse)
#rm(list=c("train.flag","test"))
#### Mod�le SVM ####
#Estimation du mod�le
gbmGrid<-expand.grid(eta=0.2,nrounds=500,max_depth=c(3,6,9),colsample_bytree=1,gamma=0,min_child_weight=3,subsample=0.7)

gbmGrid <- expand.grid(eta=0.2,nrounds=1000,max_depth=3,colsample_bytree=1,gamma=0,min_child_weight=3,subsample=0.7)
GridRF<-expand.grid(mtry=4)

ctrl<-trainControl(method = "none",
                   # number = 3,
                   # repeats = 3,
                   summaryFunction = twoClassSummary,
                   classProbs = TRUE,
                   allowParallel = TRUE,
                   returnData = FALSE,
                   returnResamp = "none",
                   savePredictions = "none",
                   p=0.6)

mod_fitXGBOOSTPrev <- train(x=trainPredict,y=trainResponse,
                          method = "xgbTree",
                          verbose = FALSE,
                          metric = "ROC",
                          trControl = ctrl,
                          tuneGrid= gbmGrid)

mod_fitRF <- train(x=trainPredictRF,y=trainResponseRF,
                   method = "rf",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl,
                   tuneGrid= GridRF)

trainRF<-training[sample(nrow(training),150000),]
trainPredictRF<-trainRF[,-c(3,5)]
trainResponseRF<-trainRF[,c(3)]
trainPredict<-trainPredict[,-c(3,6)]

correlationMatrix <- cor(trainPredict)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)



mod_fitCART <- train(x=as.matrix(trainPredict),y=trainResponse,
                     method = "rpart",
                     #verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl,
                     tuneGrid=expand.grid(cp=0.004))

test_roc <- function(model, data) {
  
  roc(data$Lapse,
      predict(model, data, type = "prob")[, "X1"])
  
}

model_list <- list(XgBoostMensuel = mod_fitXGBOOSTPrev
  # Xgboost1  = xgb_train_1,
  # XgBoost2= xgb_train_2,
  # XgBoost3 = xgb_train_3,
  # XgBoost4 = xgb_train_4,
  # XgBoost5 = xgb_train_5,
  # XgBoost6 = xgb_train_6,
  # XgBoost7 = xgb_train_7,
  # final = xgb_model,
  # tuning1 = xgb_tune,
  # tuning2 = xgb_tune2,
  # tuning3 = xgb_tune3,
  # tuning4 = xgb_tune4,
  # tuning5 = xgb_tune5
  #xgboost = mod_fit2
  #origtest = mod_fit_test,
  #lastone= mod_fit2$modelInfo
  #gbmUp = mod_fitUp,
  #gbmDown = mod_fitDown
  #gbmSmote= mod_fitSmote
)

test<-cbind(trainPredict,trainResponse)
colnames(test)[colnames(test)=="trainResponse"] <- "Lapse"

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

model_list_roc[[1]]$auc



results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7","black","green","blue")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  #scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


full<-cbind(trainPredict,trainResponse)
colnames(full)[colnames(full)=="trainResponse"] <- "Lapse"
full<-rbind(full,validation[,-5])

library(FactoMineR)
library(explor)
library(ggplot2)
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("shiny", "rstudio")

install.packages("httpuv")
install.packages("shiny")

install.packages('httpuv', type='binary')
install.packages('shiny', type='binary')
library("shiny")
full.acp <- PCA(trainingSepTest[,-6],)
explor(full.acp)
# fitsvm<-svm(trainingY~.,data=cbind(trainingY,trainingX),type="C")
# pred.svm<-predict(fitsvm,newdata=validation,type="class")
# confusionMatrix(pred.svm, validation$Lapse)


#### Essai CART ####
# training <- test2[which(test$DateCalcul<as.Date("2014-01-01")),]
# validation <- test2[which(test$DateCalcul>=as.Date("2014-01-01")),]
# 
# modfit <- train(Species~.,method="rpart",data=training)
fitCARTNoEco <- rpart(Lapse~INITIAL_SUM_INSURED + DURACTION +Prime + Indice.Boursier + Taux.Chomage + IndicePrix + ConfianceConso + Age + DureeEcoulee,method="class", data=training,parms=list(prior=c(0.05,0.95))) 
pred.cartNoEco<-predict(fitCARTNoEco,newdata=validation,type="class")
confusionMatrix(pred.cartNoEco, validation$Lapse)


#### Random forest ####
# test2<-test[,-c(1:12,14,15,17:41)]
# training <- test2[which(test$DateCalcul<as.Date("2014-01-01")),]
# validation <- test2[which(test$DateCalcul>=as.Date("2014-01-01")),]
# 
# training$Lapse <- as.factor(training$Lapse)
# training<-training[,-3]
# training<-na.omit(training)
# trainingX<-training[,-11]
# trainingY<-training[,11]
# validation$Lapse <- as.factor(validation$Lapse)
# validation<-na.omit(validation)
# trainingX<-trainingX[,-c(1,2,5,6,7,8,10)]
# training<-training[,-c(1,2)]
# validation<-validation[,-c(1,2)]
# training<-training[,-c(4)]
# validation<-validation[,-c(4)]
# 
# validation<-validation[,-c(1,2,5,6,7,8,10)]
# propLapse<-nrow(training[which(training$Lapse==1),])/nrow(training)

gc()
fitRFNoEco <- randomForest(Lapse~INITIAL_SUM_INSURED + DURACTION +Prime + Indice.Boursier + Taux.Chomage + IndicePrix + ConfianceConso + Age + DureeEcoulee,data=training,ntree=100,nodesize = 25,cutoff=c(0.99,0.01))
pred.RFNoEco<-predict(fitRFNoEco,validation)
confusionMatrix(pred.RFNoEco, validation$Lapse)
#### Regression Logistique ####


# fitRL <- glm(training$Lapse ~.,data=training, family=binomial(link="logit"),y=F)
# pred.RL<-predict(fitRL,newdata=validation,type="response")
# confusionMatrix(data = as.numeric(pred.RL>0.5), reference = validation$Lapse)


#### Naive Bayes ####

# fitBayes <-naiveBayes(Lapse ~ INITIAL_SUM_INSURED + DURACTION + Primes.2015 + Age + DureeEcoulee + Taux.6.mois + Taux.12.mois, data = training)
# pred.Bayes<-predict(fitBayes,validation,type="class")
# confusionMatrix(pred.Bayes, validation$Lapse)


#### KNN ####

# help(knn)
# fitknn<-knn(Lapse~., data=training)

#### GBM ####
# fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)
# fit.GBM <- train(trainingX,trainingY, method = "gbm", trControl = fitControl,verbose = FALSE)
# pred.GBM<-predict(fit.GBM,validation,type="class")
# confusionMatrix(pred.GBM, validation$Lapse)
# 
# data$Lapse<-0
# data$Lapse[which(data$STATUS_AT_THE_CLOSING_DATE == "Lapse")]<-1
# sum(data$Lapse[which(year(data$STATUS_CHANGE_DATE)==2014)])
# SRG <- function (result,DateDebut,Duree){
#   rac<-rep(0,Duree)
#   for(i in 1:Duree){
#     if (year(as.yearmon(DateDebut)+i-1)==2015){
#       rac[i]<-sum(result$Lapse[which(year(result$STATUS_CHANGE_DATE)== year(as.yearmon(DateDebut)+i-1))])/((nrow(result[which(result$CONTRACT_EFFECTIVE_DATE<=as.Date(as.yearmon(DateDebut) +(i-1))  & result$STATUS_CHANGE_DATE>as.Date(as.yearmon(DateDebut) +(i-1))),])+nrow(result[which(result$CONTRACT_EFFECTIVE_DATE<=as.Date(as.yearmon(DateDebut) +(i))  & result$STATUS_CHANGE_DATE>=as.Date(as.yearmon(DateDebut) +(i))-1),]))/2)
#     }
#     else{
#       rac[i]<-sum(result$Lapse[which(year(result$STATUS_CHANGE_DATE)== year(as.yearmon(DateDebut)+i-1))])/((nrow(result[which(result$CONTRACT_EFFECTIVE_DATE<=as.Date(as.yearmon(DateDebut) +(i-1))  & result$STATUS_CHANGE_DATE>as.Date(as.yearmon(DateDebut) +(i-1))),])+nrow(result[which(result$CONTRACT_EFFECTIVE_DATE<=as.Date(as.yearmon(DateDebut) +(i))  & result$STATUS_CHANGE_DATE>as.Date(as.yearmon(DateDebut) +(i))),]))/2)
#     }
#   }
#   return(rac)
# }
# d<-as.Date("2011-01-01")
# rachatParAnnee<-SRG(data,d,5)
# 
# #Graphs descriptifs pour les variables
# par(mfrow=c(3,3))
# 
# #Repartition des raisons de changement de statut
# barplot(prop.table(table(data$STATUS_AT_THE_CLOSING_DATE)))
# 
# #Repartition dates de naissance
# barplot(table(data$INSURED_BIRTH_DATE),main="insured birthdate")
# #Beaucoup de gens néés au début de la période d'étude et à la fin. peut etre problématique ? 
# 
# #repartition des sommes assurées
# hist(training$INITIAL_SUM_INSURED,main="somme assurée")
# 
# #Duration
# hist(training$DURACTION,main="duration")
# 
# data$THEORETICAL_ENDING_DATE<-as.yearqtr(data$THEORETICAL_ENDING_DATE)
# 
# #repartition dates de fin de contrat
# 
# barplot(table(data$THEORETICAL_ENDING_DATE),main="Date fin de contrat")
# 
# #date changement de statut
# 
# barplot(table(data$STATUS_CHANGE_DATE),main="Date changement statut")
# 
# 
# plot(STATUS_CHANGE_DATE,main="Date changement statut",type='p')
# 
# 
# 
# data[which(data$INSURED_BIRTHDATE < data$Contract_effective_date),]
# data[which(data$Status_at_the_closing_date < data$Contract_effective_date),]
# 
# 
