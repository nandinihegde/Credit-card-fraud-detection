
#######################################################Install libraries####################################################################

library(tidyverse)
#install.packages("datadr")
library(datadr)
#install.packages('randomForest')
library(randomForest)
# Data transaformation and manipulation
library(gbm)
library(MASS)
library(plyr)
library(nnet)

#######################################################Load and manipulate dataset####################################################################

setwd("D:/UCLA Nov-Dec17/Term4/Fraud Analytics/Group project")
credit_Card<-read.csv("credit_Card_expertvar_merge3.csv")

#Subset by required variables identified in the previous step 
credit_Card<- credit_Card[, append( c("Fraud","Date"),bestvariables)]

credit_Card$Date<- as.Date(credit_Card$Date, format="%Y-%m-%d")

#Sort by time variables
credit_Card<- credit_Card %>% arrange(Date)
credit_Card$Fraud<- as.factor(credit_Card$Fraud)
#Replace Inf values
credit_Card[mapply(is.infinite, credit_Card)] <- 0
credit_Card[mapply(is.na, credit_Card)] <- 0
str(credit_Card)

# Subsetting out of time dataset post November,2010

oot_validation<- subset(credit_Card, Date>= as.Date("2010-11-01"))

credit_Card_split<- subset(credit_Card, Date< as.Date("2010-11-01"))


# Split dataset into train,test and out of time validation dataset
set.seed(1234)

spec = c(train = .7, test = .3)

g = sample(cut(
  seq(nrow(credit_Card_split)), 
  nrow(credit_Card_split)*cumsum(c(0,spec)),
  labels = names(spec)
), replace = FALSE)

res = split(credit_Card_split, g)

sapply(res, nrow)/nrow(credit_Card_split)
addmargins(prop.table(table(g)))

head(res$train,10)

################################################model1: Logistic regression model###################################################################

model1 <- glm(Fraud ~ .-Date, data = res$train, family = "binomial")
summary(model1)

#Training set FDR
model1_Pred_train <- predict(model1, res$train, type = "response")
model1_Pred_train <- data.frame(Fraud_actual =res$train$Fraud , Fraud_pred_prob = model1_Pred_train)
model1_Pred_train<- model1_Pred_train %>% arrange(desc(Fraud_pred_prob))

model1_train_FDR_3per<- nrow(subset(model1_Pred_train[1:(nrow(model1_Pred_train)*0.03),],Fraud_actual==1))/nrow(subset(model1_Pred_train,Fraud_actual==1))

model1_train_FDR_3per 

# Test set FDR
model1_Pred_test <- predict(model1, res$test, type = "response")
model1_Pred_test <- data.frame(Fraud_actual =res$test$Fraud , Fraud_pred_prob = model1_Pred_test)
model1_Pred_test<- model1_Pred_test %>% arrange(desc(Fraud_pred_prob))

model1_test_FDR_3per<- nrow(subset(model1_Pred_test[1:(nrow(model1_Pred_test)*0.03),],Fraud_actual==1))/nrow(subset(model1_Pred_test,Fraud_actual==1))

model1_test_FDR_3per

# OOT set FDR
model1_Pred_oot <- predict(model1, oot_validation, type = "response")
model1_Pred_oot <- data.frame(Fraud_actual =oot_validation$Fraud , Fraud_pred_prob = model1_Pred_oot)
model1_Pred_oot<- model1_Pred_oot %>% arrange(desc(Fraud_pred_prob))

model1_oot_FDR_3per<- nrow(subset(model1_Pred_oot[1:(nrow(model1_Pred_oot)*0.03),],Fraud_actual==1))/nrow(subset(model1_Pred_oot,Fraud_actual==1))

model1_oot_FDR_3per




################################################model2: Random forest model#########################################################################

model2 <- randomForest(Fraud ~ .-Date,data=res$train, 
                    importance=TRUE, 
                    ntree=50)
varImpPlot(model2)

model2

#Training set FDR
model2_Pred_train <- predict(model2, res$train, type = "prob")[,2]
model2_Pred_train <- data.frame(Fraud_actual =res$train$Fraud , Fraud_pred_prob = model2_Pred_train)
model2_Pred_train<- model2_Pred_train %>% arrange(desc(Fraud_pred_prob))

model2_train_FDR_3per<- nrow(subset(model2_Pred_train[1:(nrow(model2_Pred_train)*0.03),],Fraud_actual==1))/nrow(subset(model2_Pred_train,Fraud_actual==1))

model2_train_FDR_3per

# Test set FDR
model2_Pred_test <- predict(model2, res$test, type = "prob")[,2]
model2_Pred_test <- data.frame(Fraud_actual =res$test$Fraud , Fraud_pred_prob = model2_Pred_test)
model2_Pred_test<- model2_Pred_test %>% arrange(desc(Fraud_pred_prob))

model2_test_FDR_3per<- nrow(subset(model2_Pred_test[1:(nrow(model2_Pred_test)*0.03),],Fraud_actual==1))/nrow(subset(model2_Pred_test,Fraud_actual==1))

model2_test_FDR_3per

# OOT set FDR
model2_Pred_oot <- predict(model2, oot_validation, type = "prob")[,2]
model2_Pred_oot <- data.frame(Fraud_actual =oot_validation$Fraud , Fraud_pred_prob = model2_Pred_oot)
model2_Pred_oot<- model2_Pred_oot %>% arrange(desc(Fraud_pred_prob))

model2_oot_FDR_3per<- nrow(subset(model2_Pred_oot[1:(nrow(model2_Pred_oot)*0.03),],Fraud_actual==1))/nrow(subset(model2_Pred_oot,Fraud_actual==1))

model2_oot_FDR_3per





################################################model3: Gradient Boosting tree model#########################################################################

#Gradient boosting tree cannot take factor as Y variable
res$train$Fraud <- as.character(res$train$Fraud)
res$test$Fraud <- as.character(res$test$Fraud)
oot_validation$Fraud<- as.character(oot_validation$Fraud)

model3=gbm(Fraud ~ .-Date ,data = res$train,distribution = "bernoulli",n.trees = 1000,
                 shrinkage = 0.01, interaction.depth = 4 , verbose= TRUE, train.fraction = 0.5)
model3

#Training set FDR
model3_Pred_train <- predict.gbm(object = model3,
                                         newdata = res$train,
                                         n.trees = 1000,
                                         type = "response")
model3_Pred_train <- data.frame(Fraud_actual =res$train$Fraud , Fraud_pred_prob = model3_Pred_train)
model3_Pred_train<- model3_Pred_train %>% arrange(desc(Fraud_pred_prob))

model3_train_FDR_3per<- nrow(subset(model3_Pred_train[1:(nrow(model3_Pred_train)*0.03),],Fraud_actual==1))/nrow(subset(model3_Pred_train,Fraud_actual==1))

model3_train_FDR_3per

#Test set FDR
model3_Pred_test <- predict.gbm(object = model3,
                                 newdata = res$test,
                                 n.trees = 1000,
                                 type = "response")
model3_Pred_test <- data.frame(Fraud_actual =res$test$Fraud , Fraud_pred_prob = model3_Pred_test)
model3_Pred_test<- model3_Pred_test %>% arrange(desc(Fraud_pred_prob))

model3_test_FDR_3per<- nrow(subset(model3_Pred_test[1:(nrow(model3_Pred_test)*0.03),],Fraud_actual==1))/nrow(subset(model3_Pred_test,Fraud_actual==1))

model3_test_FDR_3per

#OOT set FDR
model3_Pred_oot <- predict.gbm(object = model3,
                                newdata = oot_validation,
                                n.trees = 1000,
                                type = "response")
model3_Pred_oot <- data.frame(Fraud_actual =oot_validation$Fraud , Fraud_pred_prob = model3_Pred_oot)
model3_Pred_oot<- model3_Pred_oot %>% arrange(desc(Fraud_pred_prob))

model3_oot_FDR_3per<- nrow(subset(model3_Pred_oot[1:(nrow(model3_Pred_oot)*0.03),],Fraud_actual==1))/nrow(subset(model3_Pred_oot,Fraud_actual==1))

model3_oot_FDR_3per





################################################model4: Neural Network model#########################################################################

res$train$Fraud <- as.factor(res$train$Fraud)
res$test$Fraud <- as.factor(res$test$Fraud)
oot_validation$Fraud<- as.factor(oot_validation$Fraud)

ideal <- class.ind(res$train$Fraud )
set.seed(1234)
model4 = nnet(x= res$train[,-c(1,2)],ideal, size=15,  softmax = TRUE)

#Training set FDR
model4_Pred_train <-predict(model4, res$train[,-c(1,2)], type="raw")[,2]
model4_Pred_train <- data.frame(Fraud_actual =res$train$Fraud , Fraud_pred_prob = model4_Pred_train)
model4_Pred_train<- model4_Pred_train %>% arrange(desc(Fraud_pred_prob))

model4_train_FDR_3per<- nrow(subset(model4_Pred_train[1:(nrow(model4_Pred_train)*0.03),],Fraud_actual==1))/nrow(subset(model4_Pred_train,Fraud_actual==1))

model4_train_FDR_3per

#Test set FDR
model4_Pred_test <-predict(model4, res$test[,-c(1,2)], type="raw")[,2]
model4_Pred_test <- data.frame(Fraud_actual =res$test$Fraud , Fraud_pred_prob = model4_Pred_test)
model4_Pred_test<- model4_Pred_test %>% arrange(desc(Fraud_pred_prob))

model4_test_FDR_3per<- nrow(subset(model4_Pred_test[1:(nrow(model4_Pred_test)*0.03),],Fraud_actual==1))/nrow(subset(model4_Pred_test,Fraud_actual==1))

model4_test_FDR_3per


#OOT set FDR
model4_Pred_oot <-predict(model4, oot_validation[,-c(1,2)], type="raw")[,2]
model4_Pred_oot <- data.frame(Fraud_actual =oot_validation$Fraud , Fraud_pred_prob = model4_Pred_oot)
model4_Pred_oot<- model4_Pred_oot %>% arrange(desc(Fraud_pred_prob))

model4_oot_FDR_3per<- nrow(subset(model4_Pred_oot[1:(nrow(model4_Pred_oot)*0.03),],Fraud_actual==1))/nrow(subset(model4_Pred_oot,Fraud_actual==1))

model4_oot_FDR_3per


############################################ Population Bin wise report for Gradient Boosting tree- final model#######################################

###############Training dataset#############################################
nrow(res$train)
table(res$train$Fraud)

# Binning Train dataset
popln_bin_train<-split(model3_Pred_train, rep(1:ceiling(nrow(model3_Pred_train)/100), each=ceiling(nrow(model3_Pred_train)/100), length.out=nrow(model3_Pred_train)))

stats_popln_train<- data.frame(matrix(nrow = 100, ncol = 0))
#names(stats_popln_train)<- c("Population Bin %",	"# Records",	"# Goods","	# Bads","	% Goods",	"% Bads")
stats_popln_train$`Population Bin %`<- seq(1,100,by= 1)

total_fraud_train<- nrow(subset(model3_Pred_train, model3_Pred_train$Fraud_actual==1))
total_notfraud_train<- nrow(subset(model3_Pred_train, model3_Pred_train$Fraud_actual==0))

for(i in 1:100)
{
  stats_popln_train[i,"# Records"]<- nrow(popln_bin_train[[i]][1])
  
  stats_popln_train[i,"# Goods"]<- nrow(subset(popln_bin_train[[i]][1],Fraud_actual==0))
  
  stats_popln_train[i,"	# Bads"]<- nrow(subset(popln_bin_train[[i]][1],Fraud_actual==1))
  
  stats_popln_train[i,"	% Good"]<- (stats_popln_train[i,"# Goods"]*100)/stats_popln_train[i,"# Records"]
  
  stats_popln_train[i,"% Bads"]<- (stats_popln_train[i,"	# Bads"]*100)/stats_popln_train[i,"# Records"]
  
  stats_popln_train[i," Total # Records"]<- sum(stats_popln_train[1:i,"# Records"])
  
  stats_popln_train[i,"Cumulative # Goods"]<- sum(stats_popln_train[1:i,"# Goods"])
  
  stats_popln_train[i,"Cumulative	# Bads"]<- sum(stats_popln_train[1:i,"	# Bads"])
  
  stats_popln_train[i,"Cumulative	% Good"]<-stats_popln_train[i,"Cumulative # Goods"]*100/total_notfraud_train
  stats_popln_train[i,"Cumulative % Bads (FDR)"]<- stats_popln_train[i,"Cumulative	# Bads"]*100/total_fraud_train
  
  #nfr_mat <- as.matrix(subset(model3_Pred_train[1:stats_popln_train[i," Total # Records"],],Fraud_actual==0)[2]) %>% as.numeric()
  #fr_mat <- as.matrix(subset(model3_Pred_train[1:stats_popln_train[i," Total # Records"],],Fraud_actual==1)[2]) %>% as.numeric()
  #ks01 <-  ks.test(nfr_mat, fr_mat)
  
  stats_popln_train[i,"KS"] <- stats_popln_train[i,"Cumulative % Bads (FDR)"]-stats_popln_train[i,"Cumulative	% Good"]
  
  stats_popln_train[i,"FPR"]<- stats_popln_train[i,"Cumulative # Goods"]/stats_popln_train[i,"Cumulative	# Bads"]
  
  
}

write.csv(stats_popln_train,"stats_popln_train.csv")


###############Test dataset#############################################
nrow(res$test)
table(res$test$Fraud)

# Binning test dataset
popln_bin_test<-split(model3_Pred_test, rep(1:ceiling(nrow(model3_Pred_test)/100), each=ceiling(nrow(model3_Pred_test)/100), length.out=nrow(model3_Pred_test)))

stats_popln_test<- data.frame(matrix(nrow = 100, ncol = 0))
#names(stats_popln_test)<- c("Population Bin %",	"# Records",	"# Goods","	# Bads","	% Goods",	"% Bads")
stats_popln_test$`Population Bin %`<- seq(1,100,by= 1)

total_fraud_test<- nrow(subset(model3_Pred_test, model3_Pred_test$Fraud_actual==1))
total_notfraud_test<- nrow(subset(model3_Pred_test, model3_Pred_test$Fraud_actual==0))

for(i in 1:100)
{
  stats_popln_test[i,"# Records"]<- nrow(popln_bin_test[[i]][1])
  
  stats_popln_test[i,"# Goods"]<- nrow(subset(popln_bin_test[[i]][1],Fraud_actual==0))
  
  stats_popln_test[i,"	# Bads"]<- nrow(subset(popln_bin_test[[i]][1],Fraud_actual==1))
  
  stats_popln_test[i,"	% Good"]<- (stats_popln_test[i,"# Goods"]*100)/stats_popln_test[i,"# Records"]
  
  stats_popln_test[i,"% Bads"]<- (stats_popln_test[i,"	# Bads"]*100)/stats_popln_test[i,"# Records"]
  
  stats_popln_test[i," Total # Records"]<- sum(stats_popln_test[1:i,"# Records"])
  
  stats_popln_test[i,"Cumulative # Goods"]<- sum(stats_popln_test[1:i,"# Goods"])
  
  stats_popln_test[i,"Cumulative	# Bads"]<- sum(stats_popln_test[1:i,"	# Bads"])
  
  stats_popln_test[i,"Cumulative	% Good"]<-stats_popln_test[i,"Cumulative # Goods"]*100/total_notfraud_test
  stats_popln_test[i,"Cumulative % Bads (FDR)"]<- stats_popln_test[i,"Cumulative	# Bads"]*100/total_fraud_test
  
  #nfr_mat <- as.matrix(subset(model3_Pred_test[1:stats_popln_test[i," Total # Records"],],Fraud_actual==0)[2]) %>% as.numeric()
  #fr_mat <- as.matrix(subset(model3_Pred_test[1:stats_popln_test[i," Total # Records"],],Fraud_actual==1)[2]) %>% as.numeric()
  #ks01 <-  ks.test(nfr_mat, fr_mat)
  
  stats_popln_test[i,"KS"] <- stats_popln_test[i,"Cumulative % Bads (FDR)"]-stats_popln_test[i,"Cumulative	% Good"]
  
  stats_popln_test[i,"FPR"]<- stats_popln_test[i,"Cumulative # Goods"]/stats_popln_test[i,"Cumulative	# Bads"]
  
  
}

write.csv(stats_popln_test,"stats_popln_test.csv")


###############OTT dataset#############################################
nrow(oot_validation)
table(oot_validation$Fraud)

# Binning ott dataset
popln_bin_oot<-split(model3_Pred_oot, rep(1:ceiling(nrow(model3_Pred_oot)/100), each=ceiling(nrow(model3_Pred_oot)/100), length.out=nrow(model3_Pred_oot)))

stats_popln_oot<- data.frame(matrix(nrow = 100, ncol = 0))
#names(stats_popln_oot)<- c("Population Bin %",	"# Records",	"# Goods","	# Bads","	% Goods",	"% Bads")
stats_popln_oot$`Population Bin %`<- seq(1,100,by= 1)

total_fraud_oot<- nrow(subset(model3_Pred_oot, model3_Pred_oot$Fraud_actual==1))
total_notfraud_oot<- nrow(subset(model3_Pred_oot, model3_Pred_oot$Fraud_actual==0))

for(i in 1:100)
{
  stats_popln_oot[i,"# Records"]<- nrow(popln_bin_oot[[i]][1])
  
  stats_popln_oot[i,"# Goods"]<- nrow(subset(popln_bin_oot[[i]][1],Fraud_actual==0))
  
  stats_popln_oot[i,"	# Bads"]<- nrow(subset(popln_bin_oot[[i]][1],Fraud_actual==1))
  
  stats_popln_oot[i,"	% Good"]<- (stats_popln_oot[i,"# Goods"]*100)/stats_popln_oot[i,"# Records"]
  
  stats_popln_oot[i,"% Bads"]<- (stats_popln_oot[i,"	# Bads"]*100)/stats_popln_oot[i,"# Records"]
  
  stats_popln_oot[i," Total # Records"]<- sum(stats_popln_oot[1:i,"# Records"])
  
  stats_popln_oot[i,"Cumulative # Goods"]<- sum(stats_popln_oot[1:i,"# Goods"])
  
  stats_popln_oot[i,"Cumulative	# Bads"]<- sum(stats_popln_oot[1:i,"	# Bads"])
  
  stats_popln_oot[i,"Cumulative	% Good"]<-stats_popln_oot[i,"Cumulative # Goods"]*100/total_notfraud_oot
  stats_popln_oot[i,"Cumulative % Bads (FDR)"]<- stats_popln_oot[i,"Cumulative	# Bads"]*100/total_fraud_oot
  
  #nfr_mat <- as.matrix(subset(model3_Pred_oot[1:stats_popln_oot[i," Total # Records"],],Fraud_actual==0)[2]) %>% as.numeric()
  #fr_mat <- as.matrix(subset(model3_Pred_oot[1:stats_popln_oot[i," Total # Records"],],Fraud_actual==1)[2]) %>% as.numeric()
  #ks01 <-  ks.oot(nfr_mat, fr_mat)
  
  stats_popln_oot[i,"KS"] <- stats_popln_oot[i,"Cumulative % Bads (FDR)"]-stats_popln_oot[i,"Cumulative	% Good"]
  
  stats_popln_oot[i,"FPR"]<- stats_popln_oot[i,"Cumulative # Goods"]/stats_popln_oot[i,"Cumulative	# Bads"]
  
  
}

write.csv(stats_popln_oot,"stats_popln_oot.csv")
