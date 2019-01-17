
#######################################################Install libraries####################################################################

library(tidyverse)
library (leaps)


#######################################################Load and split dataset####################################################################

setwd("D:/UCLA Nov-Dec17/Term4/Fraud Analytics/Group project")

credit_Card<-read.csv("credit_Card_expertvar_merge3.csv")

# Subsetting out of time dataset post November,2010

oot_validation<- subset(credit_Card, Date>= as.Date("2010-11-01"))

credit_Card_split<- subset(credit_Card, Date< as.Date("2010-11-01"))


# Split dataset into train,test and out of time validation dataset
# seed ensures we get the same split everytime
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


#For further analysis using only Training dataset to make all deductions

df <- res$train



############################################# KS score calculation######################################################################
scores01 <- matrix(nrow = 181, ncol = 4)

for (i in 15:195){
  abc01 <- as.matrix(df[which(df[,11] == 0), i]) %>% as.numeric()
  abc02 <- as.matrix(df[-which(df[,11] == 0), i]) %>% as.numeric()
  ks01 <- ks.test(abc01, abc02)
  scores01[(i-14), 1] = names(df)[i]
  scores01[(i-14), 2] = i
  scores01[(i-14), 3] = ks01$statistic
}


#####################################################FDR @ 3%#############################################################################

percentage_used = as.integer(NROW(df) * 0.03)
total_fraud<- nrow(subset(df,Fraud==1))

for (i in 15:195){
  abc03 <- df[,c(11,i)]
  colnames(abc03) <- c("fraud", "temp01")
  abc04 <- arrange(abc03, temp01)
  sum01 <- sum(abc04[1:percentage_used, 1])
  abc05 <- arrange(abc03, desc(temp01))
  sum02 <- sum(abc05[1:percentage_used, 1])
  scores01[i-14,4] = max(sum01, sum02, na.rm = T)/total_fraud
}

colnames(scores01) <- c("Variable Name", "colnumber", "KS-Test", "FDR 3%")


# eliminating the variables based on KS and FDR scores

scores01 <- as.tibble(scores01)
scores01 <- arrange(scores01,desc(`KS-Test`))
scores02 <- scores01[1:(NROW(scores01)*0.7),]
scores03 <- arrange(scores02,desc(`FDR 3%`))
scores04 <- scores03[1:(NROW(scores03)*0.75),]

df01 <- as.tibble(df[,11])
colnames(df01)[1]<- "Fraud"
for (i in 1:94){
  df01 <- cbind(df01, df[,as.numeric(scores04[i,2])])
  colnames(df01)[i+1]<-as.character(scores04[i,1])
}

#### Variable Selection ####
# eliminating low KS score and FDR variables
for (i in 1:94){
  df01[is.infinite(df01[,i]),i] <-0
}
#forward selection
regfit.full = regsubsets (Fraud ~.,df01, nvmax =20, method ="forward")
regfit.full
# Extracting columns of the best model with 13 variables
bestvariables <-names(which(summary(regfit.full)$which[21,]))[-1]
bestvariables
plot(regfit.full,scale="adjr2")

