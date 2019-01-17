
#######################################################Install libraries and load dataset####################################################################
library(tidyverse)
#install.packages("datadr")
library(datadr)

# Data transaformation and manipulation

setwd("D:/UCLA Nov-Dec17/Term4/Fraud Analytics/Group project")
credit_Card<-read.csv("card transactions.csv")
credit_Card[,c(1,11:18)]<-NULL

credit_Card$Date<- as.Date(credit_Card$Date, format="%m/%d/%Y")
credit_Card$card_merch<- paste(credit_Card$Cardnum,"-",credit_Card$Merchnum)
credit_Card$card_zip<- paste(credit_Card$Cardnum,"-",credit_Card$Merch.zip)
credit_Card$card_state<- paste(credit_Card$Cardnum,"-",credit_Card$Merch.state)


credit_Card$abs_amt <- as.numeric(gsub('[$,]', '', credit_Card$Amount))


#######################################################Amount expert variables####################################################################

rollingfunction<- function(colname,Date,abs_amt,funcs,window)
{
 # print(colname)
  calcvalue<-c()
  for(i in Date)
  {
    startdate <- i-window
    enddate <- i-1
    interval <- seq(startdate,enddate,1)
    tmp <- abs_amt[Date %in% interval]
    #print(tmp)
    calc<-get(funcs)(tmp)
    #calc<-ifelse(is.na(calc),0,calc)
    calcvalue<- append(calcvalue,calc)
  #  print( calcvalue)
  }
  return(calcvalue) 
  
  
}

#stat<-c("avg","max","med","total","act-avg","act-med","act_by_avg","act_by_max","act_by_total","act_by_med") 
stat<-c("avg","max","med","total")
funcs<- c("mean","max","median","sum")
#funcs<- c("sum")
#vars<-c("Cardnum","Merchnum","card_merch","card_zip","card_state")
vars<-c("Merchnum")
window<- c(1,7,14,30)
#window<- c(1,7)


credit_Card_expertvar<-credit_Card
#credit_Card_expertvar<- credit_Card_expertvar %>% arrange(credit_Card_expertvar$Cardnum,desc(Date))
#credit_Card_expertvar<-credit_Card_expertvar[1:150,]
credit_Card_expertvar$colname<- as.character(credit_Card_expertvar$Merchnum)


for( j in 1:length(vars))
{
  print("top loop")
  credit_Card_expertvar<- credit_Card_expertvar %>% arrange(credit_Card_expertvar[,vars[j]],desc(Date))
  credit_Card_expertvar$colname<- credit_Card_expertvar[,vars[j]]
  print(nrow(credit_Card_expertvar))
for( i in 1:length(stat))
{
  
    
    for( k in 1:length(window))
    {
     varname<- paste(stat[i],"_amt_by_",vars[j],"_over",window[k],"days", sep = "")
     print(varname)
     
     credit_Card_expertvar<-credit_Card_expertvar %>% 
                         group_by(credit_Card_expertvar$colname) %>%
                        mutate(t1= rollingfunction(colname,Date,abs_amt,funcs[i],window[k])) %>%
                       ungroup()
     
     credit_Card_expertvar[,varname]<- credit_Card_expertvar$t1
     credit_Card_expertvar$t1<-NULL
     print("last loop")
      print(nrow(credit_Card_expertvar))
      print(ncol(credit_Card_expertvar))
    
    } 
    
  }
 # credit_Card_expertvar$colname<-NULL
}


# writng and merging csv

write.csv(credit_Card_expertvar,"credit_Card_expertvar_Merchnum_all.csv")
cname<- names(credit_Card_expertvar1)[1:13]
cname
credit_Card_expertvar1<-read.csv("credit_Card_expertvar_Cardnum_all.csv")
credit_Card_expertvar1[,c(1,15:16)]<-NULL
credit_Card_expertvar2<-read.csv("credit_Card_expertvar_Merchnum_all.csv")
credit_Card_expertvar2[,14:15]<-NULL
credit_Card_expertvar3<-read.csv("credit_Card_expertvar_Card_state_all.csv")
credit_Card_expertvar3[,14:15]<-NULL
credit_Card_expertvar4<-read.csv("credit_Card_expertvar_Card_zip_all.csv")
credit_Card_expertvar4[,14:15]<-NULL
credit_Card_expertvar_merge1<-read.csv("credit_Card_expertvar_merge1.csv")

credit_Card_expertvar1<- credit_Card_expertvar1 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)
credit_Card_expertvar1$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 

credit_Card_expertvar2<- credit_Card_expertvar2 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar2$X<- seq(1,nrow(credit_Card_expertvar2), by=1) 

credit_Card_expertvar3<- credit_Card_expertvar3 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar3$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 
credit_Card_expertvar4<- credit_Card_expertvar4 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar4$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 



credit_Card_expertvar_merge1<- merge(credit_Card_expertvar_merge1,credit_Card_expertvar4, by= c("X","Cardnum","Date","Merchnum","Merch.description",
                                                                                          "Merch.state","Merch.zip",
                                                                                          "Transtype","Amount","Fraud",
                                                                                          "card_merch","card_zip", "card_state" ,"abs_amt"), all.x = TRUE)




write.csv(credit_Card_expertvar_merge1,"credit_Card_expertvar_merge1.csv")

# read merged file for processing

merge1<-read.csv("credit_Card_expertvar_merge1.csv")
merge1[,1:2]<-NULL
merge1[, 14:77][is.na(merge1[, 14:77])] <- 0
merge1[mapply(is.infinite, merge1)] <- 0


#create the subtract and div features

addtional_stats<-c("act-avg","act-med","act_by_avg","act_by_max","act_by_total","act_by_med") 
subtr<- names(merge1)[grepl("avg", names(merge1), fixed = FALSE)]
subtr<- append(subtr,names(merge1)[grepl("med", names(merge1), fixed = FALSE)])
div<- names(merge1)[grepl("avg", names(merge1), fixed = FALSE)]
div<- append(div,names(merge1)[grepl("max", names(merge1), fixed = FALSE)])
div<- append(div,names(merge1)[grepl("total", names(merge1), fixed = FALSE)])
div<- append(div,names(merge1)[grepl("med", names(merge1), fixed = FALSE)])


for( i in 1:length(subtr))
{
  varname<- paste("actual-",subtr[i],sep = "")
  print(varname)
  merge1[,varname]<- merge1$abs_amt- merge1[,subtr[i]]
  
}

for( i in 1:length(div))
{
  varname<- paste("actual_div",div[i],sep = "")
  print(varname)
  merge1[,varname]<- merge1$abs_amt/ merge1[,div[i]]
  
}

write.csv(merge1,"credit_Card_expertvar_merge2.csv")

#######################################################Frequency expert variables####################################################################

rollingfreqfunction<- function(Dates,window,abs_amt)
{
  
  calcvalue<-c()
  for(i in Dates)
  {
    startdate <- i-window
    
    enddate <- i-1
   
    interval <- seq(startdate,enddate,1)
    tmp <- abs_amt[Dates %in% interval]
    
    calc<-length(tmp)
    #calc<-ifelse(is.na(calc),0,calc)
    calcvalue<- append(calcvalue,calc)
    #  print( calcvalue)
  }
  return(calcvalue) 
  
  
}


stat<-c("freq")

#vars<-c("Cardnum","Merchnum","card_merch","card_zip","card_state")
vars<-c("card_state")
window<- c(1,7,14,30)



credit_Card_expertvar<-credit_Card
credit_Card_expertvar<- credit_Card_expertvar %>% arrange(credit_Card_expertvar$Cardnum,desc(Date))
#credit_Card_expertvar<-credit_Card_expertvar[1:25,]
credit_Card_expertvar$colname<- as.character(credit_Card_expertvar$Cardnum)

for( j in 1:length(vars))
{
  print("top loop")

  credit_Card_expertvar$colname<- as.character(credit_Card_expertvar[,vars[j]])
  credit_Card_expertvar<- credit_Card_expertvar %>% arrange(colname,desc(Date))
  print(nrow(credit_Card_expertvar))
 
    for( k in 1:length(window))
    {
      varname<- paste(stat[1],"_by",vars[j],"_over",window[k],"days", sep = "")
      print(varname)
      
      credit_Card_expertvar<-credit_Card_expertvar %>% 
        group_by(credit_Card_expertvar$colname) %>%
        mutate(t1= rollingfreqfunction(Date,window[k],abs_amt)) %>%
        ungroup()
      
      credit_Card_expertvar[,varname]<- credit_Card_expertvar$t1
      credit_Card_expertvar$t1<-NULL
      print("last loop")
      print(ncol(credit_Card_expertvar))
      
    } 
  
  
    
}

credit_Card_expertvar[,14:15]<-NULL
write.csv(credit_Card_expertvar,"credit_Card_expertvar_merge3.5.csv")





# CREATE Merge3
credit_Card_expertvar1<-read.csv("credit_Card_expertvar_merge3.1.csv")
credit_Card_expertvar1[,1]<-NULL
credit_Card_expertvar2<-read.csv("credit_Card_expertvar_merge3.2.csv")
credit_Card_expertvar2[,1]<-NULL
credit_Card_expertvar3<-read.csv("credit_Card_expertvar_merge3.3.csv")
credit_Card_expertvar3[,1]<-NULL
credit_Card_expertvar4<-read.csv("credit_Card_expertvar_merge3.4.csv")
credit_Card_expertvar4[,1]<-NULL
credit_Card_expertvar5<-read.csv("credit_Card_expertvar_merge3.5.csv")
credit_Card_expertvar5[,1]<-NULL

credit_Card_expertvar1<- credit_Card_expertvar1 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)
credit_Card_expertvar1$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 

credit_Card_expertvar2<- credit_Card_expertvar2 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar2$X<- seq(1,nrow(credit_Card_expertvar2), by=1) 

credit_Card_expertvar3<- credit_Card_expertvar3 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar3$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 
credit_Card_expertvar4<- credit_Card_expertvar4 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar4$X<- seq(1,nrow(credit_Card_expertvar1), by=1) 
credit_Card_expertvar5<- credit_Card_expertvar5 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar5$X<- seq(1,nrow(credit_Card_expertvar1), by=1)


credit_Card_expertvar_merge3<- merge(credit_Card_expertvar_merge3,credit_Card_expertvar5, by= c("X","Cardnum","Date","Merchnum","Merch.description",
                                                                                                "Merch.state","Merch.zip",
                                                                                                "Transtype","Amount","Fraud",
                                                                                                "card_merch","card_zip", "card_state" ,"abs_amt"), all.x = TRUE)
credit_Card_expertvar_merge2<-read.csv("credit_Card_expertvar_merge2.csv")




credit_Card_expertvar_merge2<- credit_Card_expertvar_merge2 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar_merge2$X<- seq(1,nrow(credit_Card_expertvar_merge2), by=1) 
credit_Card_expertvar_merge3<- credit_Card_expertvar_merge3 %>% arrange(Cardnum,Date,Merchnum,Merch.description,Merch.state,Merch.zip,
                                                            Transtype,Amount,Fraud,card_merch,card_zip,card_state,abs_amt)    
credit_Card_expertvar_merge3$X<- seq(1,nrow(credit_Card_expertvar_merge3), by=1)


credit_Card_expertvar_merge3<- merge(credit_Card_expertvar_merge2,credit_Card_expertvar_merge3, by= c("X","Cardnum","Date","Merchnum","Merch.description",
                                                                                                "Merch.state","Merch.zip",
                                                                                                "Transtype","Amount","Fraud",
                                                                                                "card_merch","card_zip", "card_state" ,"abs_amt"), all.x = TRUE)

write.csv(credit_Card_expertvar_merge3,"credit_Card_expertvar_merge3.csv")


