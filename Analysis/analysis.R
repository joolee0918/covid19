## Analysis
#author: "Jooyoung Lee"
#date: "5/11/2020"

library(mgcv)
library(dlnm)
library(gamm4)
library(lme4)
library(splines)
library(Hmisc)
library(MuMIn)


state_predictors <- read.csv("/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/state_predictors.csv")
state_case <-  read.csv("/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/us_state_case_JHU.csv")
state_mobile <- read.csv("/Users/jooyounglee/Dropbox/2020_covid19/covid19/Data/us_state_mobile.csv")

mydata <- merge(state_case, state_predictors, by=c("state", "date"))
mydata$date <- as.Date(mydata$date)
mydata <- mydata[order(mydata$state, as.Date(mydata$date)), ]
mydata <- mydata[, !names(mydata) %in% c("X.x", "X.y")]

mydata$susceptabie <- (mydata$population - mydata$cases)/(mydata$population)
mydata$relative_test <- mydata$test/(mydata$population/1000)
mydata <- mydata[mydata$cases>0, ]

# Testing: going back days (1 to tt days)
tt <- 20
mylist <- split(mydata, mydata$state)
for(i in (1:length(mylist))){
  lagdat <- matrix(0, nrow=nrow(mylist[[i]]), ncol=tt)
  for(k in tt:1){
    days <- paste("days_ago", tt, sep="")
    dates_before = mylist[[i]]$date-k
    ind = match(dates_before,mylist[[i]]$date)
    lagdat[, k] <- mylist[[i]][ind,]$relative_test
  }
  mylist[[i]] <- cbind(mylist[[i]], lagdat)
}

mydata <- do.call(rbind, mylist)
colnames(mydata)[(ncol(mydata)-tt+1):ncol(mydata)] <- paste("days_ago", 1:tt, sep="")
mydata <- mydata[order(mydata$stateFIPS, as.Date(mydata$date)), ]





# PCA for the movement data

pca_move <- prcomp(mydata[, names(state_mobile)[-c(1:6)]], center=TRUE, scale.=TRUE)
mydata <- cbind(mydata, pca_move$x)

## set lag
lagk<-15

