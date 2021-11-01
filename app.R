#Let's make a dashboard

#Setup
cat("\f")
rm(list = ls())
options(warn=-1)

#Installing required Package
    
plist <- c("parallel", "foreach", "doParallel", "doSNOW", "doRNG", "tictoc", 
           "readxl", "xts", "zoo", "lubridate", "tidyverse", "ggplot2", "dplyr",
           "strucchange", "tseries", "fUnitRoots", "timeSeries", "exuber", 
           "urca", "vars", "rmgarch", "FinTS", "shiny", "forecast", "ftsa", 
           "tsDyn", "rstudioapi","PerformanceAnalytics", "gdata")
npackage <- plist[!(plist %in% installed.packages()[,"Package"])]
if(length(npackage)>0) {install.packages(npackage)}
    
env <- c("plist", "npackage", "env")
remove(list = c(env))
    
    
#Package - Parallel Computing
library(parallel)
library(foreach)
library(doParallel)

    
#Packages - Data import and Manipulation
library(readxl)#To import data set
library(xts) #To create Time series Data Frames
library(zoo) #To create Time series Data Frames
library(lubridate)#Manipulating Dates
library(tidyverse) 
library(gdata)
    
#Package - Plotting
library(ggplot2)#To create Plots
library(dplyr)#To create overlapping plots
    
#Package - Identifying Structural Breaks
library(strucchange) # To Identify Structural Breaks
    
#Package - Unit Root Test
library(tseries)
library(timeSeries)
library(urca)
library(fUnitRoots) #CorrelationTest
    
#Package - VAR and VECM Models
library(vars)

#Package - Performance Evaluation
library(PerformanceAnalytics)
    
#Package - Dashboard
library(shiny)
    
#Package Miscellaneous 
library(tsDyn)
library(rstudioapi)
    
     
#Custom Setup and function
cores<- detectCores()
`%ni%` <- Negate(`%in%`)
    
#Setup - Working Directory
setwd(dirname(getActiveDocumentContext()$path)) #Sets it to file location

    
    
#--------------DATA IMPORT AND INITIAL MANIPULATION-----------------------------    
        
#Import the data
numeric <- rep("numeric", 505)
SData <- read_excel("Cointegration with S&P.xlsx", 
                     col_types = c("date", numeric))
        
#Convert to weekly series
Dates <- SData$Dates
SData$day <- weekdays(SData$Dates)
Monday <- "Monday"
SPweekly <- subset(SData, day %in% Monday)
SPweekly <- SPweekly[,-507] #remove the days
    
#Reformatting Dates
Dates <- SPweekly$Dates

#Number of observation for each Stock
nobs <- matrix(rep(0), 505, 1)
nobs <- as.data.frame(nobs)
for (i in 2:ncol(SPweekly)) {
    nobs[(i-1),1] <- nrow(na.omit(SPweekly[,(i)]))
}

rownames(nobs) <- colnames(SPweekly[-1])
        
#Stocks with less than 156 Observations
remove <- subset(nobs, nobs<156)
rstock <- rownames(remove)
SPweekly <- subset(SPweekly, select = colnames(SPweekly) %ni% rstock)
    
#Stock Names
SSym <- colnames(SPweekly)[-1]
SSym <- substr(SSym, 1, nchar(SSym)-10)
colnames(SPweekly)[-1] <- SSym
SPweekly <- SPweekly[,-1]
Sym <- "GOOG" #Initializing
PSym <- "FTNT" #Initializing

#S&P
SnP <- read_excel("S&P.xlsx")
SnPDates <- SnP$Dates
SnP$day <- weekdays(SnP$Dates)
Monday <- "Monday"
SnPweekly <- subset(SnP, day %in% Monday)
SnPweekly <- SnPweekly[,-3] #remove the days
        
env <- c("numeric", "Monday", "SData","remove", "i","rstock","nobs", "env")
remove(list=c(env))

#--------------IMPORT TRUE VALUES FOR TESTING-----------------------------------

True <- read_excel("True Value.xlsx")

True$day <- weekdays(True$...1)
Monday <- "Monday"
True <- subset(True, day %in% Monday)
True <- True[,-508] #remove the days

True$Date <- as.Date(True$...1)

TDates <- True$Date
TDates <- as.POSIXct(TDates,format='%Y/%m/%d', tz = "UCT")
TDates <- format(as.POSIXct(TDates,format='%m/%d/%Y %H:%M:%S'),format='%Y-%m-%d')

True <- True[,(colnames(True) %ni% "Date")]
True <- True[,(colnames(True) %ni% "...1")]
rownames(True) <- TDates

Tsname <- colnames(True)
Tsname <- substr(Tsname, 1, nchar(Tsname)-10)

colnames(True) <- Tsname
colnames(True)[506] <- "SPX"

env <- c("Monday", "env")
remove(list=c(env))


#-------------------------------------------------------------------------------
        
#-------------------------------------------------------------------------------    
#--------------------GLOBAL ENVIRONMENT-----------------------------------------
#-------------------------------------------------------------------------------
    
#---------------STRUCTURAL BREAKS-----------------------------------------------    
    
CPU <- makeCluster(cores[1]-2)
registerDoParallel(CPU)
Breakpoints <- foreach (i = 1:ncol(SPweekly),.packages = c("strucchange"),.combine = rbind) %dopar% {
ts <- ts(SPweekly[,(i)], start = c(2000, 1), frequency=52)
bp.ts <- breakpoints(ts~1, h=.15)
bp.ts$breakpoints
}
stopCluster(CPU)
        
Breakpoints <- as.data.frame(Breakpoints)
colnames(Breakpoints) <- c("BP1", "BP2", "BP3", "BP4", "BP5")
rownames(Breakpoints) <- colnames(SPweekly)
    
first<-matrix(rep(0), ncol(SPweekly),1 )
first <- as.data.frame(first)
colnames(first) <- "First Observation"
    
for (i in 1:ncol(SPweekly)){
    first[i,1] <- min(which(!is.na((SPweekly)[,i])))
}
    
for (i in 1:ncol(SPweekly)) {
    fvalue <- first[i,1]
    for (a in 1:ncol(Breakpoints)){
        Breakpoints[i,a] <- Breakpoints[i,a]+fvalue
    }
}
        
for (i in 1:length(SSym)) {
    Breakpoints[i,6] <- length(unique(c(Breakpoints[i,-6]))) 
}
    
colnames(Breakpoints)[6] <- "Number of Breakpoints"
        
Breakpoints <- Breakpoints %>% relocate(`Number of Breakpoints`, .before = BP1)
        
for (i in 1:length(SSym)){
    a <- Breakpoints$`Number of Breakpoints`[i]
    for (b in 1:6)
    if((b-1)>a){
    Breakpoints[i,b] = ""
    }
}
    
Breakpoints$BP1 <- as.numeric(Breakpoints$BP1)
Breakpoints$BP2 <- as.numeric(Breakpoints$BP2)
Breakpoints$BP3 <- as.numeric(Breakpoints$BP3)
Breakpoints$BP4 <- as.numeric(Breakpoints$BP4)
Breakpoints$BP5 <- as.numeric(Breakpoints$BP5)
    
env <- c("first", "a", "b", "fvalue", "i", "env")
remove(list = c(env))

#---------------ADF TEST--------------------------------------------------------
    
adf <- matrix(0, ncol(SPweekly), 3)
adf <- as.data.frame(adf)
colnames(adf) <- c("Stationary", "Unit Root", "Explosive")
rownames(adf) <- colnames(SPweekly)
    
Tsdata <- xts(SPweekly, order.by = Dates)
    
for(i in 1:ncol(Tsdata)) {
    S <- colnames(Tsdata)[i]
    bp <- Breakpoints[S,]
    n <- bp$`Number of Breakpoints`
    bp <- bp[,(n+1)]
    bp <- as.numeric(bp)
    bp <- bp+12 #include cooling period
    adfsta <- adf.test(na.omit(Tsdata[((bp):nrow(Tsdata)),i]))
    adf[i,1] <- adfsta$p.value
    adf[i,1] <- ifelse(adf[i,1]<.05, "Yes", "No")
    adfexp <- adf.test(na.omit(Tsdata[,i]), alternative = c("explosive"))
    adf[i,3] <- adfexp$p.value
    adf[i,3] <- ifelse(adf[i,3]<.05, "Yes", "No")
    adf[i,2] <- ifelse((adf[i,1]=="No"& adf[i,3]=="No"), "Yes", "No")
}
    
Useries <- which(adf$`Unit Root`=="Yes")
Useries <- as.data.frame(Useries)

for (i in 1:nrow(Useries)) {
    locate <- Useries[i,1]
    locate <- as.numeric(locate)
    Useries[i,1] <- rownames(adf)[locate]
}
    
#----------------------------------
#----------------------------------

Eseries <- which(adf$`Explosive`=="Yes")
Eseries <- as.data.frame(Eseries)
    
for (i in 1:nrow(Eseries)) {
    locate <- Eseries[i,1]
    locate <- as.numeric(locate)
    Eseries[i,1] <- rownames(adf)[locate]
}
    
Stseries <- which(adf$`Stationary`=="Yes")
Stseries <- as.data.frame(Stseries)

for (i in 1:nrow(Stseries)) {
    locate <- Stseries[i,1]
    locate <- as.numeric(locate)
    Stseries[i,1] <- rownames(adf)[locate]
}
    
env <- c("Tsdata", "adfsta", "adfexp", "i", "locate", "adf", "bp", "S","n", "env")
remove(list = (env))
    
    
#Testing I(1)
    
adf1 <- matrix(0, nrow(Useries), 3)
adf1 <- as.data.frame(adf1)
colnames(adf1) <- c("Stationary", "Unit Root", "Explosive")
rownames(adf1) <- Useries$Useries

SPU <- SPweekly[,colnames(SPweekly) %in% Useries$Useries]
    
Tsdata <- xts(SPU, order.by = Dates)
Tsdata <- diff(Tsdata)
Tsdata <- xts(Tsdata, order.by = Dates)
    
    
for(i in 1:ncol(Tsdata)) {
    S <- colnames(Tsdata)[i]
    bp <- Breakpoints[S,]
    n <- bp$`Number of Breakpoints`
    bp <- bp[,(n+1)]
    bp <- as.numeric(bp)
    bp <- bp+12 #include cooling period
    adfsta <- adf.test(na.omit(Tsdata[((bp):nrow(Tsdata)),i]))
    adf1[i,1] <- adfsta$p.value
    adf1[i,1] <- ifelse(adf1[i,1]<.05, "Yes", "No")
    adfexp <- adf.test(na.omit(Tsdata[,i]), alternative = c("explosive"))
    adf1[i,3] <- adfexp$p.value
    adf1[i,3] <- ifelse(adf1[i,3]<.05, "Yes", "No")
    adf1[i,2] <- ifelse((adf1[i,1]=="No"& adf1[i,3]=="No"), "Yes", "No")
}
    
#Removing Series that are not I(1) from the Unit Root Stocks
    
noni1 <- subset(adf1, adf1$Stationary=="No")
noni1 <- rownames(noni1)
Useries <- Useries[!Useries$Useries %in% noni1,]
Useries <- as.data.frame(Useries)
    
#----------------------------------
#----------------------------------
    
adf1 <- matrix(0, nrow(Eseries), 3)
adf1 <- as.data.frame(adf1)
colnames(adf1) <- c("Stationary", "Unit Root", "Explosive")
rownames(adf1) <- Eseries$Eseries
    
SPE <- SPweekly[,colnames(SPweekly) %in% Eseries$Eseries]
    
Tsdata <- xts(SPE, order.by = Dates)
Tsdata <- diff(Tsdata)
Tsdata <- xts(Tsdata, order.by = Dates)

for(i in 1:ncol(Tsdata)) {
    S <- colnames(Tsdata)[i]
    bp <- Breakpoints[S,]
    n <- bp$`Number of Breakpoints`
    bp <- bp[,(n+1)]
    bp <- as.numeric(bp)
    bp <- bp+12 #include cooling period
    adfsta <- adf.test(na.omit(Tsdata[((bp):nrow(Tsdata)),i]))
    adf1[i,1] <- adfsta$p.value
    adf1[i,1] <- ifelse(adf1[i,1]<.05, "Yes", "No")
    adfexp <- adf.test(na.omit(Tsdata[,i]), alternative = c("explosive"))
    adf1[i,3] <- adfexp$p.value
    adf1[i,3] <- ifelse(adf1[i,3]<.05, "Yes", "No")
    adf1[i,2] <- ifelse((adf1[i,1]=="No"& adf1[i,3]=="No"), "Yes", "No")
}
    
#Removing Series that are not I(1) from the Unit Root Stocks
noni1 <- subset(adf1, adf1$Stationary=="No")
noni1 <- rownames(noni1)
Eseries <- Eseries[!Eseries$Eseries %in% noni1,]
Eseries <- as.data.frame(Eseries)

env <- c("SPE", "SPU", "noni1", "Tsdata", "adfsta", "adfexp", "i", "locate", "adf1", "bp", "S","n", "env")
remove(list = (env))

#------------JOHANSEN TEST FOR COINTEGRATING RELATIONSHIP-----------------------

Jou <- matrix(rep(0), (nrow(Useries)-1),(nrow(Useries)-1))
rownames(Jou) <- Useries$Useries[-1]  
colnames(Jou) <- Useries$Useries[-(nrow(Useries))]  
Jou <- as.data.frame(Jou)  

CPU <- makeCluster(cores[1]-2)
registerDoParallel(CPU)
Jouraw <- foreach (a = 1:(ncol(Jou)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel"),.combine = rbind) %dopar% {
    #for each base stock #a = column number
    Bstock    <- (colnames(Jou))[a] #base stock
    stockleft = nrow(Jou)-(a-1) #pairs left
    foreach (i = (1:(stockleft)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel" ),.combine = rbind) %dopar% { 
        #for each pair stock
        pairedstock <- a + (i-1) 
        tstock <- rownames(Jou)[pairedstock]#current pair for testing
        Stocknames <- c(Bstock, tstock)
        bp <- Breakpoints[Stocknames,]#Breakpoints
        nbs <- bp[Bstock, 1]
        nps <- bp[tstock,1]
        n <- max(nbs,nps)+1 #number of stable period
        result <- vector(,n)
        j = a+(i-1) #rownumber
        foreach (b = (1:(n)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel" ),.combine = rbind) %dopar% { 
            #for each stable period
            sb <- bp[1,b+1] 
            sp <- bp[2,b+1]
            sb[is.null(sb)] <- NA
            sp[is.null(sp)] <- NA
            sb[is.na(sb)] <- bp[1,nbs+1]
            sp[is.na(sp)] <- bp[2,nps+1]
            ib <- min(which(!is.na(SPweekly[,Bstock])))
            ip <- min(which(!is.na(SPweekly[,tstock])))
            start <- ifelse(b == 1, max(ib,ip), max(sb,sp))#Time-series start point
            eb <- bp[1,b+2]
            ep <- bp[2,b+2]
            eb[is.null(eb)] <- NA
            ep[is.null(ep)] <- NA
            eb[is.na(eb)] <- nrow(SPweekly)
            ep[is.na(ep)] <- nrow(SPweekly)
            end <- ifelse(b == n ,nrow(SPweekly), min(eb,ep)) #Time-series End point
            if (start>=end) { 
                result[b] <- 0 #No overlapping stable period
                } else {
                stockseries <- SPweekly[(start:end),Stocknames]
                stockseries <- na.omit(stockseries)
                if (nrow(stockseries) <= 50){
                result[b] <- 0
                } else if (length(unique(stockseries[,1]))<=50){
                result[b] <- 0
                } else if (length(unique(stockseries[,2]))<=50){
                result[b] <- 0
                } else {    
                lag <- (VARselect(stockseries))$selection
                lag <- min(lag)
                lag <- max(lag, 2)
                Jotr <- ca.jo(stockseries, type = "trace", ecdet ="const", K=lag)
                tvalue <- Jotr@teststat
                tvalue <- as.data.frame(tvalue)
                cval <- Jotr@cval
                cval <- as.data.frame(cval)
                result[b] <- ifelse(tvalue[1,1]<cval[1,2], 0, 1)
                }}#close if
            r <- sum(result)
            relationship <- ifelse(r == 0, "No", "Yes")
            period <- which(result==1)
            period <- ifelse(length(period)==0, 0, period)
            return(c(j,a,b,result[b],relationship, start, end))
        }#close for each stable period  
     }#close for each pair
}#close for each base
stopCluster(CPU)
    
Jouraw <- as.data.frame(Jouraw)
Jouraw$V1 <- as.numeric(Jouraw$V1)
Jouraw$V2 <- as.numeric(Jouraw$V2)
Jouraw$V3 <- as.numeric(Jouraw$V3)
Jouraw$V4 <- ifelse(Jouraw$V4==0,"No", "Yes")
Jouraw$V6 <- as.numeric(Jouraw$V6)
Jouraw$V7 <- as.numeric(Jouraw$V7)

    
for (i in 1:nrow(Jouraw)){
    a <- Jouraw$V1[i]
    b <- Jouraw$V2[i]
    c <- Jouraw$V5[i]
    Jou[a,b] <- c
}

#----------------------
#----------------------

Joe <- matrix(rep(0), (nrow(Eseries)-1),(nrow(Eseries)-1))
rownames(Joe) <- Eseries$Eseries[-1]  
colnames(Joe) <- Eseries$Eseries[-(nrow(Eseries))]  
Joe <- as.data.frame(Joe)  
    
CPU <- makeCluster(cores[1]-2)
registerDoParallel(CPU)
Joeraw <- foreach (a = 1:(ncol(Joe)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel"),.combine = rbind) %dopar% {
    #for each base stock #a = column number
    Bstock    <- (colnames(Joe))[a] #Base Stock 
    stockleft = nrow(Joe)-(a-1) #Unique pairs available for the stock
    foreach (i = (1:(stockleft)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel" ),.combine = rbind) %dopar% { 
        #for each pair stock
        pairedstock <- a + (i-1) 
        tstock <- rownames(Joe)[pairedstock]#current pair for testing
        Stocknames <- c(Bstock, tstock)
        bp <- Breakpoints[Stocknames,]#Breakpoints
        nbs <- bp[Bstock, 1]
        nps <- bp[tstock,1]
        n <- max(nbs,nps)+1 #number of stable period
        result <- vector(,n)
        j = a+(i-1) #rownumber
        foreach (b = (1:(n)),.packages = c("urca", "vars", "parallel", "foreach", "doParallel" ),.combine = rbind) %dopar% { 
            #for each stable period
            sb <- bp[1,b+1] 
            sp <- bp[2,b+1]
            sb[is.null(sb)] <- NA
            sp[is.null(sp)] <- NA
            sb[is.na(sb)] <- bp[1,nbs+1]
            sp[is.na(sp)] <- bp[2,nps+1]
            ib <- min(which(!is.na(SPweekly[,Bstock])))
            ip <- min(which(!is.na(SPweekly[,tstock])))
            start <- ifelse(b == 1, max(ib,ip), max(sb,sp))#Time-series start point
            eb <- bp[1,b+2]
            ep <- bp[2,b+2]
            eb[is.null(eb)] <- NA
            ep[is.null(ep)] <- NA
            eb[is.na(eb)] <- nrow(SPweekly)
            ep[is.na(ep)] <- nrow(SPweekly)
            end <- ifelse(b == n ,nrow(SPweekly), min(eb,ep)) #Time-series End point
            if (start>=end) { 
                result[b] <- 0 #No overlapping stable period
            } else {
                stockseries <- SPweekly[(start:end),Stocknames]
                stockseries <- na.omit(stockseries)
                if (nrow(stockseries) <= 50){
                    result[b] <- 0
                } else if (length(unique(stockseries[,1]))<=50){
                    result[b] <- 0
                } else if (length(unique(stockseries[,2]))<=50){
                    result[b] <- 0
                } else {    
                    lag <- (VARselect(stockseries))$selection
                    lag <- min(lag)
                    lag <- max(lag, 2)
                    Jotr <- ca.jo(stockseries, type = "trace", ecdet ="const", K=lag)
                    tvalue <- Jotr@teststat
                    tvalue <- as.data.frame(tvalue)
                    cval <- Jotr@cval
                    cval <- as.data.frame(cval)
                    result[b] <- ifelse(tvalue[1,1]<cval[1,2], 0, 1)
                }}#close if
            r <- sum(result)
            relationship <- ifelse(r == 0, "No", "Yes")
            period <- which(result==1)
            period <- ifelse(length(period)==0, 0, period)
            return(c(j,a,b,result[b],relationship, start,end))
        }#close for each stable period  
    }
}
stopCluster(CPU)
    
Joeraw <- as.data.frame(Joeraw)
Joeraw$V1 <- as.numeric(Joeraw$V1)
Joeraw$V2 <- as.numeric(Joeraw$V2)
Joeraw$V3 <- as.numeric(Joeraw$V3)
Joeraw$V4 <- ifelse(Joeraw$V4==0,"No", "Yes")
Joeraw$V6 <- as.numeric(Joeraw$V6)
Joeraw$V7 <- as.numeric(Joeraw$V7)

    
for (i in 1:nrow(Joeraw)){
    a <- Joeraw$V1[i]
    b <- Joeraw$V2[i]
    c <- Joeraw$V5[i]
    Joe[a,b] <- c
     
}


#------------BACKTEST ALL PAIRS---------------------------------

#Cointegrated stock and cointegrated period for each pair
PeriodU <- Jouraw[Jouraw$V4 == "Yes",]
PeriodE <- Joeraw[Joeraw$V4 == "Yes",]
PeriodU$Base <- colnames(Jou)[PeriodU$V2]
PeriodU$Pair <- rownames(Jou)[PeriodU$V1]
PeriodE$Base <- colnames(Joe)[PeriodE$V2]
PeriodE$Pair <- rownames(Joe)[PeriodE$V1]

PeriodU <- PeriodU[,6:9]
PeriodE <- PeriodE[,6:9]

PeriodU <- unique(PeriodU)
PeriodE <- unique(PeriodE)

cnames <- c("Start", "End", "Pair A", "Pair B")

Cpair <- rbind(PeriodU, PeriodE)
colnames(Cpair) <- cnames

Cpair <- Cpair[,c(3,4,1,2)]
CoP <- Cpair[,c(1,2)]
del <- which(duplicated(CoP))

#Remove stocks cointegrated for less than 3 years

Cpair$Period <- Cpair$End-Cpair$Start

Cpair <- Cpair[-c(del),]
rownames(Cpair) <- index(Cpair)
`Cointeg-Pairs` <- Cpair


env <- c("Joeraw","a","b","c","i", "Jouraw", "BcopairsE", "BcopairsU",
         "BECopair","BUCopair", "PeriodU", "PeriodE", "cnames",
         "CoP", "del", "env")
remove(list = (env))


#-------BACKTEST FOR ENTIRE PERIOD----------------------------------------------
    
CpairEP <- Cpair 

for (i in 1:nrow(CpairEP)){
    #Pair Selection
    Bs <- CpairEP[i,1]
    Ps <- CpairEP[i,2]
    Pair <- c(Bs, Ps)
    
    #Rejection based on length of the relationship
    period <- CpairEP$Period[i]
    if (period<=151) next
    
    #Rejection based on possibility of trading
    start <- CpairEP$Start[i]+150
    if (start>=1100) next
    
    #Observation Selection
    stockseries <- SPweekly[(start:(start+150)),Pair]
    colnames(stockseries) <- c("Base", "Pair")
    
    #VAR Equation
    lag <- (VARselect(na.omit(stockseries)))$selection
    lag <- min(lag)
    lag <- 2
    VAR <- VAR(na.omit(stockseries),lag,type ="const")
    df <- VAR$varresult
    BModel <- as.data.frame(VAR$varresult$Base$coefficients)
    colnames(BModel) <- "Coefficients"
    rownames(BModel) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag","Constant")
    PModel <- as.data.frame(VAR$varresult$Pair$coefficients)
    colnames(PModel) <- "Coefficients"
    rownames(PModel) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag", "Constant")
    
    #Start of trading strategy
    start <- CpairEP[i,3]+150
    end <- ifelse(start+99>1122, 1122, start+99)
    
    
    #True and Forecast value
    BTrue <- SPweekly[((start-2):end),Pair]
    BTrue <- na.omit(BTrue)
  

    
    colnames(BTrue) <- c("Base", "Pair")
    BFcast <- matrix(0,(nrow(BTrue)-2),2)
    BFcast <- as.data.frame(BFcast)
    colnames(BFcast) <- c("Forecast Base", "Forecast Pair")
    
    for (a in 1:(nrow(BFcast))) {
        BFcast[a,1] <- (BModel$Coefficients[1]*BTrue$Base[a+1])+(BModel$Coefficients[2]*BTrue$Pair[a+1])+(BModel$Coefficients[3]*BTrue$Base[a])+(BModel$Coefficients[4]*BTrue$Pair[a])+BModel$Coefficients[5]
        BFcast[a,2] <- (PModel$Coefficients[1]*BTrue$Base[a+1])+(PModel$Coefficients[2]*BTrue$Pair[a+1])+(PModel$Coefficients[3]*BTrue$Base[a])+(PModel$Coefficients[4]*BTrue$Pair[a])+PModel$Coefficients[5]
    }
    BFcast <- round(BFcast, 2)
    SnPt <- tail(SnPweekly[3:nrow(BTrue),2], nrow(BFcast))
    colnames(BTrue) <- c("True Base", "True Pair")
    
    #Backtest using the forecasted and true value
    Backtest <- cbind(BFcast, BTrue[(3:nrow(BTrue)),], SnPt)
    colnames(Backtest)[5] <- "SnP"
    Backtest$TSB <- ifelse(lead(Backtest$`Forecast Base`)>=Backtest$`True Base`, "Buy", "Sell")
    Backtest$TSP <- ifelse(lead(Backtest$`Forecast Pair`)>=Backtest$`True Pair`, "Buy", "Sell")
    n <- nrow(Backtest)-1
    for (b in 1:n) {
        Backtest$BR[b] <- (((Backtest$`True Base`[b+1]-Backtest$`True Base`[b])/
                                Backtest$`True Base`[b])*100)
        Backtest$PR[b] <- (((Backtest$`True Pair`[b+1]-Backtest$`True Pair`[b])/
                                Backtest$`True Pair`[b])*100)
        Backtest$SnPR[b] <- (((Backtest$`SnP`[b+1]-Backtest$`SnP`[b])/
                                  Backtest$`SnP`[b])*100)
    }
    for (c in 1:(n+1)){
        Backtest$TSBR[c] <- ifelse(Backtest$TSB[c]=="Buy", Backtest$BR[c], ((Backtest$BR)[c]*-1)) 
        Backtest$TSPR[c] <- ifelse(Backtest$TSP[c]=="Buy", Backtest$PR[c], ((Backtest$PR)[c]*-1)) 
        Backtest$TSR[c] <- (.5*(Backtest$TSBR[c]))+(.5*(Backtest$TSPR[c]))
        Backtest$Hitrate[c] <- ifelse((Backtest$TSR[c])>0 ,1, 0)
        Backtest$SHitrate[c] <- ifelse((Backtest$SnPR[c])>0 ,1, 0)
        Backtest$R[c] <- 1+((Backtest$TSR[c])/100)
        Backtest$R[is.na(Backtest$R)] <- 1
        Backtest$Creturn[c] <- ((((cumprod(na.omit(Backtest$R))[c]))-1)*100)
        Backtest$Creturn[c] <- ifelse(Backtest$Creturn[c]== -100, 0, Backtest$Creturn[c])
        Backtest$BR[c] <- 1+(na.omit(Backtest$SnPR[c])/100)
        Backtest$SPCreturn[c] <- ((((na.omit(cumprod(Backtest$BR)[c])))-1)*100)
        Pairs <- paste(Bs, Ps, sep = "-")
        rownames(Backtest)[c] <- Dates[start+c-1]
    }
    df <- Backtest
    mv(from = "df", to = Pairs)
    
    #Metrics for the trading strategy
    Metrics <- matrix(0, 7,3)
    Metrics <- as.data.frame(Metrics)
    colnames(Metrics) <- c("", "TS", "S&P")
    Metrics[1,1] <- "Average Weekly Return"
    Metrics[2,1] <- "Volatility"
    Metrics[3,1] <- "Sharpe Ratio"
    Metrics[4,1] <- "Peak to Drawdown"
    Metrics[5,1] <- "Recovery"
    Metrics[6,1] <- "Hit Rate"
    Metrics[7,1] <- "Correlation with S&P returns"
    #mean
    n <- nrow(Backtest)-1
    Treturn <- Backtest$Creturn[n]/100
    ARe <- ((((1+Treturn)^(1/n))-1)*100)
    Metrics[1,2] <- ARe
    Treturn <- Backtest$SPCreturn[n]/100
    BRe <- ((((1+Treturn)^(1/n))-1)*100)
    Metrics[1,3] <- BRe
    #Standard Deviation
    Metrics[2,2] <- sd(na.omit(Backtest$TSR))
    Metrics[2,3] <- sd(na.omit(Backtest$SnPR))
    #Sharpe
    Metrics[3,2] <- sharpe(na.omit(Backtest$Creturn), scale = sqrt(52))
    Metrics[3,3] <- sharpe(na.omit(Backtest$SPCreturn), scale = sqrt(52))
    #Maximum Drawdown
    mdpb <- maxdrawdown(na.omit(Backtest$Creturn))
    Metrics[4,2] <- mdpb$maxdrawdown
    mdbb <- maxdrawdown(na.omit(Backtest$SPCreturn))
    Metrics[4,3] <- mdbb$maxdrawdown
    #Recovery rate
    #Portfolio
    sdatep <- mdpb$from
    lowdatep <- mdpb$to
    sdatepricep <- Backtest$Creturn[sdatep]
    locrecp <- min(which((Backtest$Creturn[(lowdatep+1):nrow(Backtest)])>sdatepricep))
    Metrics[5,2] <- locrecp
    #S&P
    sdateb <- mdbb$from
    lowdateb <- mdbb$to
    sdatepriceb <- Backtest$SPCreturn[sdateb]
    locrecb <- min(which((Backtest$SPCreturn[(lowdateb+1):nrow(Backtest)])>sdatepriceb))
    Metrics[5,3] <- locrecb
    #Hitrate
    Metrics[6,2] <- (sum(na.omit(Backtest$Hitrate)))/(nrow(Backtest)-1)
    Metrics[6,3] <- (sum(na.omit(Backtest$SHitrate)))/(nrow(Backtest)-1)
    #Correlation with S&P return
    Corr <- correlationTest(Backtest$TSR, Backtest$SnPR)
    Metrics[7,2] <- Corr@test$estimate
    Metrics[,2:3] <- round(Metrics[,2:3], digits = 4)
    CpairEP$`Pair`[i] <- paste(CpairEP$`Pair A`[i], CpairEP$`Pair B`[i], sep = "-")
    CpairEP$StartTS[i] <- start
    CpairEP$EndTS[i] <- end
    CpairEP$`Average Weekly Return`[i] <- Metrics$TS[1]
    CpairEP$`Average S&P Return`[i] <- Metrics$`S&P`[1]
    CpairEP$`Volatility`[i] <- Metrics$TS[2]
    CpairEP$`Sharpe Ratio`[i] <- Metrics$TS[3]
    CpairEP$`Peak to Drawdown`[i] <- Metrics$TS[4]
    CpairEP$`Recovery`[i] <- Metrics$TS[5]
    CpairEP$`Hit Rate`[i] <- Metrics$TS[6]
    CpairEP$`Correlation with S&P return`[i] <- Metrics$TS[7]
}

CpairEP <- CpairEP[(CpairEP$Period>151),]
CpairEP <- CpairEP[((CpairEP$Start+150)<1100),]

dropcol <- c("Pair A", "Pair B", "Start", "End")
CpairEP <- CpairEP[,!(colnames(CpairEP) %in% dropcol)]

FcastEP <- list()

for (i in 1:nrow(CpairEP)){
    envname <- CpairEP$Pair[i]
    FcastEP[[i]] <- get(envname)
}

names(FcastEP) <- CpairEP$Pair
cnames <- colnames(Backtest)

env <- c(c(CpairCP$Pair), "Backtest", "BFcast", "BModel", "bseries", "BTval",
         "Cointeg-Pairs", "Corr", "mdbb", "mdpb", "Metrics", "PModel",
         "SnP", "SnPt", "stockseries", "Steries", "VAR", "start", "end", 
         "sdateb", "sdatep", "sdatepriceb", "sdatepricep", "Bs", "Ps", "n", 
         "a", "locrecb", "locrecp", "Sym", "PSym", "Treturn", "envname", "ep", 
         "i", "ARe", "b", "D", "D1", "D2", "z", "bp", "dropcol", "BRe", "c", 
         "cnames" ,"lowdateb", "lowdatep", "lag", "period",
         "Pair", "Pairs", "env")
remove(list = env)



#-------------------------------------------------------------------------------    
#----------------USER INTERFACE-------------------------------------------------
#-------------------------------------------------------------------------------
    
#User Interface

ui <- shinyUI(
        navbarPage("Cointegrated Stocks",
            tabPanel("Introduction",
                mainPanel(
                    htmlOutput("Introduction"), #Introduction
                    ) #Close mainPanel
                   ),#Close TabPanel
            tabPanel("Base Stock",
                # Sidebar with a dropdown input for stock names and their breakpoints
                sidebarLayout(
                    sidebarPanel(
                        selectInput("Stock_Sym","Stock Symbol","A"),
                        dataTableOutput("Breakpoint") #Output Breakpoints
                                ), #Close SidePanel
                    mainPanel(
                        plotOutput("Stock Price"), #Price Plot
                        verbatimTextOutput("Unit Root"), #Output Unitroot
                        verbatimTextOutput("Possible Pairs") #Cointegrated Pairs
                         )) #Close mainPanel and sidebarLayout
                    ),#Close TabPanel  
            tabPanel("Pair Stability and Diagnostics",
                     #Sidebar with a dropdown input for the pair stocks
                sidebarLayout(
                    sidebarPanel(
                        selectInput("Pair_Sym","Stock Symbol","A"),
                        dataTableOutput("DT")
                                ),#Close sidebarPanel
                        mainPanel("Price Plots",
                            fluidRow(
                                verticalLayout(
                                    splitLayout(cellWidths = c("50%", "50%"),
                                                plotOutput("Pair Plot - E"),
                                                plotOutput("S Plot - E")),
                                    splitLayout(cellWidths = c("50%", "50%"),
                                                plotOutput("Pair Plot - CP"),
                                                plotOutput("S Plot - CP"))
                            ))#Close verticalLayout and fluidRow
                ))#Close mainPanel and sidebarLayout
            ),#Close TabPanel
            tabPanel("Forecast and Performance Evaluation",
                sidebarLayout(
                    sidebarPanel(
                        dataTableOutput("Forecast")
                    ),#Close sidebarPanel
                    mainPanel(fluidRow(
                        verticalLayout(
                            splitLayout(cellWidths = c("50%", "50%"),
                                        plotOutput("Baseplot"),
                                        plotOutput("Pairplot")),
                            splitLayout(cellWidths = c("50%", "50%"),
                                        plotOutput("Returns"),
                                        plotOutput("Dotplot"))
                        ))#Close verticalLayout and fluidRow
                    ))#Close mainPanel and sidebarLayout
            ),#Close TabPanel
            tabPanel("BackTest",
                     sidebarLayout(
                         sidebarPanel(
                             dataTableOutput("Metrics")
                         ),#Close sidebarPanel
                         mainPanel(
                             plotOutput("CRplot"),#Cumulative Returns
                             plotOutput("dotplotbt") #Dotplot - TSR and S&P
                             )) #Close mainPanel and sidebarLayout
            ),#Close TabPanel
            tabPanel("Portfolio - Entire Period",
                     sidebarLayout(
                         sidebarPanel(
                             dataTableOutput("PMetricsEP")
                         ),#Close sidebarPanel
                         mainPanel(
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"),
                                             plotOutput("portplotEP"),#Cumulative Returns
                                             plotOutput("portretplotEP"))), #Return analysis
                             plotOutput("portdotEP") #Dotplot - Portfolio and S&P
                         )) #Close mainPanel and sidebarLayout
            )#Close TabPanel
    ))# Close NavBar and UI

    
#-------------------------------------------------------------------------------    
#----------------SERVER LOGIC---------------------------------------------------
#-------------------------------------------------------------------------------
    
server <- function(input, output, session) {

#-------------------------------------------------------------------------------
#----------------PAGE 1---------------------------------------------------------
#-------------------------------------------------------------------------------    

    
    
#Output - Existence of Unit Roots/Explosive Root
output$`Introduction` <- renderPrint({
Introduction <- cat("In this Project, I have created a dashboard that can be 
used to find stocks that can used for pair trades in S&P 500 and analyze the
relationship and create trading signals based their relationship. I do this 
by testing for cointegrating relationship between the stock's price series.\n

I have then created portfolios based on these trading signals and analyze their
performance by backtesting.\n

Step 1  -> Importing the data
Step 2  -> Finding Structural Breaks in each stock                               - Bai and Perron Test\n
Step 3  -> Testing for Unit/Explosive roots for each stock                       - Augmented Dickey Fuller Test\n
Step 4  -> Testing for Cointegrating relationships                               - Johansen Cointegration Test\n
Step 5  -> Forecasting prices to backtest                                        - VAR model\n
Step 6  -> Diagnostics for the cointegrated relationships                        - Correlation, Error Correction Term, Granger Causality\n
Step 7  -> Forecast and Performance Evaluation of individual Trading Strategy    - Weekly return, Volatility, Sharpe, Peak to trough drawdown, Recovery, Hitrate \n
Step 8  -> Backtest and Performance Evaluation of individual Trading Strategy    - Weekly return, Volatility, Sharpe, Peak to trough drawdown, Recovery, Hitrate\n
Step 9  -> Constructing Portfolio using the Pairs                                - Based on Optimization of Sharpe\n
Step 10 -> Forecast and Performance Evaluation of the portfolio                  - Weekly return, Volatility, Sharpe, Peak to trough drawdown, Recovery, Hitrate \n
Step 11 -> Backtest and Performance Evaluation of the portfolio                  - Weekly return, Volatility, Sharpe, Peak to trough drawdown, Recovery, Hitrate")

Introduction
})        
    
#--------------------STOCK SELECTION--------------------------------------------    
    
#Input - Stock Selection
observe({
        updateSelectInput(session, "Stock_Sym" , choices = as.data.frame(SSym))
})
        
#-----------------------PRICE PLOT-----------PAGE 1-----------------------------
        
#Process - Stock Series + Dates
SSeries <- reactive({
            req(input$Stock_Sym)
            Sym <- input$Stock_Sym
            df <- SPweekly[,Sym]
            colnames(df)[1] <- "Price"
            df <- cbind(df,Dates)
            })
        
#Plot
Priceplot <- reactive({
            ggplot(SSeries(), mapping = aes(x=Dates, y=Price))+geom_line()
            })
        
#Output - Plot
output$`Stock Price` <- renderPlot({
            Priceplot <- Priceplot()
            Priceplot
        })
        
#----------------------STRUCTURAL BREAK--------PAGE 1---------------------------    
        
#Data for Structural Break
BPSeries <- reactive({
            req(input$Stock_Sym)
            Sym <- input$Stock_Sym
            df <- Breakpoints[Sym,(-1)]
            df <- as.data.frame(df)
            df <- t(df)
            df <- as.character(Dates[df])
            df
        })
        
#Output - Structural Breaks
output$`Breakpoint` <- renderDataTable({
            Breakpoints <- BPSeries()
            Breakpoints
            }, options = list(dom = 't'))
    
#----------------UNIT ROOT TEST--------------PAGE 1-----------------------------
        
#ADF Test
ADF <- reactive({
        req(input$Stock_Sym)
        Sym <- input$Stock_Sym
        if (Sym %in% Stseries){
            Statement <- "The stock has a Stationary Root after last Structural Break"
        } else if (Sym %in% Eseries$Eseries){
            Statement <- "The stock has an Explosive Root after last Structural Break"
        } else if (Sym %in% Useries$Useries){
            Statement <- "The stock has a Unit Root after last Structural Break"
        } else {
            Statement <- "The stock do not have enough Observation"
        }
    })
    
#Output - Existence of Unit Roots/Explosive Root
output$`Unit Root` <- renderPrint({
                    ADF <- ADF()
                    ADF
                    })
        
#-----TESTING FOR COINTEGRATING RELATIONSHIP-----PAGE 1-------------------------
        
Coin <- reactive({
        req(input$Stock_Sym)
        Sym <- input$Stock_Sym
        remove(Row)
        remove(Col)
        if (Sym %in% Stseries$Stseries) {
            print("The Stock is stationary and cannot have cointegrating relationship")
        } else if (Sym %in% Useries$Useries) {
            Row <- as.data.frame(Jou[,Sym])
            rownames(Row) <- rownames(Jou)
            Col <- as.data.frame(Jou[Sym,])
            Col <- as.data.frame(t(Col))
            rownames(Col) <- colnames(Jou)
            CopairsR <- rownames(Row)[Row[,1]=="Yes"]
            CopairsC <- rownames(Col)[Col[,1]=="Yes"]
            CopairsU <- c(unique(CopairsR, CopairsC))
            Copairs <- (CopairsU)
            CopairsU <- ifelse(length(CopairsU)==0, print("The Stock is not cointegrated with any other stock"), print(CopairsU))
            return(Copairs)
        } else if (Sym %in% Eseries$Eseries) {
            Row <- as.data.frame(Joe[,Sym])
            rownames(Row) <- rownames(Joe)
            Col <- as.data.frame(Joe[Sym,])
            Col <- as.data.frame(t(Col))
            rownames(Col) <- colnames(Joe)
            CopairsR <- rownames(Row)[Row[,1]=="Yes"]
            CopairsC <- rownames(Col)[Col[,1]=="Yes"]
            CopairsE <- c(CopairsR, CopairsC)
            CopairsE <- unique(CopairsE)
            Copairs <- (CopairsE)
            CopairsE <- ifelse(length(CopairsE)==0, print("The Stock is not cointegrated with any other stock"), print(CopairsE))
            return(Copairs)
        }   else {
            print("The stock either do not have enough Observation or is not I(1)")
        }
    })
        
output$`Possible Pairs` <- renderPrint({
                        Coin <- Coin()
                        })
        
#-------------------------------------------------------------------------------
#------------PAGE 2-------------------------------------------------------------
#-------------------------------------------------------------------------------    
    
#------------PAIR SELECTION--------------------PAGE 2---------------------------
    
#Input - Pair Selection
        
observe({
    updateSelectInput(session, "Pair_Sym" , choices = Coin())
    })
    
#------------PRICE PLOT WITH THE PAIRS---------PAGE 2---------------------------
#Process - Stock Series + Dates
PSeriesE <- reactive({
            req(input$Stock_Sym)
            req(input$Pair_Sym)
            Sym <- input$Stock_Sym
            PSym <- input$Pair_Sym
            Stock <- c(Sym, PSym)
            df <- SPweekly[,c(Stock)]
            colnames(df) <- c("Price Bs", "Price Ps")
            df <- cbind(df,Dates)
        })
        
#Price Plot for the Pair - Entire Period
PairplotE <- reactive({
            PS <- PSeriesE()
            req(input$Stock_Sym)
            req(input$Pair_Sym)
            Sym <- input$Stock_Sym
            PSym <- input$Pair_Sym
            min <- min(PS[,(1:2)])
            max <- max(PS[,(1:2)])
            ggplot(PS, mapping = aes(x=Dates, y=(min:max)))+
                geom_line(aes(y=`Price Bs`, color = Sym))+
                geom_line(aes(y=`Price Ps`, color = PSym))+
                labs(title = "Pair Price Plot for the entire Period")+
                ylab("Price")
        })
        
#Output - Price Plot for the Pair - Entire Period
output$`Pair Plot - E` <- renderPlot({
                         Pairplot <- `PairplotE`()
                          Pairplot
                         })
        
#Scatter Plot for the Pair - Entire Period
SPlotE <- reactive({
        PSP <- PSeriesE()
        PSP <- PSP[,(1:2)]
        ggplot(PSP, aes(x= `Price Bs`, y=`Price Ps`))+
            geom_point()+
            labs(title = "Scatter Plot for the Entire Period")
        })
        
#Output - Scatter Plot for the Pair - Entire Period
output$`S Plot - E` <- renderPlot({
                       Splot <- `SPlotE`()
                       Splot
                       })
        
#Process - Stock Series + Dates for cointegrated time period
PSeriesCP <-reactive({
            req(input$Stock_Sym)
            req(input$Pair_Sym)
            Sym <- input$Stock_Sym
            PSym <- input$Pair_Sym
            Stock <- c(Sym, PSym)
            df <- SPweekly[,c(Stock)]
            colnames(df) <- c("Price Bs", "Price Ps")
            df <- cbind(df,Dates)
            bp <- Breakpoints[c(Stock),]
            nbs <- bp[Sym, 1]
            bpbs <- Breakpoints[Sym, (nbs+1)]
            nps <- bp[PSym,1]
            bpps <- Breakpoints[PSym, (nps+1)]
            bp <- max(bpbs,bpps)
            bp <- bp+12
            df <- df[(bp:nrow(SPweekly)),]
            })
        
#Price Plot for the Pair - Cointegrated Period
PairplotCP <- reactive({
            PS <- PSeriesCP()
            req(input$Stock_Sym)
            req(input$Pair_Sym)
            Sym <- input$Stock_Sym
            PSym <- input$Pair_Sym
            min <- min(PS[,(1:2)])
            max <- max(PS[,(1:2)])
            ggplot(PS, mapping = aes(x=Dates, y=(min:max)))+
                geom_line(aes(y=`Price Bs`, color = Sym))+
                geom_line(aes(y=`Price Ps`, color = PSym))+
                labs(title = "Pair Price Plot for Cointegrated Period")+
                ylab("Price")
        })
        
#Output - Price Plot for the Pair - Cointegrated Period

output$`Pair Plot - CP` <- renderPlot({
                          Pairplot <- `PairplotCP`()
                          Pairplot
})
        
#Scatter Plot for the Pair - Cointegrated Period
SPlotCP <- reactive({
    PSP <- PSeriesCP()
    PSP <- PSP[,(1:2)]
    ggplot(PSP, aes(x= `Price Bs`, y=`Price Ps`))+
        geom_point()+
        labs(title = "Scatter Plot for Cointegrated Period")
})

#Output - Scatter Plot for the Pair - Cointegrated Period
output$`S Plot - CP` <- renderPlot({
    Splot <- `SPlotCP`()
    Splot
})

#xts object for diagnostics

ts <- reactive({
    PP <- PSeriesCP()
    DatesCP <- PP[,3]
    PP <- PP[,-3]
    ts <- xts(PP, order.by = DatesCP)
    ts
})

#OLS residual analysis

residual <- reactive({
    xts <- ts()
    ols <- lm(xts$"Price Bs" ~ xts$"Price Ps", data = xts)
    adf <- adf.test(ols$residuals, k=1)
    residual <- ifelse(adf$p.value<.05, "Stationary", "Non-Stationary")
    residual
    })

#Correlation
DT <- reactive({
    DT <- matrix(0, 10, 2)
    DT <- as.data.frame(DT)
    DT[,1] <- c("Correlation", "ECT - B", "ECT - P", "GC B->P", "GC P->B",
                "OLS Residual", "Serial Correlation", "ARCH Effect", "Skew", 
                "Kurtosis")
    PP <- PSeriesCP()
    PP <- PP[,-3]
    DT[1,2] <- round(cor(PP$`Price Bs`, PP$`Price Ps`), 4)
    ts <- ts()
    VECM <- VECM(na.omit(ts),2,r=1,estim="2OLS")
    VECM <- as.data.frame(VECM$coefficients)
    DT[2,2] <- round(VECM[1,1], 4)
    DT[3,2] <- round(VECM[2,1],4)
    dum <- matrix(0, nrow(PP),2)
    Jot <- ca.jo(PP, type = "trace", ecdet ="const", K=2)
    VAR <- vars::VAR(PP, type = "const")
    gcBs <- causality(VAR, cause = "Price.Bs")
    gcPs <- causality(VAR, cause = "Price.Ps")
    DT[4,2] <- ifelse(gcBs$Granger$p.value<.05, "Yes", "No")
    DT[5,2] <- ifelse(gcPs$Granger$p.value<.05, "Yes", "No")
    DT[6,2] <- residual()
    serial <- serial.test(VAR, lags.pt=12, type="BG")
    DT[7,2] <- ifelse(serial$serial$p.value<.05, "Not Present", "Present")
    arch <- arch.test(VAR, lags.multi=12, multivariate.only = TRUE)
    DT[8,2] <- ifelse(arch$arch.mul$p.value<.05, "Not Present", "Present")
    normality <- normality.test(VAR, multivariate.only=TRUE)
    DT[9,2] <- ifelse(normality$jb.mul$Skewness$p.value<.05, "Normal Skew", "Non-normal Skew")
    DT[10,2] <- ifelse(normality$jb.mul$Kurtosis$p.value<.05, "Normal Kurtosis", "Non-normal Kurtosis")
    DT
})

#Output - Diagnostics Table
output$`DT` <- renderDataTable({
    DT <- DT()
    DT
}, options = list(dom = 't'))

#-------------------------------------------------------------------------------
#------------PAGE 3-------------------------------------------------------------
#-------------------------------------------------------------------------------    

#------------FORECAST--------------------PAGE 3---------------------------------

#Forecast the prices

PriceTable <- reactive({
    req(input$Stock_Sym)
    req(input$Pair_Sym)
    Sym <- input$Stock_Sym
    PSym <- input$Pair_Sym
    Pnames <- c(Sym, PSym)
    PS <- PSeriesCP()
    rdates <- c(tail(PS[,3], 2))
    Rowdates <- rdates
    
    Fcast <- matrix(rep(0), 12, 2)
    Fcast <- as.data.frame(Fcast) 
    
    Rowdates[3:12] <- TDates
    Rowdates <- format(as.POSIXct(Rowdates,format='%m/%d/%Y %H:%M:%S'),format='%Y-%m-%d')
    
    rownames(Fcast) <- Rowdates
    colnames(Fcast) <- c("Base", "Pair")
    
    Fcast[1,1] <- PS$`Price Bs`[nrow(PS)-1]
    Fcast[2,1] <- PS$`Price Bs`[nrow(PS)]
    Fcast[1,2] <- PS$`Price Ps`[nrow(PS)-1]
    Fcast[2,2] <- PS$`Price Ps`[nrow(PS)]
    
    rownames(PS) <- PS[,3]
    PS <- PS[,-3]
    
    lag <- 2
    VAR <- vars::VAR(PS, p = lag, type="const")
    
    Bscoff <- as.data.frame(VAR$varresult$Price.Bs$coefficients)
    colnames(Bscoff) <- "Coefficients"
    rownames(Bscoff) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag","Constant")#-------
    
    Pscoff <- as.data.frame(VAR$varresult$Price.Ps$coefficients)
    colnames(Pscoff) <- "Coefficients"
    rownames(Pscoff) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag", "Constant")
    
    Tval <- Fcast
    Tval[3:12,] <- True[,(Pnames)]
    
    for (i in 1:(nrow(Fcast)-lag)) {
        Fcast[i+2,1] <- (Bscoff$Coefficients[1]*Tval$Base[i+1])+(Bscoff$Coefficients[2]*Tval$Pair[i+1])+(Bscoff$Coefficients[3]*Tval$Base[i])+(Bscoff$Coefficients[4]*Tval$Pair[i])+Bscoff$Coefficients[5]
        Fcast[i+2,2] <- (Pscoff$Coefficients[1]*Tval$Base[i+1])+(Pscoff$Coefficients[2]*Tval$Pair[i+1])+(Pscoff$Coefficients[3]*Tval$Base[i])+(Pscoff$Coefficients[4]*Tval$Pair[i])+Pscoff$Coefficients[5]
    }
    Fcast <- round(Fcast, 2)
})

output$Forecast <- renderDataTable({
    Forecast <- PriceTable()
    Forecast
}, options = list(dom = 't'))

Baseplot <- reactive({
    req(input$Stock_Sym)
    Sym <- input$Stock_Sym
    Forecast <- PriceTable()
    BForecast <- as.data.frame(Forecast$Base)
    BTrue <- as.data.frame(Forecast$Base)
    BTrue[3:12,] <- True[,Sym]
    fvt <- cbind(BTrue, BForecast)
    colnames(fvt) <- c("True", "Forecast")
    rownames(fvt) <- rownames(Forecast)
    min <- min(fvt)
    max <- max(fvt)
    ggplot(fvt, aes(x=rownames(fvt), y=min:max, group =1))+
    geom_line(aes(y= True, color ="True"))+
    geom_line(aes(y= Forecast, color= "Forecast"))+
    labs(title = Sym, x = "Dates", y= "Price")+
    scale_x_discrete(breaks = rownames(fvt)[c(T,F,F,F,F)])
})

output$Baseplot <- renderPlot({
    Baseplot <- Baseplot()
    Baseplot
})

Pairplot <- reactive({
    req(input$Pair_Sym)
    PSym <- input$Pair_Sym
    Forecast <- PriceTable()
    BForecast <- as.data.frame(Forecast$Pair)
    BTrue <- as.data.frame(Forecast$Pair)
    BTrue[3:12,] <- True[,PSym]
    fvt <- cbind(BTrue, BForecast)
    colnames(fvt) <- c("True", "Forecast")
    rownames(fvt) <- rownames(Forecast)
    min <- min(fvt)
    max <- max(fvt)
    ggplot(fvt, aes(x=rownames(fvt), y=min:max, group =1))+
    geom_line(aes(y= True, color ="True"))+
    geom_line(aes(y= Forecast, color= "Forecast"))+
    labs(title = PSym, x = "Dates", y= "Price")+
    scale_x_discrete(breaks = rownames(fvt)[c(T,F,F,F,F)])
})

output$Pairplot <- renderPlot({
    Pairplot <- Pairplot()
    Pairplot
})

CReturns <- reactive({
    req(input$Stock_Sym)
    req(input$Pair_Sym)
    Sym <- input$Stock_Sym
    PSym <- input$Pair_Sym
    SPX <- "SPX"
    
    SPXprice <- True[,SPX]
    SPXprice$Returns[1] <-0
    
    for (i in 1:nrow(SPXprice)) {
        SPXprice$Returns[i] <- ((SPXprice$SPX[i+1]-SPXprice$SPX[i])/
                                    SPXprice$SPX[i])*100
        }
    
    SPXprice$Returns <- round(SPXprice$Returns, 2)
    
    Forecast <- PriceTable()
    
    Bstock <- True[,Sym]
    colnames(Bstock) <- "True"
    Bstock$Returns[1] <- 0
    for (i in 1:nrow(Bstock)) {
        Bstock$Returns[i] <- ((Bstock[(i+1),1]-Bstock[(i),1])/
                                  Bstock[(i),1])*100
    }
    Bstock$Returns <- as.numeric(Bstock$Returns)
    Bstock$Returns <- round(Bstock$Returns, 2)
    Bstock$Forecast <- Forecast$Base[3:12]
    TS <- ifelse(lead(Bstock$Forecast)>=Bstock$True, "Buy", "Sell")
    Bstock$TS <- TS
    Bstock$Tsreturn <- 0
    Bstock$Tsreturn <- ifelse(lag(Bstock$TS=="Buy"), Bstock$Returns, (Bstock$Returns*-1))
    
    Pstock <- True[,PSym]
    colnames(Pstock) <- "True"
    
    Pstock$Return[1] <- 0
    
    for (i in 1:nrow(Pstock)) {
        Pstock$Return[i] <- ((Pstock[(i+1),1]-Pstock[(i),1])/
                                  Pstock[(i),1])*100
    }
    
    Pstock$Return <- as.numeric(Pstock$Return)
    Pstock$Return <- round(Pstock$Return, 2)
    Pstock$Forecast <- Forecast$Pair[3:12]
    Pstock$TS <- ifelse(lead(Pstock$Forecast)>=Pstock$True, "Buy", "Sell")
    Pstock$Tsreturn <- ifelse(lag(Pstock$TS=="Buy"), Pstock$Return, (Pstock$Return*-1))
    
    HoldStock <- as.data.frame((.5*Bstock$Returns)+(.5*Pstock$Return))
    colnames(HoldStock) <- "Returns"
    `TS Return` <- as.data.frame((.5*Bstock$Tsreturn)+(.5*Pstock$Tsreturn))
    colnames(`TS Return`) <- "Returns"
    
    Returns <- matrix(0, 9, 3)
    Returns <- as.data.frame(Returns)
    colnames(Returns) <- c("SPX", "Hold-CS", "TS-CS")
    Returns$SPX <- cumsum(SPXprice$Returns)[-nrow(SPXprice)]
    Returns$`Hold-CS` <- cumsum(HoldStock$Returns)[-nrow(HoldStock)]
    Returns$`TS-CS` <- cumsum(`TS Return`$Returns)[-nrow(`TS Return`)]
    
    min <- min(Returns)
    max <- max(Returns)
    plotreturn <- ggplot(Returns, aes(x=rownames(True)[-nrow(True)], y=min:max, group =1))+
                  geom_line(aes(y=SPX, color ="SPX"))+
                  geom_line(aes(y=`Hold-CS`, color = "Hold-CS"))+
                  geom_line(aes(y=`TS-CS`, color = "Trading Strategy"))+
                  labs(title = "Cumulative returns", x= "Dates", y="C-returns")+
                  scale_x_discrete(breaks = rownames(True)[c(T,F,F,F,F)])
    
})

output$Returns <- renderPlot({
    CReturns <- CReturns()
    CReturns
})

Dotplot <- reactive({
    req(input$Stock_Sym)
    req(input$Pair_Sym)
    Sym <- input$Stock_Sym
    PSym <- input$Pair_Sym
    SPX <- "SPX"
    
    SPXprice <- True[,SPX]
    SPXprice$Returns[1] <-0
    
    for (i in 2:nrow(SPXprice)) {
        SPXprice$Returns[i] <- ((SPXprice$SPX[i]-SPXprice$SPX[i-1])/
                                    SPXprice$SPX[i-1])*100
    }
    
    SPXprice$Returns <- round(SPXprice$Returns, 2)
    
    Forecast <- PriceTable()
    
    Bstock <- True[,Sym]
    colnames(Bstock) <- "True"
    Bstock$Returns[1] <- 0
    for (i in 2:nrow(Bstock)) {
        Bstock$Returns[i] <- ((Bstock[i,1]-Bstock[(i-1),1])/
                                  Bstock[(i-1),1])*100
    }
    Bstock$Returns <- as.numeric(Bstock$Returns)
    Bstock$Returns <- round(Bstock$Returns, 2)
    Bstock$Forecast <- Forecast$Base[3:12]
    TS <- ifelse(lead(Bstock$Forecast)>=Bstock$True, "Buy", "Sell")
    Bstock$TS <- TS
    Bstock$Tsreturn <- 0
    Bstock$Tsreturn <- ifelse(lag(Bstock$TS=="Buy"), Bstock$Returns, (Bstock$Returns*-1))
    
    Pstock <- True[,PSym]
    colnames(Pstock) <- "True"
    Pstock$Return[1] <- 0
    for (i in 2:nrow(Pstock)) {
        Pstock$Return[i] <- ((Pstock[i,1]-Pstock[(i-1),1])/
                                 Pstock[(i-1),1])*100
    }
    Pstock$Return <- as.numeric(Pstock$Return)
    Pstock$Return <- round(Pstock$Return, 2)
    Pstock$Forecast <- Forecast$Pair[3:12]
    Pstock$TS <- ifelse(lead(Pstock$Forecast)>=Pstock$True, "Buy", "Sell")
    Pstock$Tsreturn <- ifelse(lag(Pstock$TS=="Buy"), Pstock$Return, (Pstock$Return*-1))
    
    HoldStock <- (.5*Bstock$Returns)+(.5*Pstock$Return)
    `TS Return` <- (.5*Bstock$Tsreturn)+(.5*Pstock$Tsreturn)
    
    
    Dotplot <- `TS Return`*SPXprice$Returns
    plot(Dotplot)
})

output$Dotplot <- renderPlot({
    Dotplot <- Dotplot()
    Dotplot
})

#-------------------------------------------------------------------------------
#------------PAGE 4-------------------------------------------------------------
#-------------------------------------------------------------------------------    

#-----------BACKTEST AND PERFORMANCE EVALUATION---------------------------------

Backtest <- reactive({
    req(input$Stock_Sym)
    req(input$Pair_Sym)
    Sym <- input$Stock_Sym
    PSym <- input$Pair_Sym
    Snames <- c(Sym, PSym)
    
    #3 Year - Including 2 lags
    start <- nrow(SPweekly)-151
    end <- nrow(SPweekly)
    series <- SPweekly[start:end,Snames]
    rownames(series) <- Dates[start:end]
    colnames(series) <- c("Base", "Pair")
    
    #Last cointegrated Period - For VAR equation
    PS <- PSeriesCP() #392
    rownames(PS) <- PS$Dates
    PS <- as.data.frame(PS)
    remove <- "Dates"
    PS <- PS[,(!colnames(PS) %in% remove)]
    lag <- 2
    VAR <- vars::VAR(PS, p = lag, type="const")
    
    #VAR Coefficient 
    Bscoff <- as.data.frame(VAR$varresult$Price.Bs$coefficients)
    colnames(Bscoff) <- "Coefficients"
    rownames(Bscoff) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag","Constant")
    Pscoff <- as.data.frame(VAR$varresult$Price.Ps$coefficients)
    colnames(Pscoff) <- "Coefficients"
    rownames(Pscoff) <- c("Bs - Lag", "Ps- Lag","Bs - 2nd Lag","Ps - 2nd Lag", "Constant")
    
    #Backtest - Forecast Table
    BFcast <- matrix(0,150,2)
    BFcast <- as.data.frame(BFcast)
    colnames(BFcast) <- c("Forecast Base", "Forecast Pair")
    rownames(BFcast) <- tail(Dates, 150)
    
    #Backtest - Forecast
    for (i in 1:(nrow(BFcast))) {
        BFcast[i,1] <- (Bscoff$Coefficients[1]*series$Base[i+1])+(Bscoff$Coefficients[2]*series$Pair[i+1])+(Bscoff$Coefficients[3]*series$Base[i])+(Bscoff$Coefficients[4]*series$Pair[i])+Bscoff$Coefficients[5]
        BFcast[i,2] <- (Pscoff$Coefficients[1]*series$Base[i+1])+(Pscoff$Coefficients[2]*series$Pair[i+1])+(Pscoff$Coefficients[3]*series$Base[i])+(Pscoff$Coefficients[4]*series$Pair[i])+Pscoff$Coefficients[5]
    }
    BFcast <- round(BFcast, 2)
    BTval <- series[3:152,]
    SnPt <- tail(SnPweekly[,2], 150)
    
    colnames(BTval) <- c("True Base", "True Pair")
    Backtest <- cbind(BFcast, BTval, SnPt)
    colnames(Backtest)[5] <- "SnP"
    Backtest
})

`Trading Strategy` <- reactive({
    Backtest <- Backtest()
    #Trading Strategy
    Backtest$TSB <- ifelse(lead(Backtest$`Forecast Base`)>=Backtest$`True Base`, "Buy", "Sell")
    Backtest$TSP <- ifelse(lead(Backtest$`Forecast Pair`)>=Backtest$`True Pair`, "Buy", "Sell")
    n <- nrow(Backtest)-1
    for (i in 1:n) {
        Backtest$BR[i] <- (((Backtest$`True Base`[i+1]-Backtest$`True Base`[i])/
                                Backtest$`True Base`[i])*100)
        Backtest$PR[i] <- (((Backtest$`True Pair`[i+1]-Backtest$`True Pair`[i])/
                                Backtest$`True Pair`[i])*100)
        Backtest$SnPR[i] <- (((Backtest$`SnP`[i+1]-Backtest$`SnP`[i])/
                                Backtest$`SnP`[i])*100)
    }
    for (i in 1:(n+1)){
        Backtest$TSBR[i] <- ifelse(Backtest$TSB[i]=="Buy", Backtest$BR[i], ((Backtest$BR)[i]*-1)) 
        Backtest$TSPR[i] <- ifelse(Backtest$TSP[i]=="Buy", Backtest$PR[i], ((Backtest$PR)[i]*-1)) 
        Backtest$TSR[i] <- (.5*(Backtest$TSBR[i]))+(.5*(Backtest$TSPR[i]))
        Backtest$Hitrate[i] <- ifelse(Backtest$TSR[i]>0 ,1, 0)
        Backtest$SHitrate[i] <- ifelse(Backtest$SnPR[i]>0 ,1, 0)
        Backtest$R[i] <- 1+(Backtest$TSR[i]/100)
        Backtest$Creturn[i] <- ((((cumprod(Backtest$R)[i]))-1)*100)
        Backtest$BR[i] <- 1+(Backtest$SnPR[i]/100)
        Backtest$SPCreturn[i] <- ((((cumprod(Backtest$BR)[i]))-1)*100)
        }
    Backtest
})



Metrics <- reactive({
    Backtest <- `Trading Strategy`()
    Metrics <- matrix(0, 6,3)
    Metrics <- as.data.frame(Metrics)
    colnames(Metrics) <- c("", "TS", "S&P")
    Metrics[1,1] <- "Average Weekly Return"
    Metrics[2,1] <- "Volatility"
    Metrics[3,1] <- "Sharpe Ratio"
    Metrics[4,1] <- "Peak to Drawdown"
    Metrics[5,1] <- "Recovery"
    Metrics[6,1] <- "Hit Rate"
    #mean
    n <- nrow(Backtest)-1
    Treturn <- Backtest$Creturn[n]/100
    ARe <- ((((1+Treturn)^(1/n))-1)*100)
    Metrics[1,2] <- ARe
    Treturn <- Backtest$SPCreturn[n]/100
    BRe <- ((((1+Treturn)^(1/n))-1)*100)
    Metrics[1,3] <- BRe
    
    #Standard Deviation
    Metrics[2,2] <- sd(na.omit(Backtest$TSR))
    Metrics[2,3] <- sd(na.omit(Backtest$SnPR))
    
    #Sharpe
    Metrics[3,2] <- sharpe(na.omit(Backtest$Creturn), scale = sqrt(52))
    Metrics[3,3] <- sharpe(na.omit(Backtest$SPCreturn), scale = sqrt(52))
    
    #Maximum Drawdown
    mdpb <- maxdrawdown(na.omit(Backtest$Creturn))
    Metrics[4,2] <- mdpb$maxdrawdown
    mdbb <- maxdrawdown(na.omit(Backtest$SPCreturn))
    Metrics[4,3] <- mdbb$maxdrawdown
    
    #Recovery rate
    #Portfolio
    sdatep <- mdpb$from
    lowdatep <- mdpb$to
    sdatepricep <- Backtest$Creturn[sdatep]
    locrecp <- min(which((Backtest$Creturn[(lowdatep+1):nrow(Backtest)])>sdatepricep))
    Metrics[5,2] <- locrecp
    #S&P
    sdateb <- mdbb$from
    lowdateb <- mdbb$to
    sdatepriceb <- Backtest$SPCreturn[sdateb]
    locrecb <- min(which((Backtest$SPCreturn[(lowdateb+1):nrow(Backtest)])>sdatepriceb))
    Metrics[5,3] <- locrecb
    
    #Hitrate
    Metrics[6,2] <- (sum(na.omit(Backtest$Hitrate)))/(nrow(Backtest)-1)
    Metrics[6,3] <- (sum(na.omit(Backtest$SHitrate)))/(nrow(Backtest)-1)
    Metrics[,2:3] <- round(Metrics[,2:3], digits = 4)
    Metrics
})


output$Metrics <- renderDataTable({
    Metrics <- Metrics()
    Metrics
}, options = list(dom = 't'))


output$CRplot <- renderPlot({
    Backtest <- `Trading Strategy`()
    min <- min(min(Backtest$Creturn), min(Backtest$SPCreturn))-10
    max <- max(max(Backtest$Creturn), max(Backtest$SPCreturn))+10
    ggplot(Backtest, aes(x = rownames(Backtest), y=min:max, group = 1))+
    geom_line(aes(y=Creturn, color = "TSR Cumulative Return"))+
    geom_line(aes(y=SPCreturn, color = "S&P Cumulative Return"))+
    geom_line(aes(y=TSR, color = "Weekly Return"))+
    labs(title = "Cumulative Returns", x = "Dates", y= "Return")+
    scale_x_discrete(breaks = rownames(Backtest)[c(T,rep(F,25))])
})

output$dotplotbt <- renderPlot({
    Backtest <- `Trading Strategy`()
    Backtest$DP <- Backtest$BR*Backtest$TSR
    ggplot(Backtest, aes(x = rownames(Backtest), y=DP, group = 1))+
    geom_point(aes(y=DP))+
    labs(title = "Dotplot", x = "Dates", y= "")+
    scale_x_discrete(breaks = rownames(Backtest)[c(T,rep(F,25))])
})

#-------------------------------------------------------------------------------
#------------PAGE 5-------------------------------------------------------------
#-------------------------------------------------------------------------------    


#------------PORTFOLIO SELECTION--------------------PAGE 5----------------------

#Input - Portfolio Selection

PortfolioCP <- c("Average Weekly Return", "Volatility","Sharpe Ratio", "Peak to Drawdown", "Recovery",
                "Hit Rate", "Correlation with S&P return")

observe({
    updateSelectInput(session, "Portfolio" , choices = as.data.frame(PortfolioCP))
})

Cpairs <- reactive({
    req(input$Portfolio)
    input <- input$Portfolio
    itp <- 151
    m <- 50
    ttp <- round(nrow(SPweekly)/50)
    for (i in 1:ttp){
        st <- ifelse(i==1, itp, (itp+(50*(i-1))))
        et <- st+49
        tradeavai <- CpairEP[CpairEP$StartTS<=st & CpairEP$EndTS>=et,]
        if (input == "Average Weekly Return") {
            tradeavai <- top_n(tradeavai, 10, `Average Weekly Return`)
        } else if (input == "Volatility") {
            tradeavai <- top_n(tradeavai, -10, `Volatility`)
        } else if (input == "Sharpe Ratio") {
            tradeavai <- top_n(tradeavai, 10, `Sharpe Ratio`)
        } else if (input == "Peak to Drawdown") {
            tradeavai <- top_n(tradeavai, -10, `Peak to Drawdown`)
        } else if (input == "Recovery") {
            tradeavai <- top_n(tradeavai, -10, `Recovery`)
        } else if (input == "Hit Rate") {
            tradeavai <- top_n(tradeavai, 10, `Hit Rate`)
        } else {
            for (i in 1:nrow(Cpair)){
                tradeavai$Df0[i] <- abs(tradeavai$`Correlation with S&P return`[i]-0)
            }
            tradeavai <- top_n(tradeavai, -10, `Df0`)
            dcol <- "Df0"
            tradeavai <- tradeavai[,!colnames(tradeavai) %in% dcol]
        }
        Cpairs <- ifelse(i==1, as.data.frame(tradeavai), rbind(Cpairs, tradeavai))
        Cpairs <- as.data.frame(Cpairs)
    
        }
    Cpairs
})


#-------------------------------------------------------------------------
#-----------PAGE 5------------BACKTEST FOR 17 YEARS------------------
#-------------------------------------------------------------------------



OpfEP <- reactive({
    Pf <- Cpairs()
    Pf1 <- as.data.frame(FcastEP[Pf$Pair[1]])
    colnames(Pf1) <- cnames
    Pf2 <- as.data.frame(FcastEP[Pf$Pair[2]])
    colnames(Pf2) <- cnames
    Pf3 <- as.data.frame(FcastEP[Pf$Pair[3]])
    colnames(Pf3) <- cnames
    Pf4 <- as.data.frame(FcastEP[Pf$Pair[4]])
    colnames(Pf4) <- cnames
    Pf5 <- as.data.frame(FcastEP[Pf$Pair[5]])
    colnames(Pf5) <- cnames
    Pf6 <- as.data.frame(FcastEP[Pf$Pair[6]])
    colnames(Pf6) <- cnames
    Pf7 <- as.data.frame(FcastEP[Pf$Pair[7]])
    colnames(Pf7) <- cnames
    Pf8 <- as.data.frame(FcastEP[Pf$Pair[8]])
    colnames(Pf8) <- cnames
    Pf9 <- as.data.frame(FcastEP[Pf$Pair[9]])
    colnames(Pf9) <- cnames
    Pf10 <- as.data.frame(FcastEP[Pf$Pair[10]])
    colnames(Pf10) <- cnames
    
    #Creating multiplier since not all stocks are available from
    #the beginning. And removing the last 200 data points to remove the
    #training data
    r <- nrow(FcastEP[[1]])
    c <- nrow(Pf)+1
    TSR <- matrix(,r,c)
    TSR <- as.data.frame(TSR)
    TSR <- TSR[1:(nrow(TSR)-200),]
    colnames(TSR) <- c(Pf$Pair, "Multiplier")
    
    for (i in 1:nrow(TSR)){
        TSR[i,1] <- .1*Pf1$TSR[i]
        TSR[i,2] <- .1*Pf2$TSR[i]
        TSR[i,3] <- .1*Pf3$TSR[i]
        TSR[i,4] <- .1*Pf4$TSR[i]
        TSR[i,5] <- .1*Pf5$TSR[i]
        TSR[i,6] <- .1*Pf6$TSR[i]
        TSR[i,7] <- .1*Pf7$TSR[i]
        TSR[i,8] <- .1*Pf8$TSR[i]
        TSR[i,9] <- .1*Pf9$TSR[i]
        TSR[i,10] <- .1*Pf10$TSR[i]
    }
    
    TSR$Multiplier <- rowSums(!is.na(TSR))
    TSR$Multiplier <- 10/TSR$Multiplier
    TSR$SnPR <- Pf1$SnPR[1:(nrow(TSR))]
    
    #Optimal Portfolio
    Opf <- vector()
    Opf <- rowSums(TSR[1:nrow(Pf)], na.rm=TRUE)*TSR$Multiplier
    Opf <- as.data.frame(Opf)
    colnames(Opf) <- "TSR"
    Opf$SnPR <- TSR$SnPR
    
    for (i in 1:nrow(Opf)){
        Opf$R[i] <- 1+(Opf$TSR[i]/100)
        Opf$R[is.na(Opf$R)] <- 1
        Opf$Creturn[i] <- ((((cumprod(Opf$R)[i]))-1)*100)
        Opf$Hitrate[i] <- ifelse(Opf$TSR[i]>0 ,1, 0)
        Opf$SHitrate[i] <- ifelse(Opf$SnPR[i]>0 ,1, 0)
        Opf$BR[i] <- 1+(Opf$SnPR[i]/100)
        Opf$SPCreturn[i] <- (((cumprod(Opf$BR)[i])-1)*100)
    } 
    rownames(Opf) <- tail(Dates,nrow(Opf))
    Opf
})

output$portplotEP <- renderPlot({
    Opf <- OpfEP()
    Opf <- as.data.frame(Opf)
    min <- min((min(na.omit(Opf$Creturn))), (min(na.omit(Opf$SPCreturn))))-10
    max <- max((max(na.omit(Opf$Creturn))), (max(na.omit(Opf$SPCreturn))))+10
    ggplot(Opf, aes(x = rownames(Opf), y=min:max, group = 1))+
    geom_line(aes(y=Creturn, color = "TSR Cumulative Return"))+
    geom_line(aes(y=SPCreturn, color = "S&P Cumulative Return"))+
    labs(title = "Cumulative Returns", x = "Dates", y= "Return")+
    scale_x_discrete(breaks = rownames(Opf)[c(T,rep(F,208))])
})


output$portdotEP <- renderPlot({
    Opf <- OpfEP()
    Opf <- as.data.frame(Opf)
    Opf$DP <- Opf$BR*Opf$TSR
    ggplot(Opf, aes(x = rownames(Opf), y=DP, group = 1))+
    geom_point(aes(y=DP))+
    labs(title = "Dotplot", x = "Dates", y= "")+
    scale_x_discrete(breaks = rownames(Opf)[c(T,rep(F,208))])
})

PMetricsEP <- reactive({
    Opf <- OpfEP()
    Opf <- as.data.frame(Opf)
    PMetrics <- matrix(0, 6,3)
    PMetrics <- as.data.frame(PMetrics)
    colnames(PMetrics) <- c("", "TS", "S&P")
    PMetrics[1,1] <- "Average Weekly Return"
    PMetrics[2,1] <- "Volatility"
    PMetrics[3,1] <- "Sharpe Ratio"
    PMetrics[4,1] <- "Peak to Drawdown"
    PMetrics[5,1] <- "Recovery"
    PMetrics[6,1] <- "Hit Rate"
    
    #Find the beginning of the strategy
    
    #mean
    n <- nrow(Opf)-1
    Treturn <- Opf$Creturn[n]/100
    ARe <- ((((1+Treturn)^(1/n))-1)*100)
    PMetrics[1,2] <- ARe
    Treturn <- Opf$SPCreturn[n]/100
    BRe <- ((((1+Treturn)^(1/n))-1)*100)
    PMetrics[1,3] <- BRe
    
    #Standard Deviation
    PMetrics[2,2] <- sd(na.omit(Opf$TSR))
    PMetrics[2,3] <- sd(na.omit(Opf$SnPR))
    
    #Sharpe
    PMetrics[3,2] <- sharpe(na.omit(Opf$Creturn), scale = sqrt(52))
    PMetrics[3,3] <- sharpe(na.omit(Opf$SPCreturn), scale = sqrt(52))
    
    #Maximum Drawdown
    mdpb <- maxdrawdown(na.omit(Opf$Creturn))
    PMetrics[4,2] <- mdpb$maxdrawdown
    mdbb <- maxdrawdown(na.omit(Opf$SPCreturn))
    PMetrics[4,3] <- mdbb$maxdrawdown
    
    #Recovery rate
    #Portfolio
    sdatep <- mdpb$from
    lowdatep <- mdpb$to
    sdatepricep <- Opf$Creturn[sdatep]
    locrecp <- min(which((Opf$Creturn[(lowdatep+1):nrow(Opf)])>sdatepricep))
    PMetrics[5,2] <- locrecp
    #S&P
    sdateb <- mdbb$from
    lowdateb <- mdbb$to
    sdatepriceb <- Opf$SPCreturn[sdateb]
    locrecb <- min(which((Opf$SPCreturn[(lowdateb+1):nrow(Opf)])>sdatepriceb))
    PMetrics[5,3] <- locrecb
    #Hitrate
    PMetrics[6,2] <- (sum(na.omit(Opf$Hitrate)))/(nrow(Opf)-1)
    PMetrics[6,3] <- (sum(na.omit(Opf$SHitrate)))/(nrow(Opf)-1)
    PMetrics[,2:3] <- round(PMetrics[,2:3], digits = 4)
    PMetrics
})

output$PMetricsEP <- renderDataTable({
    PMetrics <- PMetricsEP()
    PMetrics
}, options = list(dom = 't'))

PRMetricsEP <- reactive({
    Opf <- OpfEP()
    Opf <- as.data.frame(Opf)
    
    #Recursive Sharpe
    for (i in 1:(nrow(Opf))){
        Opf$Sharpe[i] <- sharpe(na.omit(Opf$Creturn)[1:i], scale = sqrt(52))
    }
    for (i in 1:(nrow(Opf)-1)){
        Opf$SPSharpe[i] <- sharpe(na.omit(Opf$SPCreturn)[1:i], scale = sqrt(52))
    }
    
    
    #Recursive Vol
    for (i in 1:(nrow(Opf)-1)){
        Opf$Vol[i] <- sd((Opf$TSR[1:i]))
    }
    Opf
})

output$portretplotEP <- renderPlot({
    Opf <- PRMetricsEP()
    Opf <- as.data.frame(Opf)
    min <- min((min(na.omit(Opf$Sharpe))), min(na.omit(Opf$Vol)))-5
    max <- max((max(na.omit(Opf$Sharpe))), max(na.omit(Opf$Vol)))+5
    ggplot(Opf, aes(x = rownames(Opf), y=min:max, group = 1))+
    geom_line(aes(y=Sharpe, color = "Sharpe"))+
    geom_line(aes(y=SPSharpe, color = "S&P Sharpe"))+
    geom_line(aes(y=Vol, color = "Volatility"))+
    labs(title = "Cumulative Returns", x = "Dates", y= "Return")+
    scale_x_discrete(breaks = rownames(Opf)[c(T,rep(F,208))])
})

}#Close Server
    
# Run the application 
shinyApp(ui = ui, server = server)

