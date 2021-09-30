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
           "tsDyn", "rstudioapi","PerformanceAnalytics")
npackage <- plist[!(plist %in% installed.packages()[,"Package"])]
if(length(npackage)>0) {install.packages(npackage)}
    
env <- c("plist", "npackage", "env")
remove(list = c(env))
    
    
#Package - Parallel Computing
library(parallel)
library(foreach)
library(doParallel)
library(doSNOW)
library(doRNG)
library(tictoc)
    
#Packages - Data import and Manipulation
library(readxl)#To import data set
library(xts) #To create Time series Data Frames
library(zoo) #To create Time series Data Frames
library(lubridate)#Manipulating Dates
library(tidyverse) 
    
#Package - Plotting
library(ggplot2)#To create Plots
library(dplyr)#To create overlapping plots
    
#Package - Identifying Structural Breaks
library(strucchange) # To Identify Structural Breaks
    
#Package - Unit Root Test
library(tseries)
library(fUnitRoots)
library(timeSeries)
library(exuber)
library(urca)
    
#Package - VAR and VECM Models
library(vars)

#Package - Cointegrating Relationship
library(cointReg)
library(egcm) #engle granger test
    
#Package - GARCH Model
library(rmgarch)
library(FinTS)

#Package - Performance Evaluation
library(PerformanceAnalytics)
    
#Package - Dashboard
library(shiny)
    
#Package Miscellaneous 
library(forecast)
library(ftsa)
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
Jouraw <- foreach (a = 1:(nrow(Useries)-1),.packages = c("urca", "vars", "parallel", "foreach", "doParallel"),.combine = rbind) %dopar% {
    Bstock    <- (Useries$Useries)[a]
    stockleft = nrow(Useries)-a
    foreach (i = (1:(stockleft)),.packages = c("urca", "vars"),.combine = rbind) %dopar% { 
        pairedstock <- a + i
        tstock <- Useries$Useries[pairedstock]
        Stocknames <- c(Bstock, tstock)
        bp <- Breakpoints[Stocknames,]
        nbs <- bp[Bstock, 1]
        bpbs <- Breakpoints[Bstock, nbs]
        nps <- bp[tstock,1]
        bpps <- Breakpoints[tstock, nps]
        bp <- max(bpbs,bpps)
        bp <- bp+12
        stockseries <- SPweekly[(bp:nrow(SPweekly)),Stocknames]
        lag <- (VARselect(stockseries))$selection
        lag <- min(lag)
        lag <- max(lag, 2)
        Jotr <- ca.jo(stockseries, type = "trace", ecdet ="const", K=lag)
        tvalue <- Jotr@teststat
        tvalue <- as.data.frame(tvalue)
        cval <- Jotr@cval
        cval <- as.data.frame(cval)
        j = a+(i-1)
        Jou[j,a] <- ifelse(tvalue[1,1]<cval[1,2], "No", "Yes")
        return(c(j,a,Jou[j,a]))
    }
}
stopCluster(CPU)
    
Jouraw <- as.data.frame(Jouraw)
Jouraw$V1 <- as.numeric(Jouraw$V1)
Jouraw$V2 <- as.numeric(Jouraw$V2)
    
for (i in 1:nrow(Jouraw)){
    a <- Jouraw$V1[i]
    b <- Jouraw$V2[i]
    c <- Jouraw$V3[i]
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
Joeraw <- foreach (a = 1:(nrow(Eseries)-1),.packages = c("urca", "vars", "parallel", "foreach", "doParallel"),.combine = rbind) %dopar% {
    Bstock    <- (Eseries$Eseries)[a]
    stockleft = nrow(Eseries)-a
    foreach (i = (1:(stockleft)),.packages = c("urca", "vars"),.combine = rbind) %dopar% { 
        pairedstock <- a + i
        tstock <- Eseries$Eseries[pairedstock]
        Stocknames <- c(Bstock, tstock)
        bp <- Breakpoints[Stocknames,]
        nbs <- bp[Bstock, 1]
        bpbs <- Breakpoints[Bstock, (nbs+1)]
        nps <- bp[tstock,1]
        bpps <- Breakpoints[tstock, (nps+1)]
        bp <- max(bpbs,bpps)
        bp <- bp+12
        stockseries <- SPweekly[(bp:nrow(SPweekly)),Stocknames]
        lag <- (VARselect(stockseries))$selection
        lag <- min(lag)
        lag <- max(lag, 2)
        Jotr <- ca.jo(stockseries, type = "trace", ecdet ="const", K=lag)
        tvalue <- Jotr@teststat
        tvalue <- as.data.frame(tvalue)
        cval <- Jotr@cval
        cval <- as.data.frame(cval)
        j = a+(i-1)
        Joe[j,a] <- ifelse(tvalue[1,1]<cval[1,2], "No", "Yes")
        return(c(j,a,Joe[j,a]))
    }
}
stopCluster(CPU)
    
Joeraw <- as.data.frame(Joeraw)
Joeraw$V1 <- as.numeric(Joeraw$V1)
Joeraw$V2 <- as.numeric(Joeraw$V2)
    
for (i in 1:nrow(Joeraw)){
    a <- Joeraw$V1[i]
    b <- Joeraw$V2[i]
    c <- Joeraw$V3[i]
    Joe[a,b] <- c
}
    
#-------------------------------------------------------------------------------    
#----------------USER INTERFACE-------------------------------------------------
#-------------------------------------------------------------------------------
    
#User Interface

ui <- shinyUI(
        navbarPage("Cointegrated Stocks",
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
            )#Close TabPanel 
    ))# Close NavBar and UI

    
#-------------------------------------------------------------------------------    
#----------------SERVER LOGIC---------------------------------------------------
#-------------------------------------------------------------------------------
    
server <- function(input, output, session) {

#-------------------------------------------------------------------------------
#----------------PAGE 1---------------------------------------------------------
#-------------------------------------------------------------------------------    
        
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

}#Close Server
    
# Run the application 
shinyApp(ui = ui, server = server)

