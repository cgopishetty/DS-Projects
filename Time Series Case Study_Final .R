##################################################################################################################################################################################

# PGDDS - IIIT-B 
# Course 4 - Group Case Study

# Done by:
# 1. Deepika Bansal
# 2. Chaitanya Gopishetty
# 3. Veenu Bhanot
# 4. Aditya Kumar

##################################################################################################################################################################################

##################################################################################################################################################################################

# PRELIMINARY

##################################################################################################################################################################################

# Set Working Directory:
setwd("C:/Users/Aditya Kumar/Desktop/Group Assignment")

# Load Dataset:
GS <- read.csv("Global Superstore.csv")

# Getting a feel of the data:
summary(GS)
str(GS)

# lOADING REQUIRED lIBRARIES

library(forecast)
library(tseries)
require(graphics)

##################################################################################################################################################################################

# DATA CLEANING

##################################################################################################################################################################################

# 1. Changing data type 
# Columns Order.Date and Ship.Date are stored as factors and need to be changed to date.
GS$Order.Date<-as.Date(GS$Order.Date,"%d-%m-%Y")
GS$Ship.Date<-as.Date(GS$Ship.Date,"%d-%m-%Y")

# 2. NA Treatment
# Looking for NA values in dataset:
sum(is.na(GS))
# Identifying where these NA values exist
colnames(GS)[colSums(is.na(GS)) > 0]
# As seen, there are 41,296 NA values in the dataset, all of which exist in the 'Postal.Code' column.
# Upon closely inspecting the dataset, it is seen that for addresses that are not in the United States, Postal Code is not entered.

##################################################################################################################################################################################

# DATA PREPARATION

##################################################################################################################################################################################

# 1. Creating 21 subsets for analysis, based on Market and Segment
levels(GS$Market)
levels(GS$Segment)
# 7 markets and 3 segments have been identified, based on which 21 subsets of data will be created, using different combinations of market and segment.
Africa_Consumer <- subset(GS, GS$Market=="Africa" & GS$Segment=="Consumer")
Africa_Corporate <- subset(GS, GS$Market=="Africa" & GS$Segment=="Corporate")
Africa_HO <- subset(GS, GS$Market=="Africa" & GS$Segment=="Home Office")  
APAC_Consumer <- subset(GS, GS$Market=="APAC" & GS$Segment=="Consumer")
APAC_Corporate <- subset(GS, GS$Market=="APAC" & GS$Segment=="Corporate")
APAC_HO <- subset(GS, GS$Market=="APAC" & GS$Segment=="Home Office")  
Canada_Consumer <- subset(GS, GS$Market=="Canada" & GS$Segment=="Consumer")
Canada_Corporate <- subset(GS, GS$Market=="Canada" & GS$Segment=="Corporate")
Canada_HO <- subset(GS, GS$Market=="Canada" & GS$Segment=="Home Office")  
EMEA_Consumer <- subset(GS, GS$Market=="EMEA" & GS$Segment=="Consumer")
EMEA_Corporate <- subset(GS, GS$Market=="EMEA" & GS$Segment=="Corporate")
EMEA_HO <- subset(GS, GS$Market=="EMEA" & GS$Segment=="Home Office")  
EU_Consumer <- subset(GS, GS$Market=="EU" & GS$Segment=="Consumer")
EU_Corporate <- subset(GS, GS$Market=="EU" & GS$Segment=="Corporate")
EU_HO <- subset(GS, GS$Market=="EU" & GS$Segment=="Home Office")  
LATAM_Consumer <- subset(GS, GS$Market=="LATAM" & GS$Segment=="Consumer")
LATAM_Corporate <- subset(GS, GS$Market=="LATAM" & GS$Segment=="Corporate")
LATAM_HO <- subset(GS, GS$Market=="LATAM" & GS$Segment=="Home Office")   
US_Consumer <- subset(GS, GS$Market=="US" & GS$Segment=="Consumer")
US_Corporate <- subset(GS, GS$Market=="US" & GS$Segment=="Corporate")
US_HO <- subset(GS, GS$Market=="US" & GS$Segment=="Home Office")  

# 2. Aggregating Sales over Order Date for each segment created:
Sales_Africa_Consumer <- aggregate(Africa_Consumer$Sales, by=list(Africa_Consumer$Order.Date), FUN=sum)
Sales_Africa_Corporate <- aggregate(Africa_Corporate$Sales, by=list(Africa_Corporate$Order.Date), FUN=sum)
Sales_Africa_HO <- aggregate(Africa_HO$Sales, by=list(Africa_HO$Order.Date), FUN=sum)
Sales_APAC_Consumer <- aggregate(APAC_Consumer$Sales, by=list(APAC_Consumer$Order.Date), FUN=sum)
Sales_APAC_Corporate <- aggregate(APAC_Corporate$Sales, by=list(APAC_Corporate$Order.Date), FUN=sum)
Sales_APAC_HO <- aggregate(APAC_HO$Sales, by=list(APAC_HO$Order.Date), FUN=sum)
Sales_Canada_Consumer <- aggregate(Canada_Consumer$Sales, by=list(Canada_Consumer$Order.Date), FUN=sum)
Sales_Canada_Corporate <- aggregate(Canada_Corporate$Sales, by=list(Canada_Corporate$Order.Date), FUN=sum)
Sales_Canada_HO <- aggregate(Canada_HO$Sales, by=list(Canada_HO$Order.Date), FUN=sum)
Sales_EMEA_Consumer <- aggregate(EMEA_Consumer$Sales, by=list(EMEA_Consumer$Order.Date), FUN=sum)
Sales_EMEA_Corporate <- aggregate(EMEA_Corporate$Sales, by=list(EMEA_Corporate$Order.Date), FUN=sum)
Sales_EMEA_HO <- aggregate(EMEA_HO$Sales, by=list(EMEA_HO$Order.Date), FUN=sum)
Sales_EU_Consumer <- aggregate(EU_Consumer$Sales, by=list(EU_Consumer$Order.Date), FUN=sum)
Sales_EU_Corporate <- aggregate(EU_Corporate$Sales, by=list(EU_Corporate$Order.Date), FUN=sum)
Sales_EU_HO <- aggregate(EU_HO$Sales, by=list(EU_HO$Order.Date), FUN=sum)
Sales_LATAM_Consumer <- aggregate(LATAM_Consumer$Sales, by=list(LATAM_Consumer$Order.Date), FUN=sum)
Sales_LATAM_Corporate <- aggregate(LATAM_Corporate$Sales, by=list(LATAM_Corporate$Order.Date), FUN=sum)
Sales_LATAM_HO <- aggregate(LATAM_HO$Sales, by=list(LATAM_HO$Order.Date), FUN=sum)
Sales_US_Consumer <- aggregate(US_Consumer$Sales, by=list(US_Consumer$Order.Date), FUN=sum)
Sales_US_Corporate <- aggregate(US_Corporate$Sales, by=list(US_Corporate$Order.Date), FUN=sum)
Sales_US_HO <- aggregate(US_HO$Sales, by=list(US_HO$Order.Date), FUN=sum)

# 3. Aggregating Quantity over Order Date for each segment created:
Quantity_Africa_Consumer <- aggregate(Africa_Consumer$Quantity, by=list(Africa_Consumer$Order.Date), FUN=sum)
Quantity_Africa_Corporate <- aggregate(Africa_Corporate$Quantity, by=list(Africa_Corporate$Order.Date), FUN=sum)
Quantity_Africa_HO <- aggregate(Africa_HO$Quantity, by=list(Africa_HO$Order.Date), FUN=sum)
Quantity_APAC_Consumer <- aggregate(APAC_Consumer$Quantity, by=list(APAC_Consumer$Order.Date), FUN=sum)
Quantity_APAC_Corporate <- aggregate(APAC_Corporate$Quantity, by=list(APAC_Corporate$Order.Date), FUN=sum)
Quantity_APAC_HO <- aggregate(APAC_HO$Quantity, by=list(APAC_HO$Order.Date), FUN=sum)
Quantity_Canada_Consumer <- aggregate(Canada_Consumer$Quantity, by=list(Canada_Consumer$Order.Date), FUN=sum)
Quantity_Canada_Corporate <- aggregate(Canada_Corporate$Quantity, by=list(Canada_Corporate$Order.Date), FUN=sum)
Quantity_Canada_HO <- aggregate(Canada_HO$Quantity, by=list(Canada_HO$Order.Date), FUN=sum)
Quantity_EMEA_Consumer <- aggregate(EMEA_Consumer$Quantity, by=list(EMEA_Consumer$Order.Date), FUN=sum)
Quantity_EMEA_Corporate <- aggregate(EMEA_Corporate$Quantity, by=list(EMEA_Corporate$Order.Date), FUN=sum)
Quantity_EMEA_HO <- aggregate(EMEA_HO$Quantity, by=list(EMEA_HO$Order.Date), FUN=sum)
Quantity_EU_Consumer <- aggregate(EU_Consumer$Quantity, by=list(EU_Consumer$Order.Date), FUN=sum)
Quantity_EU_Corporate <- aggregate(EU_Corporate$Quantity, by=list(EU_Corporate$Order.Date), FUN=sum)
Quantity_EU_HO <- aggregate(EU_HO$Quantity, by=list(EU_HO$Order.Date), FUN=sum)
Quantity_LATAM_Consumer <- aggregate(LATAM_Consumer$Quantity, by=list(LATAM_Consumer$Order.Date), FUN=sum)
Quantity_LATAM_Corporate <- aggregate(LATAM_Corporate$Quantity, by=list(LATAM_Corporate$Order.Date), FUN=sum)
Quantity_LATAM_HO <- aggregate(LATAM_HO$Quantity, by=list(LATAM_HO$Order.Date), FUN=sum)
Quantity_US_Consumer <- aggregate(US_Consumer$Quantity, by=list(US_Consumer$Order.Date), FUN=sum)
Quantity_US_Corporate <- aggregate(US_Corporate$Quantity, by=list(US_Corporate$Order.Date), FUN=sum)
Quantity_US_HO <- aggregate(US_HO$Quantity, by=list(US_HO$Order.Date), FUN=sum)

# 4. Aggregating Profit over Order Date for each segment created:
Profit_Africa_Consumer <- aggregate(Africa_Consumer$Profit, by=list(Africa_Consumer$Order.Date), FUN=sum)
Profit_Africa_Corporate <- aggregate(Africa_Corporate$Profit, by=list(Africa_Corporate$Order.Date), FUN=sum)
Profit_Africa_HO <- aggregate(Africa_HO$Profit, by=list(Africa_HO$Order.Date), FUN=sum)
Profit_APAC_Consumer <- aggregate(APAC_Consumer$Profit, by=list(APAC_Consumer$Order.Date), FUN=sum)
Profit_APAC_Corporate <- aggregate(APAC_Corporate$Profit, by=list(APAC_Corporate$Order.Date), FUN=sum)
Profit_APAC_HO <- aggregate(APAC_HO$Profit, by=list(APAC_HO$Order.Date), FUN=sum)
Profit_Canada_Consumer <- aggregate(Canada_Consumer$Profit, by=list(Canada_Consumer$Order.Date), FUN=sum)
Profit_Canada_Corporate <- aggregate(Canada_Corporate$Profit, by=list(Canada_Corporate$Order.Date), FUN=sum)
Profit_Canada_HO <- aggregate(Canada_HO$Profit, by=list(Canada_HO$Order.Date), FUN=sum)
Profit_EMEA_Consumer <- aggregate(EMEA_Consumer$Profit, by=list(EMEA_Consumer$Order.Date), FUN=sum)
Profit_EMEA_Corporate <- aggregate(EMEA_Corporate$Profit, by=list(EMEA_Corporate$Order.Date), FUN=sum)
Profit_EMEA_HO <- aggregate(EMEA_HO$Profit, by=list(EMEA_HO$Order.Date), FUN=sum)
Profit_EU_Consumer <- aggregate(EU_Consumer$Profit, by=list(EU_Consumer$Order.Date), FUN=sum)
Profit_EU_Corporate <- aggregate(EU_Corporate$Profit, by=list(EU_Corporate$Order.Date), FUN=sum)
Profit_EU_HO <- aggregate(EU_HO$Profit, by=list(EU_HO$Order.Date), FUN=sum)
Profit_LATAM_Consumer <- aggregate(LATAM_Consumer$Profit, by=list(LATAM_Consumer$Order.Date), FUN=sum)
Profit_LATAM_Corporate <- aggregate(LATAM_Corporate$Profit, by=list(LATAM_Corporate$Order.Date), FUN=sum)
Profit_LATAM_HO <- aggregate(LATAM_HO$Profit, by=list(LATAM_HO$Order.Date), FUN=sum)
Profit_US_Consumer <- aggregate(US_Consumer$Profit, by=list(US_Consumer$Order.Date), FUN=sum)
Profit_US_Corporate <- aggregate(US_Corporate$Profit, by=list(US_Corporate$Order.Date), FUN=sum)
Profit_US_HO <- aggregate(US_HO$Profit, by=list(US_HO$Order.Date), FUN=sum)

# 5. Finding the Mean profit per segment:
Mean_Profit_Africa_Consumer <- mean(Profit_Africa_Consumer$x)
Mean_Profit_Africa_Corporate <- mean(Profit_Africa_Corporate$x)
Mean_Profit_Africa_HO <- mean(Profit_Africa_HO$x)
Mean_Profit_APAC_Consumer <- mean(Profit_APAC_Consumer$x)
Mean_Profit_APAC_Corporate <- mean(Profit_APAC_Corporate$x)
Mean_Profit_APAC_HO <- mean(Profit_APAC_HO$x)
Mean_Profit_Canada_Consumer <- mean(Profit_Canada_Consumer$x)
Mean_Profit_Canada_Corporate <- mean(Profit_Canada_Corporate$x)
Mean_Profit_Canada_HO <- mean(Profit_Canada_HO$x)
Mean_Profit_EMEA_Consumer <- mean(Profit_EMEA_Consumer$x)
Mean_Profit_EMEA_Corporate <- mean(Profit_EMEA_Corporate$x)
Mean_Profit_EMEA_HO <- mean(Profit_EMEA_HO$x)
Mean_Profit_EU_Consumer <- mean(Profit_EU_Consumer$x)
Mean_Profit_EU_Corporate <- mean(Profit_EU_Corporate$x)
Mean_Profit_EU_HO <- mean(Profit_EU_HO$x)
Mean_Profit_LATAM_Consumer <- mean(Profit_LATAM_Consumer$x)
Mean_Profit_LATAM_Corporate <- mean(Profit_LATAM_Corporate$x)
Mean_Profit_LATAM_HO <- mean(Profit_LATAM_HO$x)
Mean_Profit_US_Consumer <- mean(Profit_US_Consumer$x)
Mean_Profit_US_Corporate <- mean(Profit_US_Corporate$x)
Mean_Profit_US_HO <- mean(Profit_US_HO$x)

# 6. Finding the standard deviation for profit per segment:
SD_Profit_Africa_Consumer <- sd(Profit_Africa_Consumer$x)
SD_Profit_Africa_Corporate <- sd(Profit_Africa_Corporate$x)
SD_Profit_Africa_HO <- sd(Profit_Africa_HO$x)
SD_Profit_APAC_Consumer <- sd(Profit_APAC_Consumer$x)
SD_Profit_APAC_Corporate <- sd(Profit_APAC_Corporate$x)
SD_Profit_APAC_HO <- sd(Profit_APAC_HO$x)
SD_Profit_Canada_Consumer <- sd(Profit_Canada_Consumer$x)
SD_Profit_Canada_Corporate <- sd(Profit_Canada_Corporate$x)
SD_Profit_Canada_HO <- sd(Profit_Canada_HO$x)
SD_Profit_EMEA_Consumer <- sd(Profit_EMEA_Consumer$x)
SD_Profit_EMEA_Corporate <- sd(Profit_EMEA_Corporate$x)
SD_Profit_EMEA_HO <- sd(Profit_EMEA_HO$x)
SD_Profit_EU_Consumer <- sd(Profit_EU_Consumer$x)
SD_Profit_EU_Corporate <- sd(Profit_EU_Corporate$x)
SD_Profit_EU_HO <- sd(Profit_EU_HO$x)
SD_Profit_LATAM_Consumer <- sd(Profit_LATAM_Consumer$x)
SD_Profit_LATAM_Corporate <- sd(Profit_LATAM_Corporate$x)
SD_Profit_LATAM_HO <- sd(Profit_LATAM_HO$x)
SD_Profit_US_Consumer <- sd(Profit_US_Consumer$x)
SD_Profit_US_Corporate <- sd(Profit_US_Corporate$x)
SD_Profit_US_HO <- sd(Profit_US_HO$x)

# 7. Finding Coefficient of Variation of profits for the segments:
CV_Profit_Africa_Consumer <- SD_Profit_Africa_Consumer/Mean_Profit_Africa_Consumer
CV_Profit_Africa_Corporate <- SD_Profit_Africa_Corporate/Mean_Profit_Africa_Corporate
CV_Profit_Africa_HO <- SD_Profit_Africa_HO/Mean_Profit_Africa_HO
CV_Profit_APAC_Consumer <- SD_Profit_APAC_Consumer/Mean_Profit_APAC_Consumer
CV_Profit_APAC_Corporate <- SD_Profit_APAC_Corporate/Mean_Profit_APAC_Corporate
CV_Profit_APAC_HO <- SD_Profit_APAC_HO/Mean_Profit_APAC_HO
CV_Profit_Canada_Consumer <- SD_Profit_Canada_Consumer/Mean_Profit_Canada_Consumer
CV_Profit_Canada_Corporate <- SD_Profit_Canada_Corporate/Mean_Profit_Canada_Corporate
CV_Profit_Canada_HO <- SD_Profit_Canada_HO/Mean_Profit_Canada_HO
CV_Profit_EMEA_Consumer <- SD_Profit_EMEA_Consumer/Mean_Profit_EMEA_Consumer
CV_Profit_EMEA_Corporate <- SD_Profit_EMEA_Corporate/Mean_Profit_EMEA_Corporate
CV_Profit_EMEA_HO <- SD_Profit_EMEA_HO/Mean_Profit_EMEA_HO
CV_Profit_EU_Consumer <- SD_Profit_EU_Consumer/Mean_Profit_EU_Consumer
CV_Profit_EU_Corporate <- SD_Profit_EU_Corporate/Mean_Profit_EU_Corporate
CV_Profit_EU_HO <- SD_Profit_EU_HO/Mean_Profit_EU_HO
CV_Profit_LATAM_Consumer <- SD_Profit_LATAM_Consumer/Mean_Profit_LATAM_Consumer
CV_Profit_LATAM_Corporate <- SD_Profit_LATAM_Corporate/Mean_Profit_LATAM_Corporate
CV_Profit_LATAM_HO <- SD_Profit_LATAM_HO/Mean_Profit_LATAM_HO
CV_Profit_US_Consumer <- SD_Profit_US_Consumer/Mean_Profit_US_Consumer
CV_Profit_US_Corporate <- SD_Profit_US_Corporate/Mean_Profit_US_Corporate
CV_Profit_US_HO <- SD_Profit_US_HO/Mean_Profit_US_HO

# 8. Finding the 2 most profitable segments based on coefficient of variation of profits:
Average_Profits <- c(Mean_Profit_US_HO,Mean_Profit_US_Corporate,Mean_Profit_US_Consumer,
                     Mean_Profit_LATAM_HO,Mean_Profit_LATAM_Corporate,Mean_Profit_LATAM_Consumer,
                     Mean_Profit_EU_HO,Mean_Profit_EU_Corporate,Mean_Profit_EU_Consumer,
                     Mean_Profit_EMEA_HO,Mean_Profit_EMEA_Corporate,Mean_Profit_EMEA_Consumer,
                     Mean_Profit_Canada_Consumer,Mean_Profit_Canada_Corporate,Mean_Profit_Canada_HO,
                     Mean_Profit_Africa_Consumer,Mean_Profit_Africa_Corporate,Mean_Profit_Africa_HO,
                     Mean_Profit_APAC_Consumer,Mean_Profit_APAC_Corporate,Mean_Profit_APAC_HO)

CV_Profits <- c(CV_Profit_US_HO,CV_Profit_US_Corporate,CV_Profit_US_Consumer,
                CV_Profit_LATAM_HO,CV_Profit_LATAM_Corporate,CV_Profit_LATAM_Consumer,
                CV_Profit_EU_HO,CV_Profit_EU_Corporate,CV_Profit_EU_Consumer,
                CV_Profit_EMEA_HO,CV_Profit_EMEA_Corporate,CV_Profit_EMEA_Consumer,
                CV_Profit_Canada_Consumer,CV_Profit_Canada_Corporate,CV_Profit_Canada_HO,
                CV_Profit_Africa_Consumer,CV_Profit_Africa_Corporate,CV_Profit_Africa_HO,
                CV_Profit_APAC_Consumer,CV_Profit_APAC_Corporate,CV_Profit_APAC_HO)
Average_Profits
CV_Profits
# Coefficient of variation gives us the dispersion of values around the mean. Higher the number, higher the dispersion.
# We need to find 2 segments that give us high and stable profits.
# This means, average profits must be high and the coefficient of variation must be low.
# Segments with highest average profits are 'APAC_Consumer' and 'EU_Consumer'.
# For these segments, are 2.13 and 2.40 respectively, which is relatively low.
# This tells us that APAC Consumer and EU Consumer generate high and consistent profits.
# Giong forward, these 2 segments will be used for analysis.

##################################################################################################################################################################################

# TIME SERIES ANALYSIS

##################################################################################################################################################################################

#Aggregating the data w.r.t month for segment APAC-Consmer

APCO_Mon <- transform(APAC_Consumer,Order.Month=format(as.Date(APAC_Consumer$Order.Date),"%Y%m"))
APCO_AGG <- aggregate(cbind(APCO_Mon$Sales,APCO_Mon$Quantity),by=list(APCO_Mon$Order.Month),FUN=sum)
names(APCO_AGG)<- list("Order.Month","Sales","Quantity")

#Aggregating the data w.r.t month for segment EU-Consmer

EUCO_Mon <- transform(EU_Consumer,Order.Month=format(as.Date(EU_Consumer$Order.Date),"%Y%m"))
EUCO_AGG <- aggregate(cbind(EUCO_Mon$Sales,EUCO_Mon$Quantity),by=list(EUCO_Mon$Order.Month),FUN=sum)
names(EUCO_AGG)<- list("Order.Month","Sales","Quantity")


#Taking first 42 months to Model the data.Last 6 months will be used for Model evaluation

APCO_AGG_Mod<-APCO_AGG[1:42,]
EUCO_AGG_Mod<-APCO_AGG[1:42,]

xlabel<-c("Months from Jan 2011")

##  Time Series for Sales in APAC-Consumer Segment 
ylabelAPCOS<-c("Sales for APAC-Consumer")

TS_APCO_Sales<-ts(APCO_AGG_Mod$Sales)
plot(TS_APCO_Sales,xlab = xlabel,ylab= ylabelAPCOS)


##  Time Series for Quantity in APAC-Consumer Segment 
ylabelAPCOQ<-c("Quantity for APAC-Consumer")

TS_APCO_Quantity<-ts(APCO_AGG_Mod$Quantity)

plot(TS_APCO_Quantity,xlab = xlabel,ylab= ylabelAPCOQ)

##  Time Series for Sales in EU-Consumer Segment 
ylabelEUCOS<-c("Sales for EU-Consumer")

TS_EUCO_Sales<-ts(EUCO_AGG_Mod$Sales)
plot(TS_EUCO_Sales,xlab = xlabel,ylab= ylabelEUCOS)

##  Time Series for Quantity in EU-Consumer Segment 
ylabelEUCOQ<-c("Quantity for EU-Consumer")
TS_EUCO_Quantity<-ts(EUCO_AGG_Mod$Quantity)
plot(TS_EUCO_Quantity,xlab = xlabel,ylab= ylabelEUCOQ)


#***************Time Series Modeling for Sales in APAC-Consumer Segment************

#Smoothing using Exponential menthod


plot(TS_APCO_Sales,xlab = xlabel,ylab= ylabelAPCOS)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.1, 0.5, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(TS_APCO_Sales, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

## Picking up alpha = 0.5 from the observations as it has the trend and is smoothed properly

smoothedseries_APCOS<-HoltWinters(TS_APCO_Sales, alpha=0.5,
                                  beta=FALSE, gamma=FALSE)

fitted(smoothedseries_APCOS)[,1]

plot(smoothedseries_APCOS)

Mon_APCOS<-APCO_AGG_Mod$Order.Month

#Building a model on the smoothed time series using classical decomposition
#Toconvert the time series to a dataframe
#As first observation in actual data ins equal to secnd observation insmoothed data,ignoring the first month
smootheddf_APCOS <- as.data.frame(cbind(Mon_APCOS[2:42], as.vector(fitted(smoothedseries_APCOS)[,1])))

colnames(smootheddf_APCOS)<-c("Month","Sales")



#Trying to fit a linear Model

lmfit_APCOS <- lm(Sales ~ Month, data=smootheddf_APCOS)
lmfit_APCOS
global_pred_APCOS <- predict(lmfit_APCOS, Month=Mon_APCOS[2:42])
global_pred_APCOS
summary(global_pred_APCOS)
lines(Mon_APCOS[2:42], global_pred_APCOS, col='red', lwd=2)

#lmfit_APCOS has the global predictable data
#Now, Modeling the locally predictable series as an ARMA series

local_pred_ACPOS <- fitted(smoothedseries_APCOS)[,1]-global_pred_APCOS
plot(local_pred_ACPOS, col='red', type = "l")
#ACF Plot
acf(local_pred_ACPOS)
#As the ACF plot has values crossing the confidence intervals,this series is not white noise
#Hence plotting the ARMA Model
armafit_ACPOS <- auto.arima(local_pred_ACPOS)

tsdiag(armafit_ACPOS)
armafit_ACPOS
#The local predicted model is ARIMA(2,0,1)


#Verify if the residual series is white noise

resi_ACPOS <- local_pred_ACPOS-fitted(armafit_ACPOS)

adf.test(resi_ACPOS,alternative = "stationary")

#p value in adf test is close to 0.05. Hence the residue is stationery
kpss.test(resi_ACPOS)
#Also p value in kpss test is 0.1>0.05. Hence the residue is stationery





#Building a model on the smoothed time series using ARIMA

autoarima_APCOS <- auto.arima(fitted(smoothedseries_APCOS)[,1])
autoarima_APCOS
tsdiag(autoarima_APCOS)
# The AIC and BIC values are lower than classical decomposition,hence the autoarima model is better
plot(autoarima_APCOS$x, col="black")
lines(fitted(autoarima_APCOS), col="red")

#Test if the residual series is white noise

resi_auto_arima_apcos <- fitted(smoothedseries_APCOS)[,1] - fitted(autoarima_APCOS)

adf.test(resi_auto_arima_apcos,alternative = "stationary")
kpss.test(resi_auto_arima_apcos)

#ADF test and kpss test say that the residue is stationary.



#*****Model Evaluation for Sales of APAC Consumer*****


# last 6 rows in APCO_AGG_Mod are available for Model evaluation


APCO_AGG_TEST<-APCO_AGG[43:48,]

# Accuracy Test for Classical decomposition 
MAPE_APCOS_TEST_C<-accuracy(global_pred_APCOS+fitted(armafit_ACPOS),APCO_AGG_TEST[,2])[5]
MAPE_APCOS_TEST_C  #65.4

# Accuracy Test for Auto Arima  
MAPE_APCOS_TEST_AA<-accuracy(global_pred_APCOS+fitted(autoarima_APCOS),APCO_AGG_TEST[,2])[5]
MAPE_APCOS_TEST_AA  #34.05

# The accuracy for Autoarima model is better than classical decomposition model
# Hence we choose autoarima model in terms of predictions and AIC,BIC values.


#*******************************************


#***************Time Series Modeling for Quantity in APAC-Consumer Segment************

#Smoothing using Exponential menthod


plot(TS_APCO_Quantity,xlab = xlabel,ylab= ylabelAPCOQ)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.1, 0.4, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(TS_APCO_Quantity, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

## Picking up alpha = 0.4 from the observations as it has the trend and is smoothed properly

smoothedseries_APCOQ<-HoltWinters(TS_APCO_Quantity, alpha=0.4,
                                  beta=FALSE, gamma=FALSE)

fitted(smoothedseries_APCOQ)[,1]

plot(smoothedseries_APCOQ)

Mon_APCOQ<-APCO_AGG_Mod$Order.Month

#Building a model on the smoothed time series using classical decomposition
#Toconvert the time series to a dataframe
#As first observation in actual data ins equal to secnd observation insmoothed data,ignoring the first month
smootheddf_APCOQ <- as.data.frame(cbind(Mon_APCOQ[2:42], as.vector(fitted(smoothedseries_APCOQ)[,1])))

colnames(smootheddf_APCOQ)<-c("Month","Quantity")



#Trying to fit a linear Model

lmfit_APCOQ <- lm(Quantity ~ Month, data=smootheddf_APCOQ)
lmfit_APCOQ
global_pred_APCOQ <- predict(lmfit_APCOQ, Month=Mon_APCOQ[2:42])
global_pred_APCOQ
summary(global_pred_APCOQ)
lines(Mon_APCOQ[2:42], global_pred_APCOQ, col='red', lwd=2)

#lmfit_APCOQ has the global predictable data
#Now, Modeling the locally predictable series as an ARMA series

local_pred_APCOQ <- fitted(smoothedseries_APCOQ)[,1]-global_pred_APCOQ
plot(local_pred_APCOQ, col='red', type = "l")
#ACF Plot
acf(local_pred_APCOQ)
#As the ACF plot has values crossing the confidence intervals,this series is not white noise
#Hence plotting the ARMA Model
armafit_APCOQ <- auto.arima(local_pred_APCOQ)

tsdiag(armafit_APCOQ)
armafit_APCOQ
#The local predicted model is ARIMA(2,0,1)


#Verify if the residual series is white noise

resi_APCOQ <- local_pred_APCOQ-fitted(armafit_APCOQ)

adf.test(resi_APCOQ,alternative = "stationary")

#p value in adf test is 0.02< 0.05. Hence the residue is stationery
kpss.test(resi_APCOQ)
#Also p value in kpss test is 0.1>0.05. Hence the residue is stationery





#Building a model on the smoothed time series using ARIMA

autoarima_APCOQ <- auto.arima(fitted(smoothedseries_APCOQ)[,1])
autoarima_APCOQ
tsdiag(autoarima_APCOQ)
# The AIC and BIC values are higher than classical decomposition,hence the classical model is better
plot(autoarima_APCOQ$x, col="black")
lines(fitted(autoarima_APCOQ), col="red")

#Test if the residual series is white noise

resi_auto_arima_APCOQ <- fitted(smoothedseries_APCOQ)[,1] - fitted(autoarima_APCOQ)

adf.test(resi_auto_arima_APCOQ,alternative = "stationary")
kpss.test(resi_auto_arima_APCOQ)

#ADF test and kpss test say that the residue is stationary.


#*****Model Evaluation for Quantity of APAC Consumer*****

APCO_AGG_TEST<-APCO_AGG[43:48,]

# Accuracy Test for Classical decomposition 
MAPE_APCOQ_TEST_C<-accuracy(global_pred_APCOQ+fitted(armafit_APCOQ),APCO_AGG_TEST[,3])[5]
MAPE_APCOQ_TEST_C  #65.6

# Accuracy Test for Auto Arima  
MAPE_APCOQ_TEST_AA<-accuracy(global_pred_APCOQ+fitted(autoarima_APCOQ),APCO_AGG_TEST[,3])[5]
MAPE_APCOQ_TEST_AA  #38.05

# The accuracy for Autoarima model is better than classical decomposition model
# Hence we choose autoarima model in terms of predictions and AIC,BIC values.










#*******************************************




#***************Time Series Modeling for Sales in EU-Consumer Segment************

#Smoothing using Exponential menthod


plot(TS_EUCO_Sales,xlab = xlabel,ylab= ylabelEUCOS)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.1, 0.4, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(TS_EUCO_Sales, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

## Picking up alpha = 0.4 from the observations as it has the trend and is smoothed properly

smoothedseries_EUCOS<-HoltWinters(TS_EUCO_Sales, alpha=0.4,
                                  beta=FALSE, gamma=FALSE)

fitted(smoothedseries_EUCOS)[,1]

plot(smoothedseries_EUCOS)

Mon_EUCOS<-EUCO_AGG_Mod$Order.Month

#Building a model on the smoothed time series using classical decomposition
#Toconvert the time series to a dataframe
#As first observation in actual data ins equal to secnd observation insmoothed data,ignoring the first month
smootheddf_EUCOS <- as.data.frame(cbind(Mon_EUCOS[2:42], as.vector(fitted(smoothedseries_EUCOS)[,1])))

colnames(smootheddf_EUCOS)<-c("Month","Sales")



#Trying to fit a linear Model

lmfit_EUCOS <- lm(Sales ~ Month, data=smootheddf_EUCOS)
lmfit_EUCOS
global_pred_EUCOS <- predict(lmfit_EUCOS, Month=Mon_EUCOS[2:42])
global_pred_EUCOS
summary(global_pred_EUCOS)
lines(Mon_EUCOS[2:42], global_pred_EUCOS, col='red', lwd=2)

#lmfit_EUCOS has the global predictable data
#Now, Modeling the locally predictable series as an ARMA series

local_pred_EUCOS <- fitted(smoothedseries_EUCOS)[,1]-global_pred_EUCOS
plot(local_pred_EUCOS, col='red', type = "l")
#ACF Plot
acf(local_pred_EUCOS)
#As the ACF plot has values crossing the confidence intervals,this series is not white noise
#Hence plotting the ARMA Model
armafit_EUCOS <- auto.arima(local_pred_EUCOS)

tsdiag(armafit_EUCOS)
armafit_EUCOS
#The local predicted model is ARIMA(2,0,1)


#Verify if the residual series is white noise

resi_EUCOS <- local_pred_EUCOS-fitted(armafit_EUCOS)

adf.test(resi_EUCOS,alternative = "stationary")

#p value in adf test is close to 0.05. Hence the residue is stationery
kpss.test(resi_EUCOS)
#Also p value in kpss test is 0.1>0.05. Hence the residue is stationery





#Building a model on the smoothed time series using ARIMA

autoarima_EUCOS <- auto.arima(fitted(smoothedseries_EUCOS)[,1])
autoarima_EUCOS
tsdiag(autoarima_EUCOS)
# The AIC and BIC values are higher than classical decomposition,hence the classical model is better
plot(autoarima_EUCOS$x, col="black")
lines(fitted(autoarima_EUCOS), col="red")

#Test if the residual series is white noise

resi_auto_arima_EUCOS <- fitted(smoothedseries_EUCOS)[,1] - fitted(autoarima_EUCOS)

adf.test(resi_auto_arima_EUCOS,alternative = "stationary")
kpss.test(resi_auto_arima_EUCOS)

#ADF test and kpss test say that the residue is stationary.


#*****Model Evaluation for Sales of EU Consumer*****

EUCO_AGG_TEST<-EUCO_AGG[43:48,]

# Accuracy Test for Classical decomposition 
MAPE_EUCOS_TEST_C<-accuracy(global_pred_EUCOS+fitted(armafit_EUCOS),EUCO_AGG_TEST[,2])[5]
MAPE_EUCOS_TEST_C  #61.1

# Accuracy Test for autoarima  
MAPE_EUCOS_TEST_AA<-accuracy(global_pred_EUCOS+fitted(autoarima_EUCOS),EUCO_AGG_TEST[,2])[5]
MAPE_EUCOS_TEST_AA  #27.9

# The accuracy for Autoarima model is better than classical decomposition model
# Hence we choose autoarima model in terms of predictions and AIC,BIC values.










#*******************************************





#***************Time Series Modeling for Quantity in EU-Consumer Segment************

#Smoothing using Exponential menthod


plot(TS_EUCO_Quantity,xlab = xlabel,ylab= ylabelEUCOQ)

cols <- c("red", "blue", "green", "black")
alphas <- c(0.1, 0.4, 0.8)
labels <- c(paste("alpha =", alphas), "Original")
for (i in seq(1,length(alphas))) {
  smoothedseries <- HoltWinters(TS_EUCO_Quantity, alpha=alphas[i],
                                beta=FALSE, gamma=FALSE)
  
  lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
}

legend("bottomleft", labels, col=cols, lwd=2)

## Picking up alpha = 0.4 from the observations as it has the trend and is smoothed properly

smoothedseries_EUCOQ<-HoltWinters(TS_EUCO_Quantity, alpha=0.4,
                                  beta=FALSE, gamma=FALSE)

fitted(smoothedseries_EUCOQ)[,1]

plot(smoothedseries_EUCOQ)

Mon_EUCOQ<-EUCO_AGG_Mod$Order.Month

#Building a model on the smoothed time series using classical decomposition
#Toconvert the time series to a dataframe
#As first observation in actual data ins equal to secnd observation insmoothed data,ignoring the first month
smootheddf_EUCOQ <- as.data.frame(cbind(Mon_EUCOQ[2:42], as.vector(fitted(smoothedseries_EUCOQ)[,1])))

colnames(smootheddf_EUCOQ)<-c("Month","Quantity")



#Trying to fit a linear Model

lmfit_EUCOQ <- lm(Quantity ~ Month, data=smootheddf_EUCOQ)
lmfit_EUCOQ
global_pred_EUCOQ <- predict(lmfit_EUCOQ, Month=Mon_EUCOQ[2:42])
global_pred_EUCOQ
summary(global_pred_EUCOQ)
lines(Mon_EUCOQ[2:42], global_pred_EUCOQ, col='red', lwd=2)

#lmfit_EUCOQ has the global predictable data
#Now, Modeling the locally predictable series as an ARMA series

local_pred_EUCOQ <- fitted(smoothedseries_EUCOQ)[,1]-global_pred_EUCOQ
plot(local_pred_EUCOQ, col='red', type = "l")
#ACF Plot
acf(local_pred_EUCOQ)
#As the ACF plot has values crossing the confidence intervals,this series is not white noise
#Hence plotting the ARMA Model
armafit_EUCOQ <- auto.arima(local_pred_EUCOQ)

tsdiag(armafit_EUCOQ)
armafit_EUCOQ
#The local predicted model is ARIMA(2,0,1)


#Verify if the residual series is white noise

resi_EUCOQ <- local_pred_EUCOQ-fitted(armafit_EUCOQ)

adf.test(resi_EUCOQ,alternative = "stationary")

#p value in adf test is 0.02< 0.05. Hence the residue is stationery
kpss.test(resi_EUCOQ)
#Also p value in kpss test is 0.1>0.05. Hence the residue is stationery





#Building a model on the smoothed time series using ARIMA

autoarima_EUCOQ <- auto.arima(fitted(smoothedseries_EUCOQ)[,1])
autoarima_EUCOQ
tsdiag(autoarima_EUCOQ)
# The AIC and BIC values are higher than classical decomposition,hence the classical model is better
plot(autoarima_EUCOQ$x, col="black")
lines(fitted(autoarima_EUCOQ), col="red")

#Test if the residual series is white noise

resi_auto_arima_EUCOQ <- fitted(smoothedseries_EUCOQ)[,1] - fitted(autoarima_EUCOQ)

adf.test(resi_auto_arima_EUCOQ,alternative = "stationary")
kpss.test(resi_auto_arima_EUCOQ)

#ADF test and kpss test say that the residue is stationary.


#*****Model Evaluation for Quantity of EU Consumer*****

# Accuracy Test for Classical decomposition 
MAPE_EUCOQ_TEST_C<-accuracy(global_pred_EUCOQ+fitted(armafit_EUCOQ),EUCO_AGG_TEST[,3])[5]
MAPE_EUCOQ_TEST_C  #63.6

# Accuracy Test for autoarima decomposition 
MAPE_EUCOQ_TEST_AA<-accuracy(global_pred_EUCOQ+fitted(autoarima_EUCOQ),EUCO_AGG_TEST[,3])[5]
MAPE_EUCOQ_TEST_AA  #31.7

# The accuracy for Autoarima model is better than classical decomposition model
# Hence we choose autoarima model in terms of predictions and AIC,BIC values.










#*******************************************



# **********Prediction for next 6 months************

# Sales in APAC Consumer

fcast_APCOS <- predict(autoarima_APCOS, n.ahead = 6)

AA_pred_APCOS <- c(fitted(autoarima_APCOS),ts(fcast_APCOS$pred))
plot(TS_APCO_Sales, col = "black")
lines(AA_pred_APCOS, col = "red")
# Predicted values in Red

# Quantity in APAC Consumer

fcast_APCOQ <- predict(autoarima_APCOQ, n.ahead = 6)

AA_pred_APCOQ <- c(fitted(autoarima_APCOQ),ts(fcast_APCOQ$pred))
plot(TS_APCO_Quantity, col = "black")
lines(AA_pred_APCOQ, col = "red")
# Predicted values in Red


# Sales in EU Consumer

fcast_EUCOS <- predict(autoarima_EUCOS, n.ahead = 6)

AA_pred_EUCOS <- c(fitted(autoarima_EUCOS),ts(fcast_EUCOS$pred))
plot(TS_EUCO_Sales, col = "black")
lines(AA_pred_EUCOS, col = "red")
# Predicted values in Red



# Quantity in EU Consumer

fcast_EUCOQ <- predict(autoarima_EUCOQ, n.ahead = 6)

AA_pred_EUCOQ <- c(fitted(autoarima_EUCOQ),ts(fcast_EUCOQ$pred))
plot(TS_EUCO_Quantity, col = "black")
lines(AA_pred_EUCOQ, col = "red")
# Predicted values in Red


