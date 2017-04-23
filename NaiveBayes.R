## Import data 
library("quantmod")
library("lubridate")
library("e1071")
library(xts)


## Set the start and end date of the data to be considered
start_Date = as.Date("2010-01-01")
end_Date = as.Date("2017-04-18")


## Get market data for all symbols making up the NASDAQ 100 Index
Nasdaq100_Symbols <- c("MAT","NFLX","AAPL","QCOM","MAR")
getSymbols(Nasdaq100_Symbols, from = start_Date, to = end_Date) 
nasdaq100 <- data.frame(as.xts(merge(MAT,NFLX,AAPL,QCOM,MAR)))
head(nasdaq100)


## Set outcome variable
outcomeSymbol <- 'AAPL.Close'
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

## Delete the last row from our data frame as it doesn't have an outcome variable (that is in the future): drop most recent entry as we don't have an outcome
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]
dput(names(nasdaq100)[grepl('MAT',names(nasdaq100))])

## Retrieving Apple's daily OHLCV from Yahoo Finance  
nasdaq100$wday  <- as.POSIXlt(nasdaq100$date)$wday


## Find the day of the week
head(nasdaq100)
PriceDiff <- (nasdaq100$NFLX.Close) - (nasdaq100$NFLX.Open)


## Find the difference between the close price and open price
TrendClass <- ifelse(PriceDiff>0,"UP","DOWN")


## Convert to a binary classification. (In our data set, there are no bars with an exactly 0 price change so, for simplicity sake, we will not address bars that had the same open and close price.)
D1 <- data.frame(nasdaq100$wday,TrendClass)


## Create our data set
M1 <- naiveBayes(D1[,1],D1[,2])
M1

#Analysis:
#- The input variable is (D1,1]) which is the independent variable, and the dependent variable is (D1 [,2]) is what we are trying to predict.
#- Obviously you are going to want a slightly more sophisticated strategy than just looking at the day of the week. Lets now add a moving average cross to our model. (You can get some more information on adding other indicators, or features, to your model here.)
#- I prefer using exponential moving averages, so lets look at a 5-period and 10-period exponential moving average (EMA) cross.
#- First, we need to calculate the EMAs:
#- We are calculating a 5-period EMA off the open price
#- Then the 10-period EMA, also off the open price
#- Then calculate the cross

MA5 <- EMA(nasdaq100$NFLX.Open,n = 5)
MA10 <- EMA(nasdaq100$NFLX.Open,n = 10)
MACross <- MA5 - MA10 

#- Positive values correspond to the 5-period EMA being above the 10-period EMA
#- By rounding to 2 decimal places, we greatly mitigate this risk as a large enough dataset it should have seen most values of the indicator. This is an important limitation to remember when building your own models.

MACross <- round(MACross,2)

# Lets create a new dataset and split it into a training and test set so we are able to see how well our model does over new data

D2 <- data.frame(nasdaq100$wday,MACross, TrendClass)
D2 <- D2[-c(1:10),]
# We need to remove the instances where the 10-period moving average is still being calculated
TrainingData <- D2[1:1216,]
TestData <- D2[1217:1823,] 
MACrossModel <- naiveBayes(TrainingData[,1:2],TrainingData[,3])
MACrossModel

# The Conditional Probability of the EMA Cross, a numeric variable, shows the mean value for each case ([,1]), and the standard deviation ([,2]). We can see that the mean difference between the 5-period EMA and 10-period EMA for long and short trades was 0.54 and - 0.24, respectively.

table(predict(MACrossModel,TestData),TestData[,3],dnn=list('predicted','actual'))