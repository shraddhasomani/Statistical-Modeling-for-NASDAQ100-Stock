library(quantmod)
library("lubridate")
library("e1071")

# get market data for all symbols making up the NASDAQ 100 Index
Stocks <- c("MAT","SNE","AAPL","AMGN","MAR")
getSymbols(Stocks,from = '2012-01-01', to = '2017-04-17'  )



# merge them all together
stocks <- data.frame(as.xts(merge(AMGN,SNE,AAPL,MAT,MAR)))
head(stocks[,1:12],2)


# set outcome variable
outcomeSymbol <- 'MAR.Close'

#We are trying to determine whether the close price of 'Apple' stock is higher or 
#lower than the current trading day


# shift outcome value to be on same line as predictors
library(xts)
stocks <- xts(stocks,order.by=as.Date(rownames(stocks)))
stocks <- as.data.frame(merge(stocks, lm1=lag(stocks[,outcomeSymbol],-1)))
stocks$outcome <- ifelse(stocks[,paste0(outcomeSymbol,'.1')] > stocks[,outcomeSymbol], 1, 0)


# We're trying to predict down one trading day using the lag function. 
#This will add the close field of our outcome symbol with a lag of 1 trading day so it's 
#on the same line as the predictors. We will rely on this value for training and testing purposes.
#A value of 1 means the close price went up, and a 0, that it went down:
#As we took the outcomeSymbol as close price of Apple stock we can interpret the output in terms of high and low where 1 being high and 0 being low.





# remove shifted down close field as we don't care by the value
stocks <- stocks[,!names(stocks) %in% c(paste0(outcomeSymbol,'.1'))]
head(stocks)


#Cast the date field to type date as it currently is of type character and sort by decreasing order
# cast date to true date and order in decreasing order
stocks$date <- as.Date(row.names(stocks))
stocks <- stocks[order(as.Date(stocks$date, "%m/%d/%Y"), decreasing = TRUE),]



#Here is the pattern maker function. This will take our raw market data and scale it so 
#that we can compare any symbol with any other symbol. 
#It then subtracts the different day ranges requested by the days parameter 
#using the diff and lag calls and puts them all on the same row along with the outcome. 
#To make things even more compatible, the roundByScaler parameter can round results.
# calculate all day differences and populate them on same row
Difference_in_Days <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}


#Call the function with the following differences and scale it down to 2 decimal points (this takes a little while to run):
# call the function with the following differences
stocks <- Difference_in_Days(stocks, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)



#We delete the last row from our data frame as it doesn't have an outcome variable (that is in the future):
# drop most recent entry as we don't have an outcome
stocks <- stocks[2:nrow(stocks),]




#We extract the day of the week, day of the month, day of the year as predictors using POSIXlt:
# well use POSIXlt to add day of the week, day of the month, day of the year
stocks$wday <- as.POSIXlt(stocks$date)$wday
stocks$yday <- as.POSIXlt(stocks$date)$mday
stocks$mon<- as.POSIXlt(stocks$date)$mon


#Next we remove the date field as it won't help us as a predictor 
#as they are all unique and we shuffle the data set using the sample function:
# remove date field and shuffle data frame
stocks <- subset(stocks, select=-c(date))
stocks <- stocks[sample(nrow(stocks)),]

# let's model
#We'll use a simple gbm model to get an AUC score. 
#You can get more details regarding parameter settings for this model at the xgboost wiki:
library(caret)
#{caret} - modeling wrapper, functions, commands

predictorNames <- names(stocks)[names(stocks) != 'outcome']
head(predictorNames)
set.seed(1234)
split <- sample(nrow(stocks), floor(0.7*nrow(stocks)))
train <-stocks[split,]
test <- stocks[-split,]




# gbm supports both regression and classification. 
#As this is a binary classification, we need to force gbm 
#into using the classification mode. We do this by changing the 
#outcome variable to a factor: 
train$outcome <- ifelse(train$outcome==1,'yes','nope')
head(train$outcome)

#Caret offers many tuning functions to help you get as much as possible out of your models; 
#the trainControl function allows you to control the resampling of your data. 
#This will split the training data set internally and do it's own train/test runs 
#to figure out the best settings for your model.
#In this case, we're going to cross-validate the data 2 times, therefore training it 2 times on different portions of the data before settling on the best tuning parameters (for gbm it is trees, shrinkage, and interaction depth).
#You can also set these values yourself if you don't trust the function.


# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=2, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)


#This is the heart of our modeling adventure, time to teach our model how to 
#recognize closing price of Stocks. Because this is a classification model, 
#we're requesting that our metrics use ROC instead of the default RMSE:

# run model
bst <- train(train[,predictorNames],  as.factor(train$outcome),
             method='gbm',
             trControl=objControl,
             metric = "ROC",
             tuneGrid = expand.grid(n.trees = 5, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 1)
)





print(bst)


#There are two types of evaluation we can do here, raw or prob. 
#Raw gives you a class prediction, in our case yes and nope, 
#while prob gives you the probability on how sure the model is about it's choice. 
#I always use prob, as I like to be in control of the threshold and 
#also like to use AUC score which requires probabilities, not classes. 
#There are situations where having class values can come in handy, 
#such as with multinomial models where you're predicting more than two values.



  

library(pROC)

#{pROC} - Area Under the Curve (AUC) functions
predictions <- predict(object=bst, train[,predictorNames], type='prob')
head(predictions)
auc <- auc(train$outcome,predictions[[2]])
print(paste('AUC score:', auc))
# AUC of ROC is a better measure than accuracy
# AUC as a criteria for comparing learning algorithms
# AUC of 0.649285290455624 for very little work (remember that an AUC ranges between 0.5 and 1, 
#where 0.5 is random and 1 is perfect). Hopefully this will pique your 
#imagination with the many possibilities of quantmod and R. 


# AUC replaces accuracy when comparing classifiers
# Experimental results show AUC indicates a difference
#in performance between decision trees and Naive
#Bayes (significantly better)