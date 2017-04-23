# Install Packages
library(quantmod)
library(fpp)
library(lattice)
library(timeSeries)
library(rugarch)
library(fUnitRoots)
library(stats)

# Get the data
getSymbols("^NDX", from="2012-01-01")
NDX[1:10]
dim(NDX)


# Chart for NASDAQ100 data
chartSeries(NDX, TA = NULL)


# Daily log return
ndxReturns = dailyReturn(NDX, type = "log")
head(ndxReturns)
length(ndxReturns)

# Remove missing values if any
ndxReturns = na.omit(ndxReturns)
length(ndxReturns)

# ACF and PACF plot for log return
par(mfcol = c(2,1))
acf(ndxReturns)
pacf(ndxReturns)

# Box-test 
Box.test(ndxReturns, type = "Ljung-Box")

# Analysis:
# - From ACF and PACF plot we can see significant trend and seasonality 
# - From the box test we can conclude that, the data are not independently distributed; they exhibit serial correlation since the p-value is greater than 0.05


# Create forecast vector
winLength = 500
forecastLength = length(ndxReturns) - winLength
forecastLength
forecasts = vector(mode = "character", length = forecastLength)
length(forecasts)


# Fit ARIMA model
fitarima = auto.arima(ndxReturns)
summary(fitarima)
orderarima = c(length(fitarima$model$phi), 0, length(fitarima$model$theta))
orderarima


# Fit GARCH model
for (i in 0:forecastLength) {
  ndxWinReturns = ndxReturns[(1+i):(winLength + i)]
  
  # Fit GARCH
  ##--- Method for creating a univariate GARCH specification object prior to fitting. It allows a wide choice in univariate GARCH models , distributions and mean equation modelling---##
  ##--- Standard specification referred from : https://www.rdocumentation.org/packages/rugarch/versions/1.3-6/topics/ugarchspec-methods ---## 
  
  specification = ugarchspec(variance.model = list(garchOrder=c(1,1)),
                             mean.model = list(armaOrder = c(length(fitarima$model$phi), length(fitarima$model$theta)), include.mean=T),
                             distribution.model="sged")
  
  fitgarch = tryCatch(ugarchfit(specification, ndxWinReturns, solver = 'hybrid'), 
                      error=function(e) e, warning=function(w) w)
  
  if(is(fitgarch, "warning")) {
    forecasts[i+1] = paste(index(ndxWinReturns[winLength]), 1, sep="|")
  } else {
    fore = ugarchforecast(fitgarch, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[i+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep="|")
  }    
}

df = data.frame(do.call('rbind', strsplit(as.character(forecasts), '|', fixed = TRUE)))
head(df)

# Convert to time series
ndxArimaGarch = xts(df[,-1], order.by = as.Date(df$X1))
class(ndxArimaGarch)
head(ndxArimaGarch)


# Create the ARIMA+GARCH returns
ndxIntersect = merge(ndxArimaGarch[,1], ndxReturns, all = F)
head(ndxIntersect)
ndxArimaGarchReturns = ndxIntersect[,1] * ndxIntersect[,2]
head(ndxArimaGarchReturns)


# Backtesting
ndxArimaGarchCurve = log(cumprod( 1 + ndxArimaGarchReturns))
head(ndxArimaGarchCurve)
ndxBuyHoldCurve = log(cumprod( 1 + ndxIntersect[,2] ) )
head(ndxBuyHoldCurve)
ndxCombinedCurve = merge(ndxArimaGarchCurve, ndxBuyHoldCurve, all = F)
head(ndxCombinedCurve)


# Plot
xyplot( 
  ndxCombinedCurve,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list( 
    text=list(
      c("ARIMA+GARCH", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)
