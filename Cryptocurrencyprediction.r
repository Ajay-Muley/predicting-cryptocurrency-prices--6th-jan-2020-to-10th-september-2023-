cryptocurrencyprediction<- function(filename,location){

#installing libraries

library(prophet)
library(lubridate)
library(ggplot2)
library(tidyverse)
#data

data <- Ethereum_Historical_Data_1_
str(data)
head(data)
data$Date <- dmy(data$Date)
head(data)

#plot

qplot(ds , y , data = df,
      main = Ethereum_Historical_Data_1_)


# log transformation 
ds <- data$Date
y <- log(data$Price)
df <- data.frame(ds , y)

#forecasting 
m<- prophet(df)
future<- make_future_dataframe(m, periods = 365)
tail(future)
forecast<- predict(m,future)

#plot forecast
plot(m,forecast)
dyplot.prophet(m,forecast)
prophet_plot_components(m,forecast)

#model performence
#pred <- forecast$yhat[1:1344]
#actual<- m$history$y
#plot(actual, pred)
#abline(lm(pred-actual), col= red)
#summary(lm(pred-actual))
# model performance
pred <- forecast$yhat[1:1344]
actual <- m$history$y

# plot actual vs. predicted
plot(actual, pred)

# add a regression line
abline(lm(pred ~ actual), col = "red")
#cross validation

x<- cross_validation(m, 43, units = 'days')
performance_metrics(x, rolling_window = 0.1)

}

