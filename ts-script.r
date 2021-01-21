setwd("C:/Users/tuukk/Documents/Repos/Time-series-analysis-Wolt-internship-2021")

library(forecast)
library(TSPred)
library(lubridate)
library(nlme)
library(mgcv)

dataset = read.table(file = "orders_autumn_2020.csv",
                     sep = ",",
                     skip = 1,
                     col.names = c('Timestamp',
                                   'Delivery_D', 
                                   'Items', 
                                   'User_lat', 
                                   'User_lon',
                                   'Venue_lat',
                                   'Venue_lon',
                                   'Estimate_delivery', 
                                   'Actual_delivery', 
                                   'Cloud_coverage', 
                                   'Temperature', 
                                   'Wind', 
                                   'Precipitation')
                     )


#converting the time series to clever form. 
#It's using UTC now but it doesn't matter much as the primary time zone of the data is not given 
for(i in 1:length(dataset$Timestamp)) {
  dataset$TimeData[i] = as_datetime(dataset$Timestamp[i])
}

#so since we have an irregular ts I though about converting it 
#to a certain frequency based ts with number of occurrences. 
#a 30-minute interval should be good enough for such use with not too much data loss

#some basics to help understanding the code later:
aMinute = 60
anHour = 60*aMinute
aDay = 24*anHour
aWeek = 7*aDay

#initialize the first datetime to be 
i = dataset$TimeData[1]
index = 1
i = i - 7*60 
#this above to convert time starting from 6am in data. NOTE: only designed for this data

#Setting the step size for our analysis, 30minutes will do:
stepSize = aMinute*30

#"cluster" the data
df = data.frame(matrix(vector(), as.integer((dataset$TimeData[length(dataset$TimeData)] - i)/stepSize)+1, 8,
                      dimnames=list(c(), c("TimeStart",
                                           "TimeEnd", 
                                           "Orders_sum", 
                                           'Items_sum', 
                                           'Cloud_avg',
                                           'Temp_avg',
                                           'Wind_avg',
                                           'Precipitation_avg'))))

while (i <= dataset$TimeData[length(dataset$TimeData)]) {
  i_next = i+stepSize;
  thisRange = dataset[ (dataset$TimeData < i_next & dataset$TimeData >= i) ,]
  n = length(thisRange$Timestamp);
  df[index,1] = i
  df[index,2] = i_next
  df[index,3] = n
  df[index,4] = sum(thisRange$Items)
  if (n != 0) {
    df[index,5] = sum(thisRange$Cloud_coverage)/n #if needed later
    df[index,6] = sum(thisRange$Temperature)/n
    df[index,7] = sum(thisRange$Wind)/n
    df[index,8] = sum(thisRange$Precipitation)/n
  } else {
    df[index,5] = 0
    df[index,6] = 0
    df[index,7] = 0
    df[index,8] = 0
  }
  i = i_next
  index = index + 1
}

#data is looking ok, a week of orders data below:
plotLen = 2*aWeek/stepSize
dt = df$TimeStart[1:plotLen]
plot(as_datetime(dt), df$Orders_sum[1:plotLen], type="l", ylab="Sum of orders each 30min", xlab="Date")



##################################
#### BUILDING A PROPER MODEL #####
##################################
#Let's first create a time series of the data and analyze potential parameters for an arma-model
# base for arma: SARIMA(p,d,q)(P,D,Q)[s]

tsData = ts(df, start = 1, frequency = 2)
plot(tsData[,3]) 
# seems like a relatively regular shape
plot(decompose(tsData[,3])) 
#doesn't show much clever stuff here
#but at least seems to be no trend

acf(tsData[,3], lag.max = 48*10, xlab="Lag in hours", na.action = na.pass, main="ACF over longer period")
#we see acf highly correlating at around 48 units (that is 24h and slightly at a range of one week)
#however, it seems like there s a correlation between 7 days thus the day of the week plays a role
#we might have to skip this tho since the tool allows only certainly short seasons
acf(tsData[,3], lag.max = 48*2, xlab="Lag in hours", na.action = na.pass, main="ACF over 2 days")
#clear correlation of 24 hours
pacf(tsData[,3], lag.max = 48*10, main="PACF", na.action = na.pass) 
#above does not give any relevant info atm without diff
pacf(tsData[,3], lag.max = 48, main="PACF", na.action = na.pass) 
#same here

## thus setting:
s = 48

#diff for the chosen s to see other info
tsDataDiff = diff(tsData[,3], lag = s, differences = 1)
par(mfrow=c(2,1))
acf(tsDataDiff, lag.max = 24*20, main="ACF of once 24-diff data", na.action = na.pass)
pacf(tsDataDiff, lag.max = 24*20, main="PACF of once 24-diff data", na.action = na.pass)

#also diff for 1 to see if non-serial modeling is clever 
tsDataDiffDiff = diff(tsDataDiff, lag = 1, differences = 1)

acf(tsDataDiffDiff, lag.max = 24*20, main="ACF of 1&24-diff data", na.action = na.pass)
pacf(tsDataDiffDiff, lag.max = 24*20, main="PACF of 1&24-diff data", na.action = na.pass)
par(mfrow=c(1,1))
#based on the gaphs above we might want to consider following parameters for 
#SARIMA(p,d,q)(P,D,Q)[s]
# s already done
#for p it would likely be clever to select 1 since the PACF for further spikes tends to be quite small already the first being the value itself
p = 1
#for d we set 1 since we diff once for one step
d = 0
#two remarkable spikes in ACF
q = 2
#for P it would likely be clever to select 1 since the PACF for further seasonal spikes tends to be quite small
P = 1
#for D we set 1 since we diff only once for s
D = 1
#two remarkable seasonal spikes in ACF
Q = 2

#testing auto arima:
tsModelAuto = auto.arima(tsData[,3])
tsModelAuto 
#seems to have quite irrelevant sar2 and sar2
#could be better..

#testing SARIMA(p,d,q)(P,D,Q)[24] based on the ACF and PACF plots before
# season is to be set to 48 which is equivivalent to 24 here
tsModel = arima(tsData[,3], order= c(p,d,q), seasonal=list(order = c(P, D, Q), period = s), method = "CSS")
tsModel

#seems like ma2 is very small. let's remove if for the sake of simplicity
q = 1
tsModel = arima(tsData[,3], order= c(p,d,q), seasonal=list(order = c(P, D, Q), period = s), method = "CSS")
tsModel

tsModel.prediction = forecast(tsModel, h=48*2)
plot(tsModel.prediction, xlim=c(1300,1500), ylim=c(0,30))
#all seems pretty good now

tsModel.prediction = forecast(tsModel, h=48*2)
plot(tsModel.prediction, xlim=c(1300,1500), ylim=c(0,25), ylab="Orders within each 30 minutes")
#the model does not cover night times as well as the one with higher degree of p and P. Tho it's a simpler model thus should be relied with it's quite as good performance



####################################
##### THE MEANING OF THE MODEL #####
####################################

#we are now not considering the number of orders, the effect of each weekday nor external factors due to the simplicity of the analysis
#for a delivery-centric analysis the pure number of orders should work fine since the size of the order shouldn't generally make relevant difference
#therefore we get following results for number of orders 
#forecast length in hours
f_len = 24*7

#prepare data for better handling
tsModel.prediction = forecast(tsModel, h=f_len*2)
plot(tsModel.prediction, xlim=c(1000,1610), ylab="Orders within each 30 minutes")
forecast_time = df$TimeStart[length(df$TimeStart)] + aMinute*30
as_datetime(forecast_time)

df_forecast = data.frame(matrix(vector(), aWeek/stepSize, 4,
                       dimnames=list(c(), c("TimeStart",
                                            "Orders_sum",
                                            "Orders_lower80",
                                            "Orders_upper80"))))
for (i_f in 1:(aWeek/stepSize)) {
  df_forecast$TimeStart[i_f] = forecast_time
  if (tsModel.prediction$mean[i_f] < 0) {
    df_forecast$Orders_sum[i_f] = 0
  } else {
    df_forecast$Orders_sum[i_f] = tsModel.prediction$mean[i_f]
  }
  if (tsModel.prediction$lower[i_f,1] < 0) {
    df_forecast$Orders_lower80[i_f] = 0
  } else {
    df_forecast$Orders_lower80[i_f] = tsModel.prediction$lower[i_f,1]
  }
  df_forecast$Orders_upper80[i_f] = tsModel.prediction$upper[i_f,1]
  forecast_time = forecast_time + 30*aMinute
}

# THE NUMBER OF ORDERS DURING THE FOLLOWING 24h:
sum(df_forecast$Orders_sum[1:48])
#predicting a total of 329 orders during the next week
sum(df_forecast$Orders_upper80[1:48])
#80% likely there will be less than 541 orders next week
sum(df_forecast$Orders_lower80[1:48])
#and 80% likely of over 206 orders

# THE NUMBER OF ORDERS DURING THE FOLLOWING WEEK:
sum(df_forecast$Orders_sum)
#predicting a total of 2279 orders during the next week
sum(df_forecast$Orders_upper80)
#80% likely there will be less than 3800 orders next week
sum(df_forecast$Orders_lower80)
#and 80% likely of over 1426 orders

