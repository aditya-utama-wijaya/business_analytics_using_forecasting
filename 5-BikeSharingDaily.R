bike.daily.df = read.csv('5-BikeSharingDaily.csv')
bike.daily.msts = msts(bike.daily.df$cnt, seasonal.periods = c(7, 365.25))

bike.daily.tbats = tbats(bike.daily.msts)
bike.daily.tbats.pred = forecast(bike.daily.tbats, h = 365)

bike.daily.stlm = stlm(bike.daily.msts, s.window = 'periodic', method = 'ets')
bike.daily.stlm.pred = forecast(bike.daily.stlm, h = 365)

par(mfrow = c(1, 2))
plot(bike.daily.tbats.pred, ylim = c(0, 11000), xlab = 'Year', ylab = 'Daily Bike Rentals', main = 'TBATS')
plot(bike.daily.stlm.pred, ylim = c(0, 11000), xlab = 'Year', ylab = 'Daily Bike Rentals', main = 'STL + ETS')


# chapter 7
# page 162
install.packages('lubridate')
library(lubridate)
bike.df = read.csv('5-BikeSharingDaily.csv')
str(bike.df)
bike.df$Date = as.Date(bike.df$dteday, format = '%Y-%m-%d')
bike.df$Month = month(bike.df$dteday, label = TRUE)
bike.df$DOW = wday(bike.df$Date, label = TRUE)
bike.df$WorkingDay = factor(bike.df$workingday, levels = c(0, 1), labels = c('Not_Working', 'Working'))
bike.df$Weather = factor(bike.df$weathersit, levels = c(1, 2, 3), labels = c('Clear', 'Mist', 'Rain_Snow'))

Month.dummies = model.matrix(~ 0 + Month, data = bike.df)
DOW.dummies = model.matrix(~ 0 + DOW, data = bike.df)
WorkingDay_Weather.dummies = model.matrix(~ 0 + WorkingDay:Weather, data = bike.df)

colnames(Month.dummies) = gsub('Month', '', colnames(Month.dummies))
colnames(DOW.dummies) = gsub('DOW', '', colnames(DOW.dummies))
colnames(WorkingDay_Weather.dummies) = gsub('WorkingDay', '', colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) = gsub('Weather', '', colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) = gsub(':', '_', colnames(WorkingDay_Weather.dummies))

x = as.data.frame(cbind(Month.dummies[, -12], DOW.dummies[, -7], WorkingDay_Weather.dummies[, -6]))
y = bike.df$cnt
nValid = 90
nTrain = length(y) - nValid
xTrain = x[1:nTrain, ]
yTrain = y[1:nTrain]
xValid = x[(nTrain + 1):(nTrain + nValid), ]
yValid = y[(nTrain + 1):(nTrain + nValid)]

yTrain.ts = ts(yTrain)
formula = as.formula(paste('yTrain.ts', paste(c('trend', colnames(xTrain)), collapse = '+'), sep = '~'))
library(forecast)
bike.tslm = tslm(formula, data = xTrain, lambda = 1)
bike.tslm.pred = forecast(bike.tslm, newdata = xValid)
plot(bike.tslm.pred, ylim = c(0, 9000), ylab = 'Daily Bike Rentals', xlab = 'Days')
options(scipen = 999, digits = 6)
summary(bike.tslm)
