# 1
library(forecast)

library(openxlsx)

souvenirSales = read.xlsx('3-SouvenirSales.xlsx', sheet = 'SouvenirSales')
str(souvenirSales)
souvenirSales$Date = as.Date(souvenirSales$Date, origin = '1899-12-30')
souvenirSales$Date = format(souvenirSales$Date, '%b-%y')
souvenirSales$Date = as.factor(souvenirSales$Date)

souvenirSales.ts = ts(souvenirSales$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)
plot(souvenirSales.ts, xlab = 'Time', ylab = 'Sales', ylim = c(0, 120000), xlim = c(1995, 2002.25), xaxt = 'n', bty = 'l', main = '')
axis(1, at = seq(1995, 2002, 1), labels = format(seq(1995, 2002, 1)))

nValid = 12
nTrain = length(souvenirSales.ts) - nValid
train.ts = window(souvenirSales.ts, start = c(1995, 1), end = c(1995, nTrain))
valid.ts = window(souvenirSales.ts, start = c(1995, nTrain + 1), end = c(1995, nTrain + nValid))
naive.pred = naive(train.ts, h = nValid, level = 0)
accuracy(naive.pred, valid.ts)


# 2
hist(naive.pred$residuals, ylab = 'Frequency', xlab = 'Forecast Error', main = '', bty = 'l')

plot(naive.pred, ylim = c(1664, 104661), ylab = 'Sales', xlab = 'Time', bty = 'l', xaxt = 'n', xlim = c(2001, 2002.25), main = '', flty = 2)
axis(1, at = seq(2001, 2002, 1), labels = format(seq(2001, 2002, 1)))
lines(valid.ts)


# chapter 6 problem 5
par(mfrow = c(2, 1))
plot(souvenirSales.ts, xlab = 'Time', ylab = 'Sales', ylim = c(0, 120000), bty = 'l', main = 'Actual and Forecasted Values')
plot(log(souvenirSales.ts), xlab = 'Time', ylab = 'log(sales)', bty = 'l')

nValid = 12
nTrain = length(souvenirSales.ts) - nValid
train.ts = window(souvenirSales.ts, start = c(1995, 1), end = c(1995, nTrain))
valid.ts = window(souvenirSales.ts, start = c(1995, nTrain + 1), end = c(1995, nTrain + nValid))

# 5b
model_A = tslm(train.ts ~ trend + season, lambda = 1)
model_A.pred = forecast(model_A, h = nValid, level = 0)
plot(model_A.pred, ylim = c(0, 120000), main = 'Model A', bty = 'l', flty = 2)
lines(model_A.pred$fitted, lwd = 2, col = 'orange')
lines(valid.ts)
summary(model_A)
lines(model_A.pred$mean, lwd = 2, col = 'orange', lty = 2)

# 5c
model_B = tslm(train.ts ~ trend + season, lambda = 0)
model_B.pred = forecast(model_B, h = nValid + 2, level = 0)
tail(model_B.pred$mean, 2)

summary(model_B)
plot(model_B.pred, ylim = c(0, 120000), main = 'Model B', bty = 'l', flty = 2)
lines(model_B.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)
tail(model_B.pred$mean, 2)
lines(model_B.pred$mean, lwd = 2, col = 'blue', lty = 2)
legend(1995, 130000, c('Sales', 'Model A', 'Model B'), lty = c(1, 1, 1), lwd = c(1, 2, 2), col = c('black', 'orange', 'blue'), bty = 'n')

plot(model_A.pred$residuals, ylab = 'Values', main = 'Forecast Errors', bty = 'l', ylim = c(-30000, 30000), xlim = c(1995, 2002.25), xaxt = 'n', col = 'orange', lwd = 2)
axis(1, at = seq(1995, 2002, 1), labels = format(seq(1995, 2002, 1)))
lines(train.ts - model_B.pred$fitted, col = 'blue', lwd = 2)
lines(valid.ts - model_A.pred$mean, col = 'orange', lty = 2, lwd = 2)
lines(valid.ts - model_B.pred$mean, col = 'blue', lty = 2, lwd = 2)
legend(1995, 30000, c('Model A Forecast Errors', 'Model B Forecast Errors'), lty = c(1, 1), lwd = c(2, 2), col = c('orange', 'blue'), bty = 'n')


# chapter 7
# page 172
# a
train.lm.expo.trend.season = tslm(train.ts ~ trend + season, lambda = 0)
train.lm.expo.trend.season.pred = forecast(train.lm.expo.trend.season, h = nValid, level = 0)
plot(train.lm.expo.trend.season.pred, ylim = c(0, 120000), ylab = 'Sales', xlab = 'Time', bty = 'l', flty = 2, main = 'Actual vs. Forecasted Sales')
lines(train.lm.expo.trend.season.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)

train.lm.expo.trend.season.pred.2 = forecast(train.lm.expo.trend.season, h = nValid + 2, level = 0)
tail(train.lm.expo.trend.season.pred.2$mean, 2)

#b
Acf(train.ts - train.lm.expo.trend.season.pred$fitted, lag.max = 15, main = '')
train.res.arima = Arima(train.ts - train.lm.expo.trend.season.pred$fitted, order = c(2, 0, 0))
train.res.arima.pred = forecast(train.res.arima, h = nValid, level = 0)
plot(train.res.arima.pred, ylim = c(-20000, 20000), ylab = 'Residuals', xlab = 'Time', bty = 'l', flty = 2, main = '')
lines(train.res.arima.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts - train.lm.expo.trend.season.pred$mean)
accuracy(train.lm.expo.trend.season.pred, valid.ts)
accuracy(train.lm.expo.trend.season.pred$mean + train.res.arima.pred$mean, valid.ts)
accuracy(train.lm.expo.trend.season.pred$fitted + train.res.arima.pred$fitted, train.ts)

lines(train.lm.expo.trend.season.pred$fitted + train.res.arima.pred$fitted, lwd = 2, col = 'orange')
lines(train.lm.expo.trend.season.pred$mean + train.res.arima.pred$mean, lwd = 2, lty = 2, col = 'orange')
legend(1995, 120000, c('Actual', 'Forecasted (Regression)', 'Forecasted (Regression + AR(2))'), lwd = c(1, 2, 2), lty = c(1, 1, 1), col = c('black', 'blue', 'orange'), bty = 'n')
Acf(train.ts - (train.lm.expo.trend.season.pred$fitted + train.res.arima.pred$fitted), lag.max = 15, main = '')

train.res.arima.pred = forecast(train.res.arima, h = nValid + 2, level = 0)
train.res.arima.pred$mean
