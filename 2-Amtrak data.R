# page 31
Amtrak.data = read.csv('2-Amtrak data.csv')
ridership.ts = ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

plot(ridership.ts, xlab = 'Time', ylab = 'Ridership', ylim = c(1300, 2300), xlim = c(1991, 2005.25), xaxt = 'n', bty = 'l')


# page 34
install.packages('forecast')
library(forecast)
ridership.lm = tslm(ridership.ts ~ trend + I(trend ^ 2))

par(mfrow = c(2, 1))
plot(ridership.ts, xlab = 'Time', ylab = 'Ridership', ylim = c(1300, 2300), bty = 'l')
lines(ridership.lm$fitted, lwd = 2)

ridership.ts.zoom = window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
plot(ridership.ts.zoom, xlab = 'Time', ylab = 'Ridership', ylim = c(1300, 2300), bty = 'l')


# page 49
nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ridership.lm = tslm(train.ts ~ trend + I(trend ^ 2))
ridership.lm.pred = forecast(ridership.lm, h = nValid, level = 95)

plot(ridership.lm.pred, ylim = c(1300, 2600), ylab = 'Ridership', xlab = 'Time', bty = 'l', xaxt = 'n', xlim = c(1991, 2006.25), main = '', flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.lm$fitted, lwd = 2)
lines(valid.ts)


# page 53
accuracy(ridership.lm.pred$mean, valid.ts)


# page 57
names(ridership.lm.pred)
ridership.lm.pred$residuals
valid.ts - ridership.lm.pred$mean


# page 58
hist(ridership.lm.pred$residuals, ylab = 'Frequency', xlab = 'Forecast Error', main = '', bty = 'l')


# page 64
fixed.nValid = 36
fixed.nTrain = length(ridership.ts) - fixed.nValid
stepsAhead = 1
error = rep(0, fixed.nValid - stepsAhead + 1)
percent.error = rep(0, fixed.nValid - stepsAhead + 1)

for(j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)){
  train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, j))
  valid.ts = window(ridership.ts, start = c(1991, j + stepsAhead), end = c(1991, j + stepsAhead))
  naive.pred = naive(train.ts, h = stepsAhead)
  error[j - fixed.nTrain + 1] = valid.ts - naive.pred$mean[stepsAhead]
  percent.error[j - fixed.nTrain + 1] = error[j - fixed.nTrain + 1] / valid.ts
}

mean(abs(error))
sqrt(mean(error ^ 2))
mean(abs(percent.error))


# page 66
fixed.nValid = 36
fixed.nTrain = length(ridership.ts) - fixed.nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, fixed.nTrain))
valid.ts = window(ridership.ts, start = c(1991, fixed.nTrain + 1), end = c(1991, fixed.nTrain + fixed.nValid))
naive.pred = naive(train.ts, h = fixed.nValid)
snaive.pred = snaive(train.ts, h = fixed.nValid)
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)


# page 82
library(zoo)
library(forecast)
ma.trailing = rollmean(ridership.ts, k = 12, align = 'right')
ma.centered = ma(ridership.ts, order = 12)
plot(ridership.ts, ylim = c(1300, 2200), ylab = 'Ridership', xlab = 'Time', bty = 'l', xaxt = 'n', xlim = c(1991, 2004.25), main = '')
axis(1, at = seq(1991, 2004.25, 1), labels = format(seq(1991, 2004.25, 1)))
lines(ma.centered, lwd = 2)
lines(ma.trailing, lwd = 2, lty = 2)
legend(1994, 2200, c('Ridership', 'Centered Moving Average', 'Trailing Moving Average'), lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = 'n')


# page 84
nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ma.trailing = rollmean(train.ts, k = 12, align = 'right')
last.ma = tail(ma.trailing, 1)
ma.trailing.pred = ts(rep(last.ma, nValid), start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid), freq = 12)
plot(train.ts, ylim = c(1300, 2400), ylab = 'Ridership', xlab = 'Time', bty = 'l', xaxt = 'n', xlim = c(1991, 2006.25), main = '')
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ma.trailing, lwd = 2, col = 'blue')
lines(ma.trailing.pred, lwd = 2, col = 'blue', lty = 2)
lines(valid.ts)


# page 91
diff.twice.ts = diff(diff(ridership.ts, lag = 12), lag = 1)
nValid = 36
nTrain = length(diff.twice.ts) - nValid
train.ts = window(diff.twice.ts, start = c(1992, 2), end = c(1992, nTrain + 1))
valid.ts = window(diff.twice.ts, start = c(1992, nTrain + 2), end = c(1992, nTrain + 1 + nValid))

library(forecast)
ses = ets(train.ts, model = 'ANN', alpha = 0.2)
ses.pred = forecast(ses, h = nValid, level = 0)
ses.pred
plot(ses.pred, ylim = c(-250, 300), xlim = c(1991, 2006.25), ylab = 'Ridership (Twice-Differenced)', xlab = 'Time', bty = 'l', xaxt = 'n', main = '', flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ses.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)

ses.opt = ets(train.ts, model = 'ANN')
ses.opt.pred = forecast(ses.opt, h = nValid, level = 0)
accuracy(ses.pred, valid.ts)
accuracy(ses.opt.pred, valid.ts)
ses.opt


# page 97
nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
library(forecast)
hwin = ets(train.ts, model = 'MAA')
hwin.pred = forecast(hwin, h = nValid, level = 0)
plot(hwin.pred, ylim = c(1300, 2200), xlim = c(1991, 2006.25), ylab = 'Ridership', xlab = 'Time', bty = 'l', xaxt = 'n', main = '', flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(hwin.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)
hwin
hwin$states[1, ] # Initial states
hwin$states[nrow(hwin$states), ] # Final states
# page 100
ets.opt = ets(train.ts, model = 'ZZZ', restrict = FALSE, allow.multiplicative.trend = TRUE)
ets.opt.pred = forecast(ets.opt, h = nValid, level = 0)
accuracy(hwin.pred, valid.ts)
accuracy(ets.opt.pred, valid.ts)


# page 119
nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
library(forecast)
train.lm = tslm(train.ts ~ trend)
plot(train.ts, ylab = 'Ridership', xlab = 'Time', ylim = c(1300, 2300), bty = 'l')
lines(train.lm$fitted.values, lwd = 2)
train.lm.pred = forecast(train.lm, h = nValid, level = 0)
plot(train.lm.pred, ylim = c(1300, 2300), xlim = c(1991, 2006.25), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', main = '', bty = 'l', flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)
summary(train.lm)


# page 123
train.lm.expo.trend = tslm(train.ts ~ trend, lambda = 0)
train.lm.expo.trend.pred = forecast(train.lm.expo.trend, h = nValid, level = 0)

train.lm.linear.trend = tslm(train.ts ~ trend, lambda = 1)
train.lm.linear.trend.pred = forecast(train.lm.linear.trend, h = nValid, level = 0)
plot(train.lm.expo.trend.pred, ylim = c(1300, 2300), xlim = c(1991, 2005.25), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', bty = 'l', main = '', flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(train.lm.expo.trend$fitted.values, lwd = 2, col = 'blue')
lines(train.lm.linear.trend$fitted.values, lwd = 2, lty = 3)
lines(train.lm.linear.trend.pred$mean, lwd = 2, lty = 3)
lines(valid.ts)

train.lm.poly.trend = tslm(train.ts ~ trend + I(trend ^ 2))
summary(train.lm.poly.trend)
train.lm.poly.trend.pred = forecast(train.lm.poly.trend, h = nValid, level = 0)
plot(train.lm.poly.trend.pred, ylim = c(1300, 2300), xlim = c(1991, 2005.25), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', main = '', bty = 'l', flty = 2)
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(train.lm.poly.trend.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)


# page 127
train.lm.season = tslm(train.ts ~ season)
summary(train.lm.season)

train.lm.season.pred = forecast(train.lm.season, h = nValid, level = 0)
plot(train.lm.season.pred, ylim = c(1300, 2300), xlim = c(1991, 2005.25), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', main = '', bty = 'l', flty = 2)
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(train.lm.season.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)


# page 129
train.lm.trend.season = tslm(train.ts ~ trend + I(trend ^ 2) + season)
summary(train.lm.trend.season)
train.lm.trend.season.pred = forecast(train.lm.trend.season, h = nValid, level = 0)
plot(train.lm.trend.season.pred, ylim = c(1300, 2500), xlim = c(1991, 2005.25), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', main = '', bty = 'l', flty = 2)
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(train.lm.trend.season.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)


# page 132
train.lm.trend.sine.cosine = tslm(train.ts ~ trend + I(trend ^ 2) + I(sin(2 * pi * trend / 12) + I(cos(2 * pi * trend / 12))))
train.lm.trend.sine.cosine.pred = forecast(train.lm.trend.sine.cosine, h = nValid, level = 0)
plot(train.lm.trend.sine.cosine.pred, ylim = c(1300, 2400), xlim = c(1991, 2005), xaxt = 'n', ylab = 'Ridership', xlab = 'Time', main = '', bty = 'l', flty = 2)
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(train.lm.trend.sine.cosine.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)

# chapter 7
# page 145
library(forecast)
ridership.24.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, 24))
Acf(ridership.24.ts, lag.max = 12, main = '')

nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
train.lm.trend.season = tslm(train.ts ~ trend + I(trend ^ 2) + season)
train.lm.trend.season.pred = forecast(train.lm.trend.season, h = nValid, level = 0)
Acf(train.lm.trend.season.pred$residuals, lag.max = 12, main = '')

# page 150
train.res.arima = Arima(train.lm.trend.season$residuals, order = c(1, 0, 0))
train.res.arima.pred = forecast(train.res.arima, h = nValid)
plot(train.lm.trend.season.pred$residuals, ylim = c(-250, 250), xlim = c(1991, 2002.25), xaxt = 'n', ylab = 'Residuals', bty = 'l', main = '')
axis(1, at = seq(1991, 2002, 1), labels = format(seq(1991, 2002, 1)))
lines(train.res.arima.pred$fitted, lwd = 2, col = 'blue')
summary(train.res.arima)
Acf(train.res.arima.pred$residuals, lag.max = 12, main = '')

library(tseries)
adf.test(ridership.ts)


# page 200
nValid = 36
nTrain = length(ridership.ts) - nValid
train.ts = window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts = window(ridership.ts, start = c(1991, nTrain + 1),
                  end = c(1991, nTrain + nValid))

set.seed(201)
ridership.nnetar = nnetar(train.ts, repeats = 20, p = 11, P = 1, size = 7)
summary(ridership.nnetar$model[[1]])
ridership.nnetar.pred = forecast(ridership.nnetar, h = nValid)
accuracy(ridership.nnetar.pred, valid.ts)

plot(train.ts, ylim = c(1300, 2200), ylab = 'Ridership', xlab = 'Time',
     bty = 'l', xaxt = 'n', xlim = c(1991, 2005), lty = 1)
axis(1, at = seq(1991, 2005, 1), labels = format(seq(1991, 2005, 1)))
lines(ridership.nnetar.pred$fitted, lwd = 2, col = 'blue')
lines(ridership.nnetar.pred$mean, lwd = 2, col = 'blue', lty = 2)
lines(valid.ts)
