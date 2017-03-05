red = read.csv('4-AustralianWines.csv')
str(red)
red$Fortified = NULL
red$Rose = NULL
red$sparkling = NULL
red$Sweet.white = NULL
red$Dry.white = NULL
red.ts = ts(red$Red, start = c(2001, 1), end = c(2015, 12), freq = 12)
plot(red.ts, ylim = c(310, 3680), xlab = 'Time', ylab = 'Sales (thousand liters)', bty = 'l', main = 'Timeplot of red wine sales', xaxt = 'n')
axis(1, at = seq(2001, 2016, 1), labels = format(seq(2001, 2016, 1)))


# MA
diff1 = diff(red.ts, lag = 12)
diff2 = diff(diff1, lag = 1)
plot(diff2, ylim = c(-1500, 1500), xlim = c(2002, 2016.25), ylab = 'Sales', main = 'Time plot of Red Wine Sales (Twice-Differenced)', xaxt = 'n', bty = 'l')
axis(1, at = seq(2002, 2016, 1), labels = format(seq(2002, 2016, 1)))

library(zoo)
ma.trailing = rollmean(diff2, k = 12, align = 'right')
last.ma = tail(ma.trailing, 1)
ma.trailing.pred = ts(rep(last.ma, 2), start = c(2002, length(diff2) + 1), end = c(2002, length(diff.twice.ts) + 2), freq = 12)
lines(ma.trailing, lwd = 2, col = 'blue')
lines(ma.trailing.pred, lwd = 2, col = 'blue', lty = 2)
legend(2006, 1500, c('Sales (Twice-Differenced)', 'Trailing Moving Average'), col = c('black', 'blue'), lty = c(1, 1), lwd = c(1, 2), bty = 'n')

plot(diff2, ylim = c(-1500, 1500), xlim = c(2015, 2016.5), ylab = 'Sales', main = 'Forecast of January and February 2016', bty = 'l', xaxt = 'n')
axis(1, at = seq(2015, 2016, 1), labels = format(seq(2015, 2016, 1)))
legend(2015, 1500, c('Sales (Twice-Differenced)', 'Trailing Moving Average', 'Forecast'), col = c('black', 'blue', 'blue'), lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = 'n')

undiff = rep(0, length(ma.trailing))
for(j in 1:length(ma.trailing)){
  undiff[j] = ma.trailing[j] + diff1[j + 11] + red.ts[j + 12]
}
undiff.ts = ts(undiff, start = c(2003, 1), end = c(2015, 12), freq = 12)
lines(undiff.ts, lwd = 2, col = 'blue')
last.ma = tail(undiff.ts, 1)
undiff.ts.pred = ts(rep(last.ma, 2), start = c(2003, length(undiff.ts) + 1), end = c(2003, length(undiff.ts) + 2), freq = 12)
lines(undiff.ts.pred, lwd = 2, lty = 2, col = 'orange')
legend(2001, 3500, c('Actual Sales', 'Undifferenced Moving Average', 'Forecast'), col = c('black', 'blue', 'orange'), lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = 'n')


# Regression
nValid = 12
nTrain = length(red.ts) - nValid
train.ts = window(red.ts, start = c(2001, 1), end = c(2001, nTrain))
valid.ts = window(red.ts, start = c(2001, nTrain + 1), end = c(2001, nTrain + nValid))
train.lm.trend.season.1 = tslm(train.ts ~ trend + season)
train.lm.trend.season.1.pred = forecast(train.lm.trend.season.1, h = nValid, level = 0)
accuracy(train.lm.trend.season.1.pred, nValid)

train.lm.trend.season.2 = tslm(train.ts ~ trend + season, lambda = 0)
train.lm.trend.season.2.pred = forecast(train.lm.trend.season.2, h = nValid, level = 0)
accuracy(train.lm.trend.season.2.pred, nValid)

train.lm.trend.season.3 = tslm(train.ts ~ trend + I(trend ^ 2) + season)
train.lm.trend.season.3.pred = forecast(train.lm.trend.season.3, h = nValid, level = 0)
accuracy(train.lm.trend.season.3.pred, nValid)

train.lm.trend.season.4 = tslm(train.ts ~ trend + I(trend ^ 2) + season, lambda = 0)
train.lm.trend.season.4.pred = forecast(train.lm.trend.season.4, h = nValid + 2, level = 0)
accuracy(train.lm.trend.season.4.pred, nValid)

plot(train.lm.trend.season.4.pred, ylab = 'Sales (in thousand liters)', xlab = 'Time', ylim = c(0, 4000), xlim = c(2001, 2016.25), xaxt = 'n', main = 'Actual and Forecasted Sales', bty = 'l', flty = 2)
axis(1, at = seq(2001, 2016, 1), labels = format(seq(2001, 2016, 1)))
lines(train.lm.trend.season.4.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)
legend(2001, 4000, c('Actual Sales', 'Forecasted Sales'), lwd = c(1, 2), col = c('black', 'blue'), bty = 'n')

plot(train.ts - train.lm.trend.season.4.pred$fitted, ylab = 'Values', main = 'Forecast Errors', bty = 'l', ylim = c(-800, 800), xlim = c(2001, 2016.25), xaxt = 'n')
axis(1, at = seq(2001, 2016, 1), labels = format(seq(2001, 2016, 1)))
lines(valid.ts - train.lm.trend.season.1.pred$mean, lwd = 2, lty = 2, col = 'blue')
legend(2001, 800, c('Forecast Errors (Train)', 'Forecast Errors (Valid)'), col = c('black', 'blue'), lwd = c(1, 2), lty = c(1, 2), bty = 'n')


# page 201
library(gdata)
perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'

wines = read.xls('4-AustralianWines.xls', sheet = 'Sheet1', perl = perl)
wines = wines[, c(1, 2)]
str(wines)

fortified.ts = ts(wines$Fortified, start = c(1980, 1), end = c(1994, 12), freq = 12)
plot(fortified.ts, xlab = 'Time', ylab = 'Sales (thousand liters)', bty = 'l',
     main = 'Timeplot of fortified wine sales', xlim = c(1980, 1995.25), xaxt = 'n')
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))

nValid = 12
nTrain = length(fortified.ts) - nValid
train.ts = window(fortified.ts, start = c(1980, 1), end = c(1980, nTrain))
valid.ts = window(fortified.ts, start = c(1980, nTrain + 1),
                  end = c(1980, nTrain + nValid))
plot(train.ts, xlab = 'Time', ylab = 'Sales (thousand liters)', bty = 'l',
     main = 'Timeplot of fortified wine sales', xlim = c(1980, 1995.25),
     xaxt = 'n')
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(valid.ts, lwd = 2, lty = 2, col = 'blue')
legend(1992, 5500, c('Training', 'Validation'), lwd = c(1, 2), bty = 'n',
       lty = c(1, 2), col = c('black', 'blue'))

# neural networks
set.seed(201)
library(forecast)
fortified.nnetar = nnetar(train.ts, p = 11)
fortified.nnetar.pred = forecast(fortified.nnetar, h = nValid)

plot(train.ts, xlab = 'Time', ylab = 'Sales (thousand liters)', bty = 'l',
     main = 'Timeplot of fortified wine sales (Training Period)',
     xlim = c(1980, 1994.25), xaxt = 'n')
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(fortified.nnetar.pred$fitted, lwd = 2, col = 'blue')
legend(1991, 5500, c('Actual', 'Forecast'), lwd = c(1, 2), bty = 'n',
       col = c('black', 'blue'))

plot(train.ts - fortified.nnetar.pred$fitted, ylab = 'Errors', bty = 'l',
     xlim = c(1980, 1994.25), xaxt = 'n',
     main = 'Forecast Errors (Training Period)')
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
fortified.nnetar.pred$mean

# exponential smoothing
accuracy(ets.opt.pred, valid.ts)

fortified.ets = ets(train.ts, model = 'ZZZ', restrict = FALSE,
                    allow.multiplicative.trend = TRUE)
summary(fortified.ets)
fortified.ets.pred = forecast(fortified.ets, h = nValid, level = 0)
fortified.ets.pred

plot(train.ts - fortified.ets.pred$fitted, ylab = 'Errors', bty = 'l',
     xlim = c(1980, 1994.25), xaxt = 'n',
     main = 'Forecast Errors (Training Period)')
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))
lines(train.ts - fortified.nnetar.pred$fitted, lwd = 2, col = 'blue')
legend(1991, 1000, c('ETS', 'NN'), col = c('black', 'blue'), lwd = c(1, 2), bty = 'n')

plot(valid.ts - fortified.ets.pred$mean, ylab = 'Errors', bty = 'l',
     xlim = c(1994, 1995.25), xaxt = 'n',
     main = 'Forecast Errors (Validation Period)')
axis(1, at = seq(1994, 1995, 1), labels = format(seq(1994, 1995, 1)))
lines(valid.ts - fortified.nnetar.pred$mean, lwd = 2, col = 'blue')
legend(1995, 600, c('ETS', 'NN'), col = c('black', 'blue'), lwd = c(1, 2), bty = 'n')

library(knitr)
kable(accuracy(fortified.ets.pred, valid.ts))
kable(accuracy(fortified.nnetar.pred, valid.ts))
