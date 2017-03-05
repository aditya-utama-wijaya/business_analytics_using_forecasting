library(gdata)
perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'
sales = read.xls('5-DepartmentStoreSales.xls', sheet = 'Sheet1', perl = perl)
sales.ts = ts(sales$Sales, start = c(1, 1), end = c(1, 24), freq = 4)
plot(sales.ts, ylab = 'Sales ($)', xlab = 'Year', xaxt = 'n', bty = 'l', ylim = c(0, 120000), xlim = c(1, 7))
axis(1, at = seq(1, 24, 1), labels = format(seq(1, 24, 1)))

nValid = 4
nTrain = length(sales.ts) - nValid
train.ts = window(sales.ts, start = c(1, 1), end = c(1, nTrain))
valid.ts = window(sales.ts, start = c(1, nTrain + 1), end = c(1, nTrain + nValid))

library(forecast)

hwin = ets(train.ts, model = 'MMM', alpha = 0.2, beta = 0.15, gamma = 0.05)
hwin.pred = forecast(hwin, h = nValid, level = 0)
accuracy(hwin.pred, nValid)

plot(hwin.pred, ylim = c(0, 120000), xaxt = 'n', bty = 'l', main = '', flty = 2)
axis(1, at = seq(1, 24, 1), labels = format(seq(1, 24, 1)))
lines(hwin.pred$fitted, lwd = 2, col = 'blue')
lines(valid.ts)
accuracy(hwin.pred, nValid)


# d
diff1 = diff(sales.ts, lag = 4)
diff2 = diff(diff1, lag = 1)
nValid = 4
nTrain = length(diff2) - nValid
train.ts = window(diff2, start = c(2, 2), end = c(2, nTrain + 1))
valid.ts = window(diff2, start = c(2, nTrain + 2), end = c(2, nTrain + 1 + nValid))

library(zoo)
ma.trailing = rollmean(train.ts, k = 4, align = 'right')
last.ma = tail(ma.trailing, 1)
ma.trailing.pred = ts(rep(last.ma, 2), start = c(2, length(train.ts) + 2), end = c(2, length(train.ts) + 3), freq = 4)
lines(train.ts, lwd = 2, col = 'blue')
lines(ma.trailing, lwd = 2, col = 'orange')
lines(ma.trailing.pred, lwd = 2, col = 'orange', lty = 2)
legend(1, 130000, c('Actual Sales', 'Twice-Differenced Sales (on training period)', 'Trailing Moving Average', 'Forecast'), col = c('black', 'blue', 'orange', 'orange'), lty = c(1, 1, 1, 2), lwd = c(1, 2, 2, 2), bty = 'n')

undiff = rep(0, length(ma.trailing))
for(j in 1 : length(ma.trailing)){
  undiff[j] = ma.trailing[j] + diff1[j + 3] + sales.ts[j + 4]
}
undiff.ts = ts(undiff, start = c(3, 1), end = c(3, length(ma.trailing)), freq = 4)
last.ma = tail(undiff.ts, 1)
undiff.ts.pred = ts(rep(last.ma, 4), start = c(3, length(ma.trailing) + 1), end = c(3, length(ma.trailing) + 4), freq = 4)
lines(undiff.ts, lwd = 2, col = 'blue')
lines(undiff.ts.pred, lwd = 2, col = 'blue', lty = 2)
legend(1, 120000, c('Actual Sales', 'Undifferenced Moving Average (on training period)', 'Forecast'), col = c('black', 'blue', 'blue'), lty = c(1, 1, 2), lwd = c(1, 2, 2), bty = 'n')

