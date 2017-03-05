bicup = read.csv('3-bicup2006.csv')
bicup$DATETIME = as.POSIXct(strptime(paste(bicup$DATE, bicup$TIME, sep = ' '), format = '%d-%b-%y %H:%M'))
bicup$DATE = NULL
bicup$TIME = NULL
bicup = bicup[, c(2, 1)]

install.packages('xts')
library(xts)
bicup.xts = xts(bicup$DEMAND, order.by = bicup$DATETIME)
plot(bicup.xts, ylab = 'Demand', ylim = c(0, 150), auto.grid = FALSE, main = '', las = 2)
bicup.ts = ts(bicup.xts, frequency = 21 * 24 * 60 * 4)
plot(bicup.ts)

nValid = length(bicup.ts) / 3
nTrain = length(bicup.ts) - nValid
train.ts = bicup.ts[1:nTrain]
valid.ts = bicup.ts[(nTrain + 1):(nTrain + nValid)]

library(forecast)
naive.pred = naive(train.ts, h = nValid, level = 0)
snaive.pred = snaive(train.ts, h = nValid, level = 0)
plot(snaive.pred, ylim = c(0, 150), ylab = 'Demand', bty = 'l', xaxt = 'n', main = '', flty = 2)
