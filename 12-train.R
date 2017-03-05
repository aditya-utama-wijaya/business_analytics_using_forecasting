# 4
library(forecast)
bike.hourly.df = read.csv('12-train-4.csv')
nTotal = length(bike.hourly.df$count)
bike.hourly.msts = msts(bike.hourly.df$count, seasonal.periods = c(24, 168),
                        start = c(0, 1))
plot(bike.hourly.msts)

threes = rep(0, 19)
for (i in 1:19) {
  threes[i] = bike.hourly.df$count[i * 24 - 20]
}
sum(threes[1:19]) / 18

nTrain = 19 * 24
nValid = nTotal - nTrain
yTrain.msts = window(bike.hourly.msts, start = c(0, 1), end = c(0, nTrain))

bike.hourly.dshw.pred = dshw(yTrain.msts, h = nValid)
bike.hourly.dshw.pred.mean = msts(bike.hourly.dshw.pred$mean,
                                  seasonal.periods = c(24, 168),
                                  start = c(0, nTrain + 1))
plot(yTrain.msts, xlim = c(0, 4.5), ylim = c(0, 700), xlab = 'Week',
     ylab = 'Hourly Bike Rentals (April 2011)', bty = 'l')
lines(bike.hourly.dshw.pred.mean, lwd = 2, col = 'blue')


#5
library(forecast)
bike.hourly.df = read.csv('12-train-5.csv')
nTotal = length(bike.hourly.df$count)
bike.hourly.msts = msts(bike.hourly.df$count, seasonal.periods = c(24, 168),
                        start = c(0, 1))
plot(bike.hourly.msts)

nTrain = 19 * 24
nValid = nTotal - nTrain
yTrain.msts = window(bike.hourly.msts, start = c(0, 1), end = c(0, nTrain))

bike.hourly.dshw.pred = dshw(yTrain.msts, h = nValid)
bike.hourly.dshw.pred.mean = msts(bike.hourly.dshw.pred$mean,
                                  seasonal.periods = c(24, 168),
                                  start = c(0, nTrain + 1))
plot(yTrain.msts, xlim = c(0, 4.5), ylim = c(0, 700), xlab = 'Week',
     ylab = 'Hourly Bike Rentals (May 2011)', bty = 'l')
lines(bike.hourly.dshw.pred.mean, lwd = 2, col = 'blue')


# 6
library(forecast)
bike.hourly.df = read.csv('12-train-6.csv')
nTotal = length(bike.hourly.df$count)
bike.hourly.msts = msts(bike.hourly.df$count, seasonal.periods = c(24, 168),
                        start = c(0, 1))
plot(bike.hourly.msts)

nTrain = 19 * 24
nValid = nTotal - nTrain
yTrain.msts = window(bike.hourly.msts, start = c(0, 1), end = c(0, nTrain))

bike.hourly.dshw.pred = dshw(yTrain.msts, h = nValid)
bike.hourly.dshw.pred.mean = msts(bike.hourly.dshw.pred$mean,
                                  seasonal.periods = c(24, 168),
                                  start = c(0, nTrain + 1))
plot(yTrain.msts, xlim = c(0, 4.5), ylim = c(0, 700), xlab = 'Week',
     ylab = 'Hourly Bike Rentals (June 2011)', bty = 'l')
lines(bike.hourly.dshw.pred.mean, lwd = 2, col = 'blue')


write.excel = function(x, row.names=FALSE, col.names=TRUE) {
  write.table(x, sep="\t")
}

write.excel(bike.hourly.dshw.pred.mean)
