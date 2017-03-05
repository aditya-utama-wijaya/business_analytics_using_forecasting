library(openxlsx)

one.pair = read.xlsx('8-Walmart_One_Pair.xlsx', sheet = 'Sheet1')
one.pair$Date = as.Date(one.pair$Date, origin = '1899-12-30')
one.pair$Date = format(one.pair$Date, '%Y-%m-%d')
one.pair$Date = as.factor(one.pair$Date)
str(one.pair)

nTrain = 143
nTest = 39
yTrain.ts = ts(one.pair$Weekly_Sales[1:nTrain], freq = 52, start = c(2011, 5))
plot(yTrain.ts)
stl.run = stl(yTrain.ts, s.window = 'periodic')
plot(stl.run)

xTrain = data.frame(IsHoliday = one.pair$IsHoliday[1:nTrain])
xTest = data.frame(IsHoliday = one.pair$IsHoliday[(nTrain + 1):(nTrain + nTest)])
library(forecast)
stlm.reg.fit = stlm(yTrain.ts, s.window = 'periodic', xreg = xTrain, method = 'arima')
stlm.reg.fit$model
stlm.reg.pred = forecast(stlm.reg.fit, xreg = xTest, h = nTest)
plot(stlm.reg.pred, ylab = 'Weekly Sales', xlab = 'Year')

# Equivalent alternative approach to stlm
seasonal.comp = stl.run$time.series[, 1]
deseasonalized.ts = yTrain.ts - seasonal.comp

# Alternatively, this line returns the deseasonalized time series
seasadj(stl.run)

arima.fit.deas = auto.arima(deseasonalized.ts, xreg = xTrain)
arima.fit.deas.pred = forecast(arima.fit.deas, xreg = xTest, h = nTest)
seasonal.comp.pred = snaive(seasonal.comp, h = nTest)
alt.forecast = arima.fit.deas.pred$mean + seasonal.comp.pred$mean

stlm.reg.pred$mean - alt.forecast
