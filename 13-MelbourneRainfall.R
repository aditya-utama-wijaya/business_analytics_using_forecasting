library(gdata)

perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'
rain.df = read.xls('13-MelbourneRainfall.xls', sheet = 'data', perl = perl)
rain.df$Date = as.Date(rain.df$Date, format = '%Y-%m-%d')
rain.df$Rainy = ifelse(rain.df$Rainfall.amount..millimetres. > 0, 1, 0)
nPeriods = length(rain.df$Rainy)
rain.df$Lag1 = c(NA, rain.df$Rainfall.amount..millimetres.[1:(nPeriods - 1)])
rain.df$t = seq(1, nPeriods, 1)
rain.df$Seasonal_sine = sin(2 * pi * rain.df$t / 365.25)
rain.df$Seasonal_cosine = cos(2 * pi * rain.df$t / 365.25)

train.df = rain.df[rain.df$Date <= as.Date('2009-12-31',
                                           format = '%Y-%m-%d'), ]
train.df = train.df[-1, ]
valid.df = rain.df[rain.df$Date > as.Date('2009-12-31',
                                          format = '%Y-%m-%d'), ]
xValid = valid.df[, c(4, 6)]

rainy.lr = glm(Rainy ~ Lag1 + Seasonal_sine + Seasonal_cosine,
               data = train.df, family = 'binomial')
summary(rainy.lr)
rainy.lr.pred = predict(rainy.lr, xValid, type = 'response')

library(caret)
confusionMatrix(ifelse(rainy.lr$fitted.values > 0.5, 1, 0), train.df$Rainy)
confusionMatrix(ifelse(rainy.lr.pred > 0.5, 1, 0), valid.df$Rainy)
