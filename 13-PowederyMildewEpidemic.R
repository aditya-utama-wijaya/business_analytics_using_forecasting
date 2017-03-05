library(openxlsx)

pwe = read.xlsx('13-PowederyMildewEpidemic.xlsx', sheet = 'Data')
str(pwe)

pwe$Outbreak = ifelse(pwe$Outbreak == 'Yes', 1, 0)
data = pwe[, c(1, 2)]
pwe.ts = ts(data$Outbreak, start = 1987, end = 2000)
pwe.ts = pwe.ts[c(1:11, 14)]

library(forecast)
naive.pred = naive(pwe.ts, h = 1)
tail(pwe$Outbreak, 4)
tail(naive.pred$fitted, 4)

library(caret)
confusionMatrix(tail(naive.pred$fitted, 4), tail(pwe$Outbreak, 4))


# 5, 6, 7
train.df.1 = pwe[pwe$Year <= 1994, ]
valid.df.1 = pwe[pwe$Year > 1994, ]
xValid.1 = valid.df.1[, c(3, 4)]

train.df.2 = pwe[pwe$Year <= 1995, ]
valid.df.2 = pwe[pwe$Year > 1995, ]
xValid.2 = valid.df.2[, c(3, 4)]

train.df.3 = pwe[pwe$Year <= 1996, ]
valid.df.3 = pwe[pwe$Year > 1996, ]
xValid.3 = valid.df.3[, c(3, 4)]

train.df.4 = pwe[pwe$Year <= 1997, ]
valid.df.4 = pwe[pwe$Year > 1997, ]
xValid.4 = valid.df.4[, c(3, 4)]

pwe.lr.1 = glm(Outbreak ~ Max.temp + Rel.humidity,
               data = train.df.1, family = 'binomial')
pwe.lr.pred.1 = predict(pwe.lr.1, xValid.1, type = 'response')

pwe.lr.2 = glm(Outbreak ~ Max.temp + Rel.humidity,
               data = train.df.2, family = 'binomial')
pwe.lr.pred.2 = predict(pwe.lr.2, xValid.2, type = 'response')

pwe.lr.3 = glm(Outbreak ~ Max.temp + Rel.humidity,
               data = train.df.3, family = 'binomial')
pwe.lr.pred.3 = predict(pwe.lr.3, xValid.3, type = 'response')

pwe.lr.4 = glm(Outbreak ~ Max.temp + Rel.humidity,
               data = train.df.4, family = 'binomial')
pwe.lr.pred.4 = predict(pwe.lr.4, xValid.4, type = 'response')

pwe.lr.pred = c(pwe.lr.pred.1[1], pwe.lr.pred.2[1], pwe.lr.pred.3[1], pwe.lr.pred.4[1])

confusionMatrix(ifelse(pwe.lr.pred > 0.5, 1, 0),
                tail(pwe$Outbreak, 4))


# 10
nPeriods = length(pwe$Outbreak)
pwe$Lag1 = c(NA, pwe$Outbreak[1:(nPeriods - 1)])
train.df.5 = pwe[pwe$Year <= 1997, ]
valid.df.5 = pwe[pwe$Year > 1997, ]
xValid.5 = valid.df.5[, c(5)]
pwe.lr.5 = glm(Outbreak ~ Lag1, data = train.df.5, family = 'binomial')
pwe.lr.5.pred = predict(pwe.lr.5, valid.df.5, type = 'response')

plot(pwe$Max.temp, pch = 19,  bty = 'l', ylab = 'Maximum Temperature',
     col = ifelse(pwe$Outbreak == 1, 'blue', 'red'))
