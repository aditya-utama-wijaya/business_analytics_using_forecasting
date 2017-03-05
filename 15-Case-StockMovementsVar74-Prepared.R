library(openxlsx)

stock = read.xlsx('15-Case-StockMovementsVar74-Prepared.xlsx', sheet = 'Case-StockMovementsVar74')
stock.ts = ts(stock$TargetVariable)
str(data)

nValid = 2539
nTrain = length(stock.ts) - nValid
train.ts = window(stock.ts, start = 1, end = nTrain)
valid.ts = window(stock.ts, start = nTrain + 1, end = nTrain + nValid)
table(train.ts)
stock.naive.1 = rep(1, nValid)

library(forecast)
stock.naive.2 = naive(train.ts, h = nValid)

library(caret)
confusionMatrix(stock.naive.1, valid.ts)
confusionMatrix(stock.naive.2$mean, valid.ts)

stock.2 = read.xlsx('15-Case-StockMovementsVar74-Prepared.xlsx', sheet = 'With lags and diff')
nValid = 2539
nTrain = nrow(stock.2) - nValid
stock.train.2 = stock.2[1:nTrain, ]
stock.valid.2 = stock.2[(nTrain + 1):(nTrain + nValid), ]

set.seed(201)
stock.avnnet = avNNet(Lag13_Target ~ Variable74OPEN + Variable74HIGH
                      + Variable74LOW + Variable74LAST_PRICE + diff74OPEN + diff74HIGH
                      + diff74LOW + diff74LAST, data = stock.train.2, size = 3)
summary(stock.avnnet)

stock.avnnet.pred = predict(stock.avnnet, stock.valid.2, type = 'response')

confusionMatrix(ifelse(stock.avnnet$fitted.values > 0.5, 1, 0), stock.train.2$Lag13_Target)
confusionMatrix(ifelse(stock.avnnet.pred > 0.5, 1, 0), stock.valid.2$Lag13_Target)

# tetep bikin error plot
