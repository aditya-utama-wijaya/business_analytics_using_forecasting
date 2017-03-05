library(gdata)

perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'
sept11travel = read.xls('2-Sept11Travel.xls', sheet = 'Sheet1', perl = perl)
str(sept11travel)
afterAttack.ts = ts(sept11travel$afterAttack, start = c(1990, 1), end = c(2004, 4), freq = 52)
air.ts = ts(sept11travel$Air.RPM..000s., start = c(1990, 1), end = c(2004, 4), freq = 12)
rail.ts = ts(sept11travel$Rail.PM, start = c(1990, 1), end = c(2004, 4), freq = 12)
vmt.ts = ts(sept11travel$VMT..billions., start = c(1990, 1), end = c(2004, 4), freq = 12)

nValid = 16
nTrain = length(air.ts) - nValid
afterAttack.train.ts = window(afterAttack.ts, start = c(1990, 1), end = c(1990, nTrain))
afterAttack.valid.ts = window(afterAttack.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))

air.train.ts = window(air.ts, start = c(1990, 1), end = c(1990, nTrain))
air.valid.ts = window(air.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))

rail.train.ts = window(rail.ts, start = c(1990, 1), end = c(1990, nTrain))
rail.valid.ts = window(rail.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))

vmt.train.ts = window(vmt.ts, start = c(1990, 1), end = c(1990, nTrain))
vmt.valid.ts = window(vmt.ts, start = c(1990, nTrain + 1), end = c(1990, nTrain + nValid))

air.stl.run = stl(air.train.ts, s.window = 'periodic')
plot(air.stl.run, main = 'Decomposition of Air series')
rail.stl.run = stl(rail.train.ts, s.window = 'periodic')
plot(rail.stl.run, main = 'Decomposition of Rail series')
vmt.stl.run = stl(vmt.train.ts, s.window = 'periodic')
plot(vmt.stl.run, main = 'Decomposition of VMT series')

air.model.without.intervention = tslm(air.train.ts ~ trend + season)
air.model.with.intervention = tslm(air.train.ts ~ trend + season + afterAttack.train.ts)
plot(air.train.ts - air.model.without.intervention$fitted.values, ylab = 'Residuals', main = 'Air Series Errors', bty = 'l', xlim = c(1990, 2003.25), xaxt = 'n')
axis(1, at = seq(1990, 2003, 1), labels = format(seq(1990, 2003, 1)))
lines(air.train.ts - air.model.with.intervention$fitted.values, lwd = 2, col = 'blue')
legend(1990, -40, c('Errors without intervention', 'Errors with intervention'), lwd = c(1, 2), col = c('black', 'blue'), bty = 'n')
summary(air.model.with.intervention)

rail.model.without.intervention = tslm(rail.train.ts ~ trend + season)
rail.model.with.intervention = tslm(rail.train.ts ~ trend + season + afterAttack.train.ts)
plot(rail.train.ts - rail.model.without.intervention$fitted.values, ylab = 'Residuals', main = 'Rail Series Errors', bty = 'l', xlim = c(1990, 2003.25), xaxt = 'n')
axis(1, at = seq(1990, 2003, 1), labels = format(seq(1990, 2003, 1)))
lines(rail.train.ts - rail.model.with.intervention$fitted.values, lwd = 2, col = 'blue')
legend(1995, 60, c('Errors without intervention', 'Errors with intervention'), lwd = c(1, 2), col = c('black', 'blue'), bty = 'n')
summary(rail.model.with.intervention)

vmt.model.without.intervention = tslm(vmt.train.ts ~ trend + season)
vmt.model.with.intervention = tslm(vmt.train.ts ~ trend + season + afterAttack.train.ts)
plot(vmt.train.ts - vmt.model.without.intervention$fitted.values, ylab = 'Residuals', main = 'VMT Series Errors', bty = 'l', xlim = c(1990, 2003.25), xaxt = 'n')
axis(1, at = seq(1990, 2003, 1), labels = format(seq(1990, 2003, 1)))
lines(vmt.train.ts - vmt.model.with.intervention$fitted.values, lwd = 2, col = 'blue')
legend(1991, 10, c('Errors without intervention', 'Errors with intervention'), lwd = c(1, 2), col = c('black', 'blue'), bty = 'n')
summary(vmt.model.with.intervention)
