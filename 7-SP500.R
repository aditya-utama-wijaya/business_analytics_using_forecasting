library(gdata)

perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'
sp500 = read.xls('7-SP500.xls', sheet = 'Data', perl = perl)
str(sp500)
sp500.ts = ts(sp500$Close, start = c(1995, 5), end = c(2003, 8), freq = 12)
plot(sp500.ts)

library(tseries)
adf.test(sp500.ts)
