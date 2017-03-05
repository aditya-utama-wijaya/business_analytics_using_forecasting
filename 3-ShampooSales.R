install.packages('gdata')
library(gdata)

perl = 'C:/Strawberry/perl/bin/perl5.24.0.exe'
shampooSales = read.xls('3-ShampooSales.xls', sheet = 'Sheet1', perl = perl)
