library(quantmod)
library(TFX)

# EUR, GBP, CHF, USD, JPY, CAD, AUD, NZD
getSymbols('EUR/JPY', src='oanda', auto.assign=TRUE)
getSymbols('GBP/JPY', src='oanda', auto.assign=TRUE)
getSymbols('CHF/JPY', src='oanda', auto.assign=TRUE)
getSymbols('USD/JPY', src='oanda', auto.assign=TRUE)
getSymbols('CAD/JPY', src='oanda', auto.assign=TRUE)
getSymbols('AUD/JPY', src='oanda', auto.assign=TRUE)
getSymbols('NZD/JPY', src='oanda', auto.assign=TRUE)

EURJPY = EURJPY/ 100
GBPJPY = GBPJPY/ 100
CHFJPY = CHFJPY/ 100
USDJPY = USDJPY/ 100
CADJPY = CADJPY/ 100
AUDJPY = AUDJPY/ 100
NZDJPY = NZDJPY/100


fx = cbind(EURJPY, GBPJPY, USDJPY, CADJPY, AUDJPY, NZDJPY)
colnames(fx) = c('EURJPY', 'GBPJPY', 'USDJPY', 'CADJPY', 'AUDJPY', 'NZDJPY')
fx = data.frame(fx)

fx2d = tail(fx, 2)
rs = (fx2d[2,]-fx2d[1,])/fx2d[1,]

strongest_index = which.max(c(rs[1,]))
weakest_index = which.min(c(rs[1,]))
strongest = colnames(rs)[strongest_index]
weakest = colnames(rs)[weakest_index]
strongest = sub('JPY', '', strongest)
weakest = sub('JPY', '', weakest)
