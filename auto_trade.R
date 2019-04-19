library(IBrokers)
library(quantmod)
library(TTR) # technical trading rules package
library(PerformanceAnalytics)
library(chron)


## TODO: decisions on different timeframes
## TODO: cron job to send notification for entry opportunity
## TODO: position management once entered


strongest = 'AUD'
weakest = 'CAD'
symbol = paste(strongest, weakest, sep='.')
tws = twsConnect(port=7497)
pair = twsCurrency(strongest, weakest)
data = reqHistoricalData(tws, pair, whatToShow='MIDPOINT', barSize='1 hour', duration='6 M')
index(data) = as.Date(index(data))
close = paste(symbol, 'Close', sep='.')
ret = data[, close]
daily_ret = Delt(data[,close]) 

ema9 = EMA(ret, 9)
sma20 = SMA(ret, 20)
sma50 = SMA(ret, 50)
bb = BBands(ret,20,"SMA",2)
cci = CCI(data[,c(2, 3, 4)]) # High, low, close
sar = SAR(data[,c(2, 3)]) #High, low
bbwidth = bb[,'up']-bb[,'dn']

dir_ema9 = data.frame(matrix(NA,dim(ema9)[1],1))
dir_ema9[ret >= ema9] = "Above"
dir_ema9[ret < ema9] = "Below"

dir_sma20 = data.frame(matrix(NA,dim(sma20)[1],1))
dir_sma20[ret >= sma20] = "Above"
dir_sma20[ret < sma20] = "Below"

dir_sma50 = data.frame(matrix(NA,dim(sma50)[1],1))
dir_sma50[ret >= sma50] = "Above"
dir_sma50[ret < sma50] = "Below"

dir_bb = data.frame(matrix(NA,dim(bb)[1],1))
dir_bb[(ret > bb[,'dn']) & (ret < bb[,'up'])] = "Between"
dir_bb[(ret < bb[,'dn']) | (ret > bb[,'up'])] = "Not Between"

dir_cci = data.frame(matrix(NA,dim(cci)[1],1))
dir_cci[cci[,'cci'] >= 95] = "Buy"
dir_cci[(cci[,'cci'] < 95) & (cci[,'cci']> -95)] = "DoNothing"
dir_cci[cci[,'cci'] <= -95] = "Sell"

dir_sar = data.frame(matrix(NA,dim(sar)[1],1))
dir_sar[ret > sar[,'sar']] = "Buy"
dir_sar[ret < sar[, 'sar']] = "DoNothing"




indicators = cbind(dir_ema9, dir_sma20, dir_sma50, dir_bb, dir_cci, dir_sar)
colnames(indicators)=c('dir_ema9', 'dir_sma20', 'dir_sma50', 'dir_bb', 
                       'dir_cci', 'dir_sar')


Signal = NULL
up = (indicators[,'dir_ema9']=='Above') & (indicators[,'dir_sma20']=='Above') & 
    (indicators[,'dir_sma50']=='Above')
down = (indicators[,'dir_ema9']=='Below') & (indicators[,'dir_sma20']=='Below') & 
    (indicators[,'dir_sma50']=='Below') 
up_condition = (indicators[,'dir_bb']=='Between') & (indicators[,'dir_sar']=='Buy') 
down_condition = (indicators[,'dir_bb']=='Between') & (indicators[,'dir_sar']=='Sell') 

#signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down_condition, 1, 0))
signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down, ifelse(down_condition, -1, 0), 0))




# analytics
plot(ret, main='My currency pair')
plot(daily_ret, main='My currency differenced')


trade_ret = Return.cumulative(daily_ret*signal)
annual_ret = Return.annualized(daily_ret*signal, scale=252*24)
charts.PerformanceSummary(daily_ret*signal)

summary(as.xts(daily_ret*signal))
SharpeRatio(as.xts(daily_ret*signal), Rf = 0, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(daily_ret*signal, Rf = 0, scale=252)