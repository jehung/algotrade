library(quantmod)
library(IBrokers)
library(quantmod)
library(TTR) # technical trading rules package
library(PerformanceAnalytics)
library(nnet)
library(caret)

# strategy: directional trading on forex with both momentum strategy and mean reversion strategy
# with momentum determination using the 1-hr chart
# entry setup: pullback, bear rally, bases
# algo does not set set stop: use EVT/VaR
# exit condition: use PSAR
# algo give signal for entry and exit
# algo does not use ATR but i do
# relative strength tool to dynamtically pair strongeest and weakest currency (use JPY as benchmark) 

## TODO: cron job to send notification for entry opportunity
## TODO: include position management such as stops

# EUR, GBP, CHF, USD, JPY, CAD, AUD, NZD
getSymbols('EUR/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('GBP/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('CHF/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('USD/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('CAD/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('AUD/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)
getSymbols('NZD/JPY', src='oanda', auto.assign=TRUE, from=Sys.Date()-179)

EURJPY = EURJPY/ 100
GBPJPY = GBPJPY/ 100
CHFJPY = CHFJPY/ 100
USDJPY = USDJPY/ 100
CADJPY = CADJPY/ 100
AUDJPY = AUDJPY/ 100
NZDJPY = NZDJPY/100

n = 3
fx = cbind(EURJPY, GBPJPY, CHFJPY, USDJPY, CADJPY, AUDJPY, NZDJPY)
colnames(fx) = c('EURJPY', 'GBPJPY', 'CHFJPY', 'USDJPY', 'CADJPY', 'AUDJPY', 'NZDJPY')
fx = data.frame(fx)
fx$date = time(fx[,2])+(Sys.Date()-179-1)
fx2d = tail(fx, n)
rs = (fx2d[n,1:7]-fx2d[1,7])/fx2d[1,1:7]

strongest_index = which.max(c(rs[1,]))
weakest_index = which.min(c(rs[1,]))
strongest = colnames(rs)[strongest_index]
weakest = colnames(rs)[weakest_index]
strongest = sub('JPY', '', strongest)
weakest = sub('JPY', '', weakest)

# after having chosen the new pair, query the data for the new pair
tws = twsConnect(port=7497)
pair = tryCatch({pair=reqContractDetails(tws, twsCurrency(strongest, weakest))[[1]]$contract}, 
                error=function(e) 
                {pair=reqContractDetails(tws, twsCurrency(weakest, strongest))[[1]]$contract
                return(pair)
                }
)
symbol = tryCatch({symbol=reqContractDetails(tws, twsCurrency(strongest, weakest))[[1]]$marketName}, 
                  error=function(e) 
                  {symbol=reqContractDetails(tws, twsCurrency(weakest, strongest))[[1]]$marketName
                  return(symbol)
                  }
)
data = reqHistoricalData(tws, pair, whatToShow='BID', barSize='1 hour', duration='6 M')
index(data) = as.Date(index(data))
close = paste(symbol, 'Close', sep='.')
ret = data[, close]
daily_ret = Delt(data[,close])
twsDisconnect(tws)

ema9 = EMA(ret, 9)
sma20 = SMA(ret, 20)
sma50 = SMA(ret, 50)
bb = BBands(ret,20,"SMA",2)
macd = MACD(ret, nFast =12, nSlow = 26, nSig = 9, maType="SMA", percent= FALSE)
macd_nn = macd[,'signal']-macd[,'macd']
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
dir_cci[cci[,'cci'] >= 90] = "Buy"
dir_cci[(cci[,'cci'] < 90) & (cci[,'cci']> -90)] = "DoNothing"
dir_cci[cci[,'cci'] <= -90] = "Sell"

dir_sar = data.frame(matrix(NA,dim(sar)[1],1))
dir_sar[ret > sar[,'sar']] = "Buy"
dir_sar[ret < sar[, 'sar']] = "Sell"

indicators = cbind(dir_ema9, dir_sma20, dir_sma50, dir_bb, dir_cci, dir_sar)
colnames(indicators)=c('dir_ema9', 'dir_sma20', 'dir_sma50', 'dir_bb', 
                       'dir_cci', 'dir_sar')

Signal = NULL
up = (indicators[,'dir_ema9']=='Above') & (indicators[,'dir_sma20']=='Above') & 
  (indicators[,'dir_sma50']=='Above')
down = (indicators[,'dir_ema9']=='Below') & (indicators[,'dir_sma20']=='Below') & 
  (indicators[,'dir_sma50']=='Below') 
up_condition = (indicators[,'dir_bb']=='Between') & (indicators[,'dir_sar']=='Buy') & (indicators[,'dir_cci']=='Buy')  
down_condition = (indicators[,'dir_bb']=='Between') & (indicators[,'dir_sar']=='Sell') & (indicators[,'dir_cci']=='Sell')  

#signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down_condition, 1, 0))
signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down, ifelse(down_condition, -1, 0), 0))

# analytics
plot(ret, main='My currency pair')
plot(daily_ret, main='My currency differenced')

this_daily_ret = daily_ret[(floor(dim(daily_ret)[1]*0.8)+1):dim(daily_ret)[1]]
this_signal = signal[(floor(length(signal)*0.8)+1):length(signal)]
trade_ret = Return.cumulative(this_daily_ret*this_signal)
annual_ret = Return.annualized(this_daily_ret*this_signal, scale=length(this_daily_ret))
charts.PerformanceSummary(this_daily_ret*this_signal, main='Rule-Based Trading Strategy')
summary(this_daily_ret*this_signal)
SharpeRatio((this_daily_ret*this_signal), Rf = 0, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(this_daily_ret*this_signal, Rf = 0, scale=length(this_daily_ret))


#nnet
lag_ret = (ret - Lag(ret,1)) / Lag(ret,1)
dir = data.frame(matrix(NA,dim(ret)[1],1))
dir[lag_ret> 0] = 1
dir[lag_ret< 0] = -1
dir[lag_ret == 0] = 0

nn_indicators = cbind(ema9, sma20, sma50, macd_nn, bbwidth)
nn_indicators = as.data.frame(nn_indicators)
colnames(nn_indicators) = c('ema9', 'sma20', 'sma50', 'macd', 'bbwidth')
nn_indicators = scale(nn_indicators)

nn_indicators_train = nn_indicators[1:floor(dim(nn_indicators)[1]*0.8*0.8), ]
nn_indicators_val = nn_indicators[(floor(dim(nn_indicators)[1]*0.8*0.8)+1):floor(dim(nn_indicators)[1]*0.8), ]
nn_indicators_test = nn_indicators[(floor(dim(nn_indicators)[1]*0.8)+1):dim(nn_indicators)[1], ]
dir_train = dir[1:floor(dim(nn_indicators)[1]*0.8*0.8), ]
dir_val = dir[(floor(dim(nn_indicators)[1]*0.8*0.8)+1):floor(dim(nn_indicators)[1]*0.8), ]
dir_test = dir[(floor(dim(nn_indicators)[1]*0.8)+1):dim(nn_indicators)[1], ]

nn_df = as.data.frame(cbind(nn_indicators_train, dir_train))
nn_df$dir_train = factor(nn_df$dir_train, levels=c(-1, 0, 1))
nn_dfval = as.data.frame(cbind(nn_indicators_val, dir_val))
nn_dfval$dir_val = factor(nn_dfval$dir_val, levels=c(-1, 0, 1))

nn = nnet(dir_train~., data = nn_df, size=1, trace=TRUE)
nn_val = predict(nn, nn_dfval)

pred_nn_val = data.frame(matrix(NA,dim(nn_val)[1],1))
pred_nn_val[nn_val[,1] > 0.5,] = 1
pred_nn_val[(nn_val[,1] < 0.5) & (nn_val[,1] > 0.01),] = 0
pred_nn_val[nn_val[,1] < 0.01,]  = -1

confuse_mat_val = confusionMatrix(factor(pred_nn_val[,1], levels=c(-1, 0, 1)), nn_dfval[,6])

nn_dftest = predict(nn, nn_indicators_test)
pred_nn_test = data.frame(matrix(NA,dim(nn_dftest)[1],1))
pred_nn_test[nn_dftest[,1] > 0.5,] = 1
pred_nn_test[(nn_dftest[,1] < 0.5) & (nn_dftest[,1] > 0.01),] = 0
pred_nn_test[nn_dftest[,1] < 0.01,]  = -1

confuse_mat_test = confusionMatrix(factor(pred_nn_test[,1], levels=c(-1, 0, 1)), factor(dir_test))

# plot performnace under nn
this_signal = pred_nn_test[,1]
trade_ret = Return.cumulative(tail(daily_ret, length(this_signal))*this_signal)
annual_ret = Return.annualized(tail(daily_ret, length(this_signal))*this_signal, scale=length(this_signal))
charts.PerformanceSummary(tail(daily_ret, length(this_signal))*this_signal, main='Neural Network Trading Strategy')
summary(tail(daily_ret, length(this_signal))*this_signal)
SharpeRatio(tail(daily_ret, length(this_signal))*this_signal, Rf = 0, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(tail(daily_ret, length(this_signal))*this_signal, Rf = 0, scale=length(this_signal))

