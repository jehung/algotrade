library(quantmod)
library(IBrokers)
library(quantmod)
library(TTR) # technical trading rules package
library(PerformanceAnalytics)

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


fx = cbind(EURJPY, GBPJPY, CHFJPY, USDJPY, CADJPY, AUDJPY, NZDJPY)
colnames(fx) = c('EURJPY', 'GBPJPY', 'CHFJPY', 'USDJPY', 'CADJPY', 'AUDJPY', 'NZDJPY')
fx = data.frame(fx)
fx$date = time(fx[,2])+(Sys.Date()-179-1)

# initial day and initial pair
tws = twsConnect(port=7497)
strongest = 'USD'
weakest = 'CAD'
# querying data for the initial pair
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
data = reqHistoricalData(tws, pair, whatToShow='MIDPOINT', barSize='1 hour', duration='6 M')
index(data) = as.POSIXct(index(data))
close = paste(symbol, 'Close', sep='.')
ret = data[, close]
daily_ret = Delt(data[,close])

# set parameters
n = 2 # relative strenght based on past n days: consider 2, 3, 4, 5
i = n # starting with the nth in query
this_ret = NULL
trades_signal = NULL
trades_signal_index = NULL
this_ret_index = NULL
this_window = 3
time_range = unique(as.Date(index(data)))

for (i in as.list(time_range[-c(1:3)])) {
    i = as.Date(i)
    # the first numbeer of days have no data due to sma/ema, so skip the first days
    print(paste('start analyzing hour', i, symbol))
    
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
    dir_cci[cci[,'cci'] >= 90] = "Buy"
    dir_cci[(cci[,'cci'] < 90) & (cci[,'cci']> -90)] = "DoNothing"
    dir_cci[cci[,'cci'] <= -90] = "Sell"
    
    dir_sar = data.frame(matrix(NA,dim(sar)[1],1))
    dir_sar[ret > sar[,'sar']] = "Buy"
    dir_sar[ret < sar[, 'sar']] = "Sell"
    
    datetime = as.POSIXct(index(ret), tz='PST')
    indicators = cbind(datetime, dir_ema9, dir_sma20, dir_sma50, dir_bb, dir_cci, dir_sar)
    colnames(indicators)=c('datetime', 'dir_ema9', 'dir_sma20', 'dir_sma50', 'dir_bb', 
                           'dir_cci', 'dir_sar')
    this_indicators = indicators[as.Date(indicators$datetime)==as.Date(i), ]
    
    up = (this_indicators[,'dir_ema9']=='Above') & (this_indicators[,'dir_sma20']=='Above') & 
        (this_indicators[,'dir_sma50']=='Above')
    down = (this_indicators[,'dir_ema9']=='Below') & (this_indicators[,'dir_sma20']=='Below') & 
        (this_indicators[,'dir_sma50']=='Below') 
    up_condition = (this_indicators[,'dir_bb']=='Between') & (this_indicators[,'dir_sar']=='Buy') & (this_indicators[,'dir_cci']=='Buy')  
    down_condition = (this_indicators[,'dir_bb']=='Between') & (this_indicators[,'dir_sar']=='Sell') & (this_indicators[,'dir_cci']=='Sell')  
    
    #signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down_condition, 1, 0))
    signal = ifelse(up, ifelse(up_condition, 1, 0), ifelse(down, ifelse(down_condition, -1, 0), 0))
    trades_signal = c(trades_signal, signal)
    
    if (length(signal)==0) {
        next
    }
    
    #if (!is.na(signal)) {
    #    trades_signal_index = c(trades_signal_index, i)
    #    trades_signal = c(trades_signal, signal)
    #}
    
    if (length(trades_signal)>1) {
        print('entered position')
        this_ret = c(this_ret, daily_ret[as.Date(index(daily_ret))==as.Date(i)]*signal)
            } 
    else {
        this_ret = c(this_ret, 0)
        print('have not found a position')
        this_seq = seq(as.Date(i)-this_window, as.Date(i), 'days')
        this_fx = subset(fx, fx$date %in% this_seq)
        fx2d = tail(this_fx, n)
        rs = (fx2d[n,1:7]-fx2d[1,1:7])/fx2d[1,1:7]
        
        strongest_index = which.max(c(rs[1,]))
        weakest_index = which.min(c(rs[1,]))
        strongest = colnames(rs)[strongest_index]
        weakest = colnames(rs)[weakest_index]
        strongest = sub('JPY', '', strongest)
        weakest = sub('JPY', '', weakest)
        print(strongest)
        print(weakest)   
        
        # after having chosen the new pair, query the data for the new pair
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
        data = reqHistoricalData(tws, pair, whatToShow='MIDPOINT', barSize='1 hour', duration='6 M')
        index(data) = as.POSIXct(index(data))
        close = paste(symbol, 'Close', sep='.')
        ret = data[, close]
        daily_ret = Delt(data[,close])
    }
    
    print(paste('finish analyzing hour', i, symbol, signal))
    
    Sys.sleep(5)
}

twsDisconnect(tws)
######################################################################################

# analytics
this_ret = as.ts(this_ret)

plot(this_ret, main='Trade Return Under Strategy')

trade_ret = Return.cumulative(this_ret)
annual_ret = Return.annualized(this_ret, scale=length(trades_signal))
charts.PerformanceSummary(as.ts(this_ret), main='Dynamic Deployment of Trading Strategy')

summary(as.ts(this_ret))
SharpeRatio(as.ts(this_ret), Rf = 0, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(as.ts(this_ret), Rf = 0, scale=length(trades_signal))