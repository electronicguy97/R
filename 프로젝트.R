library(TTR)
library(forecast)

df <- read.csv('d:/data/project/project_GDP.csv',header=T,fileEncoding = 'CP949')
head(df)
class(df)
dim(df)
nrow(df)
ncol(df)
library(tidyr)
df <- fill(df, .direction = 'down')
head(df)
df <- df %>% fill(세계, .direction = 'up')
#세계만 추출
library(dplyr)
df_A <- df %>% select(세계)
df_A

df_ts <- ts(df_A)
win.graph(); plot.ts(df_ts) 

df_sma3 <- SMA(df_ts, n = 3) 
df_sma8 <- SMA(df_ts, n = 8) 
df_sma12 <- SMA(df_ts, n = 12) 

win.graph();par(mfrow = c(2,2));plot.ts(df_sma3);plot.ts(df_sma8);plot.ts(df_sma12)


df_diff1 <- diff(df_ts, differences = 1)
df_diff2 <- diff(df_ts, differences = 2)
df_diff3 <- diff(df_ts, differences = 3)
win.graph();par(mfrow = c(2,2));plot.ts(df_diff1);plot.ts(df_diff2);plot.ts(df_diff3)

#acf(자기 상관함수), pacf(부분 상관함수)
win.graph(); par(mfrow = c(1,2));acf(df_A, lag.max = 20);pacf(df_A, lag.max = 20)
win.graph();par(mfrow = c(1,2)); acf(df_diff1, lag.max = 20);pacf(df_diff1, lag.max = 20)

library(tseries)
df_ts
adf<-adf.test(diff(log(df_ts[2:62,])), alternative="stationary", k=0)
auto.arima(ts(df_A))
df_arima <- arima(df_diff1, order = c(0,1,1))
#예시 (p,d,q) pacf AR(2) acf MA(3) => (3,0,2)
df_fcast <- forecast(df_arima, h = 5)
df_fcast
win.graph(); plot(df_fcast)
summary(df_fcast);
df_arima2 <- arima(df_diff1, order = c(1,1,2))
df_arima2
df_fcast <- forecast(df_arima2, h = 5)
df_fcast
win.graph(); plot(df_fcast)


