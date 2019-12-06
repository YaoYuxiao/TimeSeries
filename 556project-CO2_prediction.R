library(tseries)
library(forecast)
library(tidyverse)
library(itsmr)
library(HarmonicRegression)

raw_data=read_csv("CO2.csv")
str(raw_data) # internal structure display

#-------------------data preparation------------------------
data=raw_data%>%  
  select(Interpolated)
# TODO 1) 1958.4-2019.9? 2) missing value

#-----------------------time series ------------------------
ts<-ts(data,1:736,frequency=12,start=c(1958,4),end=c(2019,7))
ts
#-----------------------visualization ------------------------
# TODO plot using ggplot2 and ggfortify? 
# tutorial: https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
plot.ts(ts) # plot:observations
tsdisplay(ts) # plot: ts, ACF, PACF
dc<-decompose(ts) #decomposition of addictive time series # ???
plot(dc) # plot: observed,trend,seasonal,random
tslog <- log(ts)# smoothing
# adf.test # rank test ...
plot.ts(tsdiff <- diff(tslog,lag=12,difference=1))# differencing

plot.ts(tsdiff2 <- diff(tsdiff, difference=1))


acf(tsdiff2) 
pacf(tsdiff2)



# tests 


#-----------------------forecast model ------------------------

d.arima <- auto.arima(ts) #auto ARIMA model (0,1,2)(2,1,2)
d.arima  


model0<-Arima(ts,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
model0

model00<-Arima(ts,order=c(1,1,1),seasonal=list(order=c(1,1,2),period=12))
model00#387.51
model1<-Arima(ts,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12))
model1 # aic = 390.05



# tshar<-harmonic.regression(as.vector(ts), 1:length(ts),Tau=12,trend.eliminate=TRUE,trend.degree=1)
# tshardata<-ts(tshar,1:736,frequency=12,start=c(1958,4),end=c(2019,7))
# tshar
# tshardata
model1<-Arima(tshardata,order=c(1,1,1),seasonal=list(order=c(2,1,2),period=12))
model1 # aic = 390.05

#fit <- auto.arima(tslog, xreg = fourier(ts, K = 3),
               #   seasonal = FALSE, lambda = 0)

test(fit$residuals)

acf(resid(model0)) #residual analysis

test(model0$residuals)

# d.forecast <- forecast.Arima(model1, h=10,level = c(95)) # forecast
d.forecast<-predict(model0,n.ahead=3)
d.forecast#Nov 2019  410.4268

autoplot(d.forecast)

