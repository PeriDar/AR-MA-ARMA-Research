library(tseries)
library(forecast)

#Generating AR(3)
set.seed(123) 
ar_coeff=c(0.3,0.1,0.4)
ar3_ts=arima.sim(n=500,list(ar=ar_coeff))

plot(ar3_ts,ylab='')

# Checking for stationarity
## Visual
acf(ar3_ts,main='')
acf(ar3_ts,lag.max = 500,main='')
## Unit root test
adf.test(ar3_ts)
pp.test(ar3_ts)
kpss.test(ar3_ts)

# Finding order p
## Visual
pacf(ar3_ts,main='')
## Indicators
AIC=c()
BIC=c()

for (i in 1:10) {
  mod_test_loop=arima(ar3_ts,order = c(i,0,0))
  AIC[i]=AIC(mod_test_loop)
  BIC[i]=BIC(mod_test_loop)
}

plot(AIC,type = 'b',pch=16,ylim = c(1390,1520),
     ylab = '',xlab = 'p', main = '')
lines(BIC,type = 'b',col='red',pch=18)
legend('topright',legend = c('AIC','BIC'),fill = c('black','red'))

# Creating the models
my_mod=arima(ar3_ts,order = c(3,0,0))  # Model 1 
auto_mod=auto.arima(ar3_ts) # Model 2 (auto-model)

# Evaluating the models
tsdiag(my_mod)  # Fast way to check with plot
tsdiag(auto_mod)  # Fast way to check with plot
## Checking (AIC, BIC, ME, RMSE... etc)
summary(my_mod)
summary(auto_mod)
## Checking model's residuals
res_myMod=resid(my_mod)
res_myAutoMod=resid(auto_mod)
### Other possible measures:
mean((res_myMod)^2) #MSE
mean((res_myAutoMod)^2) #MSE
sum((res_myMod)^2) #SSE
sum((res_myAutoMod)^2)#SSE
### Ljung-Box test 
Box.test(res_myMod, type = 'Ljung-Box')
Box.test(res_myAutoMod,type = 'Ljung-Box')
## Best plot for residuals
tsdisplay(res_myMod,plot.type = "histogram")
tsdisplay(res_myAutoMod)


####################################################
# Example of validation 

samp=ts(ar3_ts[1:450]) 

best_mod_samp=arima(samp,order = c(3,0,0))
auto_mod_samp=auto.arima(samp)

best_forecast=forecast(best_mod_samp,h=50)
auto_forecast=forecast(auto_mod_samp,h=50)

mean((ar3_ts[451:500]-best_forecast$mean)^2)
mean((ar3_ts[451:500]-auto_forecast$mean)^2)

# Almost equal values, best_forecast slightly better