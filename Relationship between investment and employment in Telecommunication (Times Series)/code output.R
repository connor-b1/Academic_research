setwd("~/Downloads")
#running list of packages
library(lmtest)
source(file="intord.R")
library(dynlm)
library(season)
source(file="RMSPE.R")
library(vars)
library(urca)
source(file="serialcorr.R")
source(file="fitarima(1).R")
install.packages("robust")
library(forecast)
library(stargazer)
nrow(telco)
ppi <- telco$ppi
exp <- telco$usexp
emp <- telco$emp

save(list = ls(.GlobalEnv), file = "telco.Rdata")

#set as ts
ppi = ts(ppi, start = c(2000,01), end = c(2017,12), frequency = 12)
exp =ts(exp, start = c(2000,01), end = c(2017,12), frequency = 12)
emp = ts(emp, start = c(2000,01), end = c(2017,12), frequency = 12)

#plot: visual inspection!
par(mfrow=c(1,3))
plot(ppi, main ="ppi", type="l", col=3)
plot(exp, main = "exp", type="l", col=2)
plot(emp, main = "emp", type="l", col=4)

#logs
lppi <- log(ppi)
lexp <- log(exp)
lemp <- log(emp)

#determine I
setwd("~/Downloads/")
source(file="intord.R")
intord(lppi) #I1
intord(lexp) #I1
intord(lemp) #I1

intord(lexp)

#difference to get stationary version
dppi = diff(lppi)
dexp = diff(lexp)
demp = diff(lemp) 
#plot
par(mfrow=c(1,3))
plot(dppi, main = "dppi", type="l", col=3)
abline(h=mean(dppi),col=1)
plot(dexp, main = "dexp", type="l", col=2)
abline(h=mean(dexp),col=1)
plot(demp, main = "demp", type="l", col=4)
abline(h=mean(demp),col=1)

#seasonality?
#install.packages("dynlm")
library(dynlm)
#install.packages("season")
library(season)
sr = dynlm(demp~L(demp,1:12)+season(demp), start = c(2000,01), end = c(2017,12))
sr_res = dynlm(demp~L(demp,1:12), start = c(2000,01), end = c(2017,12))
anova(sr, sr_res)
#significant at the 5%
AIC(sr); BIC(sr)
AIC(sr_res); BIC(sr_res)

#tests for cointegration
#engle-granger
cr1 = lm(lemp~lppi+lexp)
res1 = cr1$residuals
intord(res1)       #nonstationary
cr2 = lm(lppi~lemp+lexp)
summary(cr2)
res2 = cr2$residuals
intord(res2)       #nonstationary
cr3 = lm(lppi~lemp)
res3 = cr3$residuals
intord(res3)       #nonstationary
#since the above residuals are all nonstationary, 
#EG fails to show evidence of cointegration
library(vars)
y = cbind(demp,dppi,dexp)
# select lags criteria
VARselect(y, lag.max=24, type="const")
# AIC says 3 lags, BIC(SC) says 1 
var3 = VAR(y, type = "const", season = 12)
summary(var3)
jc1 <- ca.jo(yy, type = "trace", ecdet = "const", K=12, season=12)
summary(jc1)
jc2 <- ca.jo(yy, type = "eigen", ecdet = "const", K=3, season=12)
summary(jc2)
yy = cbind(lemp, lppi, lexp)
library(urca)
vecm1 <- ca.jo(y, ecdet = "const", type="eigen", K=14, spec="longrun",
               season=12)
summary(vecm1)
varvec <- vec2var(vecm1)

cointv <- vecm1@V
cointj <- cointv[,1]
yym <- as.matrix(yy)
ecmj <- yym%*%cointj[1:3] + cointj[4] 
ecj <- embed(ecmj,2)
ecmj1 <- ecj[,2]
var3 <- VAR(y, p=2, type="const",exogen=ecmj1,season=12)
summary(var3)
AIC(vecm1)

#testing
source(file="RMSPE.R")
#goodness of fit
AIC(var3);BIC(var3)
AIC(varvec); BIC(varvec)
#serial correlation of residuals 
serial.test(varvec, lags.pt = 12)
serial.test(varvec, lags.bg = 1,type ="BG")
serial.test(varvec, lags.bg = 2,type ="BG")
serial.test(varvec, lags.bg = 3,type ="BG")
serial.test(varvec, lags.bg = 4,type ="BG")
#fail to reject the null at the 1%

#variance decomposition -- need to fix VAR
vard <- fevd(varvec, n.ahead=12)
vard

#impulse response functions -- need to fix VAR 
irfs <- irf(var3, impulse = "demp", response = c("dppi"), boot = TRUE)
plot(irfs)
irfs <- irf(var3, impulse = "demp", response = c("dexp"), boot = TRUE)
plot(irfs)

#dynamic regression models
library(dynlm)
rb1 = dynlm(demp~L(demp,1:3)+season(demp)+L(dppi,0:12)+ L(dexp,0:6), start=c(2000,01), end = c(2017,12), frequency = 12)
#testing
summary(rb1)
AIC(rb1); BIC(rb1)
serialcorr(rb1)
plot(rb1)

rb2 = dynlm(demp~L(demp,1:3)+season(demp)+L(dppi,0:6)+ L(dexp,0:12), start=c(2000,01), end = c(2017,12), frequency = 12)
stargazer(rb2, type="text", title="DynLM Results", out = "hello.txt")
getwd()
#testing
summary(rb2)
AIC(rb2); BIC(rb2)
serialcorr(rb2)
RMSPE(mrb2)
mrb2 <- as.numeric(rb2)

rb3 = dynlm(demp~L(demp,1:3)+season(demp)+L(dppi,0:12)+ L(dexp,0:12), start=c(2000,01), end = c(2017,12), frequency = 12)
#testing
summary(rb3)
AIC(rb3); BIC(rb3)
serialcorr(rb3)
plot(rb3)

rb4 = dynlm(demp~L(demp,1:3)+L(dppi,0:6)+ L(dexp,0:12), start=c(2000,01), end = c(2017,12), frequency = 12)
#testing
summary(rb4)
AIC(rb4); BIC(rb4)
serialcorr(rb4)
plot(rb4)

rb5 = dynlm(demp~L(demp,1:3)+L(dppi,0:12), start=c(2000,01), end = c(2017,12), frequency = 12)
#testing
summary(rb5)
AIC(rb5); BIC(rb5)
serialcorr(rb5)
plot(rb5)

#granger causality 

#deciding on utility of seas(demp)
anova(rb3,rb4)
#seas dummy significant at the 10%

#deciding on utility of dexp
anova(rb4,rb5)
#dexp is insignificant

#testing
#serial correlation
source(file="serialcorr.R")
serialcorr(rb1)
#autocorrelation 
install.packages("robust")
source(file="autocorr.R")

#SARIMA 

source(file="fitarima(1).R")
fitARIMA(log(emp), order=c(1,1,1))
fitARIMA(log(emp), order=c(2,1,1))
fitARIMA(log(emp), order=c(3,1,1))
auto.arima(log(emp))
junk <- demp[0:(length(demp)-11)]
junk
arima1 <- arima(junk), order=c(3,1,1))
arima2 <- arima(junk, order=c(3,0,1),seasonal = list(order = c(0,0,0), period = 12),method="ML")
summary(arima1)
summary(arima2)
AIC(arima1); BIC(arima1)
AIC(arima2); BIC(arima2)

#testing 
acf(as.numeric(arima2$residuals), lag.max=20)
#looks good
Box.test(arima2$residuals, lag=20, type="Ljung-Box")
#reject the null for acf and box test at 5%, no autocorrelation
mean(arima1$residuals)
#mean is close to 0

#forecasting

#VAR
# forecasting dropping the last 12 observations
ntot = length(y[,1])
yminus12 = y[1:(ntot-12),]
var312 = VAR(yminus12, type = "const", season = 12)
summary(var312)
fcast <- predict(var312, n.ahead = 12, ci = 0.95) 
plot(fcast)
matplot(fcast$fcst$demp[,1:3],type = "l")   # forecast
varfct=fcast$fcst$demp[,1]
# actual data for 12 predicted
ylast12 = y[(ntot-11):ntot,]
ylast12
# if this worked these should match
cbind(y[,1],c(yminus12[,1],ylast12[,1]))
df12 = fcast$fcst$demp[,1:3]
actfcst = cbind(ylast12[,1],df12)
par(mfrow=c(1,1))
matplot(actfcst, type = "l")
# add some previous actual values to the plot
m = length(yminus12[,1])
xf = c(yminus12[195:m],df12[,1])
xfu = c(yminus12[195:m],df12[,2])
xfl = c(yminus12[195:m],df12[,3])
xa = c(yminus12[195:m],ylast12[,1])
actf2 = cbind(xf,xfl,xfu, xa)

matplot(actf2, type = "l", col=c(3,4,4,1),lty=1)

rmsfet = sqrt(mean(((actfcst[,1]-actfcst[,2])^2)))
rmsfet
####################################

#################################################
#VECM
# forecasting dropping the last 12 observations
ntot = length(demp[,1])
yminus12 = y[1:(ntot-12),]
fcast <- predict(varvec, n.ahead = 12, ci = 0.95) 
plot(fcast)
matplot(fcast$fcst$demp[,1:3],type = "l")   # forecast
vecmfct <- fcast$fcst$demp[,1]
vecmfct
# actual data for 12 predicted
ylast12 = y[(ntot-11):ntot,]
ylast12
# if this worked these should match
cbind(y[,1],c(yminus12[,1],ylast12[,1]))
df12 = fcastarima$pred
df12
junk <- demp[(length(demp)-11):length(demp)]

actfcst = cbind(junk,df12)
par(mfrow=c(1,1))
matplot(actfcst, type = "l")
# add some previous actual values to the plot
m = length(demp)
xf = c(yminus12[195:m],totalavg)
xa = c(yminus12[195:m],ylast12[,1])
actf2 = cbind(xf, xa)
matplot(actf2, type = "l", col=c(3,4,4,1),lty=1)

rmsfet = sqrt(mean(((actfcst[,1]-actfcst[,2])^2)))
rmsfet

#ARIMA 

fcastarima <- predict(arima2,n.ahead = 12, level = c(99.5))
predict(arima2,n.ahead = 4, level = c(99.5))
plot(fcastarima$pred)
# actual data for 12 predicted
junk2 <- demp[(length(demp)-11):length(demp)]
ARIMAdemp <- cbind(junk2, fcastarima$pred)
arma<- fcastarima$pred
junk2
matplot(ARIMAdemp, type = "l")
demp
ylast12

# add some previous actual values to the plot
m = length(demp)
m
xf = c(demp[203:m])
bd = c(bd)
data = cbind(xf, bd)
matplot(data, type = "l", col=c(3,4,4,1),lty=1)

rmsfet = sqrt(mean(((actfcst[,1]-actfcst[,2])^2)))

#Forecast testing
#average 
actual <- junk2
cbind(varfct, vecmfct, arma, actual)
total = (varfct + vecmfct + arma)
totalavg = total/3
totalavg

newdata <- cbind(junk2, totalavg)
newdata
matplot(newdata, type = "l")
