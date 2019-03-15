

library(astsa)#finansal analizler için bu kütüphane kullaniliyor.
library(forecast)
RGDP=ts((money_dem[,3]),start=c(1959,1),freq=4)#datayi zaman serisine çevirdik.
plot.ts(RGDP)
library(pastecs)#desc stattistics için bu kütüphane kullaniliyor.
des_rgdp<-stat.desc(RGDP) #sag tarafta menude des_rgdpye tikla sonuçlar açilacak
library(moments)
s_rgdp<-skewness(RGDP)
#excess kurtosis
k_rgdp<-kurtosis(RGDP)



DF_RGDP=diff(RGDP,1)  #first difference
reel_growth=100*diff(log(RGDP),1)
#Finansal zaman serisinde bi seyin logaritmik farkini aldigimizda bize getirisini veriri. 
#Iktisatta ise logaritmasini almak bize büyümeyi verir.
plot.ts(DF_RGDP)#real GDP.enflasyondan arindirilmis büyüme.
plot.ts(reel_growth)


bist100<-stockdata$BIST100
r_bist100<-100*diff(log(bist100),1)
K_BIST100<-kurtosis(r_bist100)
S_BIST100<-skewness(r_bist100)
plot.ts(r_bist100)#getiri serisi,oynaklik kümelenmesi.oynakligin cok olmasi hem riski hem getiri ihtimalini arttirir.
plot.ts(bist100)#
bist100<-stockdata[,3]
dim1<-dim(bist100)
date2<-seq(as.Date("2002-01-02"), by = "day", len = (dim1[1]-1))
plot(date2, r_bist100, type="l")



#sarima forecast 
h=12 #forecast horizon
dim2<-dim(RGDP)
end1<-dim2[1]-h
forecst1 <- sarima.for(RGDP[1:end1],h,1,1,0,0,0,0,0)
sarima1<-sarima(RGDP[1:end1],1,1,0,0,0,0,0)
sarimacoef<-sarima1$fit$coef[1:3]
sarima2<-sarima(RGDP[1:end1],1,1,1,0,0,0,0)

sarima2coef<-sarima2$fit$coef[1:5]

yfcst1<-forecst1$pred

forecst2 <- sarima.for(RGDP[1:end1],h,1,1,1,0,0,0,0)
yfcst2<-forecst2$pred

#accuracy

m1<-RGDP[(end1+1):dim2[1]]
MAPE<-100*mean(abs(m1-yfcst1)/m1)


MAPE2<-100*mean(abs(m1-yfcst2)/m1)

#out of sample forecast
forecst_out1 <- sarima.for(RGDP,h,1,1,1,0,0,0,0)
yfcst2out<-forecst_out1$pred
#sarima with seasonality
fit2 <- sarima(RGDP,1,1,1,1,1,1,4)

resid2<-fit2$fit$residuals
acf2(resid2, 20, main='Empirical ACF of SAR1 residuals')
rgdpfit2<-RGDP-resid2  

forecst_seas1 <- sarima.for(RGDP[1:end1],h,1,1,0,1,1,0,4)
yfcst_seas1<-forecst_seas1$pred
#accuracy
MAPE3<-100*mean(abs(m1-yfcst_seas1)/m1)


#arima and sarima 




#accuracy


fitar11 <- sarima(RGDP,1,1,1,0,0,0,0, Model=F)
resid<-fit1$fit$residuals
acf2(resid, 20, main='Empirical ACF of AR1 residuals')







#real out of sample forecast
forecst_out_seas <- sarima.for(RGDP,h,1,1,0,1,1,0,4)
yfcst3<-forecst_out_seas$pred

#fit and forecast with deterministic seasonality

Sdummy<-seasonaldummy(RGDP)
fit3 <- sarima(RGDP,1,1,0,0,0,0,0, xreg=Sdummy)
ydsfrct<-sarima.for(RGDP[1:end1],h,1,1,0,0,0,0,0,newxreg=Sdummy[1:h,])
MAPE4<-mean(abs(m1-ydsfrct$pred)/m1)*100


??sarima.for
#returns


library(tseries)
sp500=stockdata[,2]
dim1<-dim(sp500)
date1 = seq(as.Date("2002-01-01"), by = "day", len = dim1[1])
plot(stockdata$date, stockdata$SP500, type="l")
sp500<-ts(sp500)
plot.ts(sp500)
sp_returns<- diff(log(stockdata$SP500),1)
date2<-seq(as.Date("2002-01-02"), by = "day", len = (dim1[1]-1))

plot(date2, sp_returns, type="l")
library(pastecs)
des_sp500<-stat.desc(sp_returns) 
library(moments)
skewness(sp_returns)
kurtosis(sp_returns)

#unit root test
library(fUnitRoots)

adfTest(sp500, lags = 1, type = c("ct" ), title = NULL,
        description = NULL)
adfTest(sp_returns, lags = 1, type = c("nc", "c", "ct"), title = NULL,
        description = NULL)
require(urca) 

adf1<-urdfTest(RGDP, lags = 1, type = c("c"), doplot = T)
adf1@test$test@teststat
adf1@test$test@cval

adf2<-urdfTest(RGDP, lags = 1, type = c("ct"), doplot = T)
adf2@test$test@teststat
adf2@test$test@cval

d_adf1<-urdfTest(DF_RGDP, lags = 1, type = c("nc"), doplot = T)
d_adf1@test$test@teststat
d_adf1@test$test@cval

dT_adf1<-urdfTest(DF_RGDP, lags = 1, type = c("c"), doplot = T)
dT_adf1@test$test@teststat
dT_adf1@test$test@cval

spadf1<-urdfTest(sp500, lags = 1, type = c("c"), doplot = T)
spadf1@test$test@teststat
spadf1@test$test@cval

adf2<-urdfTest(sp_returns, lags = 1, type = c("nc"), doplot = T)
adf2@test$test@teststat
adf2@test$test@cval

adf3<-urdfTest(sp_returns, lags = 1, type = c("c"), doplot = F)
adf3@test$test@teststat
adf3@test$test@cval

adf4<-urdfTest(sp_returns, lags = 1, type = c("ct"), doplot = F)
adf4@test$test@teststat
adf4@test$test@cval


ers1<-urersTest(sp_returns, type = c("DF-GLS"), model = c("constant"),
          lag.max = 4, doplot = TRUE)
ers1@test$test@teststat
ers1@test$test@testreg
ers1@test$test@cval

ers2<-urersTest(sp_returns, type = c("DF-GLS"), model = c("trend"),
                lag.max = 4, doplot = TRUE)
ers2@test$test@teststat
ers2@test$test@testreg
ers2@test$test@cval



kpss1<-urkpssTest(sp_returns, type = c("mu"), lags = c("short"),
           use.lag = NULL, doplot = F)
kpss1@test$test@teststat
kpss1@test$test@cval

kpss2<-urkpssTest(sp_returns, type = c("tau"), lags = c("short"),
                  use.lag = NULL, doplot = F)
kpss2@test$test@teststat
kpss2@test$test@cval


PP1<-urppTest(sp_returns, type = c("Z-alpha"), model = c("constant"),
         lags = c( "long"), use.lag = NULL, doplot = F)
PP1@test$test@teststat
PP1@test$test@cval

PP2<-urppTest(sp_returns, type = c("Z-tau"), model = c("trend"),
              lags = c( "long"), use.lag = NULL, doplot = F)
PP2@test$test@teststat
PP2@test$test@cval





require(graphics)

library(ggplot2)
        

data<-cbind(RGDP, rgdpfit)

ts.plot(data,gpars= list(col=rainbow(2)))
legend("topleft", legend = colnames(data),col = 1:2, lty = 1)
par(mfrow=c(2,1))
plot(RGDP)
plot(rgdpfit)


#Linear GARCH 

??garch

sp500.garch <- garch(sp_returns, order=c(1,1))  # Fit a GARCH(1,1) to SP500 returns
summary(sp500.garch)   
#non linear GARCH
library(fGarch)
fit_garch = garchFit(formula = ~ garch(1, 1), data = sp_returns, trace = FALSE, cond.dist = "norm")
convar<-fit_garch@h.t
plot(fit_garch@h.t)
plot.ts(convar)
summary(fit_garch)


t_fit_garch = garchFit(formula = ~ garch(1, 1), data = sp_returns, trace = FALSE, cond.dist = "std")
t_convar<-t_fit_garch@h.t
plot(t_fit_garch@h.t)
plot.ts(t_convar)
summary(t_fit_garch)

volgarch11=volatility(fit_garch)
plot(volgarch11)
plot.ts(volgarch11)


volatility = volatility(fit, type = "h")
head(volatility)
class(volatility)


#arma(1,1) aparch
fit2 = garchFit(formula = ~ arma(1,0)+aparch(1, 1), data = sp_returns, trace = FALSE, cond.dist = "std")
summary(fit2)
volatility = volatility(fit2, type = "h")
convar2<-fit2@h.t
plot.ts(convar2)
head(volatility)
class(volatility)
plot.ts(volatility)

# mean forecast
yfit1<-predict(t_fit_garch , n.ahead = 20, trace = FALSE, mse = c("cond"),
        plot=T, nx=NULL, crit_val=NULL, conf=0.95)

#in sample forecast var%5
var<-t_fit_garch@fitted-2*sqrt(t_convar)
plot.ts(var)
