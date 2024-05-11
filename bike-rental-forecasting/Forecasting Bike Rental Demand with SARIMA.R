


library(forecast)
library(rio)
library(tidyverse)
library(ggplot2)
library(tseries)
library(readxl)


# Data from https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=6d8bdae2be63c410VgnVCM1000000b205a0aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default

#Constructing TS
Demanda<-read.csv("TSDailyUse.csv", sep=";")
str(Demanda)

#Date formatting
Demanda$Fecha<-as.Date(Demanda$Fecha,format="%d/%m/%Y")

Demanda<-subset(Demanda, Demanda$Fecha > "2016-12-31" & Demanda$Fecha < "2020-01-01")
#"2014-12-31" & Demanda$Fecha < "2020-01-01")

#### ANALISIS EXPLORATORIO ####

#First snapshot
ggplot(Demanda,aes(Fecha, Usos.bicis.total))+geom_line()+ylab("Daily Bike Demand")+ facet_wrap( ~ format(Fecha, "%Y"),scales = "free_x")



#Clean Temporal Time Series if NAs are found
count_TSObject <- ts(Demanda[,c("Usos.bicis.total")])

Demanda$cuenta_limpia <- tsclean(count_TSObject)

ggplot(Demanda, aes(Fecha, cuenta_limpia))+geom_line()+ylab("Demanda diaria de bicicletas")+scale_x_date("Fecha")


#Moving averages

Demanda$cnt_ma   <-ma(Demanda$cuenta_limpia, order = 7)  #Weekly Moving Average
Demanda$cnt_ma30 <-ma(Demanda$cuenta_limpia, order = 30) #Monthly Moving Average


ggplot()+
  geom_line(Demanda, mapping = aes(Fecha, cuenta_limpia, colour="Bikes Number"))+
  geom_line(Demanda, mapping = aes(Fecha, cnt_ma, colour="Weekly Moving Average")) +
  geom_line(Demanda, mapping = aes(Fecha, cnt_ma30, colour="Monthly Moving Average")) +
  ylab("Bikes Daily Demand")

#### PERIODOGRAMA ####
library(TSA)

p<-periodogram(Demanda$Usos.bicis.total)
max(p$spec)
p$freq[match(max(p$spec), p$spec)]
1/p$freq[match(max(p$spec), p$spec)]

detach("package:TSA", unload = TRUE)


#### STATIONALITY AND TREND DECOMPOSITION #####

cuenta_ma<-ts(na.omit(Demanda$cnt_ma),frequency = 365)
decomp <- stl(cuenta_ma, s.window = "periodic")
sinestacionalidad_cnt<-seasadj(decomp)
plot(decomp)

#Stationary due to Dickey-fuller, p-value = 0.03397 > 0.05, 
#We accept H1 or alternative hypothesis
adf.test(cuenta_ma, alternative="stationary") #Stationary

#### AUTOCORRELATION ####
Acf(cuenta_ma, main="")
Pacf(cuenta_ma, main="")


#We take the differences
cuenta_diferencia<-diff(sinestacionalidad_cnt, differences = 1)
plot(cuenta_diferencia)

adf.test(cuenta_diferencia, alternative="stationary")

Acf(cuenta_diferencia, main="")
Pacf(cuenta_diferencia, main="")



#### ARIMA MODELS ####

#BEST ARIMA MODEL USING AUTO-ARIMA
fit<-auto.arima(sinestacionalidad_cnt, seasonal=FALSE, stepwise = FALSE, approximation = FALSE)
fit

tsdisplay(residuals(fit), lag.max = 15, main = "(2,1,3) Residuos del modelo")


#BEST ARIMA MODEL WITHOUT OUT OF BOUNDS LAGS
fit2<-Arima(sinestacionalidad_cnt, order=c(2,1,10))
fit2
tsdisplay(residuals(fit2), lag.max= 15, main = "(2,1,10) Residuos ")

prediccion <-forecast(fit2, h=50)
plot(prediccion)

#### ADJUSTING THE ARIMA MODEL ####
#CHECKING PRIOR MODEL; ARIMA WITHOUT LAG
hold<-window(ts(sinestacionalidad_cnt), start= 1019)
fit_no_holdout<-arima(ts(sinestacionalidad_cnt[-c(1019:1089)]), order=c(2,1,12))
fcast_no_holdout <- forecast(fit_no_holdout, h = 71)

plot(fcast_no_holdout, main="")
lines(ts(sinestacionalidad_cnt))


#SARIMA WITH SEASONALITY

fit_w_seasonality<-Arima(sinestacionalidad_cnt, order = c(2,1,3), seasonal=list(order =c(1,1,0) , period=30))
season_prediccion<-forecast(fit_w_seasonality, h=50)

plot(season_prediccion)
lines(ts(sinestacionalidad_cnt))

tsdisplay(residuals(fit_w_seasonality), lag.max = 15, main = "Residuals")

Pred<-season_prediccion$mean
Pred

ERROR<-data.frame(predict(fit_w_seasonality, n.ahead=3))
ERROR


#    pred     se
#1 9773.693 243.1550
#2 9938.997 447.2380
#3 9733.005 622.3419


#SARIMA WITH SEASONALITY WITHOUT OUT OF BOUNDS LAGS

fit_w_seasonality2<-Arima(sinestacionalidad_cnt, order = c(2,1,10), seasonal=list(order =c(1,1,0) , period=30))
season_prediccion2<-forecast(fit_w_seasonality2, h=50)

plot(season_prediccion2)

tsdisplay(residuals(fit_w_seasonality2), lag.max = 15, main = "Residuals")


Pred<-season_prediccion2$mean
Pred

ERROR<-data.frame(predict(fit_w_seasonality2, n.ahead=3))
ERROR

#pred       se
#1 9782.667 184.4355
#2 9744.670 349.4782
#3 9532.162 498.3749
#We see a reduction in the lag error and give us better PACF/ACF graphs

####LITTLE MONTHLY DATA POINTS - DISCARDED #### ONLY 48-72 OBSERVATIONS DO NOT HOLD PREDICTIVE POWER 

##############################################

#### OTHER TRIALS ####
library(astsa)
library(stats)

Prueba<-sarima(Demanda$Usos.bicis.total,2,1,10,1,1,0,30)
sarima.for(Demanda$Usos.bicis.total,2,1,10,1,1,0,30, n.ahead=12)

#No se puede establecer un modelo sarima con 1000 observaciones debido al lag, por lo que debemos agrupar los datos diarios en mensuales





#### SARIMA MODEL WITH MONTHLY NUMBERS #####


#Construction TS
Demanda<-read.csv("TSMonthlyUse.csv", sep=";")
str(Demanda)

#Date formatting
Demanda<-setNames(Demanda, c("Fecha","Usos.bicis.total"))
Demanda$Fecha<-as.Date(Demanda$Fecha,format="%d/%m/%Y")

Demanda<-subset(Demanda, Demanda$Fecha > "2015-12-31" & Demanda$Fecha < "2020-01-01")
#"2014-12-31" & Demanda$Fecha < "2020-01-01")

#### EXPLORATORY ANALYSIS FOR MONTHLY DATA ####

#First Snapshot
ggplot(Demanda,aes(Fecha, Usos.bicis.total))+geom_line()+ylab("Demanda diaria de bicicletas")+ facet_wrap( ~ format(Fecha, "%Y"),scales = "free_x")



#Clean Temporal Time Series if NAs are found
count_TSObject <- ts(Demanda[,c("Usos.bicis.total")])

Demanda$cuenta_limpia <- tsclean(count_TSObject)

ggplot(Demanda, aes(Fecha, cuenta_limpia))+geom_line()+ylab("Demanda diaria de bicicletas")+scale_x_date("Fecha")


#Moving averages
Demanda$cnt_ma   <-ma(Demanda$cuenta_limpia, order = 7)  #Weekly Moving Average
Demanda$cnt_ma30 <-ma(Demanda$cuenta_limpia, order = 30) #Daily Moving Average


ggplot()+
  geom_line(Demanda, mapping = aes(Fecha, cuenta_limpia, colour="Bikes Number"))+
  geom_line(Demanda, mapping = aes(Fecha, cnt_ma, colour="Weekly Moving Average")) +
  geom_line(Demanda, mapping = aes(Fecha, cnt_ma30, colour="Daily Moving Average")) +
  ylab("Monthly Bike Demand")

#### PERIODOGRAMA MENSUAL ####
library(TSA)

p<-periodogram(Demanda$Usos.bicis.total)
max(p$spec)
p$freq[match(max(p$spec), p$spec)]
1/p$freq[match(max(p$spec), p$spec)]

detach("package:TSA", unload = TRUE)


#### DECOMPOSICION DE ESTACIONALIDAD Y TENDENCIA MENSUAL #####

cuenta_ma<-ts(na.omit(Demanda$cnt_ma),frequency = 12)  
decomp <- stl(cuenta_ma, s.window = "periodic")
sinestacionalidad_cnt<-seasadj(decomp)
plot(decomp)

# p value = 0.03397 > 0.05, 
adf.test(cuenta_ma, alternative="stationary") #Stationary

#### AUTOCORRELATION ####
Acf(cuenta_ma, main="")
Pacf(cuenta_ma, main="")


#Differences taken
cuenta_diferencia<-diff(sinestacionalidad_cnt, differences = 1)
plot(cuenta_diferencia)

adf.test(cuenta_diferencia, alternative="stationary")

Acf(cuenta_diferencia, main="")
Pacf(cuenta_diferencia, main="")



#### ARIMA MODELS #### 

#BEST ARIMA MODEL
fit<-auto.arima(sinestacionalidad_cnt, seasonal=FALSE, stepwise = FALSE, approximation = FALSE)
fit

tsdisplay(residuals(fit), lag.max = 15, main = "(2,1,3) Residuos del modelo")


#BEST ARIMA MODEL WITHOUT SIGNIFICANT LAGS OUT OF BOUNDS
fit2<-Arima(sinestacionalidad_cnt, order=c(2,1,10))
fit2
tsdisplay(residuals(fit2), lag.max= 15, main = "(2,1,10) Residuos ")

prediccion <-forecast(fit2, h=50)
plot(prediccion)

#### ADJUST TO SARIMA MODEL ####

hold<-window(ts(sinestacionalidad_cnt), start= 1019)
fit_no_holdout<-arima(ts(sinestacionalidad_cnt[-c(1019:1089)]), order=c(2,1,12))
fcast_no_holdout <- forecast(fit_no_holdout, h = 71)

plot(fcast_no_holdout, main="")
lines(ts(sinestacionalidad_cnt))


#SARIMA MODEL WITH SEASONALITY

fit_w_seasonality<-Arima(sinestacionalidad_cnt, order = c(1,1,0), seasonal=list(order =c(1,1,0) , period=12))
season_prediccion<-forecast(fit_w_seasonality, h=50)

plot(season_prediccion)
lines(ts(sinestacionalidad_cnt))

tsdisplay(residuals(fit_w_seasonality), lag.max = 15, main = "Residuos del modelo estacional")

Pred<-season_prediccion$mean
Pred

ERROR<-data.frame(predict(fit_w_seasonality, n.ahead=3))
ERROR


#    pred     se
#1 9773.693 243.1550
#2 9938.997 447.2380
#3 9733.005 622.3419


#SARIMA MODEL WITH SEASONALITY WITHOUT LAGS OUT OF BOUNDS

fit_w_seasonality2<-Arima(sinestacionalidad_cnt, order = c(2,1,10), seasonal=list(order =c(1,1,0) , period=30))
season_prediccion2<-forecast(fit_w_seasonality2, h=50)

tsdisplay(residuals(fit_w_seasonality2), lag.max = 15, main = "Residuos del modelo estacional")


Pred<-season_prediccion2$mean
Pred

ERROR<-data.frame(predict(fit_w_seasonality2, n.ahead=3))
ERROR

#pred       se
#1 9782.667 184.4355
#2 9744.670 349.4782
#3 9532.162 498.3749