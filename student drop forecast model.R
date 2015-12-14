#*October#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Oactuals<- c(843,847,833,883,953,744,791,913,972)
#cacl mean and sd for the normalized actuals
Odropvariance<-sd(Oactuals)
Odropmean<-mean(Oactuals)
#run monte carlo simulations
Odrops <- mcstoc(rnorm,type="VU",mean=Odropmean,sd=Odropvariance)
Odrops2 <- mc(Odrops)
Odrops3<- mcstoc(rnorm, type="U", mean=Odropmean, sd=Odropvariance)
#print the output
print(Odrops2,digits=2)

#arima forecast
Oct<-scan("FY15ARIMA.csv", nlines=27)
Octtimeseries<-ts(Oct)
plot(Octtimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(gocttimeseries)
Octtimeseriesarima<-arima(Octtimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Octtimeseriesarima
library("forecast")
Octtimeseriesforecasts<-forecast.Arima(Octtimeseriesarima, h=01)
Octtimeseriesforecasts
Octdiff1<-diff(Octtimeseries, differences=3)
acf(Octdiff1, lag.max=20, main="Lag of model differences")
acf(Octdiff1, lag.max=20, plot=FALSE)
plot.ts(Octdiff1, main="Plot of Differeneces Oct Campus Pop forecast", col="blue")
plot.forecast(Octtimeseriesforecasts, main="", ylab="population", xlab="February Pop forecast", col="blue")


#visuals

Ohist1<-hist(Odrops2, xlab="Oct Campus Drops", col="red")
plot(Odrops2, xlab="Oct Campus Drops", ylab="frequency", col="red")
plot(Odrops3, xlab="Oct Campus Drops", ylab="frequency")
#descdist(drops)
summary(Odrops2)
summary(Odrops3)
Odropdensity <- density(Oactuals)
plot(Odropdensity, xlab ="Density of Oct Campus drops", main = "")
polygon(Odropdensity, col="red", border="blue", xlab="", ylab="")
plot(Oactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Oactualmodel<-lm(actuals~time)
abline(Oactualmodel, par(xpd=FALSE))
summary(Oactualmodel)
fitted(Oactualmodel)
predict(Oactualmodel, interval="predict")
plot(Oactualmodel)

#*Feb*#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Factuals<- c(545,571,428,545,622,613,499,558,535)
#cacl mean and sd for the normalized actuals
Fdropvariance<-sd(Factuals)
Fdropmean<-mean(Factuals)
#run monte carlo simulations
Fdrops <- mcstoc(rnorm,type="VU",mean=Fdropmean,sd=Fdropvariance)
Fdrops2 <- mc(Fdrops)
Fdrops3<- mcstoc(rnorm, type="U", mean=Fdropmean, sd=Fdropvariance)
#print the output
print(Fdrops2,digits=2)

#arima forecast
Feb<-scan("FY15ARIMA.csv", nlines=28)
Febtimeseries<-ts(Feb)
plot(Febtimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(gocttimeseries)
Febtimeseriesarima<-arima(Febtimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Febtimeseriesarima
library("forecast")
Febtimeseriesforecasts<-forecast.Arima(Febtimeseriesarima, h=01)
Febtimeseriesforecasts
Febdiff1<-diff(Febtimeseries, differences=3)
acf(Febdiff1, lag.max=20, main="Lag of model differences")
acf(Febdiff1, lag.max=20, plot=FALSE)
plot.ts(Febdiff1, main="Plot of Differeneces")
plot.forecast(Febtimeseriesforecasts, main="", ylab="population", xlab="February Pop forecast", col="blue")


#viuals

Fhist1<-hist(Fdrops2, xlab="February Campus Drops", col="red")
plot(Fdrops2, xlab="February Campus Drops", ylab="frequency", col="red")
plot(Fdrops3, xlab="Februry Campus Drops", ylab="frequency")
#descdist(drops)
summary(Fdrops2)
summary(Fdrops3)
Fdropdensity <- density(Factuals)
plot(Fdropdensity, xlab ="Density of Feb Campus Drops", main = "")
polygon(Fdropdensity, col="red", border="blue", xlab="", ylab="")
plot(Factuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Factualmodel<-lm(Factuals~time)
abline(Factualmodel)
summary(Factualmodel)
fitted(Factualmodel)
predict(Factualmodel, interval="predict")
plot(Factualmodel)


#*March
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Mactuals<- c(484,456,573,634,610,614,726,661)
#cacl mean and sd for the normalized actuals
Mdropvariance<-sd(Mactuals)
Mdropmean<-mean(Mactuals)
#run monte carlo simulations
Mdrops <- mcstoc(rnorm,type="VU",mean=Mdropmean,sd=Mdropvariance)
Mdrops2 <- mc(Mdrops)
Mdrops3<- mcstoc(rnorm, type="U", mean=Mdropmean, sd=Mdropvariance)
#print the output
print(Mdrops2,digits=2)

#arima forecast
Mar<-scan("FY15ARIMA.csv",nlines=32)
Martimeseries<-ts(Mar)
plot(Martimeseries, main="", ylab="Population", xlab="Campus population trend by month")
Martimeseriesarima<-auto.arima(Martimeseries)
#Martimeseriesarima<-arima(Martimeseries, order=c(3,1,0))
#Martimeseriesarima<-arima(Martimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Martimeseriesarima
library("forecast")
Martimeseriesforecasts<-forecast.Arima(Martimeseriesarima, h=01)
Martimeseriesforecasts
Mardiff1<-diff(Martimeseries, differences=3)
acf(Mardiff1, lag.max=20, main="Lag of model differences")
acf(Mardiff1, lag.max=20, plot=FALSE)
plot.ts(Mardiff1, main="Plot of Differeneces")
plot.forecast(Martimeseriesforecasts, main="", ylab="population", xlab="March Campus Pop forecast", col="blue")


#viuals

Mhist1<-hist(Mdrops2, xlab="March Campus Drops", col="red")
plot(Mdrops2, xlab="March Campus Drops", ylab="frequency", col="red")
plot(Mdrops3, xlab="March Campus Drops", ylab="frequency")
#descdist(drops)
summary(Mdrops2)
summary(Mdrops3)
Mdropdensity <- density(Mactuals)
plot(Mdropdensity, xlab ="Density of March Campus drops", main = "")
polygon(Mdropdensity, col="red", border="blue", xlab="", ylab="")
plot(Mactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Mactualmodel<-lm(Mactuals~time)
abline(Mactualmodel)
summary(Mactualmodel)
fitted(Mactualmodel)
predict(Mactualmodel, interval="predict")
plot(Mactualmodel)



#*January#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Jactuals<- c(817,821,807,856,923,721,767,885,942)
#cacl mean and sd for the normalized actuals
Jdropvariance<-sd(Jactuals)
Jdropmean<-mean(Jactuals)
#run monte carlo simulations
Jdrops <- mcstoc(rnorm,type="VU",mean=Jdropmean,sd=Jdropvariance)
Jdrops2 <- mc(Jdrops)
Jdrops3<- mcstoc(rnorm, type="U", mean=Jdropmean, sd=Jdropvariance)
#print the output
print(Jdrops2,digits=2)

#arima forecast
Jan<-scan("FY15ARIMA.csv", nlines=30)
Jantimeseries<-ts(Jan)
plot(Jantimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(gocttimeseries)
Jantimeseriesarima<-arima(Jantimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Jantimeseriesarima
library("forecast")
Jantimeseriesforecasts<-forecast.Arima(Jantimeseriesarima, h=01)
Jantimeseriesforecasts
Jandiff1<-diff(Jantimeseries, differences=3)
acf(Jandiff1, lag.max=20, main="Lag of model differences")
acf(Jandiff1, lag.max=20, plot=FALSE)
plot.ts(Jandiff1, main="Plot of Differeneces")
plot.forecast(Jantimeseriesforecasts, main="", ylab="population", xlab="January Campus Pop forecast", col="blue")


#visuals

Jhist1<-hist(Jdrops2, xlab="January Campus Drops", col="red")
plot(Jdrops2, xlab="January Campus Drops", ylab="frequency", col="red")
plot(Jdrops3, xlab="January Campus Drops", ylab="frequency")
#descdist(drops)
summary(Jdrops2)
summary(Jdrops3)
Jdropdensity <- density(Jactuals)
plot(Jdropdensity, xlab ="Density of Jan Campus drops", main = "")
polygon(Jdropdensity, col="red", border="blue", xlab="", ylab="")
plot(Jactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Jactualmodel<-lm(actuals~time)
abline(Jactualmodel, par(xpd=FALSE))
summary(Jactualmodel)
fitted(Jactualmodel)
predict(Jactualmodel, interval="predict")
plot(Jactualmodel)

#*Feb*#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Factuals<- c(545,571,428,545,622,613,499,558,535)
#cacl mean and sd for the normalized actuals
Fdropvariance<-sd(Factuals)
Fdropmean<-mean(Factuals)
#run monte carlo simulations
Fdrops <- mcstoc(rnorm,type="VU",mean=Fdropmean,sd=Fdropvariance)
Fdrops2 <- mc(Fdrops)
Fdrops3<- mcstoc(rnorm, type="U", mean=Fdropmean, sd=Fdropvariance)
#print the output
print(Fdrops2,digits=2)

#arima forecast
Feb<-scan("FY15ARIMA.csv", nlines=31)
Febtimeseries<-ts(Feb)
plot(Febtimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(gocttimeseries)
Febtimeseriesarima<-arima(Febtimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Febtimeseriesarima
library("forecast")
Febtimeseriesforecasts<-forecast.Arima(Febtimeseriesarima, h=01)
Febtimeseriesforecasts
Febdiff1<-diff(Febtimeseries, differences=3)
acf(Febdiff1, lag.max=20, main="Lag of model differences")
acf(Febdiff1, lag.max=20, plot=FALSE)
plot.ts(Febdiff1, main="Plot of Differeneces")
plot.forecast(Febtimeseriesforecasts, main="", ylab="population", xlab="February Pop forecast", col="blue")


#viuals

Fhist1<-hist(Fdrops2, xlab="February Campus Drops", col="red")
plot(Fdrops2, xlab="February Campus Drops", ylab="frequency", col="red")
plot(Fdrops3, xlab="Februry Campus Drops", ylab="frequency")
#descdist(drops)
summary(Fdrops2)
summary(Fdrops3)
Fdropdensity <- density(Factuals)
plot(Fdropdensity, xlab ="Density of Feb Campus Drops", main = "")
polygon(Fdropdensity, col="red", border="blue", xlab="", ylab="")
plot(Factuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Factualmodel<-lm(Factuals~time)
abline(Factualmodel)
summary(Factualmodel)
fitted(Factualmodel)
predict(Factualmodel, interval="predict")
plot(Factualmodel)


#*March
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Mactuals<- c(484,456,573,634,610,614,726,661)
#cacl mean and sd for the normalized actuals
Mdropvariance<-sd(Mactuals)
Mdropmean<-mean(Mactuals)
#run monte carlo simulations
Mdrops <- mcstoc(rnorm,type="VU",mean=Mdropmean,sd=Mdropvariance)
Mdrops2 <- mc(Mdrops)
Mdrops3<- mcstoc(rnorm, type="U", mean=Mdropmean, sd=Mdropvariance)
#print the output
print(Mdrops2,digits=2)

#arima forecast
Mar<-scan("FY15ARIMA.csv",nlines=32)
Martimeseries<-ts(Mar)
plot(Martimeseries, main="", ylab="Population", xlab="Campus population trend by month")
Martimeseriesarima<-auto.arima(Martimeseries)
#Martimeseriesarima<-arima(Martimeseries, order=c(3,1,0))
#Martimeseriesarima<-arima(Martimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Martimeseriesarima
library("forecast")
Martimeseriesforecasts<-forecast.Arima(Martimeseriesarima, h=01)
Martimeseriesforecasts
Mardiff1<-diff(Martimeseries, differences=3)
acf(Mardiff1, lag.max=20, main="Lag of model differences")
acf(Mardiff1, lag.max=20, plot=FALSE)
plot.ts(Mardiff1, main="Plot of Differeneces")
plot.forecast(Martimeseriesforecasts, main="", ylab="population", xlab="March Campus Pop forecast", col="blue")


#viuals

Mhist1<-hist(Mdrops2, xlab="March Campus Drops", col="red")
plot(Mdrops2, xlab="March Campus Drops", ylab="frequency", col="red")
plot(Mdrops3, xlab="March Campus Drops", ylab="frequency")
#descdist(drops)
summary(Mdrops2)
summary(Mdrops3)
Mdropdensity <- density(Mactuals)
plot(Mdropdensity, xlab ="Density of March Campus drops", main = "")
polygon(Mdropdensity, col="red", border="blue", xlab="", ylab="")
plot(Mactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Mactualmodel<-lm(Mactuals~time)
abline(Mactualmodel)
summary(Mactualmodel)
fitted(Mactualmodel)
predict(Mactualmodel, interval="predict")
plot(Mactualmodel)


#*Apr*#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Aactuals<- c(952,957,941,998,1077,841,894,1032,1099)
#cacl mean and sd for the normalized actuals
Adropvariance<-sd(Aactuals)
Adropmean<-mean(Aactuals)
#run monte carlo simulations
Adrops <- mcstoc(rnorm,type="VU",mean=Adropmean,sd=Adropvariance)
Adrops2 <- mc(Adrops)
Adrops3<- mcstoc(rnorm, type="U", mean=Adropmean, sd=Adropvariance)
#print the output
print(Adrops2,digits=2)

#arima forecast
Apr<-scan("FY15ARIMA.csv", nlines=33)
Aprtimeseries<-ts(Apr)
plot(Aprtimeseries, main="", ylab="Population", xlab="Campus population trend by month")
Aprtimeseriesarima<-auto.arima(Aprtimeseries)
#Aprtimeseriesarima<-arima(Aprtimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Aprtimeseriesarima
library("forecast")
Aprtimeseriesforecasts<-forecast.Arima(Aprtimeseriesarima, h=01)
Aprtimeseriesforecasts
Aprdiff1<-diff(Aprtimeseries, differences=3)
acf(Aprdiff1, lag.max=20, main="Lag of model differences")
acf(Aprdiff1, lag.max=20, plot=FALSE)
plot.ts(Aprdiff1, main="Plot of Differeneces")
plot.forecast(Aprtimeseriesforecasts, main="", ylab="population", xlab="April Campus Pop forecast", col="blue")


#viuals

Ahist1<-hist(Adrops2, xlab="April Campus Drops", col="red")
plot(Adrops2, xlab="April Campus Drops", ylab="frequency", col="red")
plot(Adrops3, xlab="April Campus Drops", ylab="frequency")
#descdist(Adrops)
summary(Adrops2)
summary(Adrops3)
Adropdensity <- density(Aactuals)
plot(Adropdensity, xlab ="Density of April Campus drops", main = "")
polygon(Adropdensity, col="red", border="blue", xlab="", ylab="")
plot(Aactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Aactualmodel<-lm(Aactuals~time)
abline(Aactualmodel)
summary(Aactualmodel)
fitted(Aactualmodel)
predict(Aactualmodel, interval="predict")
plot(Aactualmodel)



#*May*#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Myactuals<- c(538,563,422,537,613,604,492,550,527)
#cacl mean and sd for the normalized actuals
Mydropvariance<-sd(Myactuals)
Mydropmean<-mean(Myactuals)
#run monte carlo simulations
Mydrops <- mcstoc(rnorm,type="VU",mean=Mydropmean,sd=Mydropvariance)
Mydrops2 <- mc(Mydrops)
Mydrops3<- mcstoc(rnorm, type="U", mean=Mydropmean, sd=Mydropvariance)
#print the output
print(Mydrops2,digits=2)

#arima forecast
May<-scan("FY15ARIMA.csv",nlines=34)
Maytimeseries<-ts(May)
plot(Maytimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(Maytimeseries)
Maytimeseriesarima<-arima(Maytimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(Maytimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Maytimeseriesarima
library("forecast")
Maytimeseriesforecasts<-forecast.Arima(Maytimeseriesarima, h=01)
Maytimeseriesforecasts
Maydiff1<-diff(Maytimeseries, differences=3)
acf(Maydiff1, lag.max=20, main="Lag of model differences")
acf(Maydiff1, lag.max=20, plot=FALSE)
plot.ts(Maydiff1, main="Plot of Differeneces")
plot.forecast(Maytimeseriesforecasts, main="", ylab="population", xlab="May Campus Pop forecast", col="blue")


#viuals

Mayhist1<-hist(Mydrops2, xlab="May Campus Drops", col="red")
plot(Mydrops2, xlab="May Campus Drops", ylab="frequency", col="red")
plot(Mydrops3, xlab="May Campus Drops", ylab="frequency")
#descdist(drops)
summary(Mydrops2)
summary(Mydrops3)
Mydropdensity <- density(Myactuals)
plot(Mydropdensity, xlab ="Density of May Campus drops", main = "")
polygon(Mydropdensity, col="red", border="blue", xlab="", ylab="")
plot(Myactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Myactualmodel<-lm(Myactuals~time)
abline(Myactualmodel)
summary(Myactualmodel)
fitted(Myactualmodel)
predict(Myactualmodel, interval="predict")
plot(Myactualmodel)

#*June*#
#install and load mc2d package
library("mc2d")
#for the actuals vector input actuals from previous months in the same position in a term.  
#We still need to determine weights for more recent months.
Juneactuals<- c(514,484,608,673,648,652,771,702)
#cacl mean and sd for the normalized actuals
Junedropvariance<-sd(Juneactuals)
Junedropmean<-mean(Juneactuals)
#run monte carlo simulations
Junedrops <- mcstoc(rnorm,type="VU",mean=Junedropmean,sd=Junedropvariance)
Junedrops2 <- mc(Junedrops)
Junedrops3<- mcstoc(rnorm, type="U", mean=Junedropmean, sd=Junedropvariance)
#print the output
print(Junedrops2,digits=2)

#arima forecast
June<-scan("FY15ARIMA.csv", nlines=35)
Junetimeseries<-ts(June)
plot(Junetimeseries, main="", ylab="Population", xlab="Campus population trend by month")
#gocttimeseriesarima<-auto.arima(gocttimeseries)
Junetimeseriesarima<-arima(Junetimeseries, order=c(3,1,0))
#gocttimeseriesarima<-arima(gocttimeseries, order = c(1, 1, 0), xreg=1:length(gocttimeseries))
Junetimeseriesarima
library("forecast")
Junetimeseriesforecasts<-forecast.Arima(Junetimeseriesarima, h=01)
Junetimeseriesforecasts
Junediff1<-diff(Junetimeseries, differences=3)
acf(Junediff1, lag.max=20, main="Lag of model differences")
acf(Junediff1, lag.max=20, plot=FALSE)
plot.ts(Junediff1, main="Plot of Differeneces")
plot.forecast(Junetimeseriesforecasts, main="", ylab="population", xlab="June Campus Pop forecast", col="blue")


#viuals

Junehist1<-hist(Junedrops2, xlab="June Campus Drops", col="red")
plot(Junedrops2, xlab="June Campus Drops", ylab="frequency", col="red")
plot(Junedrops3, xlab="June Campus Drops", ylab="frequency")
#descdist(drops)
summary(Junedrops2)
summary(Junedrops3)
Junedropdensity <- density(Juneactuals)
plot(Junedropdensity, xlab ="Density of June Campus drops", main = "")
polygon(Junedropdensity, col="red", border="blue", xlab="", ylab="")
plot(Juneactuals, col="blue")
time<-c(1,2,3,4,5,6,7,8,9)
Juneactualmodel<-lm(Juneactuals~time)
abline(Juneactualmodel)
summary(Juneactualmodel)
fitted(Juneactualmodel)
predict(Juneactualmodel, interval="predict")
plot(Juneactualmodel)


