###PA
PAdroptime<-read.csv("PA.csv")
PAdroptimeseries<-ts(PAdroptime, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts((PAdroptimeseries), main="PA Drop Trends", xlab="Past 26 months")

totdrop<-(PAdroptime$total)
totdroptimeseries<-ts(totdrop, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts(totdroptimeseries, xaxt="n",ylab="Total Monthly Drops", col="blue", xlab="", main="PA Monthly Drop trend")
points(totdroptimeseries, col="red", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14"))                        
                        
####Wyomissing
Wyodrop<-(PAdroptime$Wyomissing)
Wyodroptimeseries<-ts(Wyodrop, start=c(2012,7), end=c(2014,9), frequency=12)
Wyoforecast<-HoltWinters(Wyodroptimeseries, gamma=TRUE)
Wyoforecast
Wyocoeffs<-coefficients(Wyoforecast)
Wyomissing<-Wyocoeffs[1]+Wyocoeffs[3:14]
Wyoforecast2<-data.frame(Wyomissing,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Wyodroptimeseries), xlab="Wyomissing")
plot(forecast(Wyoforecast,12), main="Wyomissing drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Wyodroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

Wyoforecast<-ets(Wyodroptimeseries)
plot(forecast(Wyoforecast))

###Allentown
Alldrop<-(PAdroptime$Allentown)
Alldroptimeseries<-ts(Alldrop, start=c(2012,7), end=c(2014,9), frequency=12)
Allforecast<-HoltWinters(Alldroptimeseries, gamma=TRUE)
Allcoeffs<-coefficients(Allforecast)
Allentown<-Allcoeffs[1]+Allcoeffs[3:14]
Allforecast2<-data.frame(Allentown,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Alldroptimeseries),xlab="Allentown")
plot(forecast(Allforecast,12), main="Allentown drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Alldroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Carlisle
Cardrop<-(PAdroptime$Carlisle)
Cardroptimeseries<-ts(Cardrop, start=c(2012,7), end=c(2014,9), frequency=12)
Carforecast<-HoltWinters(Cardroptimeseries, gamma=TRUE)
Carcoeffs<-coefficients(Carforecast)
Carlisle<-Carcoeffs[1]+Carcoeffs[3:14]
Carforecast2<-data.frame(Carlisle,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Cardroptimeseries),xlab="Carlisle")
plot(forecast(Carforecast,12), main="Carlisle drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Cardroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Dickson City
Dicdrop<-(PAdroptime$Dickson.City)
Dicdroptimeseries<-ts(Dicdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Dicforecast<-HoltWinters(Dicdroptimeseries, gamma=TRUE)
Diccoeffs<-coefficients(Dicforecast)
Dickson.City<-Diccoeffs[1]+Diccoeffs[3:14]
Dicforecast2<-data.frame(Dickson.City,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Dicdroptimeseries),xlab="Dickson City")
plot(forecast(Dicforecast,12), main="Dickson City drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Dicdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Hazelton
Hazdrop<-(PAdroptime$Hazleton)
Hazdroptimeseries<-ts(Hazdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Hazforecast<-HoltWinters(Hazdroptimeseries, gamma=TRUE)
Hazcoeffs<-coefficients(Hazforecast)
Hazleton<-Hazcoeffs[1]+Hazcoeffs[3:14]
Hazforecast2<-data.frame(Hazleton,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Hazdroptimeseries),xlab="Hazelton")
plot(forecast(Hazforecast,12), main="Hazelton drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Hazdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Pottsville
Pottdrop<-(PAdroptime$Pottsville)
Pottdroptimeseries<-ts(Pottdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Pottforecast<-HoltWinters(Pottdroptimeseries, gamma=TRUE)
Pottcoeffs<-coefficients(Pottforecast)
Pottsville<-Pottcoeffs[1]+Pottcoeffs[3:14]
Pottforecast2<-data.frame(Pottsville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Pottdroptimeseries),xlab="Pottsville")
plot(forecast(Pottforecast,12), main="Pottsville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Pottdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Sunbury
Sundrop<-(PAdroptime$Sunbury)
Sundroptimeseries<-ts(Sundrop, start=c(2012,7), end=c(2014,9), frequency=12)
Sunforecast<-HoltWinters(Sundroptimeseries, gamma=TRUE)
Suncoeffs<-coefficients(Sunforecast)
Sunbury<-Suncoeffs[1]+Suncoeffs[3:14]
Sunforecast2<-data.frame(Sunbury,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Sundroptimeseries),xlab="Sunbury")
plot(forecast(Sunforecast,12), main="Sunbury drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Sundroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Wilkes Barre
Wildrop<-(PAdroptime$Wilkes)
Wildroptimeseries<-ts(Wildrop, start=c(2012,7), end=c(2014,9), frequency=12)
Wilforecast<-HoltWinters(Wildroptimeseries, gamma=TRUE)
Wilcoeffs<-coefficients(Wilforecast)
WilkesBarre<-Wilcoeffs[1]+Wilcoeffs[3:14]
Wilforecast2<-data.frame(WilkesBarre,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Wildroptimeseries),xlab="Wilkes Barre")
plot(forecast(Wilforecast,12), main="Wilkes-Barre drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Wildroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)


###MJ
MJdroptime<-read.csv("MJ.csv")
MJdroptimeseries<-ts(MJdroptime, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts((MJdroptimeseries), main="MJ Drop Trends", xlab="Past 26 months")
MJtotdrop<-(MJdroptime$Region)
Regdroptimeseries<-ts(MJtotdrop, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts(Regdroptimeseries, xaxt="n",ylab="Total Monthly Drops", col="blue", xlab="", main="MJ Monthly Drop trend")
points(Regdroptimeseries, col="red", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14"))                        

####Columbus
Coldrop<-(MJdroptime$Columbus)
Coldroptimeseries<-ts(Coldrop, start=c(2012,7), end=c(2014,9), frequency=12)
Colforecast<-HoltWinters(Coldroptimeseries, gamma=TRUE)
Colforecast
Colcoeffs<-coefficients(Colforecast)
Columbus<-Colcoeffs[1]+Colcoeffs[3:14]
Colforecast2<-data.frame(Columbus,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Coldroptimeseries),xlab="Columbus")
plot(forecast(Colforecast,12), main="Columbus drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Coldroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Dayton
Daydrop<-(MJdroptime$Dayton)
Daydroptimeseries<-ts(Daydrop, start=c(2012,7), end=c(2014,9), frequency=12)
Dayforecast<-HoltWinters(Daydroptimeseries, gamma=TRUE)
Daycoeffs<-coefficients(Dayforecast)
Dayton<-Daycoeffs[1]+Daycoeffs[3:14]
Dayforecast2<-data.frame(Dayton,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Daydroptimeseries),xlab="Dayton")
plot(forecast(Dayforecast,12), main="Dayton drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Daydroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Independence
Inddrop<-(MJdroptime$Independence)
Inddroptimeseries<-ts(Inddrop, start=c(2012,7), end=c(2014,9), frequency=12)
Indforecast<-HoltWinters(Inddroptimeseries, gamma=TRUE)
Indcoeffs<-coefficients(Indforecast)
Independence<-Indcoeffs[1]+Indcoeffs[3:14]
Indforecast2<-data.frame(Independence,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Inddroptimeseries),xlab="Independence")
plot(forecast(Indforecast,12), main="Independence drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Inddroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Sharonville
Shadrop<-(MJdroptime$Sharonville)
Shadroptimeseries<-ts(Shadrop, start=c(2012,7), end=c(2014,9), frequency=12)
Shaforecast<-HoltWinters(Shadroptimeseries, gamma=TRUE)
Shacoeffs<-coefficients(Shaforecast)
Sharonville<-Shacoeffs[1]+Shacoeffs[3:14]
Shaforecast2<-data.frame(Sharonville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Shadroptimeseries),xlab="Sharonville")
plot(forecast(Shaforecast,12), main="Sharonville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Shadroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Springboro
Sprdrop<-(MJdroptime$Springboro)
Sprdroptimeseries<-ts(Sprdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Sprforecast<-HoltWinters(Sprdroptimeseries, gamma=TRUE)
Sprcoeffs<-coefficients(Sprforecast)
Springboro<-Sprcoeffs[1]+Sprcoeffs[3:14]
Sprforecast2<-data.frame(Springboro,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Sprdroptimeseries),xlab="Springboro")
plot(forecast(Sprforecast,12), main="Springboro drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Sprdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Troy
Trodrop<-(MJdroptime$Troy)
Trodroptimeseries<-ts(Trodrop, start=c(2012,7), end=c(2014,9), frequency=12)
Troforecast<-HoltWinters(Trodroptimeseries, gamma=TRUE)
Trocoeffs<-coefficients(Troforecast)
Troy<-Trocoeffs[1]+Trocoeffs[3:14]
Troforecast2<-data.frame(Troy,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Trodroptimeseries),xlab="Troy")
plot(forecast(Troforecast,12), main="Troy drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Trodroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)


###MA
MAdroptime<-read.csv("MA.csv")
MAdroptimeseries<-ts(MAdroptime, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts((MAdroptimeseries), main="MA Drop Trends", xlab="Past 26 months")

MAtotdrop<-(MAdroptime$total)
MAtotdroptimeseries<-ts(MAtotdrop, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts(MAtotdroptimeseries, xaxt="n",ylab="Total Monthly Drops", col="blue", xlab="", main="MA Monthly Drop trend")
points(MAtotdroptimeseries, col="red", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14"))                        

####Roanoke
Roadrop<-(MAdroptime$Roanoke)
Roadroptimeseries<-ts(Roadrop, start=c(2012,7), end=c(2014,9), frequency=12)
Roaforecast<-HoltWinters(Roadroptimeseries, gamma=TRUE)
Roaforecast
Roacoeffs<-coefficients(Roaforecast)
Roanoke<-Roacoeffs[1]+Roacoeffs[3:14]
Roaforecast2<-data.frame(Roanoke,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Roadroptimeseries), xlab="Roanoke")
plot(forecast(Roaforecast,12), main="Roanoke drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Roadroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Lynchburg
Lyndrop<-(MAdroptime$Lynchburg)
Lyndroptimeseries<-ts(Lyndrop, start=c(2012,7), end=c(2014,9), frequency=12)
Lynforecast<-HoltWinters(Lyndroptimeseries, gamma=TRUE)
Lyncoeffs<-coefficients(Lynforecast)
Lynchburg<-Lyncoeffs[1]+Lyncoeffs[3:14]
Lynforecast2<-data.frame(Lynchburg,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Lyndroptimeseries),xlab="Lynchburg")
plot(forecast(Lynforecast,12), main="Lynchburg drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Lyndroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Cary
Carydrop<-(MAdroptime$Cary)
Carydroptimeseries<-ts(Carydrop, start=c(2012,7), end=c(2014,9), frequency=12)
Caryforecast<-HoltWinters(Carydroptimeseries, gamma=TRUE)
Carycoeffs<-coefficients(Caryforecast)
Cary<-Carycoeffs[1]+Carycoeffs[3:14]
Caryforecast2<-data.frame(Cary,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Carydroptimeseries),xlab="Cary")
plot(forecast(Caryforecast,12), main="Cary drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Carydroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Raleigh
Raldrop<-(MAdroptime$Raleigh)
Raldroptimeseries<-ts(Raldrop, start=c(2012,7), end=c(2014,9), frequency=12)
Ralforecast<-HoltWinters(Raldroptimeseries, gamma=TRUE)
Ralcoeffs<-coefficients(Ralforecast)
Raleigh<-Ralcoeffs[1]+Ralcoeffs[3:14]
Ralforecast2<-data.frame(Raleigh,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Raldroptimeseries),xlab="Raleigh")
plot(forecast(Ralforecast,12), main="Raleigh drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Raldroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Fayetteville
Faydrop<-(MAdroptime$Fayetteville)
Faydroptimeseries<-ts(Faydrop, start=c(2012,7), end=c(2014,9), frequency=12)
Fayforecast<-HoltWinters(Faydroptimeseries, gamma=TRUE, beta=1)
Faycoeffs<-coefficients(Fayforecast)
Fayetteville<-Faycoeffs[1]+Faycoeffs[3:14]
Fayetteville
Fayforecast2<-data.frame(Fayetteville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Faydroptimeseries),xlab="Fayetteville")
plot(forecast(Fayforecast,12), main="Fayetteville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Faydroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Jacksonville
Jacdrop<-(MAdroptime$Jacksonville)
Jacdroptimeseries<-ts(Jacdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Jacforecast<-HoltWinters(Jacdroptimeseries, gamma=TRUE)
Jaccoeffs<-coefficients(Jacforecast)
Jacksonville<-Jaccoeffs[1]+Jaccoeffs[3:14]
Jacforecast2<-data.frame(Jacksonville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Jacdroptimeseries),xlab="Jacksonville")
plot(forecast(Jacforecast,12), main="Jacksonville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Jacdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Greenville
Gredrop<-(MAdroptime$Greenville)
Gredroptimeseries<-ts(Gredrop, start=c(2012,7), end=c(2014,9), frequency=12)
Greforecast<-HoltWinters(Gredroptimeseries, gamma=TRUE)
Grecoeffs<-coefficients(Greforecast)
Greenville<-Grecoeffs[1]+Grecoeffs[3:14]
Greforecast2<-data.frame(Greenville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Gredroptimeseries),xlab="Greenville")
plot(forecast(Greforecast,12), main="Greenville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Gredroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Conway
Condrop<-(MAdroptime$Conway)
Condroptimeseries<-ts(Condrop, start=c(2012,7), end=c(2014,9), frequency=12)
Conforecast<-HoltWinters(Condroptimeseries, gamma=TRUE)
Concoeffs<-coefficients(Conforecast)
Conway<-Concoeffs[1]+Concoeffs[3:14]
Conforecast2<-data.frame(Conway,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Wildroptimeseries),xlab="Conway")
plot(forecast(Conforecast,12), main="Conway drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Condroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

####Charleston
Chardrop<-(MAdroptime$Charleston)
Chardroptimeseries<-ts(Chardrop, start=c(2012,7), end=c(2014,9), frequency=12)
Charforecast<-HoltWinters(Chardroptimeseries, gamma=TRUE)
Charforecast
Charcoeffs<-coefficients(Charforecast)
Charleston<-Charcoeffs[1]+Charcoeffs[3:14]
Charforecast2<-data.frame(Charleston,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Chardroptimeseries),xlab="Charleston")
plot(forecast(Charforecast,12), main="Charleston drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Chardroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)


###South
SOdroptime<-read.csv("SO.csv")
SOdroptimeseries<-ts(SOdroptime, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts((SOdroptimeseries), main="South Drop Trends", xlab="Past 26 months")
SOtotdrop<-(SOdroptime$total)
SOdroptimeseries<-ts(SOtotdrop, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts(SOdroptimeseries, xaxt="n",ylab="Total Monthly Drops", col="blue", xlab="", main="South Monthly Drop trend")
points(SOdroptimeseries, col="red", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14"))                        

####Columbus, GA
Colgadrop<-(SOdroptime$ColumbusGA)
Colgadroptimeseries<-ts(Colgadrop, start=c(2012,7), end=c(2014,9), frequency=12)
Colgaforecast<-HoltWinters(Colgadroptimeseries, gamma=TRUE)
Colgaforecast
Colgacoeffs<-coefficients(Colgaforecast)
ColumbusGA<-Colgacoeffs[1]+Colgacoeffs[3:14]
Colgaforecast2<-data.frame(ColumbusGA,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Colgadroptimeseries),xlab="ColumbusGA")
plot(forecast(Colgaforecast,12), main="ColumbusGA drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Colgadroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Monroe
Mondrop<-(SOdroptime$Monroe)
Mondroptimeseries<-ts(Mondrop, start=c(2012,7), end=c(2014,9), frequency=12)
Monforecast<-HoltWinters(Mondroptimeseries, gamma=TRUE)
Moncoeffs<-coefficients(Monforecast)
Monroe<-Moncoeffs[1]+Moncoeffs[3:14]
Monforecast2<-data.frame(Monroe,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Mondroptimeseries),xlab="Monroe")
plot(forecast(Monforecast,12), main="Monroe drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Mondroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Shreveport
Shrdrop<-(SOdroptime$Shreveport)
Shrdroptimeseries<-ts(Shrdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Shrforecast<-HoltWinters(Shrdroptimeseries, gamma=TRUE)
Shrcoeffs<-coefficients(Shrforecast)
Shreveport<-Shrcoeffs[1]+Shrcoeffs[3:14]
Shrforecast2<-data.frame(Shreveport,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Shrdroptimeseries),xlab="Shreveport")
plot(forecast(Shrforecast,12), main="Shreveport drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Shrdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Macon
Macdrop<-(SOdroptime$Macon)
Macdroptimeseries<-ts(Macdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Macforecast<-HoltWinters(Macdroptimeseries, gamma=TRUE)
Maccoeffs<-coefficients(Macforecast)
Macon<-Maccoeffs[1]+Maccoeffs[3:14]
Macforecast2<-data.frame(Macon,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Macdroptimeseries),xlab="Macon")
plot(forecast(Macforecast,12), main="Macon drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Macdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Augusta
Augdrop<-(SOdroptime$Augusta)
Augdroptimeseries<-ts(Augdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Augforecast<-HoltWinters(Augdroptimeseries, gamma=TRUE)
Augcoeffs<-coefficients(Augforecast)
Augusta<-Augcoeffs[1]+Augcoeffs[3:14]
Augforecast2<-data.frame(Augusta,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Augdroptimeseries),xlab="Augusta")
plot(forecast(Augforecast,12), main="Augusta drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Augdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

##Madison
Maddrop<-(SOdroptime$Madison)
Maddroptimeseries<-ts(Maddrop, start=c(2012,7), end=c(2014,9), frequency=12)
Madforecast<-HoltWinters(Maddroptimeseries, gamma=TRUE)
Madcoeffs<-coefficients(Madforecast)
Madison<-Madcoeffs[1]+Madcoeffs[3:14]
Madforecast2<-data.frame(Madison,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Maddroptimeseries),xlab="Madison")
plot(forecast(Madforecast,12), main="Madison drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Maddroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

####Gulfport
Guldrop<-(SOdroptime$Gulfport)
Guldroptimeseries<-ts(Guldrop, start=c(2012,7), end=c(2014,9), frequency=12)
Gulforecast<-HoltWinters(Guldroptimeseries, gamma=TRUE)
Gulforecast
Gulcoeffs<-coefficients(Gulforecast)
Gulfport<-Gulcoeffs[1]+Gulcoeffs[3:14]
Gulforecast2<-data.frame(Gulfport,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Guldroptimeseries), xlab="Gulfport")
plot(forecast(Gulforecast,12), main="Gulfport drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Guldroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Clarksville
Cladrop<-(SOdroptime$Clarksville)
Cladroptimeseries<-ts(Cladrop, start=c(2012,7), end=c(2014,9), frequency=12)
Claforecast<-HoltWinters(Cladroptimeseries, gamma=TRUE)
Clacoeffs<-coefficients(Claforecast)
Clarksville<-Clacoeffs[1]+Clacoeffs[3:14]
Claforecast2<-data.frame(Clarksville,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Cladroptimeseries),xlab="Clarksville")
plot(forecast(Claforecast,12), main="Clarksville drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Cladroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Chattanooga
Chadrop<-(SOdroptime$Chattanooga)
Chadroptimeseries<-ts(Chadrop, start=c(2012,7), end=c(2014,9), frequency=12)
Chaforecast<-HoltWinters(Chadroptimeseries, gamma=TRUE)
Chacoeffs<-coefficients(Chaforecast)
Chattanooga<-Chacoeffs[1]+Chacoeffs[3:14]
Chaforecast2<-data.frame(Chattanooga,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Chadroptimeseries),xlab="Chattanooga")
plot(forecast(Chaforecast,12), main="Chattanooga drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Chadroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)


###Ned
Neddroptime<-read.csv("Ned.csv")
Neddroptimeseries<-ts(Neddroptime, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts((Neddroptimeseries), main="Ned Drop Trends", xlab="Past 26 months")
Nedtotdrop<-(Neddroptime$total)
Neddroptimeseries<-ts(Nedtotdrop, start=c(2012,7), end=c(2014,9), frequency=12)
plot.ts(Neddroptimeseries, xaxt="n",ylab="Total Monthly Drops", col="blue", xlab="", main="Ned Monthly Drop trend")
points(Neddroptimeseries, col="red", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14"))                        

###Wilmington
Wildrop<-(Neddroptime$Wilmington)
Wildroptimeseries<-ts(Wildrop, start=c(2012,7), end=c(2014,9), frequency=12)
Wilforecast<-HoltWinters(Wildroptimeseries, gamma=TRUE)
Wilforecast
Wilcoeffs<-coefficients(Wilforecast)
Wilmington<-Wilcoeffs[1]+Wilcoeffs[3:14]
Wilforecast2<-data.frame(Wilmington,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Wildroptimeseries),xlab="wilmington")
plot(forecast(Wilforecast,12), main="Wilmington drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Wildroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Tucson
Tucdrop<-(Neddroptime$Tucson)
Tucdroptimeseries<-ts(Tucdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Tucforecast<-HoltWinters(Tucdroptimeseries, gamma=TRUE)
Tuccoeffs<-coefficients(Tucforecast)
Tucson<-Tuccoeffs[1]+Tuccoeffs[3:14]
Tucforecast2<-data.frame(Tucson,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Tucdroptimeseries),xlab="Tucson")
plot(forecast(Tucforecast,12), main="Tucson drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Tucdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Circus 
Cirdrop<-(Neddroptime$Circus)
Cirdroptimeseries<-ts(Cirdrop, start=c(2012,7), end=c(2014,9), frequency=12)
Cirforecast<-HoltWinters(Cirdroptimeseries, gamma=TRUE)
Circoeffs<-coefficients(Cirforecast)
Circus<-Circoeffs[1]+Circoeffs[3:14]
Cirforecast2<-data.frame(Circus,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))     
plot(decompose(Cirdroptimeseries),xlab="Circus")
plot(forecast(Cirforecast,12), main="Circus drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Cirdroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

###Online
Onldrop<-(Neddroptime$Online)
Onldroptimeseries<-ts(Onldrop, start=c(2012,7), end=c(2014,9), frequency=12)
Onlforecast<-HoltWinters(Onldroptimeseries, gamma=TRUE)
Onlcoeffs<-coefficients(Onlforecast)
Online<-Onlcoeffs[1]+Onlcoeffs[3:14]
Onlforecast2<-data.frame(Online,row.names=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"))  
plot(decompose(Onldroptimeseries),xlab="Online")
plot(forecast(Onlforecast,12), main="Online drop forecast", xaxt="n", col="red", ylab="Monthly Drops")
points(Onldroptimeseries, col="blue", bg="red", pch=23)
axis(1,at=c(2012.5, 2013.0, 2013.5, 2014.0, 2014.5, 2015.0, 2015.5), labels=c("July12", "Jan13", "Jul13", "Jan14", "Jul14", "Jan15", "Jul15"), cex.axis=1)

write.xlsx(c(Wyoforecast2, Allforecast2, Carforecast2, Dicforecast2, Hazforecast2, Sunforecast2, Pottforecast2, Wilforecast2, Colforecast2, Dayforecast2, Indforecast2, Shaforecast2, Sprforecast2, Troforecast2, Roaforecast2, Lynforecast2, Caryforecast2, Fayforecast2, Jacforecast2, Ralforecast2, Conforecast2, Charforecast2,Greforecast2,Colgaforecast2, Monforecast2, Shrforecast2, Macforecast2, Augforecast2, Madforecast2, Gulforecast2, Claforecast2, Chaforecast2, Cirforecast2, Wilforecast2, Onlforecast2, Tucforecast2),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/mydata.xlsx")




