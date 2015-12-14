#load historical 
ratedata<-read.csv("groundrate.csv", header=TRUE)
ratemodel<-lm(rate~fts+drop+days+place, ratedata)
ratecoeffs<-coefficients(ratemodel)
ratecoeffs
summary(ratecoeffs)
summary(ratemodel)
plot(fts~rate, ratedata)
abline(lm(fts~rate, ratedata),par(xpd=FALSE))
plot(days~fts, ratedata)
abline(lm(days~fts, ratedata),par(xpd=FALSE))
plot(rate~drop, ratedata)
abline(lm(rate~drop, ratedata),par(xpd=FALSE))
plot(days~rate, ratedata)
abline(lm(days~rate, ratedata),par(xpd=FALSE))
plot(drop~days, ratedata)
abline(lm(drop~days, ratedata),par(xpd=FALSE))
plot(place~rate, col="red", pch=19, ratedata)
abline(lm(place~rate, ratedata),par(xpd=FALSE))

#enter actual value for the rate below in line 12,13,14 to pass to the weight calcs
fts<-97.28
drop<-5
days<-23
place<-1
ftsweight<-ratecoeffs[1]+ratecoeffs[2]*fts
dropweight<-ratecoeffs[3]*drop
daysweight<-ratecoeffs[4]*days
placeweight<-ratecoeffs[5]*place
rateperdict<-ftsweight+dropweight+daysweight+placeweight
ftsweight
dropweight
daysweight
placeweight
rateperdict

#######Online##

#load historical 
ratedata<-read.csv("onlinerate.csv", header=TRUE)
ratemodel<-lm(rate~fts+drop+days+place+ground, ratedata)
ratecoeffs<-coefficients(ratemodel)
ratecoeffs
summary(ratecoeffs)
summary(ratemodel)
plot(fts~rate, ratedata)
abline(lm(fts~rate, ratedata),par(xpd=FALSE))
plot(days~fts, ratedata)
abline(lm(days~fts, ratedata),par(xpd=FALSE))
plot(rate~drop, ratedata)
abline(lm(rate~drop, ratedata),par(xpd=FALSE))
plot(days~rate, ratedata)
abline(lm(days~rate, ratedata),par(xpd=FALSE))
plot(drop~days, ratedata)
abline(lm(drop~days, ratedata),par(xpd=FALSE))
#enter actual value for the rate below in line 12,13,14 to pass to the weight calcs
fts<-96.3
drop<-5.4
days<-30
place<-1
ground<-42.62
ftsweight<-ratecoeffs[1]+ratecoeffs[2]*fts
dropweight<-ratecoeffs[3]*drop
daysweight<-ratecoeffs[4]*days
placeweight<-ratecoeffs[5]*place
#groundweight<-ratecoeffs[6]*ground
rateperdict<-ftsweight+dropweight+daysweight+placeweight+groundweight
ftsweight
dropweight
daysweight
placeweight
groundweight
rateperdict




#load the vector of new rate values via one column csv sheet with a header
#ftspredict<-read.csv("ftspredictor.csv", header=TRUE)
#ftspred<-predict(ratemodel,newdata=ftspredict)
#ftspred


#load historical rate and fts data to build model in a csv with a header, fts in 1st columN and title it "linear"
intermdata<-read.csv("zeta.csv", header=TRUE)
t1<-data.frame(intermdata)
intermlm<-lm(zeta~alpha, t1)
intermcoeffs<-coefficients(intermlm)
intermcoeffs
summary(intermcoeffs)
summary(intermlm)
#enter actual value for the rate below in line 7 to pass to the "rate" variable
rate<-96.92
rateperdict<-intermcoeffs[1]+intermcoeffs[2]*rate
rateperdict
#load the vector of new rate values via one column csv sheet with a header
#ftspredict<-read.csv("ftspredictor.csv", header=TRUE)
#ftspred<-predict(ftslm,newdata=ftspredict)
#ftspred

###########market predict--o---matic#####
marketdata<-read.csv("market.csv", header=TRUE)
data.frame(marketdata)
marketmodel<-lm(Grads~LQ+per1k+emp, marketdata)
marketcoeffs<-coefficients(marketmodel)
marketcoeffs
summary(marketcoeffs)
summary(marketmodel)
plot(Grads~LQ, marketdata)
abline(lm(Grads~LQ, marketdata),par(xpd=FALSE))
plot(Grads~per1k, marketdata)
abline(lm(Grads~per1k, marketdata),par(xpd=FALSE))
plot(Grads~emp, marketdata)
abline(lm(Grads~emp, marketdata),par(xpd=FALSE))
plot(LQ~per1k, marketdata)
abline(lm(LQ~per1k, marketdata),par(xpd=FALSE))

#enter actual value for the rate below in line 12,13,14 to pass to the weight calcs
LQ<-2.14
per1k<-5.86
emp<-2360
LQweight<-marketcoeffs[1]+marketcoeffs[2]*LQ
per1kweight<-marketcoeffs[3]*per1k
empweight<-marketcoeffs[4]*emp
marketpredict<-LQweight+per1kweight+empweight
LQweight
per1kweight
empweight
marketpredict
