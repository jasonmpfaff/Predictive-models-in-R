install.packages("devtools")
library(devtools)
#install.packages("stringi")
#library(stringi)
devtools::install_github("google/CausalImpact")
library("CausalImpact")

###Trump
Trump<-(data$Trump)
Trumptimeseries<-ts(Trump)
Trump
plot(Trumptimeseries, col=("red"), main="Trump Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Trumptimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Trumpimpact<-CasualImpact(Trump, pre.period, post.period)
plot(Trumpimpact)
summary(Trumpimpact)
summary(Trumpimpact, "report")

###Bush
Bush<-(data$Bush)
Bushtimeseries<-ts(Bush)
Bush
plot(Bushtimeseries, col=("red"), main="Bush Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Bushtimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Bushimpact<-CausalImpact(Bush, pre.period, post.period)
plot(Bushimpact)
summary(Bushimpact)
summary(Bushimpact, "report")

###Christie
Christie<-(data$Christie)
Christietimeseries<-ts(Christie)
Christie
plot(Christietimeseries, col=("red"), main="Christie Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Christietimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Christieimpact<-CausalImpact(Christie, pre.period, post.period)
plot(Christieimpact)
summary(Christieimpact)
summary(Christieimpact, "report")

##Paul
Paul<-(data$Paul)
Paultimeseries<-ts(Paul)
Paul
plot(Paultimeseries, col=("red"), main="Paul Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Paultimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Paulimpact<-CausalImpact(Paul, pre.period, post.period)
plot(Paulimpact)
summary(Paulimpact)
summary(Paulimpact, "report")

###Walker
data<-read.csv("Trump.csv")
Walker<-(data$Walker)
Walkertimeseries<-ts(Walker)
Walker
plot(Walkertimeseries, col=("red"), main="Walker Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Walkertimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Walkerimpact<-CausalImpact(Walker, pre.period, post.period)
plot(Walkerimpact)
summary(Walkerimpact)
summary(Walkerimpact, "report")

#Rubio
Rubio<-(data$Rubio)
Rubiotimeseries<-ts(Rubio)
Rubio
plot(Rubiotimeseries, col=("red"), main="Rubio Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Rubiotimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Rubioimpact<-CausalImpact(Rubio, pre.period, post.period)
plot(Rubioimpact)
summary(Rubioimpact)
summary(Rubioimpact, "report")

ts.plot(Bushtimeseries, Trumptimeseries, Christietimeseries, Paultimeseries, col=c("blue", "red", "black", "green"), xlab=c("Poll results in chronological order"), ylab="Poll Results")
legend(x="topleft", legend=c("Bush", "Trump", "Christie", "Paul"), col=c("blue", "red", "black", "green"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5), cex=1)


#######Trump Election Calculator Function#######

trumpregression<-lm(Trump~Bush+Christie+Paul+Walker+Rubio)
summary(trumpregression)

##Trump election calculator function
trumpcoeffs<-coefficients(trumpregression)
trumpcoeffs

##Enter poll numbers for Bush, Christie, Paul, Walker, and Rubio
Trumpelect<-function(Bu, Ch, Pa, Wa, Ru){
  Buweight<-trumpcoeffs[1]+trumpcoeffs[2]*Bu
  Chweight<-trumpcoeffs[3]*Ch
  Paweight<-trumpcoeffs[4]*Pa
  Waweight<-trumpcoeffs[5]*Wa
  Ruweight<-trumpcoeffs[6]*Ru
  Trumppredict<-Buweight+Chweight+Paweight
  return(Trumppredict)
   
}
###enter poll values for Bush, Christie, Paul,Walker, Rubio####
Trumpelect(6,1,3,2,6)




















