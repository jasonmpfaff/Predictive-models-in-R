install.packages(CasualImpact)
install.packages("pollstR")
install.packages("sqldf")
library(sqldf)

library(CasualImpact)


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

#Perry
Perry<-(data$Perry)
Perrytimeseries<-ts(Perry)
Perry
plot(Perrytimeseries, col=("red"), main="Perry Poll Results", ylab="Poll Results", xlab="Polls conducted Nov 2015-Sept 2016")
points(Perrytimeseries, col="blue", pch=21)

pre.period<-c(1,67)
post.period<-c(68,92)
Perryimpact<-CausalImpact(Perry, pre.period, post.period)
plot(Perryimpact)
summary(Perryimpact)
summary(Perryimpact, "report")




ts.plot(Bushtimeseries, Trumptimeseries, Christietimeseries, Paultimeseries, col=c("blue", "red", "black", "green"), xlab=c("Poll results in chronological order"), ylab="Poll Results")
legend(x="topleft", legend=c("Bush", "Trump", "Christie", "Paul"), col=c("blue", "red", "black", "green"),
       lty=c(1,1,1,1), lwd=c(2.5,2.5,2.5,2.5), cex=1)


#######Trump Election Calculator Function#######

trumpregression<-lm(Trump~Bush+Christie+Paul+Walker+Rubio+Perry)
summary(trumpregression)

##Trump election calculator function
trumpcoeffs<-coefficients(trumpregression)
trumpcoeffs

Trumpelect<-function(Bu, Ch, Pa, Wa, Ru, Pe){
  Buweight<-trumpcoeffs[1]+trumpcoeffs[2]*Bu
  Chweight<-trumpcoeffs[3]*Ch
  Paweight<-trumpcoeffs[4]*Pa
  Waweight<-trumpcoeffs[5]*Wa
  Ruweight<-trumpcoeffs[6]*Ru
  Peweight<-trumpcoeffs[7]*Pe
  Trumppredict<-Buweight+Chweight+Paweight+Waweight+Ruweight+Peweight
  return(Trumppredict)
   
}
###enter poll values for Bush, Christie, Paul,Walker, Rubio####
Trumpelect(6,1,3,2,6,1)


##visuals from Huff Post
library(pollstR)
pollstr_charts()
trump_favorable <- pollstr_chart('donald-trump-favorable-rating')
print(trump_favorable)
(ggplot(trump_favorable[["estimates_by_date"]], aes(x = date, y = value, color = choice))
 + geom_line())

install.packages("rgl")
install.packages("scatterplot3d")
library(rgl)
library(scatterplot3d)

scatterplot3d(base2,pch=19,highlight.3d=TRUE, type="h", main="CFB Model")
s3d <-scatterplot3d( base2, pch=16, highlight.3d=TRUE,type="h", main="CFB Model")

plot3d(base2, col=rainbow(10), size=3, type="h", xlab="", ylab="", zlab="", main="3d")








