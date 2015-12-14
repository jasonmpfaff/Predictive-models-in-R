##read in base data set##
boosted<-read.csv("modeldata.csv")

install.packages("xlsx")
install.packages("ada")
install.packages("lattice")
install.packages("caret")
install.packages("e1071")


##load the below packages and install any not already installed using install.packages##
library(xlsx)
library(ada)
library(lattice)
library(caret)
library(e1071)

##run the model ensemble first##

#random forest##and it stays there

rfmodel<-train(as.factor(grad) ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex,data=boosted, method="rf")
rfmodel
rfmodel$finalModel
rfpredictions<-predict(rfmodel, newdata=boosted, type="raw")
rfpredictionsII<-data.frame(rfpredictions, boosted$SyStudentID)
rfpredictionsII

##bayes glm##
bglmmodel<-train(as.factor(grad) ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex,data=boosted, method="bayesglm")
bglmmodel
bglmmodel$finalModel
bglmpredictions<-predict(bglmmodel, newdata=boosted, type="raw")
bglmpredictionsII<-data.frame(bglmpredictions, boosted$SyStudentID)
bglmpredictionsII
confusionMatrix(bglmmodel)

##NaiveBayes##
nbmodel<-train(as.factor(grad) ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex,data=boosted, method="nb")
nbmodel
nbmodel$finalModel
nbpredictions<-predict(nbmodel, newdata=boosted, type="raw")
nbpredictionsII<-data.frame(nbpredictions, boosted$SyStudentID)
nbpredictionsII
confusionMatrix(nbmodel)

##neural network##
nnetmodel<-train(as.factor(grad) ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex,data=boosted, method="nnet")
nnetmodel
nnetmodel$finalModel
nnetpredictions<-predict(nnetmodel, newdata=boosted, type="raw")
nnetpredictionsII<-data.frame(nnetpredictions, boosted$SyStudentID)
nnetpredictionsII
confusionMatrix(nnetmodel)

##boosted ada##
adamodel<-ada(grad~age+marital+mom+efc+single+ged+year+clock+reenter+sex+bglm+nb+nnet, data=boosted, loss="logistic", type="discrete")
adamodel
adamodelpredictions<-predict(adamodel, newdata=boosted)
adamodelpredictionsII<-data.frame(adamodelpredictions, boosted$SyStudentID)
adamodelpredictionsII
confusionMatrix(adamodel)

##traditional additive multivariate logistic regression##
##this is the base scoring model##
model <- glm(grad ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex+bglm+nb+nnet+bonus, data=boosted, family=binomial)
evmodel <- evtree(grad ~ age+marital+mom+efc+single+ged+year+clock+reenter+sex+bglm+nb+nnet+bonus, data=boosted)




install.packages("partykit")
install.packages("rpart")
install.packages("evtree")

library(evtree)
library(partykit)

summary(model)

coeffs1<-coefficients(model)
coeffs2<-exp(coeffs1)
coeffs2



##final results frame
#modeldata<-data.frame(bonuslist, rfpredictionsII, bglmpredictionsII, nbpredictionsII,nnetpredictionsII, boosted)
#modeldata

## GLM score and format output and output scores to spreadsheet##
vscores<-predict(model, newdata=boosted, type="response")
vscores1<-exp(vscores)
vscores2<-(vscores1-1)
vscorepredict<-data.frame(vscores2, boosted$SyStudentID, boosted$grad)
colnames(vscorelist)<-c("V-score", "studentID", "grad")
vscorepredict

##visuals##
varplot(adamodel, TRUE, FALSE)
plot(adamodel)
plot(adamodel, FALSE, FALSE)
plot(adamodel, TRUE, FALSE)
plot(jitter(grad)~age, boosted)
boxplot(jitter(grad)~clock,boosted)
forplot<-data.frame(boosted$grad,boosted$age, boosted$marital, boosted$mom, boosted$efc, boosted$single, boosted$ged, boosted$year, boosted$clock, boosted$reenter, boosted$sex)
splom(forplot)

write.xlsx(c(vscorepredict),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/finalscorelist.xlsx")
write.xlsx(c(modeldata),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/modeldata.xlsx")
write.xlsx(c(bglmpredictionsII, nbpredictionsII, nnetpredictionsII,bonuslist ),"\\\\deltafile01/DeltaUsers/001VIR/NonVABeach/jason.pfaff/My Documents/masterlist.xlsx")


