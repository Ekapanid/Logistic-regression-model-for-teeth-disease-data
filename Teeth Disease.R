
#install.packages("stargazer")
library(stargazer)

#install.packages("xtable")
require(xtable)


library(haven)
RF<- read_sav("C:/Users/ΕΥΡΙΠΙΔΗΣ/Desktop/RF_Study_no.missing.42-47.sav")

View(RF)

RF<-na.omit(data.frame(RF))
str(RF)

RF$Gender<-as.numeric(RF$Gender)
RF$ToothType<-as.numeric(RF$ToothType)


library(dplyr)

RF$ones<-c(rep(1,951))

############################################################################################################################

grouped_by_ev<-RF%>%
  group_by(ToothType,Age,AllTeeth,Gender)%>%
  summarise(sum(RF))



grouped_by_ev2<-RF%>%
  group_by(ToothType,Age,AllTeeth,Gender)%>%
  summarise(sum(ones))


#install.packages("arm")
library(arm)

grouped_by_ev<-data.frame(grouped_by_ev)
grouped_by_ev2<-data.frame(grouped_by_ev2)


grouped_by_ev$ones<-grouped_by_ev2$sum.ones.
grouped_by_ev$failures<-grouped_by_ev$ones- grouped_by_ev$sum.RF.




counts4<-table(grouped_by_ev$sum.RF.,grouped_by_ev$AllTeeth)


counts3<-table(grouped_by_ev$ones)


counts2<- table(grouped_by_ev$sum.RF.,grouped_by_ev$ToothType)

counts<- table(grouped_by_ev$sum.RF.)

z<-c(0,1,2)
par(mfrow=c(3,2))
barplot(counts,main="Distribution of RF diseases in groups", horiz=TRUE,beside = TRUE)
barplot(counts3,main="Distribution of number of participants in groups", horiz=FALSE,beside = TRUE)
barplot(counts2,main="Distribution of RF disease by Toothtype", horiz=FALSE,beside = TRUE,legend = rownames(counts))
barplot(counts4,main="Distribution of RF disease by number of healthy teeth", horiz=FALSE,beside = TRUE,legend = rownames(counts))
hist(grouped_by_ev$Age,main="Histogram of Age",xlab="Age")
hist(log(grouped_by_ev$Age),main="Histogram of Age",xlab ="Age")




str(grouped_by_ev)

x<-cbind(grouped_by_ev$sum.RF.,grouped_by_ev$failures)


logmod1<-glm(x~ as.factor(ToothType)*Age+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):Age+as.factor(Gender):AllTeeth,family=binomial(link="logit"),data=grouped_by_ev)
summary(logmod1)

logmod2<-glm(x~ as.factor(ToothType)*Age+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):Age+as.factor(Gender):AllTeeth,family=binomial(link="probit"),data=grouped_by_ev)
summary(logmod2)
                
logmod3<-glm(x~ as.factor(ToothType)*Age+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):Age+as.factor(Gender):AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev)
summary(logmod3)

sum.m1<-summary(logmod1)
sum.m2<-summary(logmod2)
sum.m3<-summary(logmod3)


#Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred, meaning that complete separation has occured. Model needs less variables. 

stargazer(logmod1,logmod2,logmod3,align=TRUE,type="html",out="C:/Users/ΕΥΡΙΠΙΔΗΣ/Documents/table3.html")

#########################################################################################################################################


#Judging from the EDA factor toothtype has two levels, 5 and 7, that predict perfectly the outcome. Thus they must be removed
#from the dataset
RF$ToothType<-as.numeric(RF$ToothType)

RF2<-RF%>%
  filter(ToothType!=5 & ToothType!=7)

grouped_by_ev_merg<-RF2%>%
  group_by(ToothType,Age,AllTeeth,Gender)%>%
  summarise(sum(RF))



grouped_by_ev_merg<-data.frame(grouped_by_ev_merg)
str(grouped_by_ev_merg)
grouped_by_ev_merg$ToothType<-as.factor(grouped_by_ev_merg$ToothType)

levels(grouped_by_ev_merg$ToothType)


grouped_by_ev2_merg<-RF2%>%
  group_by(ToothType,Age,AllTeeth,Gender)%>%
  summarise(sum(ones))

grouped_by_ev2_merg<-data.frame(grouped_by_ev2_merg)

grouped_by_ev_merg$ones<-grouped_by_ev2_merg$sum.ones.
grouped_by_ev_merg$failures<-grouped_by_ev_merg$ones- grouped_by_ev_merg$sum.RF.

x<-cbind(grouped_by_ev_merg$sum.RF.,grouped_by_ev_merg$failures)

head(grouped_by_ev_merg)



#logit model on the new dataset


logmod1.a<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age),family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.a)

logmod1.b<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.b)

logmod1.c<-glm(x~as.factor(ToothType)+log(Age) +AllTeeth+as.factor(Gender)+as.factor(Gender):AllTeeth+as.factor(Gender):log(Age),family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.c)

logmod1.d<-glm(x~as.factor(ToothType)+log(Age)+AllTeeth+as.factor(Gender)+as.factor(Gender):AllTeeth,family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.d)

logmod1.e<-glm(x~as.factor(ToothType)+log(Age)+AllTeeth+as.factor(Gender),family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.e)

logmod1.f<-glm(x~as.factor(ToothType)+log(Age)+AllTeeth,family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.f)


logmod1.g<-glm(x~ 
                 as.factor(ToothType)+log(Age),family=binomial(link="logit"),data=grouped_by_ev_merg)

summary(logmod1.g)

logmod1.h<-glm(x~ 
                 as.factor(ToothType),family=binomial(link="logit"),data=grouped_by_ev_merg)
summary(logmod1.h)

sum.m1<-summary(logmod1.h)


anov<-anova(logmod1.h,logmod1.g,logmod1.f,logmod1.e,logmod1.d,logmod1.c,logmod1.b,logmod1.a,test = "Chisq" )

anov<-data.frame(anov)
anov2<-xtable(anov)

print(anov2, type="html", file="C:/Users/ΕΥΡΙΠΙΔΗΣ/Documents/table8.html")

#probit model on new dataset

logmod2.a<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):Age+as.factor(Gender):AllTeeth,family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.a)

logmod2.b<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.b)

logmod2.c<-glm(x~as.factor(ToothType)+log(Age) +AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.c)

logmod2.d<-glm(x~as.factor(ToothType)+log(Age) +AllTeeth+as.factor(Gender)+ as.factor(Gender):AllTeeth,family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.d)

logmod2.e<-glm(x~as.factor(ToothType)+log(Age)+AllTeeth +as.factor(Gender),family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.e)

logmod2.f<-glm(x~ 
                 as.factor(ToothType)+log(Age)+as.factor(Gender),family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.f)


logmod2.g<-glm(x~ 
                 as.factor(ToothType)+log(Age),family=binomial(link="probit"),data=grouped_by_ev_merg)

summary(logmod2.g)


logmod2.h<-glm(x~ 
                 as.factor(ToothType),family=binomial(link="probit"),data=grouped_by_ev_merg)
summary(logmod2.h)

anov<-anova(logmod2.h,logmod2.g,logmod2.f,logmod2.e,logmod2.d,logmod2.c,logmod2.b,logmod2.a,test = "Chisq" )


anov<-data.frame(anov)
anov2<-xtable(anov)

print(anov2, type="html", file="C:/Users/ΕΥΡΙΠΙΔΗΣ/Documents/table9.html")

#clogclog models on new dataset


logmod3.a<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(ToothType):AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.a)

logmod3.b<-glm(x~ as.factor(ToothType)*log(Age)+AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.b)

logmod3.c<-glm(x~as.factor(ToothType)+log(Age) +AllTeeth+as.factor(Gender)+as.factor(Gender):log(Age)+as.factor(Gender):AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.c)

logmod3.d<-glm(x~as.factor(ToothType)+log(Age)+AllTeeth+as.factor(Gender)+ as.factor(Gender):AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.d)

logmod3.e<-glm(x~as.factor(ToothType)+log(Age) +AllTeeth+as.factor(Gender),family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.e)

logmod3.f<-glm(x~ 
                 as.factor(ToothType)+AllTeeth+log(Age),family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.f)

logmod3.g<-glm(x~ 
                 as.factor(ToothType)+AllTeeth,family=binomial(link="cloglog"),data=grouped_by_ev_merg)

summary(logmod3.g)


logmod3.h<-glm(x~ 
                 as.factor(ToothType),family=binomial(link="cloglog"),data=grouped_by_ev_merg)
summary(logmod3.h)

anov<-anova(logmod3.h,logmod3.g,logmod3.f,logmod3.e,logmod3.d,logmod3.c,logmod3.b,logmod3.a,test = "Chisq" )


anov<-data.frame(anov)
anov2<-xtable(anov)

print(anov2, type="html", file="C:/Users/ΕΥΡΙΠΙΔΗΣ/Documents/table10.html")


stargazer(logmod1.h,logmod2.h,logmod3.h,align=TRUE,type="html",out="C:/Users/ΕΥΡΙΠΙΔΗΣ/Documents/table3.html")


#Goodness of fit graphical test with Deviance residuals
sum.m1<-summary(logmod1.h)
sum.m2<-summary(logmod2.h)
sum.m3<-summary(logmod3.h)

par(mfrow=c(3,1))
qqnorm(sum.m1$deviance.resid)
qqline(sum.m1$deviance.resid)
qqnorm(sum.m2$deviance.resid,col="blue")
qqline(sum.m2$deviance.resid,col="blue")
qqnorm(sum.m3$deviance.resid,col="red")
qqline(sum.m3$deviance.resid,col="red")

a<-shapiro.test(sum.m1$deviance.resid)
b<-shapiro.test(sum.m2$deviance.resid)
c<-shapiro.test(sum.m3$deviance.resid)
z<-cbind(a$p.value,b$p.value,c$p.value)

e<-subset(sum.m1$deviance.resid,sum.m1$deviance.resid>1)
y<-c(16,23,37,44,45,46,53,58,59,66,84,98,104,113,127,163,165,177,222)

grouped_by_ev_excluded<-grouped_by_ev_merg[c(16,23,37,44,45,46,53,58,59,66,84,98,104,113,127,163,165,177,222),c(5,6)]
grouped_by_ev_excluded


1-pchisq(143.28,274)

#Deviance Residuals should follow the normal distribution but that seems not be the case. However majority of groups created
#are of size 1,2,3,4,5 and in the case of small populated groups the Deviance Residuals need not be normally distributed (Dobson, Barnett)
#Indeed it can be noticed that the data that diverges from the qqplot line are actually the groups that have had the RF disease.



#AIC,BIC Scores

AIC(logmod1.h,logmod2.h,logmod3.h)
BIC(logmod1.h,logmod2.h,logmod3.h)



#Computing odds ratios and respective 95% CI's
exp(coef(logmod1.h))
exp(confint(logmod1.h))

###################################################################################################################
###############################################################################################


#Imbalanced data needs oversampling for prediction accuracy


install.packages("smotefamily")
library(smotefamily)
library(caret)
library(tree)
library(e1071)

RF$Gender<-as.factor(RF$Gender)
RF$ToothType<-as.factor(RF$ToothType)
RF$RF<-as.factor(RF$RF)

str(RF)

RF2<-RF[,-c(1,7)]

RF2

RF2<-RF2%>%
  filter(ToothType!="5"& ToothType!="7")

str(RF2)

RF2<-data.frame(RF2)



s_size <- floor(0.6 * nrow(RF2))
set.seed(123)
train_index <- sample(1:nrow(RF2), size = s_size)

train<-RF2[train_index,]
test<-RF2[-train_index, ]


str(train)


dum<-dummyVars("~.",data=train)

trsf <- data.frame(predict(dum, newdata=train))
trsf<-trsf[,-c(7,9,10)]

dum2<-dummyVars("~.",data=test)
trsf2 <- data.frame(predict(dum, newdata=test))
trsf2<-trsf2[,-c(7,9,10)]



str(trsf)

trsf_bal<-BLSMOTE(trsf[,-8],trsf$RF)

new_train<-trsf_bal$data

new_train$class



#trees
tree.tip2<-tree(as.factor(class)~.,new_train)


summary(tree.tip2)
plot(tree.tip2)
text(tree.tip2,pretty=0)


tree.pred2<-predict(tree.tip2,trsf2,type="class")

trsf2

ConMat2<-table(tree.pred2,trsf2$RF.1)
ConMat2
confusionMatrix(ConMat2)


cv.tip<-cv.tree(tree.tip2, FUN=prune.misclass)
names(cv.tip)
cv.tip

prune.tip <- prune.tree(tree.tip2, best=4)

summary(prune.tip)

plot(prune.tip)
text(prune.tip,pretty=0)


tree.pred3<-predict(prune.tip, trsf2, type="class")

ConMat3<-table(tree.pred3,trsf2$RF.1)
ConMat3
confusionMatrix(ConMat3)



#SVMs

library(ggplot2)


svmfit<-svm(as.factor(class)~.-Gender.0-Gender.1,data=new_train,kernel="linear",cost=10,scaled=FALSE)




set.seed(1)
tune.out=tune(svm,as.factor(class)~.,data=new_train,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1.5,5,10,100)))


bestmod =tune.out$best.model
summary(tune.out)
summary (bestmod)


pred<-predict(svmfit,trsf2)



xtab1<-table(pred,trsf2$RF.1)
confusionMatrix(xtab1)








