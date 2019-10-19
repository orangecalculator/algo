##Explore USArrests Data
str(USArrests)
head(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, sd)
summary(USArrests)

##Principal Component Analysis
pr.out=prcomp(USArrests, scale=TRUE)
str(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
str(pr.out$x)

##Biplot of Principal Components
plot(pr.out$x[,1], pr.out$x[,2])
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component",
     ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')

plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

##Principal Component Regression
library(pls)
library(ISLR)
str(Hitters)
head(Hitters)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

load("AlzheimerDisease.RData")
head(predictors,n=1)