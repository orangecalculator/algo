## Prerequisites
library(magrittr)
library(dplyr)

## Read Data
crab1<-read.table("crab.txt",header=T)
head(crab1)

## Edit Data
crab1$color<-factor(crab1$color-1,levels=1:4)
crab1$spine<-factor(crab1$spine,levels=1:3)
crab1$weight<-crab1$weight/1000
head(crab1)
str(crab1)

## Try some Fits
fit1<-glm(y~width,crab1,family=gaussian(link="identity"))
summary(fit1)

fit2<-glm(y~width,crab1,family=binomial)
summary(fit2)

fit3<-glm(y~weight+width+relevel(color,4)+relevel(spine,3),binomial,crab1)
summary(fit3)

## 4.8
# (a)
fit4.8.1<-glm(y~weight,binomial,crab1)
summary(fit4.8.1)
# (b)
coef4.8.1<-fit4.8.1$coefficients
rbind(coef4.8.1)%*%rbind(rep(1,3),c(1.20,2.44,5.20)) %>%
  (function(x){(exp(-as.vector(x))+1)^-1})
# (c)
-coef4.8.1[[1]]/coef4.8.1[[2]]
# (d)
1/4*coef4.8.1[[2]]
1/4*coef4.8.1[[2]]*0.1
1/4*coef4.8.1[[2]]*0.58
# (e)
summary(fit4.8.1)
confint.default(fit4.8.1) %>% exp
# (f)
summary(fit4.8.1)
(fit4.8.1$null.deviance-fit4.8.1$deviance) %>% pchisq(1,lower.tail = F)

## 4.10
# (b)
logit<-function(x){stopifnot(is.numeric(x));stopifnot(all(0<x&x<1));log(x/(1-x))}
l<-logit(c(0.32,0.30,0.19,0.17))
l[1:3]-l[4];l[4]

## 4.16
# (a)
typembti<-function(x,m,n){relevel(factor(rep(x,each=m,n),levels=x),ref=2)}
mbti<-data.frame(y=c(10,8,5,7,3,2,4,15,17,3,6,4,1,5,1,6)
                 ,n=c(67,34,101,72,20,16,27,65,123,49,132,102,12,30,30,73)
                 ,m=typembti(c('E','I'),8,1)
                 ,b=typembti(c('S','N'),4,2)
                 ,t=typembti(c('T','F'),2,4)
                 ,i=typembti(c('J','P'),1,8))
mbti
fit4.16<-glm(cbind(y,n)~m+b+t+i,binomial,mbti)
summary(fit4.16)
# (b)
p<-t(as.matrix(coef(fit4.16)))%*%as.matrix(rep(1,5))
logitinv<-function(x){stopifnot(is.numeric(x));(1+exp(-x))^-1}
logitinv(p)
# (c)
summary(fit4.16)

## 4.20
# (a)
typejoy<-function(x,m,n) typembti(x,m,n)
joy<-data.frame(y=c(11,10,16,22,14,7,2,1,6,0,1,0,1,1,4,6)
                ,n=c(25,27,4,10,5,12,14,16,11,12,10,10,4,8,2,1)
                ,treatment=typejoy(c('Drug','Control'),1,8)
                ,center=relevel(factor(rep(1:8,each=2),levels=1:8),ref=8))
joy
(joy$y/joy$n)[(1:8)*2-1]/(joy$y/joy$n)[(1:8)*2]

fit4.20S<-glm(cbind(y,n)~treatment+center,binomial,joy)
summary(fit4.20S)
pchisq(fit4.20S$null.deviance-fit4.20S$deviance,fit4.20S$df.null-fit4.20S$df.residual,lower.tail = F)
confint.default(fit4.20S)

fit4.20I<-joy %>%
  group_by(treatment) %>%
  summarize(y=sum(y),n=sum(n)) %>%
  glm(cbind(y,n)~treatment,binomial,.)
summary(fit4.20I)

pchisq(fit4.20I$null.deviance-fit4.20I$deviance,fit4.20I$df.null-fit4.20I$df.residual,lower.tail = F)
confint.default(fit4.20I)
# (b)
tablestat<-function(x){
  stopifnot(is.data.frame(x))
  stopifnot(nrow(x)==2&ncol(x)==2)
  rsum<-apply(x,2,sum)
  csum<-apply(x,1,sum)
  sums<-sum(rsum)
  
  c(n=x[[1,1]],mu=rsum[[1]]*csum[[1]]/sums,var=prod(rsum)*prod(csum)/(sums^2*(sums-1)))
}
cmh<-matrix(vector(length=24),ncol=3)
for(i in 1:8) cmh[i,]<-tablestat(joy[(2*i-1):(2*i),1:2])
cmhval<-sum(cmh[,1]-cmh[,2])^2/sum(cmh[,3])

sum(joy[,1:2])
pchisq(cmhval,1,lower.tail = F)

## 5.3
# (a)
pchisq(173.68-170.44,3,lower.tail = F)
# (c)
pchisq(186.61-177.61,166-160,lower.tail = F)

## 5.4
# (a)
fittednum5.4<-apply(mbti[,1:2],1,sum)*c(fit4.16$fitted.values,1-fit4.16$fitted.values) %>% matrix(ncol=2)
gsq5.4<-2*sum(mbti[,1:2]*log(mbti[,1:2]/fittednum5.4))
fittednum5.4>5
pchisq(gsq5.4,16-5,lower.tail = F)
# (b)
pchisq(0.8,1,lower.tail = F)
summary(glm(cbind(y,n)~m+b+t,binomial,mbti))
