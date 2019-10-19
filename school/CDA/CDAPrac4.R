## Baseline Category Logit Model
alligator <- read.csv("alligator.csv", header=T)
alligator=alligator[order(alligator$length),]
head(alligator)

library(stats4);library(splines);library(VGAM)
fit1=vglm(choice~length,family=multinomial, data=alligator)
summary(fit1)

#alternatively,
library(nnet)
alligator$choice2=relevel(alligator$choice, ref="O")
fit2=multinom(choice2~length, data=alligator)
summary(fit2)

fit0=vglm(choice~1,family=multinomial, data=alligator)
-2*(logLik(fit0)-logLik(fit1)) > qchisq(0.95,2)
{
est.prob=predict(fit1, type='response')
plot(alligator$length, est.prob[,1],type='l',xlab='Length',
ylab='Estimated Probability',ylim=c(0,1),xlim=c(1,4),lwd=2,lty=1,cex.lab=1.5,cex.axis=1.5)
lines(alligator$length,est.prob[,2],lwd=2,lty=2,col='red')
lines(alligator$length, est.prob[,3],lwd=3,lty=3,col='blue')
legend('topleft',legend=c('F','I','O'),lty=1:3, lwd=2,
  col=c('black','red','blue'))
}

## Example for Goodness of Fit
yes=c(371,250,64,25)
und=c(49,45,9,5)
no=c(74,71,15,13)
gender=as.factor(rep(c('Female','Male'),2))
gender=relevel(gender,ref='Male')
race=as.factor(rep(c('White','Black'),each=2))
after=data.frame(yes,und,no,gender,race)
fit1=vglm(cbind(yes,und,no)~gender+race,family=multinomial,data=after)
summary(fit1)

# Goodness of Fit
est.prob <- predict(fit1, type='response')
est.cnt <- est.prob
for(i in 1:nrow(after)) est.cnt[i,] <- est.prob[i,] * sum(after[i,1:3])
xSq <- sum((after[,1:3]-est.cnt)^2/est.cnt)
gSq <- 2*sum(after[,1:3]*log(after[,1:3]/est.cnt))
xSq > qchisq(0.95,2)
gSq > qchisq(0.95,2)

fitR=vglm(cbind(yes,und,no)~race,family=multinomial,data=after)
fitG=vglm(cbind(yes,und,no)~gender,family=multinomial,data=after)
-2*(logLik(fitR)-logLik(fit1)) > qchisq(0.95,2)
-2*(logLik(fitG)-logLik(fit1)) > qchisq(0.95,2)

## Cumulative Logit Model
y1=c(44,18,36,12)#Very Liberal
y2=c(47,28,34,18)#Slightly Liberal
y3=c(118,86,53,62)#Moderate
y4=c(23,39,18,45)#Slightly Conservative
y5=c(32,48,23,51)#Very Conservative
party=as.factor(rep(c('dem','rep'),2))
party=relevel(party,ref='rep')
gender=as.factor(rep(c('female','male'),each=2))
PI=data.frame(y1,y2,y3,y4,y5,party,gender)
fit1=vglm(cbind(y1,y2,y3,y4,y5)~party,family=cumulative(parallel=TRUE),data=PI)
fit2=vglm(cbind(y1,y2,y3,y4,y5)~party,family=cumulative(parallel=FALSE),data=PI)
#Parallel : A logical or formula specifying which terms have equal/unequal coefficients.
fit3=vglm(cbind(y1,y2,y3,y4,y5)~1,family=cumulative(parallel=FALSE),data=PI)
summary(fit1)
summary(fit2)
summary(fit3)

-2*(logLik(fit1)-logLik(fit2)) > qchisq(0.95,3)

fittedprob0=predict(fit1,type='response')
fittedprob=fittedprob0[1:2,]
obs0=cbind(y1,y2,y3,y4,y5)
obsct=rbind((obs0[1,]+obs0[3,]),(obs0[2,]+obs0[4,]))
expct=fittedprob*matrix(rep(as.vector(rowSums(obsct)),5),2,5)
xSq = sum((obsct-expct)^2/expct)
gSq = sum(2*obsct*log(obsct/expct))
xSq > qchisq(0.95,3)
gSq > qchisq(0.95,3)

-2*(logLik(fit1)-logLik(fit2)) > qchisq(0.95,1)

## Mental Health Data
mental=read.table('mental.txt',header=FALSE)
names(mental) = c('Subject','MI','SES','Life')
mental$SES=as.factor(mental$SES)
mental$MI2=mental$Life; mental$MI2[mental$MI=='Well']=1
mental$MI2[mental$MI=='Mild']=2
mental$MI2[mental$MI=='Moderate']=3
mental$MI2[mental$MI=='Impaired']=4
fit1=vglm(MI2~Life+SES,family=cumulative(parallel=TRUE),data=mental)
fit2=vglm(MI2~Life+SES+Life:SES,family=cumulative(parallel=TRUE),data=mental)

fit1=vglm(cbind(y1,y2,y3,y4,y5)~party,family=acat(parallel=TRUE),data=PI)
summary(fit1)

## Loglinear Model
life= data.frame(count=c(1339, 300,260, 55,88,22),
                 race=c(rep(c('whites','blacks','others'),each=2)),
                 belief=c(rep(c('yes','no'),times=3)))
life$race=relevel(life$race,ref='others')
life$belief=relevel(life$belief,ref='no')
fit1= glm(count~race+belief, family=poisson, data=life)
summary(fit1)

fit2= glm(count~race+belief+race:belief, family=poisson, data=life)
summary(fit2)

acm=data.frame(A=rep(c('yes','no'),each=4),
C=rep(c('yes','no'),each=2,times=2),
M=rep(c('yes','no'),4),
count=c(911,538,44,456,3,43,2,279))
fit.acamcm=glm(count~A+C+M+A:C+A:M+C:M, family=poisson, data=acm)
fit.amcm= glm(count~A+C+M+A:M+C:M, family=poisson, data=acm)
fit.acm= glm(count~A*C*M, family=poisson, data=acm)

library(knitr)
kable(cbind(acm[,1:3],fitted(fit.acamcm),fitted(fit.amcm),fitted(fit.acm)),
      digits=2,col.names=c('Alcohol','Cigarette','Marijuana','(AC,AM,CM)','(AM,CM)','(AMC)'),caption='aaa')

co <- coef(fit.acamcm)
se <- sqrt(diag(vcov(fit.acamcm)))
ci.AC.condi <- as.numeric(co[5]) + 1.96*as.numeric(se[5])*c(-1,1)
ci.AC.condi <- exp(ci.AC.condi)
