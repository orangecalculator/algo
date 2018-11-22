crab <- read.csv("crab.csv")
crab$spine <- factor(crab$spine)
crab$color <- factor(crab$color)
y <- ifelse(crab$satell>0,1,0)
crab <- cbind(crab,y)
fit=glm(y~width, data=crab,family=binomial(link="identity"))
## Error: no valid set of coefficients has been found: please supply starting values
fit=glm(y~width, data=crab,family=gaussian(link="identity"))
fit=glm(y~width, data=crab,family=binomial)

sampleprop=c(0.36,0.29,0.61,0.54,0.68,0.83,0.83,1.00)
samplewidth=c(23,24,25,26,27,28,29,30)
predprob=function(x)f
res=exp(fit$coef[1]+ fit$coef[2]*x)/(1+exp(fit$coef[1]+ fit$coef[2]*x))
res
g
xvec=seq(22,32,by=0.01)
plot(xvec,predprob(xvec),type=
       l
     ,lwd=2,lty=2,xlab=
       Width
     ,ylab=
       Proportion Having Satellites
     ,cex.lab=1.5,cex.axis=1.5,ylim=c(0,1.1))
points(samplewidth, sampleprop,pch=16,cex=1.5)
points(samplewidth, predprob(samplewidth),pch=3,cex=2)
legend(
  bottomright
  ,pch=c(16,3),legend=c(
    observed
    ,
    fitted
  ),cex=1.5)

res <- glm(y ~ width, data = crab, family = binomial())
EL50 <- -res$coefficients[1]/res$coefficients[2]

summary(res)
CI.wald <- c(res$coef[2] - 1.96*sqrt(vcov(res)[2,2]),
             res$coef[2] + 1.96*sqrt(vcov(res)[2,2]))
CI.LR <- confint(res)
#CI for odds
exp(confint(res))
#how CI changes when width increases by 5
exp(confint(res) * 5)

# CI for prob
x <- 26.5
hat.logit= res$coef[1] + res$coef[2]*x
se.logit= sqrt(vcov(res)[1,1] + 2*x*vcov(res)[1,2]
               + x^2*vcov(res)[2,2])
ci.logit=c(hat.logit-1.96*se.logit,hat.logit+1.96*se.logit)
#ci.logit
exp(ci.logit)/(1+exp(ci.logit))

## Significance Testing
res$deviance
res$coef[2]/sqrt(vcov(res)[2,2]) > qnorm(0.95)
fit0 <- glm(y~1, data=crab, family = binomial)
anova(fit0,res)
-2*(logLik(fit0) - logLik(res)) > qchisq(0.95, 1)

## Categorical Predictor
symptom = matrix(c(14,93,32,81,11,52,12,43),nrow=4,byrow=T)
azt=c(1,0,1,0); race=c(1,1,0,0); aids=data.frame(symptom,azt,race)
names(aids) <- c('yes','no','azt','race')
fit=glm(cbind(yes,no)~azt+race,data=aids,family=binomial)
fit0=glm(cbind(yes,no)~race,data=aids,family=binomial)
fit1=glm(cbind(yes,no)~azt,data=aids,family=binomial)
anova(fit0,fit)
anova(fit1,fit)
library(lmtest)
lrtest(fit0,fit)
lrtest(fit1,fit)

## Multiple Logistic Regression
fit1 <- glm(y~relevel(color,ref=4)+width, data=crab,family=binomial)
co=coef(fit1)
wid=seq(18,35,by=0.01)
col1=exp(co[1]+co[2]+ co[5]*wid)/(1+exp(co[1]+co[2]+co[5]*wid))
col2=exp(co[1]+co[3]+ co[5]*wid)/(1+exp(co[1]+co[3]+co[5]*wid))
col3=exp(co[1]+co[4]+ co[5]*wid)/(1+exp(co[1]+co[4]+co[5]*wid))
col4=exp(co[1]+co[5]*wid)/(1+exp(co[1]+co[5]*wid))
plot(wid,col1,type='l',lty=1,lwd=1.5,
     xlab='width', ylab='predicted probability',cex.lab=2,cex.axis=2)
lines(wid,col2,lty=2, lwd=1.5,col='red')
lines(wid,col3,lty=3, lwd=3,col='blue')
lines(wid,col4,lty=4, lwd=2,col='maroon')
legend('bottomright',legend=c('1:light medium','2:medium'
                              ,'3:dark medium','4:dark')
  ,lty=c(1,2,3,4),col=c('black','red','blue','maroon')
  ,lwd=c(1.5,1.5,3,2),cex=1.5)

## Model Comparison on Multiple Logistic Regression
fit0 <- glm(y~width, data=crab,family=binomial)
lrtest(fit0,fit1)

fit2 <- glm(y~as.numeric(color)+width, data=crab,family=binomial)
lrtest(fit2,fit1)
crab$color.bin <- as.factor(1*(!(crab$color==4)))
fit3 <- glm(y~color.bin+width+ color.bin*width, data=crab,family=binomial)
fit4 <- glm(y~color.bin+width, data=crab,family=binomial)
lrtest(fit4,fit3)
