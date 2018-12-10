library(stats4);library(splines);library(VGAM)
## 6.8
dat6.8<-data.frame(y4=c(28,4,41,12),y3=c(45,12,44,7),y2=c(29,5,20,3),y1=c(26,2,20,1),
                   therapy=factor(rep(c("Sequential","Alternating"),each=2),levels=c("Alternating","Sequential")),
                   gender=factor(rep(c("Male","Female"),times=2),levels=c("Female","Male")))

# (a)
cumfit6.8 <- vglm(cbind(y1,y2,y3,y4)~therapy+gender,cumulative(parallel=T),dat6.8)
summary(cumfit6.8)
# (b)
cumfit6.8A <- vglm(cbind(y1,y2,y3,y4)~therapy*gender,cumulative(parallel=T),dat6.8)
summary(cumfit6.8A)

## 7.4
dat7.4<-data.frame(gender=factor(rep(c("Male","Female"),each=4),levels=c("Female","Male")),
                   information=factor(rep(c("Support","Oppose"),each=2,times=2),levels=c("Oppose","Support")),
                   health=factor(rep(c("Support","Oppose"),times=4),levels=c("Oppose","Support")),
                   count=c(76,160,6,25,114,181,11,48))
# (a)
fit7.4 <- glm(count~gender*information*health-gender:information:health,poisson(link=log),dat7.4)
summary(fit7.4)

## 7.6
typembti<-function(x,m,n){relevel(factor(rep(x,each=m,n),levels=x),ref=2)}
mbti<-data.frame(y=c(10,8,5,7,3,2,4,15,17,3,6,4,1,5,1,6)
                 ,n=c(67,34,101,72,20,16,27,65,123,49,132,102,12,30,30,73)
                 ,m=typembti(c('E','I'),8,1)
                 ,b=typembti(c('S','N'),4,2)
                 ,t=typembti(c('T','F'),2,4)
                 ,i=typembti(c('J','P'),1,8))
mbti$count <- with(mbti,y+n)
# (a)
fit7.6 <- glm(count~m+b+t+i,poisson(link=log),mbti)
summary(fit7.6)
pchisq(fit7.6$deviance,fit7.6$df.residual,lower.tail = F)
# (b)
fit7.6I <- glm(count~m*b+m*t+m*i+b*t+b*i+t*i,poisson(link=log),mbti)
summary(fit7.6I)
# (c)
summary(glm(count~m*b+b*t+b*i+t*i,poisson(link=log),mbti))

## 8.1
pchisq((37-16)^2/(37+16),1,lower.tail = F)
