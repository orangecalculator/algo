## 9.7
dat1<-data.frame(Area=factor(1:3,levels=1:3),N=c(46,67,93)
                   ,n=c(9,13,20),y=c(1,2,2))
# Estimation of proportion
prop1<-with(dat1,sum(N*(y/n))/sum(N))
# Bound of error
2*sqrt(with(dat1,1/mean(N)^2*(1-3/7)/3*sum((N*(y/n-prop1))^2)/2
     +1/(3*7*mean(N)^2)*sum(N^2*(1-n/N)*(y/n)*(1-y/n)/(n-1))))

## 9.9
dat2<-data.frame(Case=factor(1:6,levels=1:6)
                   ,m=c(7.9,8,7.8,7.9,8.1,7.9)
                   ,ssd=c(0.15,0.12,0.09,0.11,0.10,0.12))
# Estimation by different method
mean1<-with(dat2,sum(12*m)/(6*12))
mean2<-with(dat2,24/6*sum(12*m)/(24*12))
# Bound of error for each case
2*sqrt(with(dat2,(1-6/24)/(6*12^2)*12^2*sum((m-mean1)^2)/5
            +1/(6*24*12^2)*sum(12^2*(1-4/12)*ssd/4)))
2*sqrt(with(dat2,(1-6/24)/(6*12^2)*12^2*sum((m-mean2)^2)/5
            +1/(6*24*12^2)*sum(12^2*(1-4/12)*ssd/4)))
