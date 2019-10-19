##Problem 1
dat1<-data.frame(x=c(0.3,0.5,0.4,0.9,0.7,0.2,0.6,0.5,0.8,0.4,0.8,0.6)
                  ,y=c(6,9,7,19,15,5,12,9,20,9,18,13))
mdat1<-apply(dat1,2,mean)
(r1<-mdat1[2]/mdat1[1])
## The ratio estimation of total volume is
r1*75
## The bound of error for estimation is
2*sqrt(250^2*(1-12/250)*var(dat1$y-r1*dat1$x)/12)

##Problem 2
dat2<-data.frame(x=c(12,30,24,24,18,30,12,6,36,42),
                   y=c(18,42,24,36,24,36,14,10,48,54))
mdat2<-apply(dat2,2,mean)
(r2<-mdat2[2]/mdat2[1])
## The ratio estimation of total total dead fir is
r2*4200
## The bound of error for estimation is
2*sqrt(200^2*(1-10/200)*var(dat2$y-r2*dat2$x)/10)

##Problem 3
dat3<-data.frame(y=c(42.40,41.40,39.60,39.45,37.00,37.80,38.55,38.60,38.80
                     ,39.65,38.45,37.80,37.20,37.60,37.50,36.90,37.30,38.60),
                 x=c(47.80,48.60,48.20,46.75,46.50,45.40,47.30,48.20,49.40
                     ,49.40,44.30,43.90,42.70,43.25,44.55,45.10,45.00,45.25))
mdat3<-apply(dat3,2,mean)
## The ratio estimation of ratio is
(r3<-mdat3[1]/mdat3[2])
## The bound of error for estimation is
2*sqrt(mdat3[2]^-2*(1-18/64)*var(dat3$y-r3*dat3$x)/18)

## Make a plot to see influential points
lr<-lm(y~x,data=dat3)
plot(dat3$x,studres(lr))   ## studentized residual with library(MASS)

## Try removing data with studentized residual over 3
dat4<-dat3[-1,]
mdat4<-apply(dat4,2,mean)
(r4<-mdat4[1]/mdat4[2])
2*sqrt(mdat4[2]^-2*(1-17/64)*var(dat4$y-r4*dat4$x)/17)
