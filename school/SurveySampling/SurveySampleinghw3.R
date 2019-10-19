##Problem 1 (Textbook 8.8, 8.9)
dat1<-data.frame(dbs=1:15,n=c(51,62,49,73,101,48,65,49,73,61,58,52,65,49,55),
                 y=c(42,53,40,45,63,31,38,30,54,45,51,29,46,37,42))

###Estimated proportion by cluster sampling
(p1<-sum(dat1$y)/sum(dat1$n))
###Estimated bound on error
(vb1<-2*sqrt(1/mean(dat1$n)^2*(87-15)/87/15/14*sum((dat1$y-p1*dat1$n)^2)))

###Estimated needed sample cluster numbers for new policy
87*(0.04^2*mean(dat1$n)^2*87/(1/14*sum((dat1$y-p1*dat1$n)^2))+1)^-1

##Problem 2 (Textbook 8.10, 8.11)
dat2<-data.frame(obs=1:20,n=c(55,60,63,58,71,78,69,58,52,71,73,64,69,58,63,75,78,51,67,70),
                 y=c(2210,2390,2430,2380,2760,3110,2780,2370,1990,2810,2930,2470,2830,2370,2390,2870,3210,2430,2730,2880))

###Estimated average cost per household
(m2<-sum(dat2$y)/sum(dat2$n))
###Estimated bound on error
(vb2<-2*sqrt(1/mean(dat2$n)^2*(60-20)/60/20*(1/19*sum((dat2$y-m2*dat2$n)^2))))

###Estimated total cost
(tot2<-60/20*sum(dat2$y))
###Estimated bound on error
(vbt2<-2*sqrt(60^2*(60-20)/60/20*(1/19*sum((dat2$y-mean(dat2$y))^2))))

##Problem 3 (Textbook 8.33)
###No need of calculation