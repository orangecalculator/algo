###Categorical Data Test Statistics
(ma<-data.frame(D=c(762,484),I=c(327,239),R=c(468,477)))
conttest<-function(ma){
(mr<-as.matrix(apply(ma,1,sum)))
(mc<-t(as.matrix(apply(ma,2,sum))))
(mu<-mr%*%mc/sum(mc))
##Pearson Chisq
sum((ma-mu)^2/mu);print(paste("Pearson Chisq: ",sum((ma-mu)^2/mu),",df is ",(nrow(ma)-1)*(ncol(ma)-1)))
##LRT
2*sum(ma*log(ma/mu));print(paste("Likelihood Ratio Statistic: ",2*sum(ma*log(ma/mu)),",df is ",(nrow(ma)-1)*(ncol(ma)-1)))
##Odds ratio for 2x2
if(nrow(ma)==2&&ncol(ma)==2)
  print(paste("Odds ratio: ",ma[1,1]*ma[2,2]/ma[1,2]/ma[2,1]))
##Cell Residual
(ma-mu)/sqrt(mu)
print((ma-mu)/sqrt(mu))
qnorm(seq(0.05,0.95,0.1))*sqrt((nrow(ma)-1)*(ncol(ma)-1)/(nrow(ma)*ncol(ma)))
##Standardized Residual
print((ma-mu)/sqrt(mu*(1-mr/sum(mr))%*%(1-mc/sum(mc))))}

conttest(ma[,-3])
conttest(cbind(apply(ma[,-3],1,sum),ma[,3]))

conttest(data.frame(Y=c(400,416,188),N=c(1380,1823,1168)))
conttest(data.frame(Y=c(816,188),N=c(3203,1168)))
conttest(data.frame(Y=c(400,416),N=c(1380,1823)))

###Ordinal Data test statistics

(oa<-matrix(c(12,307,8,246,31,439,41,245),nrow=2))
(sr<-c(1,2))
(sc<-c(1,2,3,4))
ordtest<-function(oa,sr,sc){
(oa<-as.matrix(oa))
(or<-as.matrix(apply(oa,1,sum)))
(oc<-t(as.matrix(apply(oa,2,sum))))
(sr<-t(as.matrix(sr-sum(sr*t(or))/sum(or))))
(sc<-as.matrix(sc-sum(sc*t(oc))/sum(oc)))
r<-(sr%*%oa%*%sc)/sqrt(sum((t(sr)^2 * or))*sum((sc^2*t(oc))))
print(paste("correlation: ",r))
print(paste("correlation statistic: ",r^2 * sum(oa)))
}

ordtest(oa,sr,sc)

HD<-data.frame(Y=c(24,35,21,30),N=c(1335,603,192,224),x=c(0,2,4,5))
HD<-mutate(HD,p=Y/(Y+N))
summary(glm(p~x,gaussian,data=HD))
summary(glm(p~x,quasibinomial,data=HD))
summary(glm(p~x,quasibinomial(link="probit"),data=HD))

HDlong<-data.frame()
for(i in 1:nrow(HD)){HDlong<-rbind(HDlong,cbind(c(rep(1,HD[i,1]),rep(0,HD[i,2])),rep(HD[i,3],HD[i,1]+HD[i,2])))}
summary(glm(Y~X,gaussian,data=HDlong))
summary(glm(Y~X,binomial,data=HDlong))
summary(glm(Y~X,binomial(link="probit"),data=HDlong))

summary(glm(cbind(Y,N)~x,binomial,data=HD))
