T<-c(404,286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148)
Y<-c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19)

##initial value for iteration
alpha<- -1
theta<-3

nbdertheta<-function(Y,T,alpha,theta){
  if(length(T)!=(n<-length(Y))) {print("Pairing error: put in vectors of same length");return(NULL)}
  stack<-0
  for(i in 1:n) {stack<-stack+sum((theta-1+(1:Y[i]))^-1)}
  return(stack+n*log(theta)+n-sum(log(theta+T*exp(alpha)))-sum((Y+theta)/(theta+T*exp(alpha))))
}
nbderalpha<-function(Y,T,alpha,theta){
  if(length(T)!=(n<-length(Y))) {print("Pairing error: put in vectors of same length");return(NULL)}
  sum(Y-(Y+theta)*(T*exp(alpha))/(theta+T*exp(alpha)))
}

nbinf<-function(Y,T,alpha,theta){
  if(length(T)!=(n<-length(Y))) {print("Pairing error: put in vectors of same length");return(NULL)}
  stack<-0
  mu<-T*exp(alpha)
  alp<-sum(theta*mu/(theta+mu))
  the<-length(Y)/(-theta) + sum((theta+mu)^-1)
  for(i in 1:n){the<-the+sum((theta-1+(1:Y[i]))^-2)}
  return(c(the,alp))
}

  ite<-as.matrix(c(theta,alpha))
  iteold<-100##initial value for iteration
while(sum(abs(iteold-ite))>1e-8){
  iteold<-ite
  ite<-ite+solve(diag(nbinf(Y,T,ite[2],ite[1])))%*%as.matrix(c(nbdertheta(Y,T,ite[2],ite[1]),nbderalpha(Y,T,ite[2],ite[1])))
  print(ite)
}
  
{
theta<-ite[1]
alpha<-ite[2]
}

##Dispersion parameter
  ite[1]^-1
##Variance of alpha
  sum(T*exp(alpha)/(1+T*exp(alpha)/theta))^-1
  sqrt(sum(T*exp(alpha)/(1+T*exp(alpha)/theta))^-1)

##Variance of theta
  nbinf(Y,T,ite[2],ite[1])^-1
##Approximate Variance of inverse theta by delta method
##Var(theta)/theta^4
vary<-(nbinf(Y,T,ite[2],ite[1])^-1)[1]
sqrt(vary/theta^4)

##Plot of Y and Expectation
## library(tidyverse)
ggplot()+
  geom_point(aes(T,Y))+
  geom_abline(intercept=0,slope=sum(Y)/sum(T),color="blue")
##Estimation on the poisson model
(Y-T*sum(Y)/sum(T))/sqrt(T*sum(Y)/sum(T))

T<-data.frame(A=c(8,7,6,6,3,4,7,2,3,4),B=c(9,9,8,14,8,13,11,5,7,6))
X<-cbind(rep(c(0,1),each=10),c(T$A,T$B))

derpoilog <- function(X,alpha,beta){
  return(c(
    sum(-exp(alpha+beta*X[,1])+X[,2]),
    sum(-X[,1]*exp(alpha+beta*X[,1])+X[,1]*X[,2])
    ))
}

Infpoilog <- function(X,alpha,beta){
  U<-cbind(rep(1,nrow(X)),X[,1])
  return(t(U)%*%diag(exp(alpha+beta*X[,1]))%*%U)
}

{
  ##initial value
  iteold<-1+(ite<-c(3,3))
  while(sum(abs(iteold-ite))>1e-10){
    iteold<-ite
    ite<-iteold+solve(Infpoilog(X,ite[1],ite[2]))%*%as.matrix(derpoilog(X,ite[1],ite[2]))
    print(ite)
    }
  
}

Y<-cbind(rep(c(0,1),each=10),rep(c(0,1,0,1),each=5),c(T$A,T$B))

derpoi3log <- function(X,alpha,beta,gamma){
  return(c(
    sum(-exp(alpha+beta*X[,1]+gamma*X[,2])+X[,3]),
    sum(-X[,1]*exp(alpha+beta*X[,1]+gamma*X[,2])+X[,1]*X[,3]),
    sum(-X[,2]*exp(alpha+beta*X[,1]+gamma*X[,2])+X[,2]*X[,3])
  ))
}

Infpoi3log <- function(X,alpha,beta,gamma){
  U<-cbind(rep(1,nrow(X)),X[,1],X[,2])
  return(t(U)%*%diag(exp(alpha+beta*X[,1]+gamma*X[,2]))%*%U)
}

{
  ##initial value
  iteold<-1+(ite<-c(3,3,3))
  while(sum(abs(iteold-ite))>1e-10){
    iteold<-ite
    ite<-iteold+solve(Infpoi3log(Y,ite[1],ite[2],ite[3]))%*%as.matrix(derpoi3log(Y,ite[1],ite[2],ite[3]))
    print(ite)
  }
  
}

##Problem 3.18
T<-c(404,286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148)
Y<-c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19)

derpoilogoff <- function(X,T,alpha){
  return(c(
    sum(-T*exp(alpha)+X)
  ))
}

Infpoilogoff <- function(X,T,alpha){
  return(sum(T*exp(alpha)))
}

{
  ##initial value
  iteold<-1+(ite<-3)
  while(abs(iteold-ite)>1e-10){
    iteold<-ite
    ite<-iteold+(Infpoilogoff(Y,T,ite))^-1*derpoilogoff(Y,T,ite)
    print(ite)
  }
  
}
