T<-c(404,286,443,169,222,150,321,189,258,223,211,215,108,210,224,211,168,185,158,429,226,150,148)
Y<-c(308,197,184,149,132,126,110,101,99,81,79,78,68,67,60,57,55,44,38,35,29,20,19)

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
