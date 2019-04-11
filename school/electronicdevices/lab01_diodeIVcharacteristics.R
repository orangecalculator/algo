library(tidyverse)

IVchar <- read_csv("./lab1_diodeIVdata.csv",col_names=F)
IVchar[16:33,1:2] <- -IVchar[16:33,1:2]
IVcharorgin<-IVchar
#IVchar[,2]<-IVchar[,2]*1e6
#IVchar <- IVchar[1:29,]
IVchar <- IVchar %>% filter(X1>0)
IVchar[,2] <- IVchar[,2] * 1e4

der11 <- function(x,dat){
  -sum((exp(x[[2]]*dat[,1])-1)^2)
}

der12 <- function(x,dat){
  sum(dat[,1]*exp(x[[2]]*dat[,1])*(dat[,2]-2*x[[1]]*(exp(x[[2]]*dat[,1])-1)))
}

der22 <- function(x,dat){
  sum(dat[,1]^2*x[[1]]*exp(x[[2]]*dat[,1])*(dat[,2]-2*x[[1]]*exp(x[[2]]*dat[,1])+x[[1]]))
}

fun1 <- function(x,dat){
  sum((exp(x[[2]]*dat[,1])-1)*(dat[,2]-x[[1]]*(exp(x[[2]]*dat[,1])-1)))
}

fun2 <- function(x,dat){
  sum(dat[,1]*x[[1]]*exp(x[[2]]*dat[,1])*(dat[,2]-x[[1]]*(exp(x[[2]]*dat[,1])-1)))
}

x<-c(1,50) ## initial condition
xold<-c(0,0)
while(sum(abs(xold-x))>1e-10){
  xold <- x
  
  J <- matrix(rep(0,4),ncol=2)
  J[1,1] <- der11(x,IVchar)
  J[1,2] <- J[2,1] <- der12(x,IVchar)
  J[2,2] <- der22(x,IVchar)
  
  f <- matrix(rep(0,2),ncol=1)
  f[[1]] <- fun1(x,IVchar)
  f[[2]] <- fun2(x,IVchar)
  
  x <- x - solve(J) %*% f
}

dat<-IVchar
dat[,2]-x[[1]]*(exp(x[[2]]*dat[,1])-1)

IVcharorgin <- IVchar
IVaux <- function(x){
  sum((exp(x[[2]]*IVcharorgin[,1])-1)*(IVcharorgin[,2]-x[[1]]*(exp(x[[2]]*IVcharorgin[,1])-1)))^2
  + sum(x[[1]]*IVcharorgin[,1]*exp(x[[2]]*IVcharorgin[,1])*(IVcharorgin[,2]-x[[1]]*(exp(x[[2]]*IVcharorgin[,1])-1)))^2
}

ra <- optim(c(2, 20),IVaux)
x<-ra$par
ra1 <- optim(c(2, 20),function(x){sum((IVchar[,2]-x[[1]]*(exp(x[[2]]*IVchar[,1])-1))^2)})
x<-ra1$par
#xc<-0
#while(sum(abs(xc-x))>1e-20){
#  xc<-x
#  ra2 <- optim(x,function(x){fun1(x,IVchar)^2+fun2(x,IVchar)^2})
#  x<-ra2$par
#}
## using model i / (exp(x * V)-1) - I
#ra3 <- optim(c(1.65,15),function(x){sum((IVchar[,2]/(exp(x[[2]]*IVchar[,1])-1)-x[[1]])^2)})
#x<-ra3$par

IVcharest <- IVchar %>%
  mutate(Iest=x[[1]]*(exp(x[[2]]*X1)-1))

IVchar %>%
ggplot()+
  geom_point(aes(x=X1,y=X2),color="blue")+
  #geom_point(aes(x=X1,y=Iest),color="red")+
  stat_function(fun = function(val){(exp(19.928*val-3.118))},color="red")#function(val){x[[1]]*(exp(x[[2]]*val)-1)},color="red")+
  #geom_smooth(aes(x=X1,y=X2),data=(IVchar%>%filter(X1<0.6)),method="lm",color="red",se=F)+
  #geom_smooth(aes(x=X1,y=X2),data=(IVchar%>%filter(X1>0.6)),method="lm",color="red",se=F)+
  labs(x="Voltate[V]",y="Current[mA]",title=element_text("I/V Characteristic data"))

(IVchar%>%filter(X1>0.6)) %>%
  lm(X2~X1,data=.)

ggplot()+
  geom_point(aes(X1,1-Iest/X2),data=IVcharest)
  
# good value 1.647904e-04 1.485505e+01

IVchar %>%
  mutate(Iest=x[[1]]*(exp(X1*x[[2]])-1)) %>%
  print(n=Inf)
