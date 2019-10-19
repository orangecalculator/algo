## Load library
library(tidyverse)

## Read Data
ss1<-read_csv("/media/sf_VirtualBoxShare/raw_data.csv")
row.names(ss1)<-c(str_c("KM",01:22),str_c("RE",01:27),str_c("CA",1:27))
ss1<-as.data.frame(ss1)

## NA Analysis
is.na(ss1) %>% which
(is.na(ss1) %>% which) %% nrow(ss1)
ss1[c(43,59),]

## Temporary Edit
ss2<-ss1
ss2["CA10","T6c"]<-2

## Elementary Analysis
factanal(ss2[,1:32],factors=6,methods="mle")
factanal(ss2[,1:32],factors=5,methods="mle")
factanal(ss2[,1:32],factors=4,methods="mle")
factanal(ss2[,1:32],factors=3,methods="mle")
factanal(ss2[,1:32],factors=2,methods="mle")
factanal(ss2[,1:32],factors=1,methods="mle")

for(i in c(1:5,"6a","6b","6c")) cat("T",i,"+",sep="")
for(i in c(1:5,"6a","6b","6c")) cat("EM",i,"+",sep="")
for(i in c(1:5,"6a","6b","6c")) cat("I",i,"+",sep="")
for(i in c(1:5,"6a","6b","6c")) cat("S",i,"+",sep="")

prcomp(ss1)

ss3<-ss2 %>%
  mutate(sum1=(T1+T2+T3+T4+T5+T6a+T6b+T6c-8),
         sum2=(EM1+EM2+EM3+EM4+EM5+EM6a+EM6b+EM6c-8),
         sum3=(I1+I2+I3+I4+I5+I6a+I6b+I6c-8),
         sum4=(S1+S2+S3+S4+S5+S6a+S6b+S6c-8)) %>%
  select(sum1:sum4)

apply(ss3,2,range)
ss2indeppar<-apply(ss3,2,mean)

ss2poissfit<-vector("list",length=4)
for(i in 1:4) ss2poissfit[[i]]<-data.frame(x=0:20,y=nrow(ss3)*dpois(0:20,ss2indeppar[i]))

ggplot()+
  geom_bar(aes(sum1),ss3)+
  geom_point(aes(x,y),ss2poissfit[[1]])

ggplot()+
  geom_bar(aes(sum2),ss3)+
  geom_point(aes(x,y),ss2poissfit[[2]])

ggplot()+
  geom_bar(aes(sum3),ss3)+
  geom_point(aes(x,y),ss2poissfit[[3]])

ggplot()+
  geom_bar(aes(sum4),ss3%>%filter(sum4!=0))+
  geom_point(aes(x,y),ss2poissfit[[4]])

library(MASS)
ss2nbfit<-vector("list",length=4)
for(i in 1:4) ss2nbfit[[i]]<-glm.nb(ss3[,i]~1)

ss2nbpar<-vector("list",length=4)
for(i in 1:4) ss2nbpar[[i]]<-c(exp((ss2nbfit[[i]]$coefficients)[[1]]),ss2nbfit[[i]]$theta)
for(i in 1:4) ss2nbpar[[i]]<-c(1-(1+prod(ss2nbpar[[i]]))^-1,ss2nbpar[[i]][1]^2/ss2nbpar[[i]][2]/(1-(1+prod(ss2nbpar[[i]]))^-1))

ss2nbpop<-vector("list",length=4)
for(i in 1:4) ss2nbpop[[i]]<-data.frame(x=0:20,y=nrow(ss3)*dnbinom(0:20,round(ss2nbpar[[i]][2]),ss2nbpar[[i]][1]))

ggplot()+
  geom_bar(aes(sum1),ss3)+
  geom_point(aes(x,y),ss2nbpop[[1]])

ggplot()+
  geom_bar(aes(sum2),ss3)+
  geom_point(aes(x,y),ss2nbpop[[2]])

ggplot()+
  geom_bar(aes(sum3),ss3)+
  geom_point(aes(x,y),ss2nbpop[[3]])

ggplot()+
  geom_bar(aes(sum4),ss3)+
  geom_point(aes(x,y),ss2nbpop[[4]])

## Pearson Chisquare Test with Contingency table by Classification criteria
ss4 <- ss3 %>%
  transmute(freq1=ifelse(sum1>=8,"Y1","N1"),freq2=ifelse(sum2>=9,"Y2","N2"),
            freq3=ifelse(sum3>=10,"Y3","N3"),freq4=ifelse(sum4>=12,"Y4","N4")) %>%
  count(freq1,freq2,freq3,freq4) %>%
  rbind(tibble(freq1=rep("Y1",5),freq2=c(rep("Y2",3),rep("N2",2)),
                        freq3=c("Y3",rep("N3",2),"Y3","N3"),freq4=c("N4","Y4","N4",rep("Y4",2)),
                        n=0)) %>%
  arrange(desc(freq1),desc(freq2),desc(freq3),desc(freq4))

ss5<- ss4 %>%
  group_by(freq1) %>%
  mutate(prop1=sum(n)/76) %>%
  group_by(freq2) %>%
  mutate(prop2=sum(n)/76) %>%
  group_by(freq3) %>%
  mutate(prop3=sum(n)/76) %>%
  group_by(freq4) %>%
  mutate(prop4=sum(n)/76) %>%
  mutate(proppred=prop1*prop2*prop3*prop4,countpred=76*proppred)

with(ss5,sum((n-countpred)^2/countpred)) %>% pchisq(df=16-4,lower.tail=F)

## Analysis of Multicollinearity by VIF

summary(lm(sum1~.,ss3))
summary(lm(sum2~.,ss3))
summary(lm(sum3~.,ss3))
summary(lm(sum4~.,ss3))

# sum2-sum3 might be correlated

## Pearson Chisq Test on Marginal Table 1-4
ss6<-ss4 %>%
  dplyr::select(freq1,freq4,n) %>%
  group_by(freq1,freq4) %>%
  summarize(n=sum(n))

ss6 %>%
  group_by(freq1) %>%
  mutate(prop1=sum(n)/76) %>%
  group_by(freq4) %>%
  mutate(prop4=sum(n)/76) %>%
  mutate(numpred=prop1*prop4*76) %>%
  with(sum((n-numpred)^2/numpred))

## Pearson Chisq Test on Marginal Table 1-3
ss7<-ss4 %>%
  dplyr::select(freq1,freq3,n) %>%
  group_by(freq1,freq3) %>%
  summarize(n=sum(n))

ss7 %>%
  group_by(freq1) %>%
  mutate(prop1=sum(n)/76) %>%
  group_by(freq3) %>%
  mutate(prop3=sum(n)/76) %>%
  mutate(numpred=prop1*prop3*76) %>%
  with(sum((n-numpred)^2/numpred))

## Pearson Chisq Test on Marginal Table 1-2
ss8<-ss4 %>%
  dplyr::select(freq1,freq2,n) %>%
  group_by(freq1,freq2) %>%
  summarize(n=sum(n))

ss8 %>%
  group_by(freq1) %>%
  mutate(prop1=sum(n)/76) %>%
  group_by(freq2) %>%
  mutate(prop2=sum(n)/76) %>%
  mutate(numpred=prop1*prop2*76) %>%
  with(sum((n-numpred)^2/numpred))

# sum1 is highly low skewed

## Factor Analysis on single methods
fass<-vector("list",length=4)

datfass<-ss1
datfass["CA10","T6c"]<-2

for(i in 1:4) fass[[i]]<-datfass[,(8*i-7):(8*i)]
for(i in 1:4) factanal(fass[[i]][,-1],factors=3) %>% print

## Linear Regression inside single method

lm(T1~.,fass[[1]]) %>% summary
lm(EM1~.,fass[[2]]) %>% summary
lm(I1~.,fass[[3]]) %>% summary
lm(S1~.,fass[[4]]) %>% summary

## Linear Regression inside single method with summary score
lmss<-vector("list",length=4)
for(i in 1:4) lmss[[i]]<- fass[[i]] %>% `colnames<-`(c("OV","M1","M2","X1","X2","C1","C2","C3")) %>%
  mutate(M=M1+M2,X=X1+X2,C=C1+C2+C3) %>% dplyr::select(OV,M:C) %>% lm(OV~.,data=.)
