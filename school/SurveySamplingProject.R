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

ss2<-ss1 %>%
  mutate(sum1=(T1+T2+T3+T4+T5+T6a+T6b+T6c),
         sum2=(EM1+EM2+EM3+EM4+EM5+EM6a+EM6b+EM6c),
         sum3=I1+I2+I3+I4+I5+I6a+I6b+I6c,
         sum4=S1+S2+S3+S4+S5+S6a+S6b+S6c)
pr(ss1)

