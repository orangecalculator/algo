S<-matrix(c(1794,-4.473,726.9,-2218,-4.473,1.488,-26.62,197.5,726.9,-26.62,1224,-6203,-2218,197.5,-6203,48104),nrow=4)
# R<-diag(diag(S)^(-1/2))%*%S%*%diag(diag(S)^(-1/2))
R<-matrix(c(1,-0.087,0.491,-0.239,-0.087,1,-0.624,0.738,0.491,-0.624,1,-0.808,-0.239,0.738,-0.808,1),nrow=4)
xbar <- matrix(c(138.0,17.75,74.4,1242))

ER<-svd(R)
dS<-diag(diag(S))

# library(tidyverse)
# which(!near(ER$u,ER$v))
# u and v are same

## NPC coefficients
(diag(diag(dS)^(-1/2)) %*% ER$u -> NPCM)
## NPC offset
t(NPCM) %*% xbar
## NPC Explained Proportion of Variance
sum(ER$d[1:2])/4

## Correlation matrix between Original Variable and NPC
ER$u %*% diag(ER$d^(1/2))

## Proportion of Variance explained componentwise
(ER$u %*% diag(ER$d^(1/2)))^2 #%>% apply(.,1,sum)

## PC coefficients
ES<-svd(S)
ES$u
## PC offset
t(ES$u)%*%xbar

sum(ES$d[1:2])/sum(diag(S))
