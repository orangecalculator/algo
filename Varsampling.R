
X<-1:1000

var(X)*999/1000
Y<-NULL
for(i in 1:500)
Y[i]<-var(sample(X,100,replace=T))

hist(Y)
mean(Y)
