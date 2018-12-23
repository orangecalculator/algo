AA<-matrix(c(3,-1,-1,1,0,-1,-2,5,4),3)
(AA-diag(c(3,3,3)))%*%c(-1,2,1)
det(matrix(c(-1,3,1,0,1,1,-1,2,1),3))
matrix(c(-1,3,1,0,1,1,-1,2,1),3) %*%
  matrix(c(2,0,0,1,2,0,0,0,3),3) %*%
  solve(matrix(c(-1,3,1,0,1,1,-1,2,1),3))


M<-matrix(0,ncol=2000,nrow=2000)

graph<-read.table("data_2000.txt")
colnames(graph)<-c("start","end")
graph$start[graph$start==0]<-2000
graph$end[graph$end==0]<-2000
dampher<-0.85

for(i in 1:nrow(graph)) {M[graph[i,2],graph[i,1]]<-1}
standardizer<-apply(M,2,sum)
standardizer<-ifelse(standardizer>0,standardizer^-1,0)
M<-dampher*M*rep(standardizer,each=2000)

Meig<-eigen(M,symmetric = F)

MQ<-M$vectors

Mit<-M
for(i in 1:10) Mit<-Mit%*%Mit

nearM<-vector("logical",length=1999)
which((Mit[,-2000]-Mit[,-1])>1e-8)

PRscore<-solve(diag(rep(1,2000))-M,(1-dampher)/2000*rep(1,2000))
PRscore[PRscore>0.001]
write.csv(PRscore,file="PRscore.txt")
write.csv((1-dampher)/2000*solve(diag(rep(1,2000))-M)%*%rep(1,2000),
          "PRscore2.txt")
which(((solve(diag(rep(1,2000))-M) %*% (diag(rep(1,2000))-M))-diag(rep(1,2000)))>1e-6)
