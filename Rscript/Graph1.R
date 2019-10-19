library(tidyverse)

graphstring<-read.table("~/sf_VirtualBoxShare/ASSIGN~1/data_2000.txt")
colnames(graphstring)<-c("start","end")

graphstring %>%
  filter(start %in% 4:6)

read.table("~/sf_VirtualBoxShare/ASSIGN~1/SCC_output.txt")

isol<-c(1953,1942,1912,1892,1873,1801,1790,1754,1613,1579,1565,1537,1536,1525,1505,1375,1374,1315,1313,1322,1303,1237,1218,1216,1209,1142,1083,964,944,935,1161,1073,1852,933,904,867,748,738,705,683,601,584,484,480,450,446,408,406,373,359,353,201,62,618,60,50,23)

graphstring %>%
  filter(end %in% isol)

graphstring %>%
  filter(start %in% isol)

availnode <- function(n){
availableold<-available<-vector("logical",length=2000)
available[filter(graphstring,start==n)$end]<-T
while(any(availableold!=available)){
  availableold<-available
    available[filter(graphstring,start %in% which(available))$end]<-T
}
return(which(available))
}

endnode <- map(1:2000,availnode)

counts<-0
for(i in 1:2000){
  if(!(i %in% endnode[[i]])) {cat(i,"\n"); counts <- counts + 1 }
}

{
  outgoingvert<-map_int(1:2000,~nrow(filter(graphstring,start==.)))
  dampingnum<-0.85
  pagerankold<-pagerank<-rep(1/2000,2000)
  
  while(sum(abs(pagerank-pagerankold))>0.0000001){
    pagerankold<-pagerank
    scoring<-pagerank/outgoingvert
    sumincoming<-map_dbl(1:2000,~sum(scoring[filter(graphstring,end==.)$start]))
    pagerank<-(1-dampingnum)/2000+dampingnum*sumincoming
  }
}
