library(tidyverse)

basedat <- read_csv("electronic_week4_base.csv",col_names = F)
emmdat <- read_csv("electronic_week4_emitter.csv",col_names = F)

basetidy <- basedat %>%
  filter(!is.na(X1)) %>%
  mutate(emcurr=X1,cvolt = ifelse(X1==1,X8,X7),ccurr = ifelse(X1==1,X9,X8)) %>%
  select(emcurr,cvolt,ccurr) %>%
  arrange(emcurr,cvolt)

ggplot(mapping=aes(cvolt,ccurr))+
  geom_point(data=filter(basetidy,emcurr==1),color="blue")+
  geom_path(data=filter(basetidy,emcurr==1),color="blue")+
  geom_point(data=filter(basetidy,emcurr==2),color="orange")+
  geom_path(data=filter(basetidy,emcurr==2),color="orange")+
  geom_point(data=filter(basetidy,emcurr==3),color="green")+
  geom_path(data=filter(basetidy,emcurr==3),color="green")+
  labs(x="Collector Voltate[V]",y="Collector Current[mA]",title=element_text("공통 베이스 특성곡선"))


emmtidy <- emmdat %>%
  filter(!is.na(X1)) %>%
  mutate(bacurr=X1,cvolt = X7,ccurr = X8) %>%
  select(bacurr,cvolt,ccurr) %>%
  filter(!is.na(cvolt)) %>%
  arrange(bacurr,cvolt)

emmtidy <- emmtidy[-59,]

ggplot(mapping=aes(cvolt,ccurr))+
  geom_point(data=filter(emmtidy,bacurr==10),color="blue")+
  geom_path(data=filter(emmtidy,bacurr==10),color="blue")+
  geom_point(data=filter(emmtidy,bacurr==20),color="orange")+
  geom_path(data=filter(emmtidy,bacurr==20),color="orange")+
  geom_point(data=filter(emmtidy,bacurr==30),color="green")+
  geom_path(data=filter(emmtidy,bacurr==30),color="green")+
  labs(x="Collector Voltate[V]",y="Collector Current[mA]",title=element_text("공통 이미터 특성곡선"))
