library(tidyverse)

MOS_current <- function(Vgs,Vds,mu,Vth){
  mu*ifelse(Vgs<Vth,0,ifelse(Vgs-Vth>Vds,(Vgs-Vth)*Vds-0.5*Vds^2,0.5*(Vgs-Vth)^2))
}

NMOS_Gate <- read_csv("electronics_week5_NMOS01.csv",col_names=T)
NMOS_Gate <- NMOS_Gate %>%
  select(Vgs,Vds,Id) %>%
  filter(!is.na(Vgs)) %>%
  mutate(nomVds=1)

NMOS_Gate$nomVds<-c(rep(1,15),rep(2,18),rep(3,26))

NMOS_Gate %>% print(n=Inf)

NMOSparam01 <- optim(c(1000,1),function(x){
  sum((MOS_current(NMOS_Gate$Vgs,NMOS_Gate$Vds,x[[1]],x[[2]]) - NMOS_Gate$Id)^2)
})

draw_function <- function(nomin){
  Vds_interpol <- filter(NMOS_Gate,nomVds==nomin)
  Vds_interpol_fun <- approxfun(Vds_interpol$Vgs,Vds_interpol$Vds)
  return(function(val) MOS_current(val,Vds_interpol_fun(val),NMOSparam01$par[[1]],NMOSparam01$par[[2]]))
}

ggplot(mapping=aes(Vgs,Id))+
  geom_point(data=filter(NMOS_Gate,nomVds==1),color="blue")+
  geom_path(data=filter(NMOS_Gate,nomVds==1),color="blue")+
  #geom_smooth(data=filter(NMOS_Gate,nomVds==3,Vgs<6,Vgs>4),method="lm",se=F)+
  #stat_function(data=filter(NMOS_Gate,nomVds==1),
  #              fun = draw_function(1),color="blue")+
  geom_point(data=filter(NMOS_Gate,nomVds==2),color="orange")+
  geom_path(data=filter(NMOS_Gate,nomVds==2),color="orange")+
  #stat_function(data=filter(NMOS_Gate,nomVds==2),
  #              fun = draw_function(2),color="orange")+
  geom_point(data=filter(NMOS_Gate,nomVds==3),color="green")+
  geom_path(data=filter(NMOS_Gate,nomVds==3),color="green")+
  #stat_function(data=filter(NMOS_Gate,nomVds==3),
  #              fun = draw_function(3),color="green")+
  labs(x="Gate Voltage[V]",y="Drain Current[uA]",title=element_text("Vgs-Id 특성곡선"))

filter(NMOS_Gate,nomVds==3,Vgs<6,Vgs>4) %>% lm(Id~Vgs,data=.)

MOS_current_modul <- function(Vgs,Vds,mu,Vth,lambda){
  mu*ifelse(Vgs<Vth,0,ifelse(Vgs-Vth>Vds,(Vgs-Vth)*Vds-0.5*Vds^2,0.5*(Vgs-Vth)^2*(1+lambda*Vds)))
}

NMOSparam <- optim(c(1),function(x){
  sum((MOS_current(NMOS_Drain$Vgs,NMOS_Drain$Vds,NMOS_param$par[[1]],NMOS_param$par[[2]],x) - NMOS_Drain$Id)^2)
})

NMOS_Drain <- read_csv("electronics_week5_NMOS02.csv",col_names=T)
NMOS_Drain <- NMOS_Drain %>%
  filter(!is.na(Vgs)) %>%
  mutate(nomVds=1)

NMOS_Drain$nomVds<-c(rep(1,19),rep(2,18))

NMOS_Drain %>% print(n=Inf)

ggplot(mapping=aes(Vds,Id))+
  geom_point(data=filter(NMOS_Drain,nomVds==1),color="blue")+
  geom_path(data=filter(NMOS_Drain,nomVds==1),color="blue")+
  geom_point(data=filter(NMOS_Drain,nomVds==2),color="orange")+
  geom_path(data=filter(NMOS_Drain,nomVds==2),color="orange")+
  labs(x="Drain Voltage[V]",y="Drain Current[uA]",title=element_text("Vds-Id 특성곡선"))

NMOS_lamb01 <- filter(NMOS_Drain,Vgs==3.5,Vds>.5)
NMOS_lamb01$Id <- NMOS_lamb01$Id / NMOS_lamb01$Id[[1]]
lm(Id~Vds,data=NMOS_lamb01)
NMOS_lamb02 <- filter(NMOS_Drain,Vgs==5,Vds>1)
NMOS_lamb02$Id <- NMOS_lamb02$Id / NMOS_lamb02$Id[[1]]
lm(Id~Vds,data=NMOS_lamb02)

lm(Id~Vds,data=filter(NMOS_Drain,Vgs==3.5,Vds>.5))
lm(Id~Vds,data=filter(NMOS_Drain,Vgs==5,Vds>1))
lm(Id~Vds,data=filter(NMOS_Drain,Vgs==3.5,Vds<.2))

lm(Id~Vds,data=filter(NMOS_Drain,Vgs==5,Vds<.18))

NMOS_Back <- read_csv("electronics_week5_NMOS03.csv",col_names=T)

NMOS_Back <- NMOS_Back %>%
  filter(!is.na(Vgs)) %>%
  mutate(nomVgs=1)

NMOS_Back$nomVgs <- c(rep(1,27),rep(2,23),rep(3,23))

NMOS_Back %>% print(n=Inf)

NMOS_Back <- list(filter(NMOS_Back,nomVgs==1),
                  filter(NMOS_Back,nomVgs==2),
                  filter(NMOS_Back,nomVgs==3))

ggplot(mapping=aes(Vgs,Id))+
  geom_point(data=NMOS_Back[[1]],color="blue")+
  geom_path(data=NMOS_Back[[1]],color="blue")+
  geom_point(data=NMOS_Back[[2]],color="orange")+
  geom_path(data=NMOS_Back[[2]],color="orange")+
  geom_point(data=NMOS_Back[[3]],color="green")+
  geom_path(data=NMOS_Back[[3]],color="green")+
  labs(x="Gate Voltage[V]",y="Drain Current[uA]",title=element_text("Vgs-Id 특성곡선 (기판 전압 변화)"))
