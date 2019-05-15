library(tidyverse)

MOS_current <- function(Vgs,Vds,mu,Vth){
  mu*ifelse(Vgs<Vth,0,ifelse(Vgs-Vth>Vds,(Vgs-Vth)*Vds-0.5*Vds^2,0.5*(Vgs-Vth)^2))
}

Get_CS_Out <- function(Vin,mu,Vth,VDD,RD) {
	V_sat <- VDD - RD * mu / 2 * (Vin - Vth)^2
	#V_tri <- Vin - Vth + 1/(mu*RD) - sqrt((Vin - Vth + 1/(mu*RD))^2 - 2 * VDD / (mu*RD))
	ifelse(Vin<=Vth,VDD,
	ifelse(V_sat>(Vin-Vth),V_sat,Vin - Vth + 1/(mu*RD) - sqrt((Vin - Vth + 1/(mu*RD))^2 - 2 * VDD / (mu*RD))))
}

CS_Amp <- tribble(
~Vin, ~Vout,
0.0018,4.9925,
0.49991,4.9925,
0.99813,4.9925,
1.19353,4.9923,
1.39873,4.989,
1.59394,4.9563,
1.79904,4.8296,
1.994,4.5985,
2.1995,4.2507,
2.3948,3.8349,
2.6,3.3279,
2.7952,2.7957,
3.0004,2.2026,
3.1959,1.63119,
3.4011,1.09836,
3.5963,0.81725,
3.8015,0.6827,
3.9969,0.60564,
4.2017,0.54874,
4.3973,0.5074,
4.5927,0.47425,
4.7977,0.44554,
4.9932,0.4224
)

CS_Amp

ggplot(mapping=aes(Vin,Vout),data=CS_Amp)+
  geom_point(color="blue")+
  geom_path(color="blue")+
  #geom_smooth(data=filter(NMOS_Gate,nomVds==3,Vgs<6,Vgs>4),method="lm",se=F)+
  stat_function(fun = function(value) { Get_CS_Out(value,2673.2e-6,2.4,5,988.62) },color="blue")+
  labs(x="Input Voltage[V]",y="Output Voltage[V]",title=element_text("Common Source Amplifier In-Out Ư???"))

#for(value in seq(1,5,0.1)) print(Get_CS_Out(value,2673.2e-6,2.4,5,988.62))

fitting_obj <- optim(c(3e-3,1.5),function(param) {sum(Get_CS_Out(CS_Amp$Vin,param[[1]],param[[2]],5,988.62)-CS_Amp$Vout)^2})

print(fitting_obj$par)

mu <- fitting_obj$par[[1]]
Vth <- fitting_obj$par[[2]]

ggplot(mapping=aes(Vin,Vout),data=CS_Amp)+
  geom_point(color="blue")+
  geom_path(color="blue")+
  #geom_smooth(data=filter(NMOS_Gate,nomVds==3,Vgs<6,Vgs>4),method="lm",se=F)+
  stat_function(fun = function(value) { Get_CS_Out(value,mu,Vth,5,988.62) },color="green")+
  labs(x="Input Voltage[V]",y="Output Voltage[V]",title=element_text("Common Source Amplifier In-Out 특성곡선"))
