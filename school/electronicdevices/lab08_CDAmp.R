library(tidyverse)

MOS_current <- function(Vgs,Vds,mu,Vth){
  mu*ifelse(Vgs<Vth,0,ifelse(Vgs-Vth>Vds,(Vgs-Vth)*Vds-0.5*Vds^2,0.5*(Vgs-Vth)^2))
}

Get_CD_Out <- function(Vin,mu,Vth,VDD,RL) {
	Vdrop <- RL*(Vin-Vth)
	VoltFactor <- (Vdrop + 1/mu)
	Voltsq <- VoltFactor^2-Vdrop^2
	Voltsq <- ifelse(Voltsq>0,Voltsq,0)
	Vout <- (VoltFactor - sqrt(Voltsq)) / RL
	ifelse(Vin<Vth,0,Vout * 1e3) # Out in [mV]
}

VDD <- 4.9907
R_real <- 986.75
mu_prev <- 0.002918077
Vth_prev <- 1.521161086

CD_Amp <- tribble(
~Vin, ~Vout,
0.39219, 0.002,
0.79288, 0.002,
0.99794, 0.007,
1.09562, 0.028,
1.19327, 0.145,
1.3009,	0.703,
1.39856, 2.866,
1.49628, 9.083,
1.59371, 21.323,
1.79881, 63.364,
1.99441, 116.852,
2.1986, 181.189,
2.3945, 248.15,
2.5998, 323.05,
2.795, 397.9,
3.0002, 479.66,
3.1958, 560.4,
3.4009, 647.61,
3.596, 732.73,
3.8013, 824.32,
3.9967, 913.34,
4.2015, 1008.46,
4.3971, 1100.84,
4.5925, 1194.57,
4.7975, 1294.36,
4.9931, 1390.85
)

ggplot(mapping=aes(Vin,Vout),data=CD_Amp)+
  geom_point(color="blue")+
  geom_path(color="blue")+
  #stat_function(fun = function(value) { Get_CD_Out(value,mu_prev,Vth_prev,VDD,R_real) },color="blue")+
  labs(x="Gate Input Voltage[V]",y="Source Output Voltage[V]",title=element_text("Source Follower In-Out Characteristic"))

CD_Amp_fit <- CD_Amp[-(1:5),]
fitting_obj <- optim(c(3e-3,1.5),function(param) {sum(Get_CD_Out(CD_Amp_fit$Vin,param[[1]],param[[2]],VDD,R_real)-CD_Amp_fit$Vout)^2})


print(fitting_obj$par)

mu <- fitting_obj$par[[1]]
Vth <- fitting_obj$par[[2]]

ggplot(mapping=aes(Vin,Vout),data=CD_Amp)+
  geom_point(color="blue")+
  geom_path(color="blue")+
  stat_function(fun = function(value) { Get_CD_Out(value,mu,Vth,VDD,R_real) },color="green")+
  labs(x="Gate Input Voltage[V]",y="Source Output Voltage[V]",title=element_text("Common Source Amplifier In-Out Characteristic"))