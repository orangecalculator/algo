library(ggplot2)
x <- seq(0, 1, 0.001)
y1 <- qnorm(x)
y2 <- log(x/(1-x))
df <- data.frame(x,y1,y2)
ggplot(df, aes(x)) + # basic graphical object
  geom_line(aes(y=y1), colour="red") + # first layer
  geom_line(aes(y=y2), colour="green") # second layer

##Example of Logistic Regression
##Snoring and Heart Disease data
# library(ggplot2)
snoring <- data.frame(snore = c(0, 2, 4, 5), heart.d = c(24, 35, 21, 30)
                      , n = c(1379,638, 213, 254))
res_lse <- lsfit(x = snoring$snore, y = snoring$heart.d/snoring$n)
res_logit <- glm(heart.d/n ~ snore, weights = n, family = binomial(), data = snoring)
res_ident <- glm(heart.d/n ~ snore, weights = n, family = binomial(link = "identity"),
                 data = snoring)
res_probit <- glm(heart.d/n ~ snore, weights = n, family = binomial(link = "probit"),
                  data = snoring)
xPoints <- seq(0, 5,length = 100)
y0 <- coefficients(res_lse)[1] + coefficients(res_lse)[2] * xPoints
y1 <- (1+exp(-(coefficients(res_logit)[1] + 
            coefficients(res_logit)[2] * xPoints)))^-1
y2 <- coefficients(res_ident)[1] + coefficients(res_ident)[2] * seq(0, 5, length = 100)
y3 <- pnorm(coefficients(res_probit)[1] + coefficients(res_probit)[2] * xPoints)
df <- data.frame(xPoints,y0,y1,y2,y3)
ggplot(df, aes(xPoints)) +
  geom_line(aes(y=y0), colour="black") +
  geom_line(aes(y=y1), colour="red") +
  geom_line(aes(y=y2), colour="blue") +
  geom_line(aes(y=y3), colour="green") +
  ggtitle("Relationship between Snoring and Heart Disease") +
  xlab("Snoring") + ylab("Expected Probability")
summary(res_logit)

##Example of Poisson Regression
##Horseshoe Crab data
library(MASS)
crab <- read.csv("crab.csv")
res_poi_log <- glm(satell ~ width, family = poisson(link = "log"), data = crab)
res_poi_ident <- glm(satell ~ width, family = poisson(link = "identity"), start = c(2,2), data = crab)
res_negBin_log <- glm.nb(satell ~ width, data = crab)
width.gp <- cut(crab$width,
                breaks = c(0, 23.25, 24.25, 25.25, 26.25, 27.25,28.25, 29.25, Inf),
                right = FALSE)
y <- tapply(crab$satell, width.gp, mean)
x <- c(22.75, 23.75, 24.75, 25.75, 26.75, 27.75, 28.75, 30.25)
plot(x, y, xlim = c(22, 32), ylim = c(0, 5.5))
xPoints <- seq(22.75,32, length = 100)
y1 <- exp(coefficients(res_poi_log)[1] + coefficients(res_poi_log)[2] * xPoints)
y2 <- coefficients(res_poi_ident)[1] + coefficients(res_poi_ident)[2] * xPoints
y3 <- exp(coefficients(res_negBin_log)[1] + coefficients(res_negBin_log)[2] * xPoints)
df <- data.frame(xPoints,y1,y2)
ggplot(df, aes(xPoints)) +
  geom_line(aes(y=y1), colour="red") +
  geom_line(aes(y=y2), colour="blue") +
  geom_line(aes(y=y3), colour="green") +
  ggtitle("Number of Satellites by width of female carb") +
  xlab("Width") + ylab("Number of Satellites")
summary(res_poi_log)
summary(res_negBin_log)

##Overdispersion model
crab <- read.csv("crab.csv")
breaks <- c(0, 23.25, 24.25, 25.25, 26.25, 27.25,28.25, 29.25, Inf)
sampleMean <- c(); sampleVar <- c(); width <- c()
for(i in 1:(length(breaks)-1))
  f
satellHere <- crab$satell[which(crab$width>=breaks[i] & crab$width<breaks[i+1])]
width <- append(width,paste(breaks[i],"-",breaks[i+1],sep=""))
sampleMean <- append(sampleMean,mean(satellHere))
sampleVar <- append(sampleVar,var(satellHere))
g
data.frame("Width" = width, "Sample_Mean"=sampleMean,"Sample_Var"=sampleVar)

##Example of Rate Regression
train <- read.csv("train.csv")
train$YearOff <- train$Year - 1975
res.poi<-glm(Train.load.Collisions~YearOff,offset=log(Train.km),family=poisson(),data=train)
res.nb<-glm.nb(Train.load.Collisions~YearOff+offset(log(Train.km)),dat=train)

##Example of Testing Parameter Estimation
summary(res_ident)
#Confidence interval
coefficients(res_ident)[2]+0.002805*c(-1.96,1.96)
#Wald test
dnorm(abs(coefficients(res_ident)[2]/0.002805))
#Likelihood ratio test
M0 <- glm(heart.d/n ~ 1, weights = n, family = binomial(link = "identity"),
          data = snoring)
dchisq(2*(logLik(res_ident)-logLik(M0)),df=2)

##Example of Nonlinear Equation Iterative Solution Formula
##Newton-Rhapson Method
#Result vector
beta.res<-matrix(NA,nrow=1000,ncol=2)
for(simul in 1:1000)f
#Generate data
y<-NA
beta<-c(-1,1)
for(i in 1:500)f
xi_t<-matrix(c(1, rnorm(1,0,2^2)),1,2)
pi_i<-exp(xi_t%*%beta)/(1+exp(xi_t%*%beta))
y_i<-rbinom(1,1,pi_i)
y[i]<-y_i
if(i==1)f
X<-xi_t
gelsef
X<-rbind(X,xi_t)
g
g
#Set up parameters for algorithm
beta.seed<-c(0,0)
max.iter<-100#Maximum number of iteration
epsilon<-0.0000001#Criterion of convergence
#Newton-Raphson
for(r in 1:max.iter)f
if(r==1)f
beta.new<-beta.seed
gelsef
beta.now<-beta.new
mu<-mu<-exp(X%*%beta.now)/(1+exp(X%*%beta.now))
#Score function
U<-t(X)%*%(y-mu)
#Second derivative
W<-diag(as.vector(mu*(1-mu)))
#Update
beta.new<-beta.now+solve(t(X)%*%W%*%X)%*%U
#Check the convergence
if(sum(abs(solve(t(X)%*%W%*%X)%*%U))<epsilon)f
beta.hat<-beta.new
break;
g
if(r==max.iter)f
print("Does not converge.")
g
g
g
beta.res[simul,]<-beta.hat
g
#mean of beta hat
apply(beta.res,2,mean)
#variance of beta hat
apply(beta.res,2,var)
7
