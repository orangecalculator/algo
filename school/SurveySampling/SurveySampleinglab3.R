##install.packages(c("psy", "psych"))
library(psy)
library(psych)

##Example of Calculating Cronbach Alpha
data(expsy)
dim(expsy)
cronbach(expsy[,1:10])
alpha(expsy[,1:10])
##not good because item 2 is reversed
##(1 is high and 4 is low)
cronbach(cbind(expsy[,c(1,3:10)],-1*expsy[,2]))
alpha(cbind(expsy[,c(1,3:10)],-1*expsy[,2]))
##better

##Example of Normal Test
xbar <- 294.4
mu0 <- 300
sigma <- 10
n <- 16
z <- (xbar - mu0)/(sigma/sqrt(n))
pnorm(z) # p-value

##Example of T Test
x <- c(22, 25, 34, 35, 41, 41, 46, 46, 46, 47, 49, 54, 54, 59, 60)
xbar <- mean(x)
S <- sd(x)
mu0 <- 40
n <- 15
t <- (xbar - mu0)/(S/sqrt(n))
1 - pt(t, df=n-1) # p-value

t.test(x, mu=40, alternative = "greater")

weight <- c(408, 405, 397, 405, 395, 415, 389, 403, 297, 390)
t.test(weight, mu=400, alternative = "two.sided")

##Example of Taired Test
x <- c(3.1, 3.7, 4.0, 3.2, 3.6, 3.5, 4.2, 3.8, 3.7, 3.4, 3.6,3.8,3.4,3.4)
y <- c(2.2, 2.7, 3.1, 2.9, 3.3, 2.6, 2.9, 2.8, 3.2, 2.5, 3.5, 3.1,2.3,3.5)
d <- x - y
t.test(x, y, paired = TRUE)

##Example of Test on Two Tamples
old <- c(49, 44, 47, 44, 46, 40, 48, 45, 45, 42)
new <- c(44, 41, 45, 44, 43, 39, 42, 40, 40, 42)
var.test(old, new)
t.test(old, new, var.equal = TRUE, alternative = "greater")

##Example of test on Analysis of Variance
num <- c(45, 59, 48, 46, 37, 47, 21, 12, 14, 17, 13, 17, 37, 32, 15,25,39,41,16,11,20,21,14,7)
color <- factor(rep(1:4, rep(6, 4)))
bugs <- data.frame(number=num, color=color)
fit <- lm(number ~ color, data=bugs)
         anova(fit)

##Example of Linear Regression Analysis
library(MASS)
data(Boston)
names(Boston)

lmfit <- lm(medv ~ lstat, data=Boston)
lmfit
summary(lmfit)

coef(lmfit)
confint(lmfit)
predict(lmfit, data.frame(lstat=c(5,10,15)), interval = "confidence")

plot(Boston$lstat, Boston$medv)
abline(lmfit, col=red, lwd=2)

lmfit <- lm(medv~lstat+age, data=Boston)
summary(lmfit)

lmfit <- lm(medv~.-age, data=Boston)
summary(lmfit)

lmfit <- lm(medv~.-age-indus,Boston)
summary(lmfit)

##Example of Factor Analysis
##install.packages("GPArotation")
library(GPArotation)

#Loading the dataset
bfi_data=bfi
#Remove rows with missing values and keep only complete cases
bfi_data=bfi_data[complete.cases(bfi_data),]
#Create the correlation matrix from bfi_data
bfi_cor <- cor(bfi_data)
#Factor analysis of the data
factors_data <- fa(r = bfi_cor, nfactors = 6)
#Getting the factor loadings and model analysis
factors_data
