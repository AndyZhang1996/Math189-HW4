data <- read.table("gauge.txt", header=TRUE)

#histograms
hist(data$gain, xlab="gain", main="Histogram of gain")
hist(data$density, xlab="density", main="Histogram of density")


#boxplot
boxplot(gain~density, data, xlab="density", ylab="gain", main="Boxplot of gain and density")
boxplot(data$gain)
boxplot(data$density)


#draw least sqauares line without log transformation
fit1 <- lm(gain~density, data)
fit1
plot(data)
abline(fit1, col="red")
summary(fit1)


#draw least squares line without deleting outliers but with log transformation
fit1 <- lm(log(gain)~density, data)
plot(data[,1],log(data[,2]),xlab="density",ylab="gain")
abline(fit1, col="red")
summary(fit1)
 

#check the conditions for the least squares line       
#linearity
plot(fit1$residuals, main="residual plot")
abline(0, 0, col="red")

#check the residual normality                        
hist(fit1$residuals, main="Histogram of residuals")
qqnorm(fit1$residuals)
qqline(fit1$residuals, col="red")

#check normality of residuals 
ks.test(fit1$residuals, pnorm)
shapiro.test(fit1$residual)

#quantile regression with tau=0.25 without deleting outliers
install.packages("quantreg")
library(quantreg)

fit2 <- rq(log(data$gain)~data$density, tau=0.25)
plot(data[,1],log(data[,2]),xlab="density",ylab="gain")
abline(fit2, col='red')
summary(fit2)

plot(fit2$residuals)
abline(0, 0, col="red")


#quantile regression with tau=0.5 without deleting outliers
fit2 <- rq(log(data$gain)~data$density, tau=0.5)
plot(data[,1],log(data[,2]),xlab="density",ylab="gain")
abline(fit2, col='red')
summary(fit2)

plot(fit2$residuals)
abline(0, 0, col="red")


#quantile regression with tau=0.75 without deleting outliers
fit2 <- rq(log(data$gain)~data$density, tau=0.75)
plot(data[,1],log(data[,2]),xlab="density",ylab="gain")
abline(fit2, col='red')
summary(fit2)

plot(fit2$residuals)
abline(0, 0, col="red")








