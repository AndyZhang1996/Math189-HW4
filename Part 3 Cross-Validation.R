getwd()
setwd("D:/UCSD COURSE/Math 189 Winter 2018/Case Study 4")
getwd()

Data_G <- read.table("guage data.TXT", header=TRUE)

#install.packages("chemCal")
library(chemCal)

#***********Paritial Cross-Validation under omitting density 0.508************#

ind.Data1 <- which(Data_G['density']!=0.508)
Data1 <- Data_G[ind.Data1,]

# Generating least square line regression without data 0.508
fit1 <- lm(log(gain)~density,data= Data1)
plot(Data1[,1],log(Data1[,2]))
abline(fit1, col="red")
summary(fit1)

#Check residual's linearity
plot(fit1$residuals)
abline(0, 0, col="red")

#Check residual's Normality Without eliminating Outliers
hist(fit1$residuals)
qqnorm(fit1$residuals)
qqline(fit1$residuals, col="red")

shapiro.test(fit1$residuals)
# p-value = 0.08702, since p-value > 0.05, the H0 not been rejected
# so the residuals are normally distributed


#Generating Inverse Prediction base on data without 0.508

inverse.predict(fit1,log(38.6))
#Prediction: 0.5091927
#Confidence Interval:(0.5507529, 0.4676324)

#Calculate Mean Squared Error 
MSE1= (0.5091927-0.5080)^2
MSE1
# MSE1=1.422533e-06

#***********Partial Cross-Validation under omitting density 0.001************#

ind.Data2 <- which(Data_G['density']!=0.001)
Data2 <- Data_G[ind.Data2,]

# Generating least square line regression without data 0.001
fit2 <- lm(log(gain)~density,data= Data2)
plot(Data2[,1],log(Data2[,2]))
abline(fit2, col="red")
summary(fit2)

#Check residual's linearity
plot(fit2$residuals)
abline(0, 0, col="red")

#Check residual's Normality Without eliminating Outliers
hist(fit2$residuals)
qqnorm(fit2$residuals)
qqline(fit2$residuals, col="red")

shapiro.test(fit2$residuals)
# p-value = 0.06268, since p-value > 0.05, the H0 not been rejected
# so the residuals are normally distributed

#Generating Inverse Prediction base on data without 0.001
inverse.predict(fit2,log(426.7))
#Prediction: -0.02051733
#Confidence Interval: (0.02241270, -0.06344736)

#Calculate Mean Squared Error 
MSE2= (-0.02051733-0.0010)^2
MSE2
# MSE2=0.0004629955

#************Cross Validation For All************#
############# Method 1: Predict with log(mean value of gain) in testing set ############## 

#Cutting the new data into 9 folders, exactly follow the amount of given density in original dataset
k <- 9
folds <- cut(seq(1,nrow(Data_G)),breaks=k,labels=FALSE)

#Create an empty series numbers of mean squared error
mse.pre <- rep(NA,k)

for(i in 1:k){
  train <- Data_G[folds!=i,]
  test <- Data_G[folds==i,]
  mean.gain.test <- mean(test[["gain"]])
  mean.den.test <- mean(test[["density"]])
  
  fit <- lm(log(gain)~density,data= train)
  
  invpred <- inverse.predict(fit,log(mean.gain.test))
  
  err <- (invpred$Prediction-mean.den.test)^2  
  
  mse.pre[i] <- err
}

#take mean value for all the prepare mean squared error numbers, then 
#use exponential to raise it back to real gain value.
mse.pre
cross.mse <- mean(mse.pre)
cross.mse
#Cross validation error is 0.0002630318. Such that error can be ignored!


############### Method 2: Predict with each log(density value) in testing set ###############

#Cutting the new data into 9 folders, exactly follow the amount of given density in original dataset
k <- 9
folds <- cut(seq(1,nrow(Data_G)),breaks=k,labels=FALSE)
#Giving the length of each folder
l<- nrow(Data_G)/k

#Create an empty series numbers of mean squared error
mse.pre2 <- rep(NA,k)

#Double for loop for Cross Validation
for(i in 1:k){
  train <- Data_G[folds!=i,]
  test <- Data_G[folds==i,]
  fit <- lm(log(gain)~density,data= train)
  
  invpred <- rep(NA,l)
  
  #Creating iterating prediction for each density value in the testing set
  for(j in 1:l){
    
    invall <- inverse.predict(fit,log(test[j,2]))
    invpred[j] <- invall$Prediction
  }
  
  err <- (invpred-test["density"])^2
  mse.pre2[i] <- mean(err)
}


#take mean value for all the prepare mean squared error numbers, then 
#use exponential to raise it back to real gain value.
mse.pre2
cross.mse2 <- mean(mse.pre2)
cross.mse2

#Cross validation error is 0.0003153147. Such that using both method, the error all can be ignored!




#************Experiment Zone*************#

# Trying for part2 of the case study
#fit <- lm(log(gain)~density,data= Data_G)
#plot(Data_G[,1],log(Data_G[,2]))
#abline(fit, col="red")

#inverse.predict(fit,log(38.6))
#Prediction:0.5089113
#CI:0.5454864 0.4723361

#inverse.predict(fit,log(426.7))
#Prediction:-0.01276954
#CI: 0.02664126 -0.05218034









