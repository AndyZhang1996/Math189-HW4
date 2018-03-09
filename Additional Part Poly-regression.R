#linear regression/poly regression
#wenyin ye math 189 project 4

library(readr)
library(chemCal)
data <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/MATH 189 Prj4/Full Resolution Data/64506420.csv")
pressure=data$BP
tem=data$Ts
lat=data$Lat
lon=data$Lon

# make a plot 
plot(lat, tem, main="Polynomial Regression", xlab="latitude", ylab="temperature",las=1)

# now, let's fit a linear regression
model1 <- lm(tem ~ lat)
summary(model1)
# and add the line to the plot...make it thick and red...
abline(model1, lwd=3, col="red")


model2 <- lm(tem ~ poly(lat, degree=2, raw=T))
summary(model2)

# now, let's add this model to the plot, using a thick blue line
lines(smooth.spline(lat, predict(model2)), col="blue", lwd=3)

# test if the model including x^2 i signif. better than one without
# using the partial F-test
anova(model1, model2)

# try fitting a model that includes x^3 as well
model3 <- lm(tem ~ poly(lat, degree=3, raw=T))
summary(model3)

# now, let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(lat, predict(model3)), col="green", lwd=3, lty=3)

model4 <- lm(tem ~ poly(lat, degree=4, raw=T))
summary(model4)

# now, let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(lat, predict(model4)), col="pink", lwd=4, lty=3)
anova(model3, model4)

model5 <- lm(tem ~ poly(lat, degree=5, raw=T))
summary(model5)

# now, let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(lat, predict(model5)), col="purple", lwd=4, lty=3)
anova(model4, model5)

# and, let's add a legend to clarify the lines
legend(46, 15, legend = c("model1: linear", "model2: poly x^2", "model3: poly x^2 + x^3"), 
       col=c("red", "blue", "green"), lty=c(1,1,3), lwd=3, bty="n", cex=0.9)
