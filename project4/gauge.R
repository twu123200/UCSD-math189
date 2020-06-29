data <- read.table("gauge.txt", header = TRUE)
data <- data[order(data$density), ]
m = 10
n = 9
library(moments)

########################################################
#Scenario 1: Fitting
plot(data,
     main = "Density vs. gain",
     xlab = "Density (g/cm3)",
     ylab = "Gain")

data.log <- data.frame(data['density'], log(data['gain']))
fit <- lm(formula=gain~density, data=data.log)
plot(data.log,
     main = "Density vs. log gain",
     xlab = "Density (g/cm3)",
     ylab = "log(Gain)")
abline(fit, col = "red")
cor(data.log)

data.avg_log <- aggregate(list(avg_log_gain=log(data$gain)), by= list(density=data$density), FUN = mean)
plot(data.avg_log,
     main = "Density vs. Average log gain",
     xlab = "Density (g/cm3)",
     ylab = "log(Gain)")
abline(fit, col = "red")

plot(fit$residuals,
     main = "Residuals of Least Squares line",
     ylab = "Gain")
abline(0, 0, col="red")

hist(fit$residuals,
     main = "Histogram of Residuals of Least Squares line",
     xlab = "Gain",
     col = "cornflowerblue")
skewness(fit$residuals)
kurtosis(fit$residuals)

qqnorm(fit$residuals,
       main = "Q-Q plot of Residuals of Least Squares line")
qqline(fit$residuals, col = "red")
########################################################

data <- BuoyData2018

fit <- lm(formula = BP~Ts, data = data)

plot (data$Ts, data$BP, 
      xlab = "Temperature",
      ylab = "Pressure",
      main = "Pressure vs Temperature", col="blue")
abline(fit, col="red")

# Calculating correlation between the temperature and the pressure
cor(data$BP, data$Ts)

plot(fit$residuals,
     main = "Residuals of Least Squares line",
     ylab = "Pressure")
abline(0, 0, col="red")

hist(fit$residuals,
     main = "Histogram of Residuals of Least Squares line",
     xlab = "Gain",
     col = "cornflowerblue")

skewness(fit$residuals)
kurtosis(fit$residuals)

qqnorm(fit$residuals,
       main = "Q-Q plot of Residuals of Least Squares line")
qqline(fit$residuals, col = "red")


########################################################