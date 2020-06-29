library(ggplot2)
library(moments)

#read data
setwd("/Users/twu12/Desktop/school/math189/p1")
data <- read.table("babies23.txt", header=TRUE);

#get rid of no data of smoking
noSmokeData.ind <- which(data['smoke'] == 9)
data <- data[-noSmokeData.ind,]

#create column if smoked during pregnancy
smokedDuringPreg <- c()
for(i in 1:nrow(data)) {
  if(data[i,'smoke']== 1)
    smokedDuringPreg <- c(smokedDuringPreg, 1)
  else
    smokedDuringPreg <- c(smokedDuringPreg, 0)
}
data <- cbind(data, smokedDuringPreg)

nonsmokers.ind <- which(data['smoke'] == 0);
smokers.ind <- which(data['smoke'] == 1);

data[nonsmokers.ind,] -> data.nonsmokers
data[smokers.ind,] -> data.smokers

#summary statistics of smokers
summary(data.nonsmokers$wt)
summary(data.smokers$wt)
#sample standard deviations
sd(data.nonsmokers$wt)
sd(data.smokers$wt)
#skewness
skewness(data.nonsmokers$wt)
skewness(data.smokers$wt)
#kurtosis
kurtosis(data.nonsmokers$wt)
kurtosis(data.smokers$wt)

#box plot of smokers
boxplot(wt~smokedDuringPreg, data, 
        main = "Boxplot of Baby Birth Weights of Mothers With Different Smoking Status", 
        xlab = "Mother smoking status", 
        ylab = "Babies' Birth Weight", 
        names=c("Did not smoke during pregnancy","Smoked during pregnancy"))

#2500 grams is considered low birth weight, convert to ounces
lbwOZ = 0.035274 * 2500
#proportion of babies classified as low birth weight
nonsmokersProp <- length(which(data.nonsmokers['wt'] < lbwOZ)) / nrow(data.nonsmokers)
#proportion of babies classified as low birth weight
smokersProp <- length(which(data.smokers['wt'] < lbwOZ)) / nrow(data.smokers)
barplot(c(nonsmokersProp, smokersProp),
        main = "Proportion of Low Birth Weight Babies (88oz threshold)",
        xlab = "Smoking Status during pregnancy",
        ylab = "Proportion",
        names.arg = c("Did not smoke during pregnancy", "Smoked during pregnancy"),
        col = c("#696969", "cornflowerblue"))
#install.packages('ggplot2',repos="http://cran.rstudio.com/"))
#library(ggplot2)
#library(moments)

#read data
setwd("/Users/twu12/Desktop/school/math189/p1")
data <- read.table("babies23.txt", header=TRUE);

#get rid of no data of smoking
noSmokeData.ind <- which(data['smoke'] == 9)
data <- data[-noSmokeData.ind,]

#create column if smoked during pregnancy
smokedDuringPreg <- c()
for(i in 1:nrow(data)) {
  if(data[i,'smoke']== 1)
    smokedDuringPreg <- c(smokedDuringPreg, 1)
  else
    smokedDuringPreg <- c(smokedDuringPreg, 0)
}
data <- cbind(data, smokedDuringPreg)

nonsmokers.ind <- which(data['smoke'] == 0);
smokers.ind <- which(data['smoke'] == 1);

data[nonsmokers.ind,] -> data.nonsmokers
data[smokers.ind,] -> data.smokers

#summary statistics of smokers
summary(data.nonsmokers$wt)
summary(data.smokers$wt)
#sample standard deviations
sd(data.nonsmokers$wt)
sd(data.smokers$wt)
#skewness
skewness(data.nonsmokers$wt)
skewness(data.smokers$wt)
#kurtosis
kurtosis(data.nonsmokers$wt)
kurtosis(data.smokers$wt)

#box plot of smokers
boxplot(wt~smokedDuringPreg, data, 
        main = "Boxplot of Baby Birth Weights of Mothers With Different Smoking Status", 
        xlab = "Mother smoking status", 
        ylab = "Babies' Birth Weight", 
        names=c("Did not smoke during pregnancy","Smoked during pregnancy"))

#2500 grams is considered low birth weight, convert to ounces
lbwOZ = round(0.035274 * 2500)
#proportion of babies classified as low birth weight
nonsmokersProp <- length(which(data.nonsmokers['wt'] < lbwOZ)) / nrow(data.nonsmokers)
#proportion of babies classified as low birth weight
smokersProp <- length(which(data.smokers['wt'] < lbwOZ)) / nrow(data.smokers)
barplot(c(nonsmokersProp, smokersProp),
        main = "Proportion of Low Birth Weight Babies (88oz threshold)",
        xlab = "Smoking Status during pregnancy",
        ylab = "Proportion",
        ylim = c(0.0, 0.08),
        names.arg = c("Did not smoke during pregnancy", "Smoked during pregnancy"),
        col = c("#696969", "cornflowerblue"))

#change threshold
lbwOZplus = round(lbwOZ * 1.025)      #threshold increased by 2.5% = ~90
lbwOZminus = round(lbwOZ * 0.975)     #threshold decreased by 2.5% = ~85

#proportion of babies classified as low birth weight
nonsmokersPlusProp <- length(which(data.nonsmokers['wt'] < lbwOZplus)) / nrow(data.nonsmokers)
#proportion of babies classified as low birth weight
smokersPlusProp <- length(which(data.smokers['wt'] < lbwOZplus)) / nrow(data.smokers)
barplot(c(nonsmokersPlusProp, smokersPlusProp),
        main = "Proportion of Low Birth Weight Babies (90oz threshold)",
        xlab = "Smoking Status during pregnancy",
        ylab = "Proportion",
        ylim = c(0.0, 0.08),
        names.arg = c("Did not smoke during pregnancy", "Smoked during pregnancy"),
        col = c("#696969", "cornflowerblue"))

#proportion of babies classified as low birth weight
nonsmokersMinusProp <- length(which(data.nonsmokers['wt'] < lbwOZminus)) / nrow(data.nonsmokers)
#proportion of babies classified as low birth weight
smokersMinusProp <- length(which(data.smokers['wt'] < lbwOZminus)) / nrow(data.smokers)
barplot(c(nonsmokersMinusProp, smokersMinusProp),
        main = "Proportion of Low Birth Weight Babies (86oz threshold)",
        xlab = "Smoking Status during pregnancy",
        ylab = "Proportion",
        ylim = c(0.0, 0.08),
        names.arg = c("Did not smoke during pregnancy", "Smoked during pregnancy"),
        col = c("#696969", "cornflowerblue"))

##QQplot against normal for nonsmoking
qqnorm(data.nonsmokers$wt, pch = 1, frame = FALSE)
qqline(data.nonsmokers$wt, col = "blue", lwd = 2)
qqPlot(data.nonsmokers$wt, 
       xlab = "Theoretical Quantiles",
       ylab = "Observed Quantiles", 
       main = "Normal Q-Q Plot of Birth Weights of Babies Born to Nonsmoking Mothers")
##QQplot against normal for smoking
qqnorm(data.smokers$wt, pch = 1, frame = FALSE)
qqline(data.smokers$wt, col = "blue", lwd = 2)
qqPlot(data.smokers$wt, 
       xlab = "Theoretical Quantiles",
       ylab = "Observed Quantiles", 
       main = "Normal Q-Q Plot of Birth Weights of Babies Born to Smoking Mothers")
##QQplot of smoker and nonsmoker samples
qqplot(data.smokers$wt, data.nonsmokers$wt,
       main = "Q-Q Plot Comparison of Birth Weights of Babies",
       xlab = "Quantiles of Birth Weights of Babies Born to Smokers",
       ylab = "Quantiles of Birth Weights of Babies Born to Nonsmokers" 
       )
abline(c(0,1), col = "blue") # reference line

#t-test of smokers vs nonsmokers
t.test(data.smokers$wt, data.nonsmokers$wt, "less")

#MonteCarlo
standardSkewness <- c()
standardKurtosis <- c()
nonsmokersSkewness <- c()
nonsmokersKurtosis <- c()
smokersSkewness <- c()
smokersKurtosis <- c()
for(i in 1:2000) {
  #use MonteCarlo to obtain normal
  standardSkewness <- c(standardSkewness, skewness(rnorm(nrow(data.nonsmokers))))
  standardKurtosis <- c(standardKurtosis, kurtosis(rnorm(nrow(data.nonsmokers))))
  #bootstrap
  nonsmokersSkewness <- c(nonsmokersSkewness, skewness(sample(data.nonsmokers$wt,
                                                              size=nrow(data.nonsmokers),
                                                              replace=TRUE)))
  nonsmokersKurtosis <- c(nonsmokersKurtosis, kurtosis(sample(data.nonsmokers$wt,
                                                              size=nrow(data.nonsmokers),
                                                              replace=TRUE)))
  
  smokersSkewness <- c(smokersSkewness, skewness(sample(data.smokers$wt,
                                                        size=nrow(data.smokers),
                                                        replace=TRUE)))
  smokersKurtosis <- c(smokersKurtosis, kurtosis(sample(data.smokers$wt,
                                                        size=nrow(data.smokers),
                                                        replace=TRUE)))
}
hist(main='Comparison of Skewness Coefficients', 
     standardSkewness, 
     col=rgb(0,1,0,1/2),
     breaks = seq(-1,1,by=0.1),
     ylim=c(0,800),
     xlab = "Skewness Coefficient")
hist(nonsmokersSkewness, 
     col=rgb(0.8,0.8,0.8,1/2), 
     breaks = seq(-1,1,by=0.1),
     add=TRUE)
hist(smokersSkewness, 
     col=rgb(0,0,0.8,1/2), 
     breaks = seq(-1,1,by=0.1),
     add=TRUE)
legend('topright', c('normal', "smoker", "nonsmoker"), 
       col=c("green","cornflowerblue","#696969"), 
       lwd = 4, 
       cex=0.8)

hist(main='Comparison of Kurtosis Coefficients', 
     standardKurtosis, 
     col=rgb(0,1,0,1/2),
     breaks =  seq(1,6,by=0.2),
     ylim=c(0,800),
     xlab = "Kurtosis Coefficient")
hist(nonsmokersKurtosis, 
     col=rgb(0.8,0.8,0.8,1/2), 
     breaks =  seq(1,6,by=0.2),
     add=TRUE)
hist(smokersKurtosis, 
     col=rgb(0,0,0.8,1/2), 
     breaks =  seq(1,6,by=0.2),
     add=TRUE)
legend('topright', c('normal', "smoker", "nonsmoker"), 
       col=c("green","cornflowerblue","#696969"), 
       lwd = 4, 
       cex=0.8)

###########################
