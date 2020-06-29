setwd("/Users/twu12/Desktop/school/math189/p2")
library(moments)

data <- read.table("videodata.txt", header=TRUE)
data.population = 314  #N
data.sample = 91   #n
#clean data, set unanswered results to NA
data[data == 99] <- NA

#Scenario 1
#number of players in the last week
players <- length(which(data$time > 0))
players.proportion <- players / data.sample

#95% confidence interval, not accounting for non iid
players.error_margin1 <- qnorm(0.975) * sqrt(players.proportion * (1 - players.proportion) / data.sample)
players.standard_lower <- players.proportion - players.error_margin1
players.standard_upper <- players.proportion + players.error_margin1
data.players_95CI_standard <- c(players.standard_lower, players.standard_upper)
#95% confidence interval, accounting for non iid
players.error_margin2 <- qnorm(0.975) * sqrt(((players.proportion * (1 - players.proportion)) / 
                                              (data.sample-1))*(data.population-data.sample) / data.population)
players.corrected_lower <- players.proportion - players.error_margin2
players.corrected_upper <- players.proportion + players.error_margin2
data.players_95CI_corrected <- c(players.corrected_lower, players.corrected_upper)

#bootstrap 
set.seed(0)
means <- c()
pop <- rep(data$time > 0, length.out = 314)

for(i in 1:1000){
  means <- c(means, sum(sample(pop, size = 91, replace = FALSE)) / 91)
}

hist(means, main = 'Distribution of 1000 Bootstrapped Proportion Sample Means', ylim=c(0,250), xlab = 'Proportion of Students who Played Video Games', col = "cornflower blue")
abline(v=mean(means),col="red")


#Scenario 3
time.mean <- mean(data$time)

#95% confidence interval, standard
time.error_margin1 <- qnorm(0.975) * sd(data$time) / sqrt(data.sample)
time.standard_lower <- time.mean - time.error_margin1
time.standard_upper <- time.mean + time.error_margin1
data.time_95CI_standard <- c(time.standard_lower, time.standard_upper)
#95% confidence interval, corrected
time.error_margin2 <- qnorm(0.975) * sd(data$time) / sqrt(data.sample) * sqrt((data.population - data.sample)/data.population)
time.corrected_lower <- time.mean - time.error_margin2
time.corrected_upper <- time.mean + time.error_margin2
data.time_95CI_corrected <- c(time.corrected_lower, time.corrected_upper)
#95% confidence interval using simulation study, bootstrapping
time.bootstrap <- c()
time.bootstrap_population <- rep(data$time, length.out = data.population)
for(i in 1:1000) {
  time.bootstrap <- c(time.bootstrap, mean(
                            sample(time.bootstrap_population, size=nrow(data),replace=FALSE)))
}

hist(time.bootstrap,
     main='Distribution of 1000 Bootstrapped Sample Means',
     col="cornflowerblue",
     ylim=c(0,250),
     xlab = "Amount of Time")

time.bootstrap_mean <- mean(time.bootstrap)
time.error_margin3 <- qnorm(0.975) * sd(time.bootstrap) / sqrt(data.sample)
time.bootstrap_lower <- time.bootstrap_mean - time.error_margin3
time.bootstrap_upper <- time.bootstrap_mean + time.error_margin3
data.time_95CI_bootstrap <- c(time.bootstrap_lower, time.bootstrap_upper)

#observe difference in skewness
skewness(data$time)
kurtosis(data$time)

skewness(time.bootstrap)
kurtosis(time.bootstrap)

#compare distributions
normal_test <- rnorm(data.sample)
#normalize to standard units
time.bootstrap <-(time.bootstrap - time.bootstrap_mean) / sd(time.bootstrap)
time.su = (data$time - mean(data$time)) / sd(data$time)

ks.test(time.su, normal_test)
ks.test(time.bootstrap, normal_test)

#Scenario 5
#clean data, remove if never played video games,
#group somewhat liking/very much like to Like
#group not really liking/not like at all to Dislike
like.data <- data[which(data$like != 1), ]
like.data$like[like.data$like == 2 | like.data$like == 3] <- "Like"
like.data$like[like.data$like == 4 | like.data$like == 5] <- "Dislike"
#remove those who did not answer who worked
like.data <- like.data[which(!is.na(like.data$work)),]

#change name of "sex" value
like.data$sex[like.data$sex == 0] <- "Female"
like.data$sex[like.data$sex == 1] <- "Male"
#change name of "work" value
like.data$work[like.data$work > 0] <- "Work"
like.data$work[like.data$work == 0] <- "No work"
#change name of "own" value
like.data$own[like.data$own == 0] <- "Does not own PC"
like.data$own[like.data$own == 1] <- "Own PC"

library(gmodels)
CrossTable(like.data$like, like.data$sex)
CrossTable(like.data$like, like.data$Work)
CrossTable(like.data$like, like.data$own)

##################### 
