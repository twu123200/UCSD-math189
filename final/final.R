library(gmodels)
library(moments)

#data <- read.csv("train.csv", header = TRUE, na.strings=c("","NA"))

# #clean data
# num_unique_values <- c()
# percent_null <- c()
# percent_skewness <- c()
# 
# for(col in names(data)) {
#   num_unique_values <- c(num_unique_values, length(unique(na.omit(data[[col]]))))
#   percent_null <- c(percent_null, 100 * (sum(is.na(data[[col]])) / nrow(data)))
#   percent_skewness <- c(percent_skewness,
#                         100 * (sort(table(data[[col]], useNA="always"), decreasing=TRUE)[1] / nrow(data)))
# }
# percent_null <- round(percent_null, digits = 6)
# percent_skewness <- round(percent_skewness, digits = 6)
# data.stats <- data.frame(row.names = names(data),
#                          "unique_values" = num_unique_values,
#                          "percentage_of_missing_values" = percent_null,
#                          "percentage_of_values_in_the_biggest_category" = percent_skewness)
# 
# data.stats <- data.stats[order(data.stats$percentage_of_missing_values, decreasing = TRUE),]
# data.stats <- data.stats[order(data.stats$percentage_of_values_in_the_biggest_category, decreasing = TRUE),]
# 
# #if column has more than 50% null values, drop column
# #if column has more than 99% in one category, drop column
# to_drop <- c()
# to_drop <- c(to_drop,
#              row.names(data.stats[which(data.stats$percentage_of_missing_values > 50),]))
# to_drop <- c(to_drop,
#              row.names(data.stats[which(data.stats$percentage_of_values_in_the_biggest_category > 99),]))
# #drop columns
# data <- data[ , !(names(data) %in% to_drop)]

############################################################################

barplot(setNames(table(data$HasDetections), c("No detection", "Has Detections")),
        main = "Counts of devices with malware detection",
        ylab = "Counts",
        col = "cornflowerblue")

############################################################################

gamer.df <- data[which(data["Wdft_IsGamer"] == 1),]
gamer.table <- table(data$Wdft_IsGamer, data$HasDetections)

barplot(setNames(table(data$Wdft_IsGamer), c("Non-gamer device", "Gamer device")),
        main = "Counts of gamer devices",
        ylab = "Counts",
        col = rgb(0.1, 0.5, 0.6, 0.7))
barplot(setNames(table(gamer.df$HasDetections), c("No detection", "Has Detections")),
        main = "Counts of gamer devices with malware detection",
        ylab = "Counts",
        col = rgb(0.1, 0.1, 0.8, 0.7))

ci(gamer.df$HasDetections)

set.seed(123)
gamer.prop <- c()
n <- nrow(gamer.df)
for(i in 1:100) {
  gamer.prop <- c(gamer.prop, sum(sample(gamer.df$HasDetections, size = n, replace = TRUE)) / n)
}

ci(gamer.prop)

gamer.prop <- (gamer.prop - mean(gamer.prop)) / sd(gamer.prop)
hist(gamer.prop,
     main = "Distribution of 100 Bootstrapped Gamer Devices With Malware (Standard Units)",
     xlab = "Standard units of gamer devices with malware",
     col = rgb(0.2, 0.7, 0.3, 0.5))
skewness(gamer.prop)
kurtosis(gamer.prop)

gamer.normal <- rnorm(n)
ks.test(gamer.prop, gamer.normal)

t.test(gamer.df$HasDetections, mu = 0.5)

###########################################################################

av_installed <- table(data$HasDetections, data$AVProductsInstalled)

barplot(av_installed, beside = TRUE,
        main = "Counts of malware detection based on Anti-Virus products installed",
        xlab = "Number of Anti-Virus products installed",
        ylab = "Counts",
        col = c("blue", "red"))
legend("topright", c("No detection", "Has Detections"),
       fill = c("blue", "red"),
       cex = 0.8)

av_installed.percent_malware <- c()
for(i in 1:ncol(av_installed)) {
  sum <- 0
  for(j in 1:nrow(av_installed)) {
    sum <- sum + av_installed[j, i]
  }
  av_installed.percent_malware <- c(av_installed.percent_malware, av_installed[2,i] / sum)
}

av_installed.counts <- table(data$AVProductsInstalled)
par(mar=c(5, 5, 4, 5))
bar <- barplot(av_installed.counts,
               width = 1,space = 0.2,
               main = "Counts of Anti-Virus Products installed",
               xlab = "Number of Anti-Virus Products installed",
               ylab = "Counts",
               col = "cornflowerblue")
par(new = TRUE)
plot(av_installed.percent_malware, axes = FALSE, type = "b", pch =15,
     xlab = "",
     ylab = "",
     col = "magenta")
mtext("Detection Rate", side=4, line=3) 
axis(4, las=1, col = "magenta")

expected <- as.array(margin.table(av_installed,1)) %*% t(as.array(margin.table(av_installed,2))) / margin.table(av_installed)
expected <- expected[,c(2:7)]

av_installed.observed <- av_installed[,c(2:7)]

chi <- sum((expected - as.array(av_installed.observed))^2/expected)
p_value <- 1-pchisq(chi,df=ncol(av_installed.observed) - 1)

ci(data$AVProductsInstalled, na.rm=T)

for(num in 1:6) {
  temp <- data[which(data["AVProductsInstalled"] == num),]
  print(t.test(temp$HasDetections, mu = 0.5, alternative = "less",na.rm=T))
}
###

av_enabled <- table(data$HasDetections, data$AVProductsEnabled)

barplot(av_enabled, beside = TRUE,
        main = "Counts of malware detection based on Anti-Virus products enabled",
        xlab = "Number of Anti-Virus products enabled",
        ylab = "Counts",
        col = c("blue", "red"))
legend("topright", c("No detection", "Has Detections"),
       fill = c("blue", "red"),
       cex = 0.8)

av_enabled.percent_malware <- c()
for(i in 1:ncol(av_enabled)) {
  sum <- 0
  for(j in 1:nrow(av_enabled)) {
    sum <- sum + av_enabled[j, i]
  }
  av_enabled.percent_malware <- c(av_enabled.percent_malware, av_enabled[2,i] / sum)
}

av_enabled.counts <- table(data$AVProductsEnabled)

par(mar=c(5, 5, 4, 5))
bar <- barplot(av_enabled.counts,
               width = 1,space = 0.2,
               main = "Counts of Anti-Virus Products enabled",
               xlab = "Number of Anti-Virus Products enabled",
               ylab = "Counts",
               col = "cornflowerblue")
par(new = TRUE)
plot(av_enabled.percent_malware, axes = FALSE, type = "b", pch =15,
     ylim = c(0,1),
     xlab = "",
     ylab = "",
     col = "magenta")
mtext("Detection Rate", side=4, line=3) 
axis(4, las=1, col = "magenta")

expected <- as.array(margin.table(av_enabled,1)) %*% t(as.array(margin.table(av_enabled,2))) / margin.table(av_enabled)
chi <- sum((expected - as.array(av_enabled))^2/expected)
p_value <- 1-pchisq(chi,df=ncol(av_enabled) - 1)

av_installed.gt1 <- data[which(data["AVProductsInstalled"] > 1),]
av_enabled.gt1 <- table(av_installed.gt1$HasDetections, av_installed.gt1$AVProductsEnabled)
prop.table(av_enabled.gt1, 2)

av_installed.lt1 <- data[which(data["AVProductsInstalled"] <= 1),]
av_enabled.lt1 <- table(av_installed.lt1$HasDetections, av_installed.lt1$AVProductsEnabled)
prop.table(av_enabled.lt1, 2)

ks.test(av_installed.gt1$HasDetections, av_installed.lt1$HasDetections)

#######################################################################


