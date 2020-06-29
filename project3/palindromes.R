setwd("C:/Users/twu12/Desktop/school/math189/p3")

df <- read.table("location.txt", header=TRUE)
data <- df$location
N <- 229354 #size of DNA chain
n <- 296 #number of palindromes
k <- 58 #interval size

####################################################################

#1: Random Scatter
base <- runif (n, 1, 232000)
hist (base, breaks = k,
      ylim = c(0, 20),
      main = "Uniform Random Baseline: Locations of Palindromes in consecutive Intervals of 4000 base pairs",
      col=rgb(1,0,0,0.5),
      xlab = "Location on chain of DNA base pairs",
      ylab = "Number of palindromes in the interval")
hist (data, breaks = k,
      ylim = c(0, 20),
      main = "Original Data: Locations of Palindromes in consecutive Intervals of 4000 base pairs",
      col=rgb(0,1,0,0.5),
      xlab = "Location on chain of DNA base pairs")

####################################################################

#2: counts and spacings
# Part 1: Overlapping Histograms
hist (base, breaks = k,
      ylim = c(0, 20),
      main = "Overlap: Locations of Palindromes in consecutive Intervals of 4000 base pairs",
      col=rgb(1,0,0,0.5),
      xlab = "Location on chain of DNA base pairs")
hist (data, col=rgb(0,1,0,0.5), breaks = k, add=T)

# Part 2: qq plot comparison
base <- sort(base)
qqplot(data, base,
       main = "QQ Plot Comparison of Original Data and Baseline Uniform",
       xlab = "Quantiles of Locations of Original Data",
       ylab = "Quantiles of Locations of Uniform Baseline")
abline(c(0,1), col = "blue")

# Part 3: Chi-squared test to compare the two distributions
location.expected <- n/k
location.observed <- as.vector(table(cut(base, breaks=seq(0, N, length.out = k+1), include.lowest = TRUE)))
chi2 <- sum((location.observed - location.expected)^2 / location.expected)
chi2_compare <- qchisq(p=0.95, df=k-1)
p_value <- pchisq(chi2, df=k-1, lower.tail = FALSE)

res <- (location.observed - location.expected) / sqrt(location.expected)
plot(res, type = "h",
     main = "Standard Residual of Locations (intervals of 4000 nucleotides)",
     xlab = "Palindrome location",
     ylab = "Standard Residuals")

prev <- data[1]
interval <- c()
for(i in 2:length(data)) {
  interval <- c(interval, data[i] - prev)
  prev <- data[i]
}

#histogram for distance between palindromes
hist(interval, breaks = k,
     main = "Palindrome spacing",
     col = "cornflower blue",
     xlab = "Distances between palindromes")


####################################################################
#3: counts


#get palindromes within interval
counts <- as.vector(table(cut(data, breaks = seq(0, N, length.out = k+1), include.lowest = TRUE)))
#get counts of palindromes within an interval
library(hash)
dict <- hash()
for (i in 0:12) {
  key <- toString(i)
  dict[[key]] <- 0
}
dict[["13+"]] <- 0
for (c in counts) {
  if(c < 13) {
    key <- toString(c)
    dict[[key]] <- dict[[key]] + 1
  }
  else {
    dict[["13+"]] <- dict[["13+"]] + 1
  }

}
num_in_interval <- c()
for (i in 0:12) {
  key <- toString(i)
  num_in_interval <- c(num_in_interval, dict[[key]])
}
num_in_interval <- c(num_in_interval, dict[["13+"]])
#get MLE
lambda_hat <- mean(counts)
#get expected distribution based on MLE
expected_poisson <- c()
for (i in 0:12) {
  expected_poisson <- c(expected_poisson, dpois(i, lambda_hat))
}
expected_poisson <- c(expected_poisson, 1 - sum(expected_poisson))
#convert to probability
num_in_interval <- num_in_interval / k
#plot original data set with expected
bar <- barplot(num_in_interval, names.arg = c(seq(0, 12, length.out = 13), "13+"),
               main = "Interval (of 4000 nucleotides) containing palindromes",
               xlab = "k = number of palindromes in an interval",
               ylab = "P(X=k)",
               ylim = c(0.0, 0.25))
points(x = bar, y = expected_poisson,
       pch = 19,
       col = rgb(0,0,0.5,0.5))
     
#residual
counts.observed <- num_in_interval * k
counts.expected <- expected_poisson * k
palindrome_count <- c(seq(0, 12, length.out = 13), "13+")
counts.table <- data.frame(palindrome_count, counts.observed, counts.expected)

chi2<- sum((counts.observed - counts.expected)^2 / counts.expected)
chi2_compare <- qchisq(p = 0.95, df = 12)
p_value <- pchisq(chi2, df = 12, lower.tail = FALSE)

counts.residual <- (counts.observed - counts.expected) / sqrt(counts.expected)
plot(counts.residual, type = 'h', xaxt = "n",
     main = "Standard Residual of Counts (intervals of 4000 nucleotides)",
     xlab = "Number of palindromes in an interval",
     ylab = "Standardized Residuals")
axis(1, at=1:14, labels=c(seq(0, 12, length.out = 13), "13+"))


###############################################################
#4: the biggest cluster
intervals <- c(2500,4000,5500,7000)
k_intervals <- ceiling(N / intervals)
k_lambda_hat <- c()
k_max_count <- c()
k_p_value <- c()

for(k in k_intervals) {
  
  k_count <- as.vector(table(cut(data, breaks = seq(0, N, length.out = k+1), include.lowest = TRUE)))
  lambda_hat <- mean(k_count)
  k_lambda_hat <- c(k_lambda_hat, lambda_hat)
  k_max_count <- c(k_max_count, max(k_count))
  
  library(hash)
  dict <- hash()
  for (i in 0:max(k_count)) {
    key <- toString(i)
    dict[[key]] <- 0
  }
  key <- toString(max(k_count)+1)
  dict[[key]] <- 0
  
  for (c in k_count) {
    key <- toString(c)
    dict[[key]] <- dict[[key]] + 1
  }
  
  k_counts_observed <- c()
  for (i in 0:max(k_count)) {
    key <- toString(i)
    k_counts_observed <- c(k_counts_observed, dict[[key]])
  }
  key <- toString(max(k_count)+1)
  k_counts_observed <- c(k_counts_observed, dict[[key]])
  
  k_expected_poisson <- c()
  for (i in 0:max(k_count)) {
    k_expected_poisson <- c(k_expected_poisson, dpois(i, lambda_hat))
  }
  k_expected_poisson <- c(k_expected_poisson, 1 - sum(k_expected_poisson))
  
  k_counts_expected <- k_expected_poisson * k
  
  k_chi2<- sum((k_counts_observed - k_counts_expected)^2 / k_counts_expected)
  k_chi2_compare <- qchisq(p = 0.95, df = max(k_count) - 2)
  k_p_value <- c(k_p_value, pchisq(k_chi2, df = max(k_count) - 2, lower.tail = FALSE))
   
}
result <- data.frame(intervals, k_intervals, k_lambda_hat, k_max_count, k_p_value)

###############################################################


