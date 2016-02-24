# Example 3.3 (Page 44) Verizon Permutation test --------------------------
library(resample)

# Load data
data(Verizon)

tapply(Verizon$Time, Verizon$Group, mean)

Time <- subset(Verizon, select=Time, drop=T)
Time.ILEC <- subset(Verizon, select=Time, Group=="ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time, Group=="CLEC", drop=T)

observed <- mean(Time.ILEC)-mean(Time.CLEC)
observed

B <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result <- numeric(B) # space to save the random differences
for(i in 1:B)
  {
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- mean(Time[index]) - mean(Time[-index])
}

# Plot reference distribution
title <- "Permutation Distribution for Verizon repair times"
x_label <- "xbar1 - xbar2"

hist(result, xlab = x_label, breaks=50, main=title, xlim=c(-20, 10))
abline(v = observed, col = "blue", lty=5)

(sum(result <= observed)+1)/(B + 1)  #P-value





# Different p-values ------------------------------------------------------
# Say instead the observed test statistic was -4.5
observed <- -4.5
hist(result, xlab = x_label, breaks=50, main=title, xlim=c(-20, 10))
abline(v = observed, col = "blue", lty=5)

# One-sided p-value corresponding to HA: mu1 < mu2
(sum(result <= observed)+1)/(B + 1)


# We have three possible two-sided p-values corresponding to HA: mu1 != mu2

# Method 1: Double the one-sided p-value despite the fact reference distribution
# is not symmetric
2 * (sum(result <= observed) + 1)/(B + 1)


# Method 2: More extreme on the domain
hist(result, xlab = x_label, breaks=50, main=title)
abline(v = c(observed, -observed), col = "blue", lty=5)

# Area to the left of -4.5 and to the right of 4.5
(sum(result <= observed) + sum(result >= -observed) + 1)/(B + 1) 


# Method 3: More extreme on the range
hist(result, xlab = x_label, breaks=50, main=title)
abline(v = observed, col = "blue", lty=5)
abline(h=169, col="red", lty=5)
abline(v=)