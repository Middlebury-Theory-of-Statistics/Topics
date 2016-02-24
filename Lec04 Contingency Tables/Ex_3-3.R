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