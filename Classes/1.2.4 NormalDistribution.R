#config
library(dslabs)
data(heights)

# create an object of male heights
index <- heights$sex=="Male"
x <- heights$height[index]

# average is the sum divided by the length
average <- sum(x) / length(x)
average

# standard deviation is the quare root of the sum of the differences between the values and the mean squared divided by the length
SD <- sqrt(sum((x-average)^2)/length(x))
SD

# to find the standard unit, which is 1 standard deviation away from average
SU <- (x-average)/SD
SU

# pre-built functions exist for this but it is good to know the mathematics behind it
average <- mean(x)
SD <- sd(x)
z <- scale(x)
c(average=average,SD=SD)

# finding the size of your sample within x SUs uses the below
mean(abs(SU) < 2)