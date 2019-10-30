# config
library(dslabs)

# start by defingin a series of proportions
p <- c(seq(0.05,0.95,0.05))
p

# to obtain qunaitles we can use the quantile function
index <- heights$sex=="Male"
x <- heights$height[index]
observed_quantiles <- quantile(x,p)
observed_quantiles

# to obtain the theorietical normal distribution quantiles we use the qnorm functions
index <- heights$sex=="Male"
x <- heights$height[index]
theoretical_qauntiles <- qnorm(p, mean = mean(x), sd = sd(x))
theoretical_qauntiles

plot(observed_quantiles,theoretical_qauntiles)
abline(0,1)

# the whole process is simplified if we use standardised units
# as we dont need to define the mean and the standard deviation
z <- scale(x)
observed_quantiles <- quantile(z,p)
theoretical_qauntiles <- qnorm(p)
plot(theoretical_qauntiles, observed_quantiles)
abline(0,1)