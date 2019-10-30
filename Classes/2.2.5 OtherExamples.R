# config
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(dslabs)
library(dplyr)
data(heights)

# CREATE A HISTOGRAM OF MALE HEIGHTS
# first we filter our height results by sex
p <- heights %>%
    filter(sex=="Male") %>%
    ggplot(aes(sample = height))

# second we pipe our filtered results into a histogram geometry
p + geom_histogram(binwidth =1)

# further formatting is available in the geom_histogram()
p + geom_histogram(binwidth =1, fill = "blue", col = "black") +
    xlab("Male heights in inches") +
    ggtitle("Histogram")

# CREATE A DENSITY OF MALE HEIGHTS
# as we have already filter our height results we can use the p object to layer in a geom_density()
p + geom_density(fill = "red")

# CREATE A Q-Q OF MALE HEIGHTS
# as we have already filter our height results we can use the p object to layer in a geom_qq()
p + geom_qq()

# we need to set the parameters to our source data so our q-q plot works
params <- heights %>%
    filter(sex == "Male") %>%
    summarize(mean = mean(height), sd = sd(height))

# add an identity line  to check the normal approximation
p + geom_qq(dparams = params) +
    geom_abline()

# alternatively we can computer our data into standard units and compute the q-q on this as it looks cleaner
heights %>% filter(sex=="Male") %>%
    ggplot(aes(sample = scale(height))) +
    geom_qq() +
    geom_abline()

# PLACING PLOTS NEXT TO EACH OTHER
# using the gridextra package we can place multiple plots next to each other
# first we define our objects
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p +geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p +geom_histogram(binwidth = 1, fill = "blue", col = "black")
p3 <- p +geom_histogram(binwidth = 1, fill = "blue", col = "black")

# then using the grid.arrange() function from gridExtra we add the objects and define the columns
grid.arrange(p1,p2,p3, ncol = 3)