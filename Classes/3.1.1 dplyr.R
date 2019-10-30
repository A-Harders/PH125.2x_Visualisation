# config
library(tidyverse)
library(dslabs)
data(heights)

# computing the average and sd of height in males
s <- heights %>%
    filter(sex == "Male") %>%
    summarize(average = mean(height), standard_deviation = sd(height))

# as summarize() stores the resulting data in a data frame we can access it with the $ accessor
s$average
s$standard_deviation