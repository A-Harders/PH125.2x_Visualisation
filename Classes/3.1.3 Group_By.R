# config
library(tidyverse)
library(dslabs)
data(heights)

# we can group data by categories
heights %>% group_by(sex)

# summaried() data frames after group_by() reacts differently than non-grouped data frames
heights %>%
    group_by(sex) %>%
    summarize(average = mean(height), standrd_deviation = sd(height))

# working with data with more categories
data(murders)
murders <- murders %>% mutate(murder_rate = total/population*100000)
murders %>%
    group_by(region) %>%
    summarize(median_rate = median(murder_rate))