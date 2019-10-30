# config
library(tidyverse)
library(dslabs)
data(murders)

# previously we have computed the murder rate by state and added it to the data frame
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

# the total us murder rate is computed as follows
us_murder_rate <- murders%>%
    summarize(rate = sum(total) /sum(population)*100000)

# to gain access to the data as a vector we can use the dot placeholder method
us_murder_rate %>% .$rate

# to get the number from the original data table with a single line of code we can do the below:
us_murder_rate <- murders %>%
    summarize(rate = sum(total) /sum(population)*100000) %>%
    .$rate