# config
library(tidyverse)
library(dslabs)
data(heights)

# using the arrange() function we can arrange an entire data frame in a suitable way
murders %>% arrange(population) %>% head()
murders %>% arrange(murder_rate) %>% head()

# default setting is to sort by ascending order, the function DESC() transforms the vector into descending order
murders %>% arrange(desc(population)) %>% head()

# adding more arguments to the function is called nested sorting, the first is evaluated then the second etc
murders %>% arrange(region, murder_rate) %>% head()

# using the top_n() function we can define how many results we want to see
murders %>% arrange(desc(murder_rate)) %>% top_n(10)