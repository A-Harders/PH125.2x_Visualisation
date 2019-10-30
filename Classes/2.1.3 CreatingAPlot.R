# config
install.packages("tidyverse")
library(ggplot2)
library(dslabs)
data(murders)

# create a ggplot object
ggplot(data = murders)
    # alternatively we can pipe the data
    murders %>% ggplot()
    # we can also assign the graph to an object
    p <- ggplot(data = murders)