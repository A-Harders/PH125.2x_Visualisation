#config
library(tidyverse)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)
str(us_contagious_diseases)

#first we need to create a data variable for the measels data, and removing states that only became states later (alaska & hawaii)
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
    filter(disease == the_disease & !state %in% c("Hawaii", "Alaska")) %>%
    mutate(rate = count/population *10000 * 52/weeks_reporting) %>%
    mutate(state = reorder(state, rate))

head(dat)

#we can now plot the disease rates per year
#NOTE:1963 was the year the vaccine was introduced
dat %>%
    filter(state == "California") %>%
    ggplot(aes(year,rate)) +
    geom_line() + ylab("Cases per 10,000") +
    geom_vline(xintercept=1963, col = "blue")

#taking note of the wall street journal visualisation, we can improve on this by using a better color pallete
#SEQ is used for high to low values, DIV is used for showing divergence from the middle
display.brewer.all(type="seq")
display.brewer.all(type="div")

#to include our 3rd variable with only to axes we use color, size or intensity, as in the case "Rate" is continuous
#NOTE: we transform the fill by the square root as to prevent large outliers from dominating the graph
dat %>%
    ggplot(aes(year,state,fill = rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") +
    geom_vline(xintercept=1963, col="blue") +
    theme_minimal() + theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")

#if we are willing to lose some data information we can show the values with position; as it represents values better than colour
#we can also show the average for the US
#NOTE: we are going to make every state teh same color as it is difficult to pick 50 distinct colors
avg <- us_contagious_diseases %>%
    filter(disease==the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>%
    filter(!is.na(rate)) %>%
    ggplot() +
    geom_line(aes(year, rate, group = state), color = "grey50",
        show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x=1955, y=50),
        mapping = aes(x,y,label = "US average"), color = "black") +
    geom_vline(xintercept = 1963, col = "blue")