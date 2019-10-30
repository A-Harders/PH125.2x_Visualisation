# THIS ENTIRE CHAPTER IS IN THIS WORKSHEET AS THEY ARE ALL DIRECTLY RELATED TO THE SAME PLOT

# config
library(ggplot2)
library(dslabs)
library(dplyr)
data(murders)

# 2.1.2 LAYERS
# create a ggplot object and add a geom_point() layer
ggplot(data = murders) +
    geom_point(aes(x = population/10^6, y = total))

# we can also assign layers to previously defined objects
p <- ggplot(data = murders)

# now that it is defined we can add the data points
p + geom_point(aes(x = population/10^6, y = total))

# we can now add the labels to our plot, nudged
p + geom_point(aes(x = population/10^6, y = total)) +
    geom_text(aes(population/10^6, total, label = abb))

# 2.2.2 TINKERING
# we can find more details on the functions by using the ?geom_point or ?geom_text calls
# we found that we can change the size of the data points and nudge the labels so they arent obscured
p + geom_point(aes(x = population/10^6, y = total), size = 3) +
    geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)

# we have been inefficient in our aes() mapping, we can include a global aes() when calling ggplot()
# previously defined global aes() mappings will be used in geometry calls as default
# nudge and size remaing in their own functions as they are specific to those functions
glob_p <- ggplot(data = murders, aes(population/10^6, total, label = abb))
glob_p + geom_point(size = 3) + geom_text(nudge_x = 1.5)

# 2.2.3 SCALES, LABELS, & COLOURS
# we need to change the scale of our plots to log10 scale
# we also change the nudge so the data labels are closer
glob_p + geom_point(size = 3) +
         geom_text(nudge_x = 0.075) +
         scale_x_continuous(trans = "log10") +
         scale_y_continuous(trans = "log10")

# log10 scales are so common that ggplot has it defined to make the code more efficient
glob_p + geom_point(size = 3) +
         geom_text(nudge_x = 0.075) +
         scale_x_log10() +
         scale_y_log10()

# we can use the AXISlab() function to add axis labels
# we can use the ggtitle() function to add the title also
glob_p + geom_point(size = 3) +
         geom_text(nudge_x = 0.075) +
         scale_x_log10() +
         scale_y_log10() +
         xlab("Population in millions (log scale)") +
         ylab("Total number of murders (log scale)") +
         ggtitle("US Gun Murders in US")

# we use the col argument in geom_point() to change the colour of our points
# we have redefined p to not include the points layer and re-add later
plot_p <- glob_p +
        geom_text(nudge_x = 0.075) +
        scale_x_log10() +
        scale_y_log10() +
        xlab("Population in millions (log scale)") +
        ylab("Total number of murders (log scale)") +
        ggtitle("US Gun Murders in US")
plot_p

# now we can define the colour of the geom_point()
# all points can be defined by simply the below
plot_p + geom_point(size =3, col = "blue")

# although we want to define the colour by each category by assigning a categorical variable
# the categorical value needs to be mapped so we use aes()
# NOTE: This also adds a legend
plot_p + geom_point(aes(col=region), size =3)

# we want to add an average murder rate line so furst we need to find the average murder rate
r <- murders %>% summarize(rate = sum(total) / sum(population)*10^6) %>% pull(rate)

# now that it is defined we can use the geom_abline()function to add it to our plot
plot_p + geom_point(aes(col=region), size =3) +
         geom_abline(intercept = log10(r))

# we need to change the order of our layers so that it is drawn first, is gray, and is dashed
plot_p <- plot_p +
          geom_abline(intercept = log10(r), lty = 2, color = "darkgray") +
          geom_point(aes(col=region), size =3)

# the last step is to change the labels to capitals and we can do that using the scale_color_discrete()
plot_p <- plot_p +
          scale_color_discrete(name = "Region")

# 2.2.4 ADD-ON PACKAGES
# ggplot2 is further augmeneted by add-ons such as ggthemes & ggrepel
install.packages("ggthemes")
install.packages("ggrepel")
library(ggthemes)
library(ggrepel)

# now we are going to create the whole plot step by step
# first we define our average for our abline()
r <- murders %>%
    summarize(rate = sum(total) / sum(population) *10^6) %>% .$rate

# second we create the ggplot object with all of the layers in the correct order
p <- murders %>% ggplot(aes(population/10^6,total, label = abb)) +
    geom_abline(intercept = log10(r), lty =2, color = "darkgray") + 
    geom_point(aes(col=region),size=3) +
    geom_text_repel() +
    scale_x_log10() +
    scale_y_log10() +
    xlab("Population in millions (log scale)") +
    ylab("Total number of murders (log scale)") +
    ggtitle("US Gun Murders in US 2010") +
    scale_color_discrete(name = "Region") +
    theme_economist()