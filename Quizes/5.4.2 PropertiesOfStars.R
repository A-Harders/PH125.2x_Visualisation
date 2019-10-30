#config
library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)

names(stars)
head(stars)

#Question 1 - Magnitude Mean & SD
mean(stars$magnitude)
sd(stars$magnitude)

#Question 2 - Magnitude Density Plot
stars %>%
    ggplot(aes(magnitude)) +
    geom_density()

#Question 3 - Temperature Distribution
stars %>%
    ggplot(aes(temp)) +
    geom_density()

#Question 4 - Temperature & Magnitude Scatter
stars %>%
    ggplot(aes(temp, magnitude)) +
    geom_point()

#Question 5 - transform the plot to astronomical standards, lower magnitudes are on top
stars %>%
    filter(!is.na(magnitude)) %>%
    ggplot(aes(temp, magnitude, col = type)) +
    geom_point() +
    scale_y_reverse(name="magnitude") +
    scale_x_log10() +
    scale_x_reverse(name="temp")

#Question 6 - Look for stars that are of higher temp and higher magnitude, how many?
#Answer - 4 White Dwarves

#Question 7 - Look for stars that are of lower temp and lower magnitude, Avg temp?
#Answer - 5000K these are Red Giants

#Question 8 - Find the following:
    # least luminous star with temp greater than 5000 - White Dwarf: van Maanen's Star
    # the 2 stars with lowest temperaturs and highest luminosity - Supergiants: Betelgeuse & Antares
    # classification of the sun - Main Sequence Star
stars %>%
    filter(!is.na(magnitude)) %>%
    ggplot(aes(temp, magnitude, label = star, col = type)) +
    geom_point() +
    geom_text_repel() +
    scale_y_reverse(name="magnitude") +
    scale_x_log10() +
    scale_x_reverse(name="temp")

#Question 9 - Classifying the stars by colour
stars %>%
    filter(type == "G") %>%
    ggplot(aes(temp, magnitude, label = star, col = type)) +
    geom_point() +
    geom_text_repel() +
    scale_y_reverse(name="magnitude") +
    scale_x_log10() +
    scale_x_reverse(name="temp")