# config
    library(dslabs)
    library(ggplot2)
    library(dplyr)
    data(gapminder)

# 4.1.2 GAPMINDER DATASET
    # find the Field names
    names(gapminder)

    # lets find the mortality rates for countries
    gapminder %>%
        filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
        select(country, infant_mortality)

# 4.1.3 LIFE EXPECTANCY & FERTILITY RATE
    # lets create a scatterplot of fertility rates to life expectancy
    ds_theme_set()
    filter(gapminder, year == 1962) %>%
        ggplot(aes(fertility, life_expectancy, color = continent)) +
        geom_point()