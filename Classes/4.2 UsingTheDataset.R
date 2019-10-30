# config
    library(dslabs)
    library(ggplot2)
    library(dplyr)
    data(gapminder)

# 4.2.1 FACETING
    # we can use the facet_grid() function to facet by up to 2 variables
    filter(gapminder, year%in%c(1962,2012)) %>%
        ggplot(aes(fertility, life_expectancy, col=continent)) +
        geom_point() +
        facet_grid(continent~year)

    # this is too much information though as we just want the year comparison
    # we use the dot to represent the fact that we are only using 1 variable
    filter(gapminder, year%in%c(1962,2012)) %>%
        ggplot(aes(fertility, life_expectancy, col=continent)) +
        geom_point() +
        facet_grid(.~year)

    # to compare multiple years we will want to use the facet_wrap() function
    # this means that our columns of graphs dont become too thin for comparison
    filter(gapminder, year%in%c(1962,1970,1980,1990,2000,2012)) %>%
        ggplot(aes(fertility, life_expectancy, col=continent)) +
        geom_point() +
        facet_wrap(.~year)

# 4.2.2 TIME SERIES PLOTS
    # Time Series Plots show time on the x axis and the result on the y axis
    gapminder %>%
        filter(country =="Australia") %>%
        ggplot(aes(year,fertility)) +
        geom_point()

    # for results that are densly packed and regularly space we can create a curve
    gapminder %>%
        filter(country =="Australia") %>%
        ggplot(aes(year,fertility)) +
        geom_line()

    # this is particularly useful for looking at multiple examples
    countries <- c("Australia","South Korea", "Germany")
    gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year,fertility)) +
        geom_line()

    # this is incorrect however as we havent told ggplot how to group the countries
    countries <- c("Australia","South Korea", "Germany")
    gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year,fertility,col = country)) +
        geom_line()

    # labels are preferrable over legends in most plots
    countries <- c("Australia","South Korea", "Germany")
    labels <- data.frame(country = countries, x = c(1985,1975,1965), y = c(80,60,72))
    gapminder %>% filter(country %in% countries) %>%
        ggplot(aes(year,life_expectancy, col=country)) +
        geom_line() +
        geom_text(data = labels, aes(x,y,label = country), size = 5) +
        theme(legend.position="none")

# 4.2.3 TRANSFORMATIONS
    # we starting by adding a variable to the table of gdp per day
    gapminder <- gapminder %>%
        mutate(dollars_per_day = gdp/population/365)

    # now we create a histogram of per day incomes
    past_year <- 1970
    gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black")

    # we can see that the majority of the x axis is dedicated to the countries over $10 a day
    # we can split this up into brackets to show a better scale of poverty
    gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(log2(dollars_per_day))) +
        geom_histogram(binwidth = 1, color = "black")

    # we can transform the values and log them before plotting them
    # the advantage being that we can see the original values on the axis, so it is x^log(n)
    # we add the scale_x_continuous() layer instead of transforming the data before plotting
    gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") + 
        scale_x_continuous(trans = "log2")

# 4.2.4 STRATIFY & BOXPLOT
    # the histogram from the previous section needs to be stratified in to region before we can compare the dstributions
    # as the number of regions is large, looking at smooth densities will not be useful, although stacked boxplots will be
    length(levels(gapminder$region))
    p <- gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        ggplot(aes(region,dollars_per_day))
    p + geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    # ordering the axis by alphabetical is completely arbitrary we can order in a more meaningful way using the reorder() function
    # simplified reorder function()
    fac <- factor(c("Asia", "Asia", "West","West", "West"))
    levels(fac)
    value <- c(10,11,12,6,4)
    fac <- reorder(fac, value, FUN = mean)
    levels(fac)

    # reorder() the regions
    p <- gapminder %>%
        filter(year == past_year & !is.na(gdp)) %>%
        mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
        ggplot(aes(region, dollars_per_day, fill = continent)) +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("")
    p

    # finally changing the scale to the lgo2 scale will help show the point more succinctly
    p + scale_y_continuous(trans = "log2")

    # showing the data can in this case give more information as there arent many points
    # this will show where every single country lies
    p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

# 4.2.5 COMPARING DISTRIBUTIONS
    #we have found to interesting characteristics so far, a bimdoale distribution, and the location of the predominantly rich countries
    #we are going to define a vector of the regions considered the west
    west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")

    #with this we can focus on the difference in distribution across time
    past_year <- 1970
    present_year <- 2010

    gapminder <- gapminder %>%
        mutate(dollars_per_day = gdp/population/365)

    gapminder %>%
        filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
        mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2") +
        facet_grid(year~group)

    #we noted that there was an increase in countries over the 2 years so we want to compare apples to apples by comparing only the countries in a to countries in b
    #we use the intersect() function to compare the 2 lists we create and get the matching results
    country_list_1 <- gapminder %>%
        filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>%
        filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)
        #NOTE: there is a better way of doing this but it is taught in a later course using 'tidyverse'

    #now that we have the intersected country list we can use the country %in% country_list to get this plot
    gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day)) +
        geom_histogram(binwidth = 1, color = "black") +
        scale_x_continuous(trans = "log2") +
        facet_grid(year~group) 

    #remaking the boxplot using the same code we can see which countries have gone up more
    p <- gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
        ggplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("") + scale_y_continuous(trans = "log2")

    #now we use the facet_grid
    p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
        facet_grid(year~.)

    #this was difficult to understand as it was 1 year above the other, in general it is easier to compare side by side than above and below
    #by coloring by year instead of faceting ggplot automatically creates 2 box plots side by side
    #this is called ease comparison
    p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

# 4.2.6 DENSITY PLOTS
    #we can show succinct comparisons by using the smooth density plots
    #we need to preserver the information of how many countries are in each group because the large number of developing countries will encompass the west
    gapminder %>%
        filter(year == past_year & country %in% country_list) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        group_by(group) %>%
        summarize(n=n()) %>%
        knitr::kable()

    #to have the areas of the densities be proportional to the size of the groups, we can multiply the y-axis by the size of the group
    p <- gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
        ggplot(aes(dollars_per_day, y= ..count.., fill = group)) +
        scale_x_continuous(trans = "log2")

    p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

    #we can make this plot more informative by showing key regions separately, using the case_when() function
    #it currently does not yet have a data argument (yet?) so we need to use the dot placeholder method
    gapminder <- gapminder %>%
        mutate(group = case_when(
            .$region %in% west ~ "West",
            .$region %in% c("Eastern Asia", "South Eastern Asia") ~ "East Asia",
            .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
            .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
            TRUE ~ "Others"))

    #now we need to turn this group variable into a factor to control the order of the levels
    #the order is decided for a reason and become clear later when we make the plots
    gapminder <- gapminder %>%
        mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

    #now we can easily plot the density for each one, using color and size to clearly see the top
    p <- gapminder %>%
        filter(year %in% c(past_year,present_year) & country %in% country_list) %>%
        ggplot(aes(dollars_per_day, fill = group)) +
        scale_x_continuous(trans = "log2")

    p + geom_density(alpha=0.2, bw= 0.75)+facet_grid(year ~.)

    #the plots are very congested so we can use a stacked density plot to stack the data ontop of one another
    #this is why we ordered the data the way before, so the west was printed first then sub-saharan africa etc etc
    p + geom_density(alpha=0.2, bw= 0.75, position = "stack")+facet_grid(year ~.)

    #lastly the above weights every country the same regardless of population size, we can remedy that by using the weight mapping
    gapminder %>%
        filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
        group_by(year) %>%
        mutate(weight = population/sum(population*2)) %>%
        ungroup() %>%
        ggplot(aes(dollars_per_day,fill=group,weight = weight)) +
        scale_x_continuous(trans = "log2") +
        geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
        facet_grid(year ~.)

# 4.2.7 ECOLOGICAL FALACY
    #we have been comparing prosperity by region but we also need to describe the variability
    #we need to define a few more regions
    gapminder <- gapminder %>%
        mutate(group = case_when(
            .$region %in% west ~ "The West",
            .$region %in% "Northern Africa" ~ "Northern Africa",
            .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
            .$region == "Southern Asia" ~ "Souther Asia",
            .$region %in% c("Central America", "South America", "Carribean") ~ "Latin America",
            .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
            .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"
        ))

    head(gapminder)

    #once we have done this we compute the quantites that we're interested in for each region
    present_year <- 2010

    surv_income <- gapminder %>%
        filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
        group_by(group) %>%
        summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))    
    surv_income %>% arrange(income)

    #we can now plot the data in ggplot and include the limit argument which changes the range of the axis
    surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
        scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
        scale_y_continuous(trans = "logit", limit = c(0.875,0.9981), breaks = c(.85,.90,.95,.99,.995,.998)) +
        geom_label(size = 3, show.legend = FALSE)

    #this highlights however the Ecological Fallacy, which is that assumptions drawn on the average are not representative of the individuals that make up the average
    #to do show this we need to compute surv_income per country and plot it on a point plot
    gapminder <- gapminder %>%
        filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
        group_by(country) %>%
        mutate(income = sum(gdp)/sum(population)/365,
        infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))

    gapminder %>% ggplot(aes(income, infant_survival_rate, label = country, color = group)) +
        scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
        scale_y_continuous(trans = "logit", limit = c(0.875,0.9981), breaks = c(.85,.90,.95,.99,.995,.998)) +
        geom_point(size = 3)    