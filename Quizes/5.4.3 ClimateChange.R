#config
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

update.packages()
names(historic_co2)

#TEMP_CARBON QUESTIONS
    #Question 1 - Which returns the latest year for carbon emission reports
        temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            .$year %>%
            max()

        temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            pull(year) %>%
            max()

        temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            select(year) %>%
            max()

    #Question 2 - find the differences between oldest and newest carbon emissions
        #first create a variable with the max
        max_year <- temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            select(year) %>%
            max()

        current_carbon <- temp_carbon %>%
            filter(!is.na(carbon_emissions) & year == max_year) %>%
            select(carbon_emissions) %>%
            max()

        min_year <- temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            select(year) %>%
            min()

        carbon <- temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            mutate(past_present_carbon = current_carbon/carbon_emissions)

    #Question 3 - differences in temperature min to max
        max_year <- temp_carbon %>%
            filter(!is.na(temp_anomaly)) %>%
            select(year) %>%
            max()

        current_temp <- temp_carbon %>%
            filter(!is.na(temp_anomaly) & year == max_year) %>%
            select(temp_anomaly) %>%
            max()

        min_year <- temp_carbon %>%
            filter(!is.na(temp_anomaly)) %>%
            select(year) %>%
            min()

        carbon <- temp_carbon %>%
            filter(!is.na(temp_anomaly)) %>%
            mutate(past_present_temp = current_temp - temp_anomaly)

        carbon %>%
            filter(year == min_year)

    #Question 4 - time line plot of temperatures with blue mean line
        p <- temp_carbon %>%
            filter(!is.na(temp_anomaly)) %>%
            ggplot(aes(year, temp_anomaly)) +
            geom_line()

        p <- p + geom_hline(aes(yintercept = 0), col = "red")

    #Question 5 - add title and data references
        p <- p + ylab("Temperature anomaly (degrees C)") +
            ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
            geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "red")

    #Question 6 - data analysis questions
        #earliet year with temp above 20th century mean - 1940
        #last year below the 20th century mean - 1978
        #what year did the temperature anomly exceed 0.5 degrees - 1997

    #Question 7 - Add ocean and land lines
        p + geom_line(aes(year, ocean_anomaly), col = "blue") +
            geom_line(aes(year, land_anomaly), col = "green")
    
    #Question 10 - line plot of emmisions over time
        temp_carbon %>%
            filter(!is.na(carbon_emissions)) %>%
            ggplot(aes(year, carbon_emissions)) +
            geom_line()
#
#GREENHOUSE_GASES QUESTIONS
    #Question 8 - line plot of concentrations with facet grid
        greenhouse_gases %>%
            ggplot(aes(year, concentration)) +
            geom_line() +
            facet_grid(gas~., scales = "free") +
            geom_vline(aes(xintercept = 1850))
#
#HISTORIC_CO2 QUESTIONS
    #Question 11 - line plot of sources of carbon
        co2_time <- historic_co2 %>%
            ggplot(aes(year, co2, col = source)) +
            geom_line()
        
        co2_time

    #Question 12 - changing axis limits
        #-800,000 & -775,000
        co2_time + scale_x_continuous(limit = c(-800000,-775000))

        #-375,000 & -330,000
        co2_time + scale_x_continuous(limit = c(-375000,-330000))

        #-140,000 & -120,000
        co2_time + scale_x_continuous(limit = c(-140000,-120000))

        #-3,000 & present
        co2_time + scale_x_continuous(limit = c(-3000,2018))