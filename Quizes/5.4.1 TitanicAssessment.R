#config
options(digits = 3)
install.packages("titanic")
library(tidyverse)
library(dslabs)
library(dplyr)
library(titanic)


titanic <- titanic_train %>%
    select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
    mutate(Survived = factor(Survived),
    Pclass = factor(Pclass),
    Sex = factor(Sex))

#Question 1 - Inspect data
table(titanic$Sex)
table(titanic$Fare)

#Question 2 - Make a Dnesity Plot
titanic %>%
    ggplot(aes(Age,col=Sex)) +
    geom_density() +
    geom_vline(xintercept=17,col = "black")

#Question 3 - Make a Q-Q Plot
params <- titanic %>%
    filter(!is.na(Age)) %>%
    mutate(mean = mean(Age), sd = sd(Age))

params %>%
    ggplot(aes(sample = Age) +
    geom_abline() +
    geom_qq()

head(params)
?geom_abline

#Question 4 - Barplot Survival by sex
titanic %>%
    ggplot(aes(Sex, fill = Survived)) +
    geom_bar(position = position_dodge())

#Question 5 - Density Plot of Survival by age
titanic %>%
    ggplot(aes(Age, col = Survived)) +
    geom_density(alpha = 0.2, count = TRUE)

#Question 6 - Survival by Fare Box Plot
titanic %>%
    filter(Fare != 0) %>%
    group_by(Survived) %>%
    ggplot(aes(Survived, Fare)) +
    geom_boxplot() +
    scale_y_continuous(trans = "log2") +
    geom_jitter()

#Question 7 - Survival by Class barplot
titanic %>%
    group_by(Survived) %>%
    ggplot(aes(Pclass, fill = Survived)) +
    geom_bar()

titanic %>%
    group_by(Survived) %>%
    ggplot(aes(Pclass, fill = Survived)) +
    geom_bar(position = position_fill()) 

titanic %>%
    group_by(Pclass) %>%
    ggplot(aes(Survived, fill = Pclass)) +
    geom_bar(position = position_fill())

#Question 8 - Survival by Age, Sex & Passenger Class grid plot
titanic %>%
    group_by(Pclass) %>%
    ggplot(aes(Age, y= ..count.., fill = Survived)) +
    geom_density(alpha = 0.2) +
    facet_grid(Sex ~ Pclass)
