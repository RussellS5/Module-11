#Module 11 Homework: Linear Regression
#Russell Smith

rm(list=ls(all=TRUE))

install.packages("tidyverse")
install.packages("modelr")
install.packages("broom")
install.packages("corrr")
install.packages("ggplot2")

library(tidyverse)
library(modelr)
library(broom)
library(corrr)
library(ggplot2)


#Reading csv files
gdp <- read_csv("data/gdp-2017.csv")
meat <- read_csv("data/meat-production-2017.csv")
pop <- read_csv("data/total-population-2017.csv")


#Sum the amount of meat produced per country
meat <- aggregate(meat$meat_produced, by=list(country=meat$country), FUN=sum, na.rm = T)


#joining all data files
meat_per_country <- left_join(meat, pop)
meat_per_country <- left_join(meat_per_country, gdp)
colnames(meat)[2] <- c("meat_produced")
meat_per_country <- na.omit(meat_per_country)


#Visualize data

ggplot(data = meat_per_country) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = .5) +
  labs(x = "GDP", y = "Meat Produced")

ggplot(data = meat_per_country) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = .5) +
  labs(x = "Total Population", y = "Meat Produced")

#Normalizing data

#creating a function o normalize our data
normalize <- function(x) {
  n <- (x - min(x)) / (max(x) - min(x))
  return(n)
}

#Applying the normalize function to the data
meat_per_country %>%
  mutate(meat_produced = normalize(meat_produced),
         gdp = normalize(gdp),
         total_population = normalize(total_population)) -> meat_per_country_norm 

summary(meat_per_country_norm)

ggplot(data = meat_per_country_norm) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = 0.5) +
  labs(x = "GDP", y = "Meat Produced") +
  lims(x = c(0,0.05),
       y = c(0,0.15)) +
  theme_bw()

ggplot(data = meat_per_country_norm) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = .5) +
  labs(x = "Total Population", y = "Meat Produced") +
  theme_bw()

#Calculate and visualize correlations

#Remove the country variable since the correlation function only applies to numeric data
meat_per_country_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  fashion()

#Visualizing correlation with rplot()
meat_per_country_norm %>%
  dplyr::select(-country) %>%
  correlate() %>%
  rplot()




#Fit regression models

#Creating a data frame that includes residuals and modeled values through augment()

#meat_produced as a function of gdp
gdp_fit <- lm(meat_produced ~ gdp,meat_per_country_norm)
gdp_fit_a <- augment(gdp_fit)
tidy(gdp_fit)
glance(gdp_fit)
ggplot(data = gdp_fit_a) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = .5) +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0,0.15),
       y = c(0,0.15)) +
  labs(x = "Meat Produced - fitted", y = "Meat Produced - observed")
ggplot(data = gdp_fit_a) +
  geom_point(mapping = aes(x = .fitted, y = .resid), alpha = .5) +
  lims(x = c(0,0.15),
       y = c(-0.2,0.2)) +
  labs(x = "Meat Produced", y = "Residuals")


#meat_produced as a function of total_population
pop_fit <- lm(meat_produced ~ total_population, meat_per_country_norm)
pop_fit_a <- augment(pop_fit)
tidy(pop_fit)
glance(pop_fit)
ggplot(data = pop_fit_a) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = .5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Meat Produced - fitted", y = "Meat Produced - observed")
ggplot(data = pop_fit_a) +
  geom_point(mapping = aes(x = .fitted, y = .resid), alpha = .5) +
  lims(x = c(0,0.15),
       y = c(-0.2,0.2)) +
  labs(x = "Meat Produced", y = "Residuals")

  
#meat_produced as a function of gdp, total_population, and the interaction between gdp and total_population
all_fit <- lm(meat_produced ~ gdp + total_population + (gdp * total_population),meat_per_country_norm)
all_fit_a <- augment(all_fit)
tidy(all_fit)
glance(all_fit)
ggplot(data = all_fit_a) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = .5) +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0,0.15),
       y = c(0,0.15)) +
  labs(x = "Meat Produced - fitted", y = "Meat Produced - observed")
ggplot(data = all_fit_a) +
  geom_point(mapping = aes(x = .fitted, y = .resid), alpha = .5) +
  lims(x = c(0,0.15),
       y = c(-0.2,0.2)) +
  labs(x = "Meat Produced", y = "Residuals")

  
#log(meat_produced) as a function of log(gdp)
log_gdp_fit <- lm(log(meat_produced + 0.0001) ~ log(gdp + 0.0001), data = meat_per_country_norm)
log_gdp_fit_a <- augment(log_gdp_fit)
colnames(log_gdp_fit_a)[1] <- c("meat_produced")
colnames(log_gdp_fit_a)[2] <- c("gdp")
str(log_gdp_fit_a)
tidy(log_gdp_fit)
glance(log_gdp_fit)
ggplot(data = log_gdp_fit_a) +
  geom_point(mapping = aes(x = gdp, y = meat_produced), alpha = .5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Meat Produced - fitted", y = "Meat Produced - observed")
ggplot(data = log_gdp_fit_a) +
  geom_point(mapping = aes(x = .fitted, y = .resid), alpha = .5) +
  labs(x = "Meat Produced", y = "Residuals")


#log(meat_produced) as a function of log(total_population)
log_pop_fit <- lm(log(meat_produced + 0.0001) ~ log(total_population + 0.0001), data = meat_per_country_norm)
log_pop_fit_a <- augment(log_pop_fit)
colnames(log_pop_fit_a)[1] <- c("meat_produced")
colnames(log_pop_fit_a)[2] <- c("total_population")
tidy(log_pop_fit)
glance(log_pop_fit)
ggplot(data = log_pop_fit_a) +
  geom_point(mapping = aes(x = total_population, y = meat_produced), alpha = .5) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Meat Produced - fitted", y = "Meat Produced - observed")
ggplot(data = log_pop_fit_a) +
  geom_point(mapping = aes(x = .fitted, y = .resid), alpha = .5) +
  labs(x = "Meat Produced", y = "Residuals")


#Evaluation of residuals
sum(gdp_fit_a$.resid)
sum(pop_fit_a$.resid)
sum(all_fit_a$.resid)
sum(log_gdp_fit_a$.resid)
sum(log_pop_fit_a$.resid)



#I believe that meat_produced as a function of total_population is the best for predicting the meat produced by a country.
# By looking at the observed vs. fitted plots, I am able to rule out the 4th and 5th models, since the distance the data is 
# from the 1-1 line is greater for these models indicating an unoptimized fit. By summing all of the residuals, I can see
# that the 2nd model (meat_produced as a function of total_population) is the best model since the summed residuals are
# closest to 0.
















