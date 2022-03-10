library(dplyr)
library(ggplot2)
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#1. In this section, I looked at the average number of female incarcerations per county in the united states. 
#This gives a good basis as to what the normal distribution of incarcerations is for women county by county 
#in America. This came out to an average of 35 Females incarcerated per county which is lower than I expected.
  incarceration_data %>% 
  filter(year == max(year)) %>%
  pull(female_jail_pop) %>%
  mean(na.rm = TRUE)

#2. In this section, I found what counties carry the lowest number of incarcerations and the highest number 
  #of incarcerations in the United States. The lowest counties totaled up to 336 counties that do not have 
  #any female incarcerations. On the opposite end, Los Angeles had the most incarcerations, totaling up to 
  #2137 women. This shows the disparity between counties.
  incarceration_data %>% 
  filter(year == max(year)) %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = TRUE)) %>%
  select(state, county_name, female_jail_pop) 

incarceration_data %>% 
  filter(year == max(year)) %>%
  filter(female_jail_pop == min(female_jail_pop, na.rm = TRUE)) %>%
  select(state, county_name, female_jail_pop) 
#3 In this section, I found out how much the average number of females incarcerated by county has changed 
#over the past ten years. This number came out to 1.72, which is just under 2. While this might seem low, 
#it is actually a rather large number as in the previous variable I found the disparity between counties.
avg_female_2008 <- incarceration_data %>% 
  filter(year == 2008) %>%
  pull(female_jail_pop) %>%
  mean(na.rm = TRUE)
avg_female_2018 <- incarceration_data %>% 
  filter(year == 2018) %>%
  pull(female_jail_pop) %>%
  mean(na.rm = TRUE)
change_10_years <- avg_female_2018 - avg_female_2008
change_10_years

#4In this section, I showed the variation in the female incarceration population based on race, and I 
#chose to calculate African American and Latinx populations. What this means is that when the race is 
#introduced to the female population, it shows the variation. 59.4% of the variation in our female jail 
#pop variable can be explained by the black jail population and 54.8% of the variation in our female jail 
#pop variable can be explained by the latinx jail population.
incarceration_data_2018 <- incarceration_data %>% 
  filter(year == 2018)

cor(incarceration_data_2018$female_jail_pop, incarceration_data_2018$black_jail_pop, use = "complete.obs")^2
cor(incarceration_data_2018$female_jail_pop, incarceration_data_2018$latinx_jail_pop, use = "complete.obs")^2