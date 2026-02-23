# complete the statistics lesson

# import climate data
climate <- read.csv('data/raleigh_prism_climate.csv')

# look at data
library(...)

ggplot(data = climate, aes(x = ..., y = ...)) +
  geom_...()

# statistical test
... <- ...(data = climate, ... ~ ...)
summary(...)

# getting the coefficients
...$coeff[1]
...$coeff[2]

# plot with abline
ggplot(data = climate, aes(x = tmean, y = precip)) +
  geom_point()+
  geom_...(... = precip_lm$coeff[1], ... = precip_lm$coeff[2], color = '...')

# plot with geom_smooth
ggplot(data = climate, aes(x = tmean, y = precip)) +
  geom_point()+
  geom_...(method='...')

# categorical variables
library(dplyr)
climate_season <- climate |>
  mutate(season = case_when((month %in% c(12, 1, 2)) ~ 'winter', 
                            (month %in% c(3, 4, 5)) ~ 'spring',
                            (month %in% c(6, 7, 8) ~ 'summer'), 
                            (month %in% c(9, 10, 11) ~ 'fall')))

# plot the data
ggplot(data = climate_season, aes(x = ..., y = ...)) +
  geom_...() +
  theme_classic()

# analysis
... <- lm(data = climate_season, ... ~ ...)
summary(...)

# water quality data
wq <- read.csv('data/raleigh_water_analysis.csv')
stream_codes <- read.csv('https://maryglover.github.io/bio331/open_data/stream_codes.csv')

wq <- stream_codes |>
  right_join(wq)

## Activity ##



#Turbidity by Site
turb_lm <- lm(data = ..., ... ~ ...)
summary(turb_lm)

ggplot(data = wq, aes(x = Site, y = Turbidity_NTU)) +
  geom_boxplot() +
  theme_classic()

## Homework exercise ##