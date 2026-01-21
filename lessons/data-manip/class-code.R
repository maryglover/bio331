# Data manipulation class exercise

yahtzee <- read.csv('data/yahtzee-example.csv')
yahtzee
rownames(yahtzee) <- c("ones", "twos", "" )

vinny <- sum(yahtzee$Vinny)
vinny
mary <- sum(yahtzee$Mary)
mary
quentin <- sum(yahtzee$Quentin)
quentin
yahtzee$Vinny
sum(yahtzee$Vinny[1:6])
vinny <- vinny + 35

sum(yahtzee$Mary[1:6])

install.packages('dplyr')
library(dplyr)


max(vinny, mary, quentin)

climate <- read.csv("data/raleigh_prism_climate.csv")
climate

head(climate)

climate |> 
  head()

climate |>
  filter(year > 2020, month == 1)

climate |>
  filter(year == 2023 | year == 1985)

climate |>
  filter(year %in% c(1991, 1993, 1995))

climate |>
  filter(year %in% 1990:1999)
  
climate2023 <- climate |>
  filter(year == 2023)

climate2023

climate |>
  select(precip, year, month) |>
  head()

climate |>
  select(-precip) |>
  head()

climate |>
  select(year, month, precipitation = precip ) |>
  head()

climate |> 
  filter(month == 1 ) |>
  select(year, tmax)

climate |>
  mutate(temp_difference = tmax - tmin) |>
  arrange(temp_difference)|>
  head()

climate |>
  mutate(temp_difference = tmax - tmin) |>
  arrange(desc(temp_difference))|>
  head()

head(climate)

climate |>
  distinct(year) |>
  nrow()

library(dplyr)
climate |>
  mutate(avg_temp = (tmin + tmax)/2)

climate |>
  group_by(year) |>
  summarize(average_temp = mean(tmean),
            average_precip = mean(precip),
            N = n())

climate |>
  filter(year %in% 1990:1999) |>
  group_by(year) |>
  summarize(average_temp = mean(tmean), sd_temp = sd(tmean), average_precip = mean(precip), sd_precip = sd(precip))

climate |>
  group_by(year) |>
  summarize(total_precip = sum(precip)) |>
  arrange(desc(total_precip))


climate |>
  rename(avg_temperature = tmean) |>
  head()

raleigh_example |>
  group_by(City) |>
  summarize(Temp = mean(Temperature), Precip = mean(Precipitation))

recode_raleigh <- raleigh_example |>
  mutate(City = recode(City, Raleigh = "Raleigh, NC"))

recode(raleigh_example$City, Raleigh = "Raleigh, NC")

recode_raleigh |>
  group_by(City) |>
  summarize(Temp = mean(Temperature), Precip = mean(Precipitation))



#notes

common_names <- read.csv('lessons/data-manip/data/NC.TXT', header=FALSE)
head(common_names)

library(dplyr)
head(common_names)
colnames(common_names) <- c('State', 'Sex', 'Year', 'Name', 'N')
common_names |>
  select(Year) |>
  min()

common_names |>
  filter(Year == '1990', Sex == 'F') |>
  arrange(desc(N))

# Most common name is Brittany, Ashley, Jessica, Amanda, Sarah

common_names |>
  filter(Year == '2000', Sex == 'F') |>
  arrange(desc(N))

# Most common are Hannah, Madison, Emily, Sarah, Taylor

common_names |>
  filter(Year == '2006', Sex == 'M') |>
  arrange(desc(N))



movies <- read.csv('lessons/data-manip/data/movies.csv')
head(movies)

colnames(movies)

movies_short <- movies |> 
  select(year, title, budget, budget_2013, domgross_2013, intgross_2013, runtime, rated, director, imdb_rating)

movies |>
  mutate(budget = as.numeric(budget), bu )

movies_short |> 
  mutate(profit = as.numeric(intgross_2013) - as.numeric(budget_2013)) |>
  arrange(-profit) |>
  filter(is.na(profit)==FALSE) |>
  filter(year==2000)


## What is the highest rated G movie
## filter for the movies that were released in 1990
## sum the gross of all movies in 2002
## what had a higher average imdb score 1990 or 2000
## how many moives in 1980

filter(movies_short, year == 1980)

# what movie had the highest budget

movies_short |> filter(rated == 'G') |> arrange(-imdb_rating) |>tail()
