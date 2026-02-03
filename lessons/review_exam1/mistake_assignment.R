# Fix the mistakes

# read in the data frame
raleigh_climate <- read.csv(data/raleigh_prism_climate.csv)

# view the first few rows
head(raliegh_climate)

# get the average precipitation
sum(raleigh_climate$precipitation)

# filter for just february
library(dplyer)
raleigh_climate |>
  filter(month = 2)

# select just the year and tmean columns
raleigh_climate
select(year, tmean)

# filter for 2020 and save as a new object
climate2020 < - raleigh_climate |>
  filter(year == 2020)

# arrange from 2020 climate from greatest to least tmax
climate2020 |>
  arrange(tmax)

# mutate and to get tmax - tmin and then arrange in order from greatest to least

climate2020 |>
  mutate(diff = tmax - tmean)
climate2020 |>
  arrange(-diff)

# graph month and tmean in a line graph
libary(ggplot2)
ggplot(climate2020, x = month, y = tmean)+
  geom_line()

# graph histogram of precipitation
geom_histogram(climate2020, aes(x = precip))