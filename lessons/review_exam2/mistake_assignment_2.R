# Fix the mistakes

# read in the data frame
raleigh_climate <- read csv('data/raleigh_prism_climate.csv')

#load the library
liberary(dplyr)

# make a summary table to view the number of values for each year.  There is a mistake in each line
raleigh_climate +
  group_by('year')|>
  summarize(n = n()

# Make a summary table to get the median precip for each year and then filter for only years after 2000. There is a mistake in each line
raleigh_clim |>
  group_by(month) |>
  summarize(median_precip = mean(precip)) |>
  filter(year < 2000)
 
# Make a summary table to get the average tmean for each year. Then arrange from greatest to least. 
"raleigh_climate" |>
  group_by(year) +
  summarize(mean_tmean = mean(tmean)) |>
  arrange(tmean)

# load in the package to deal with dates
library(date)

# read in the data here. This shows hurricane data. There is no mistake!
hurricane <- read.csv('https://maryglover.github.io/bio331/open_data/storms.csv')

# view the data
head('hurricane')

# format the date column into the date format
hurricane |>
  make_date(date = ymd(date))

hurricane |>
  mutate(date = ymd(date))

# extract the year from the date and make it a new column. Then make a summary table showing the number of hurricanes each year. There is a mistake on each line
hurricanes |>
  mutate(year = year(Date)) |>
  group_by(Year) |>
  summarize(number = number())


# save the code from the previous summary table as a new object. 

year_hurricane -< hurricane |>
  mutate(year = year(Date)) |>
  group_by(Year) |>
  summarize(number = number())

year_hurricane <- hurricane |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarize(number = n())

# make a bar graph for the number of hurricanes in every year. There is a mistake on each line
library(ggplot)
ggplot(year_hurricane, aes(y = year, x = number)) +
  geom_bar()
