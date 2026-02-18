# You can use this script to complete the class activity and homework exercises for the advanced R lesson

## Joins

# import data sets
growth <- read.csv('https://maryglover.github.io/bio331/open_data/growth.csv')

photosynthesis <- read.csv('https://maryglover.github.io/bio331/open_data/photosynthesis.csv')

# load package
library(...)

# join
... |>
  ...(..., by = 'Plant_ID')

# inner join
growth |>
  ...(photosynthesis)

# full join
growth |>
  ...(photosynthesis)

# Activity: try right and left join

## Aquatic macroinvertebrate example

# import data
invert <- read.csv('https://maryglover.github.io/bio331/open_data/macroinvertebrate_sampling.csv')

head(...)

# summarize the data
... |>
  group_by(...)|>
  summarize(richness = ...)

# summarize for the total for each group

# summarize for each taxa
invert |>
  group_by(... = )|>
  summarize(total = ...(N))

# save the data
invert_summary <- ... |>
  group_by(... = )|>
  summarize(total = ...(N))

# graph the data
library(...)
ggplot(..., aes(x = ..., y = ...))+
  geom_bar() # this doesn't work

# set argument in geom_bar
ggplot(..., aes(x = ..., y = ...))+
  geom_bar(stat = '...')

# rotate
ggplot(invert_summary, aes(x = Taxa, y = total))+
  geom_bar(stat = 'identity')+
  ...(axis.text.x = element_text(angle = 90))

# change direction
ggplot(..., aes(... , ...))+
  geom_bar(stat = 'identity')

# join
pollute_class <- read.csv('https://maryglover.github.io/bio331/open_data/pollution_class.csv')

head(...)

# activity join the data and make a bar graph


### Dates
climate <- read.csv('data/raleigh_prism_climate.csv')
head(climate)

# load package
...(lubridate)

# make date
... |>
  ...(date = ...(year, month))

# save and plot
... <- climate |>
  ...(date = ...(year, month))

ggplot(..., aes(x = ..., y = ...)) +
  geom_...()

# water quality
wq <- read.csv('data/raleigh_water_analysis.csv')
head(wq)

# set date
... <- wq |>
  ...(Date = ...(Date)) 

head(...)

# Activity


# extract date
wq |>
  mutate(... = ...(Date))

# summarize
wq |>
  ...(... = year(Date)) |>
  group_by(...)|>
  summarize(turbidity = mean(..., na.rm =T), nitrogen = mean(..., na.rm = T))

# join
stream_codes <- read.csv('https://maryglover.github.io/bio331/open_data/stream_codes.csv')

head(stream_codes)

# join the data

# Homework exercises