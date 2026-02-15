# Use this script to complete the summaries lesson. Fill in the blanks (...)

# Read in data
climate <- read.csv('data/raleigh_prism_climate.csv')

library(...)

# Summarize to get precip for each year
climate |>
  ...(...) |>
  ...(average_precip = mean(...)) 

# summarize for precip in the 1990s
climate |>
  ...(year %in% 1990:1999) |>
  group_by(...) |>
  summarize(... = ...(precip), sd_precip = ...(precip))

# Activity

#  What year had the highest average temperature tmean?

# Which year had the highest total precipitation in Raleigh?
  
## Raleigh cycling data
# load data
cycling <- read.csv('https://maryglover.github.io/bio331/open_data/cycling_crashes.csv')

...(cycling)
...elt(cycling)

# summarize by year
... |>
  group_by(...)|>
  summarise(... = ... )

# summarize for weather
cycling |>
  group_by(...)|>
  summarise(...)

# Activity: Summarize the data to determine the number of crashes for each road and then sort to see what road had the most crashes.

## Names 
# read in data
birth <- read.csv('https://maryglover.github.io/bio331/open_data/nc_birth.csv')

# review data
...(birth)

# summarize by most common name
... |>
  group_by(..., ...) |>
  summarize(... = sum(...)) |>
  arrange(...)

# summarize for only females, for each decade
birth |>
  filter(... = )|>
  group_by(..., ... ) |>
  summarize(total = ...(number))

# use slice_max to get just the highest in each group
birth |>
  filter(sex == 'F')|>
  group_by(sex, decade, name) |>
  summarize(total = sum(number)) |> 
  ...(..., n = 1)

# Activity: Filter the data for the 2000’s decade and determine the top 5 names for boys and girls.

## Invertebrate data

# read in the data
invert <- read.csv('https://maryglover.github.io/bio331/open_data/macroinvertebrate_sampling.csv')

head(...)

# Insert the code to determine how many insects each group collected. 


# abundance of each taxa
... |>
  group_by(...)|>
  summarize(... = ...(...))

# save the data
... <- invert |>
  ...(Taxa)|>
  ...(total = sum(N))

# load the package
library(...)

# plot the data
ggplot(... = , aes(x = ..., y = ...))+
  ...()

# fix the bar plot
ggplot(..., aes(x = Taxa, y = total))+
  geom_bar(...)


# rotate the labels
ggplot(..., aes(x = Taxa, y = total))+
  ...(stat = 'identity')+
  theme(axis.text.x = element_text(angle = ...))

# change direction of plot
ggplot(..., aes(...))+
  geom_bar(stat = 'identity')

# get the richness
invert |>
  group_by(...)|>
  summarize(...)

## Water quality data
# read in data
wq <- read.csv('...')

# summarize for turbidity and site
wq |>
  group_by(...) |>
  summarize(...)

# remove Nas
wq |>
  group_by(Site) |>
  summarize(... = mean(Turbidity_NTU, ...))

#### Homework exercises
# add homework exercises