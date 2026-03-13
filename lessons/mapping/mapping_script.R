# You can use this script to complete the class activity and homework exercises for the advanced R lesson

## Spatial data
# load in the packages for plotting and data manipulation
library(...)
library(...)

# save map data for US counties.
map <- map_data("county")

# Filter just for North Carolina
nc <- map |>
  ...(region =="north carolina")

head(nc)

# plot the data in ggplot with the geom_polygon layer

...(data = nc, aes(x = ..., y = ..., group = group)) +
  ...() 

# color based on county, we will also remove the legend
ggplot(data = nc, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(... = subregion))+
  theme_...()+
  theme(legend.position = 'none') 

# using the previous code at a coordinate system


## plotting in sf
# load the package
library(...)

# read in the river data

... <- ...('data/Major_Rivers/')
head(...)

# view geometry
...$geometry[1]

# view boundaries
st_bbox(...)

# view coordinates
st_crs(...)

# plot the sf data
ggplot() +
  ...(data = ..., color = '...')

# view the different streams in the data
... |>
  ...(NAME)

# filter for Cemetery Branch
... |>
  ...(NAME == 'Cemetery Branch') 

# save Cemetery Branch and plot as a separate color on the rivers data
... <- ... |>
  filter(NAME == 'Cemetery Branch')

ggplot() +
  geom_sf(data = ..., color ='...') +
  geom_sf(data = ..., color = '...') + 
  theme_void()

# Add the wake county line

## Plotting points

# save the Meadow Brook park coordinates

park <- ...(c(-78.625, 35.795))

# Add it to the previous plot

# look at the coordinate systems of the park and the previous rivers data
st_bbox(...)
st_bbox(...)

# change the coordinate system of the rivers data
...  <- st_transform(..., crs = 4269)

# set a coordinate system for park data
park <- st_sfc(park, crs = ...)

# plot the rivers data and the park coordinate
ggplot() +
  geom_sf(data = ..., color = '...') + 
  ...(data = park, color = '...') +
  theme_void()


# plot as a gps coordinate
ggplot() +
  geom_sf(data = ..., color = '...')+
  geom_point(aes(... = -78.625, ... = 35.795), color = '...') +
  theme_void()

## Water quality data

# import stream codes data
stream_codes <- read.csv('https://maryglover.github.io/bio331/open_data/stream_codes.csv')

# view the data
...(stream_codes)

# Add the stream sites to the stream map and make it pretty!




# Plotting data on maps
# complete the activity here