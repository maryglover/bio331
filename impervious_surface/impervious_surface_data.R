# impervious surfaces data
# NCLD data downloaded from MRLC 

library(FedData)
library(sf)
# install_github("r-spatial/sf")

county <- st_read('data/Wake_County_Line-shp/')
county <- st_transform(county, crs = 4269)

imp2021 <- get_nlcd(template = county, label = 'wake', year = 2021, dataset = 'impervious')

streams <- read.csv('open_data/stream_codes.csv')
#raster::extract(impervious, streams[,c(4,3)], buffer = 500, fun = mean)
#raster::extract(imp2021, streams[,c(4,3)], buffer = 500, fun = mean)


# sf points
library(ggplot2)
library(tidyterra)


streams_point <- st_as_sf(streams, coords = c("long","lat"), crs = 4326) |>
  st_transform(crs = st_crs(imp2021))

streams_buff <-st_buffer(streams_point, 500)

cbind(streams, streams_imperv)

imp2008 <- get_nlcd(template = county, label = 'wake', year = 2008, dataset = 'impervious')
imp2011 <- get_nlcd(template = county, label = 'wake', year = 2011, dataset = 'impervious')
imp2013 <- get_nlcd(template = county, label = 'wake', year = 2013, dataset = 'impervious')
imp2016 <- get_nlcd(template = county, label = 'wake', year = 2016, dataset = 'impervious')
imp2019 <- get_nlcd(template = county, label = 'wake', year = 2019, dataset = 'impervious')

streams_imperv_21 <- terra::extract( imp2021,streams_buff ) |>
  summarize(imp_2021 = mean(mrlc_download__NLCD_2021_Impervious_L48), .by=ID)
streams_imperv_08 <- terra::extract( imp2008,streams_buff ) |>
  summarize(imp_2008 = mean(mrlc_download__NLCD_2008_Impervious_L48), .by=ID)
streams_imperv_11 <- terra::extract( imp2011,streams_buff ) |>
  summarize(imp_2011 = mean(mrlc_download__NLCD_2011_Impervious_L48), .by=ID)
streams_imperv_13 <- terra::extract( imp2013,streams_buff ) |>
  summarize(imp_2013 = mean(mrlc_download__NLCD_2013_Impervious_L48), .by=ID)
streams_imperv_16 <- terra::extract( imp2016,streams_buff ) |>
  summarize(imp_2016 = mean(mrlc_download__NLCD_2016_Impervious_L48), .by=ID)
streams_imperv_19 <- terra::extract( imp2019,streams_buff ) |>
  summarize(imp_2019 = mean(mrlc_download__NLCD_2019_Impervious_L48), .by=ID)

library(tidyr)
impervious_data <- streams_imperv_08|>
  left_join(streams_imperv_11)|>
  left_join(streams_imperv_13)|>
  left_join(streams_imperv_16)|>
  left_join(streams_imperv_19)|>
  left_join(streams_imperv_21)|>
  cbind(streams) |>
  pivot_longer(cols = starts_with('imp_' ), values_to = 'impervious_surface')|>
  separate(name, c('imp', 'year'), sep = '_' )|>
  select(-ID, - imp)
  
write.csv(impervious_data, 'impervious_surface/impervious_surface.csv')

wq_rank <- read.csv('water_quality_manip/water_quality_score.csv')
head(wq_rank)
library(lubridate)

wq_imp2 <- wq_rank |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-', remove = F) |>
  full_join(impervious_data) |>
  select(-lat, -long, -year, -month, -day) |>
  left_join(streams)

write.csv(wq_imp2, 'impervious_surface/raleigh_water_impervious.csv', row.names = F)

# what site has the highest impervious surface in 2021

wq_imp2 |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-', remove = F) |>
  filter(year == 2021) |>
  distinct(Site, Stream, impervious_surface) |>
  arrange(-impervious_surface)

# average wq_score and impervious surface
# in 2021

wq_imp2 |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-', remove = F) |>
  filter(year == 2021) |>
  group_by(Site, Stream) |>
  summarize(wq = mean(wq_score, na.rm = T), impervious = mean(impervious_surface))|>
  arrange(-wq)

# overall
wq_imp2 |>
  group_by(Site, Stream) |>
  summarize(wq = mean(wq_score, na.rm = T), impervious = mean(impervious_surface, na.rm = T))|>
  arrange(-wq)


ggplot(wq_imp2, aes(x = impervious_surface, y = wq_score))+
  geom_point() +
  geom_smooth()

ggplot(wq_imp2, aes(x = impervious_surface, y = NO2_NO3_mg_L)) +
  geom_point() 

ggplot(wq_imp2, aes(x = impervious_surface, y = Turbidity_NTU)) +
  geom_point() 
ggplot(wq_imp2, aes(x = impervious_surface, y = E_coli_MPN_100mL)) +
  geom_point() 

wq_imp2 |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-', remove = F) |>
  group_by(year) |>
  summarise(imp = mean(impervious_surface, na.rm = T)) |>
  filter(imp > 1)
 
wq_imp2<- wq_imp2 |>
  separate(Date, into = c('year', 'month', 'day'), sep = '-', remove = F) 

wq_imp2 |>
  filter(year %in% c(2008, 2011, 2013, 2016, 2019, 2021)) |>
  distinct(Site, year, impervious_surface) |>
  ggplot(aes(x = year, y = impervious_surface, group = Site)) +
  geom_line()

ggplot()+
  geom_spatraster(data = imp2021) +
  scale_fill_continuous(na.value = 'transparent')  +
  geom_sf(data = streams_point)
