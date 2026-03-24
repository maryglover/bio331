# read with impervious

imp <- read.csv('data/raleigh_water_impervious.csv')

# water quality score
wq <- read.csv('data/raleigh_water_analysis.csv')
wq_rank <- wq |>
  mutate(DO_rank = case_when(do_percent_sat >= 91 ~ 4, 
                             do_percent_sat < 91 & do_percent_sat >= 71 ~ 3, 
                             do_percent_sat < 71 & do_percent_sat >= 51 ~ 2, 
                             do_percent_sat < 51 ~ 1))  |>
  mutate(ph_rank = case_when(pH >6 & pH < 8 ~ 4, 
                             pH > 5 & pH <= 6 ~ 3, 
                             pH < 9 & pH >= 8  ~ 3, 
                             pH <= 5 | pH >= 9 ~ 1 )) |>
  mutate(nitrate_rank = case_when(NO2_NO3_mg_L >= 20 ~ 1, 
                                  NO2_NO3_mg_L >=5 & NO2_NO3_mg_L < 20 ~ 2, 
                                  NO2_NO3_mg_L < 5 ~ 3)) |>
  mutate(phosp_rank = case_when(Phosphorus_total_mg_L < 1 ~ 4, 
                                Phosphorus_total_mg_L >=1 & Phosphorus_total_mg_L < 2 ~ 3, 
                                Phosphorus_total_mg_L >= 2 ~ 2)) |>
  mutate(turbidity_rank = case_when(Turbidity_NTU == 0 ~4, 
                                    Turbidity_NTU >0 & Turbidity_NTU <= 40 ~ 3, 
                                    Turbidity_NTU > 40 & Turbidity_NTU <= 100 ~ 2, 
                                    Turbidity_NTU> 100 ~ 1)) |>
  mutate(E_coli_MPN = case_when(E_coli_MPN_100mL >0  ~ 1, 
                                E_coli_MPN_100mL == 0  ~ 3)) |>
  mutate(E_coli_CFU = case_when(E_coli_CFU_100mL >0  ~ 1, 
                                E_coli_CFU_100mL == 0  ~ 3)) |>
  mutate(E_coli_rank = coalesce(E_coli_CFU, E_coli_MPN))

wq_rank <- wq_rank |>
  select(-E_coli_CFU, - E_coli_MPN) |>
mutate(wq_score = DO_rank + ph_rank + nitrate_rank + phosp_rank + turbidity_rank + E_coli_rank)

library(tidyr)
wq |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-') |>
  group_by(Year)|>
  summarize(n = n())


wq_rank |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-') |>
  group_by(Year) |>
  summarize(wq = mean(wq_score, na.rm = T))

wq_rank |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-', remove = F) |>
  filter(Year == 2022) |> 
  select(Site, Date, DO_rank, phosp_rank, nitrate_rank, E_coli_rank, wq_score) |>
  filter(Site == 'BB2')


wq_rank|>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-', remove = F) |>
  group_by(Site, Date, Year) |>
  summarize(n = n()) |>
  filter(n > 1) |>
  ungroup() |>
  distinct(Year)

# There are lots of rows with NAs, For each date after 2019, some values in one time, and some in another
wq_rank |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-', remove = F) |>
  filter(Year > 2020) |>
  filter(is.na(wq_score) == T) |>
  group_by(Year)|>
  summarize(n = n())


wq_rank |>
  group_by(Site, Date ) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1,3) |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-', remove = F) |>
  filter(Year > 2020) |>
  select(Site, Date, ph_rank, phosp_rank, turbidity_rank, nitrate_rank, E_coli_rank, wq_score) |>
  filter(Site == 'PHB12')


# redo the scores AFTER drop NAs from the original water quality data. 

wq_fix <- wq |>
  group_by(Site, Date ) %>%
  fill(everything(), .direction = "down") %>%
  fill(everything(), .direction = "up") %>%
  slice(1,3)

wq_rank <- wq_fix |>
  mutate(DO_rank = case_when(do_percent_sat >= 91 ~ 4, 
                             do_percent_sat < 91 & do_percent_sat >= 71 ~ 3, 
                             do_percent_sat < 71 & do_percent_sat >= 51 ~ 2, 
                             do_percent_sat < 51 ~ 1))  |>
  mutate(ph_rank = case_when(pH >6 & pH < 8 ~ 4, 
                             pH > 5 & pH <= 6 ~ 3, 
                             pH < 9 & pH >= 8  ~ 3, 
                             pH <= 5 | pH >= 9 ~ 1 )) |>
  mutate(nitrate_rank = case_when(NO2_NO3_mg_L >= 20 ~ 1, 
                                  NO2_NO3_mg_L >=5 & NO2_NO3_mg_L < 20 ~ 2, 
                                  NO2_NO3_mg_L < 5 ~ 3)) |>
  mutate(phosp_rank = case_when(Phosphorus_total_mg_L < 1 ~ 4, 
                                Phosphorus_total_mg_L >=1 & Phosphorus_total_mg_L < 2 ~ 3, 
                                Phosphorus_total_mg_L >= 2 ~ 2)) |>
  mutate(turbidity_rank = case_when(Turbidity_NTU == 0 ~4, 
                                    Turbidity_NTU >0 & Turbidity_NTU <= 40 ~ 3, 
                                    Turbidity_NTU > 40 & Turbidity_NTU <= 100 ~ 2, 
                                    Turbidity_NTU> 100 ~ 1)) |>
  mutate(E_coli_MPN = case_when(E_coli_MPN_100mL >0  ~ 1, 
                                E_coli_MPN_100mL == 0  ~ 3)) |>
  mutate(E_coli_CFU = case_when(E_coli_CFU_100mL >0  ~ 1, 
                                E_coli_CFU_100mL == 0  ~ 3)) |>
  mutate(E_coli_rank = coalesce(E_coli_CFU, E_coli_MPN))


wq_rank <- wq_rank |>
  select(-E_coli_CFU, - E_coli_MPN) |>
  mutate(wq_score = DO_rank + ph_rank + nitrate_rank + phosp_rank + turbidity_rank + E_coli_rank)

wq_rank |>
  separate(Date, into = c('Year', 'Month', 'Day'), sep = '-') |>
  group_by(Year) |>
  summarize(wq = mean(wq_score, na.rm = T))

wq_rank |>
  group_by(Site, Date) |>
  summarize(n = n()) |>
  filter(n != 1)

wq_rank <- wq_rank |>
  filter(Time != 0.380555556) 

write.csv(wq_rank, 'data/water_quality_score.csv', row.names = F)
write.csv(wq_rank, 'impervious_surface/water_quality_score.csv', row.names = F)

