## Create water quality data files for students to view in class

library(dplyr)
library(stringr)
library(tidyr)

wq <- read.csv("../data/raleigh_wq_2008_2023.csv")
summary(wq)
colnames(wq)
head(wq)
tail(wq)

# already you notice that there are < (not numeric)

# make a tidy but don't clean up for students to look at. 

wq_tidy <- wq |>
  distinct() |> # get rid of one duplicate
  mutate(new_name = paste(Parameter, Unit, sep = '_')) |>
  select(-Unit, -PQL) |>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = new_name, values_from = Result) |>
  rename_with(~ gsub("/", '_', .x))

# do colnames and what do you notice
head(wq_tidy)
colnames(wq_tidy)

## Can see that there is an issue with some do_percent_sat_mg/L
wq |>
  filter(Parameter == 'do_percent_sat')

# different units for E. coli
wq |>
  filter(Parameter == 'E_coli') |>
  distinct(Unit)

wq_tidy |>
  distinct(Site)|>
  print(n=28)


### clean up the data

# Make 0 if has a <
wq$Result <- ifelse(grepl("<", wq$Result), "0", wq$Result)

# if not detected, get a 0 instead of ND
wq.det <- wq |>
  mutate(Result = recode(Result, ND = '0')) 
wq.det$Result<-as.numeric(wq.det$Result)

wq_clean <- wq.det |>
  mutate(Unit = case_when(Parameter == 'do_percent_sat' ~ 'percent_sat', 
                          Unit == 'MPN' ~ 'MPN/100mL',
                          .default = Unit)) |>
  distinct() |> # get rid of one duplicate
  mutate(new_name = paste(Parameter, Unit, sep = '_')) |>
  select(-Unit, -PQL) |>
  pivot_wider(id_cols = c(Site, Date, Time), names_from = new_name, values_from = Result) |>
  rename(do_percent_sat = do_percent_sat_percent_sat, do_mg_L = `do_mgl_mg/L`, pH = pH_std_unit) |>
  rename_with(~ gsub("/", '_', .x))

write.csv(wq_tidy, 'raleigh_wq_tidy.csv', row.names = F)
write.csv(wq_clean, 'raleigh_wq_clean.csv', row.names = F)
