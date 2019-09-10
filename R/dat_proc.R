# data processing ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(lubridate)

# depth levels labels
deplev <- c('Baseline', '5', '30')
deplab <- c('Baseline', '5m', '30m')

# import all as nested list
wkbk <- here("data/raw/", "Copy of project data for Nina.xlsx") 
dat <- wkbk %>% 
  excel_sheets %>% 
  enframe %>% 
  group_by(value) %>% 
  mutate(
    data = purrr::map(value, ~ read_excel(wkbk, value))
  )

# physiological data ------------------------------------------------------

##
# growth
gro <- dat %>% 
  filter(value %in% 'growth') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  mutate(
    var = 'gro', 
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  ) %>% 
  rename(
    val = growth_month
  )

##
# edge shell strength
edg <- dat %>% 
  filter(value %in% 'shell strength-edge') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  rename(
    var = Hole, 
    val = Mpa
  ) %>% 
  mutate(
    var = tolower(var) %>% substr(1, 3),
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  )

##
# middle shell strength
mid <- dat %>% 
  filter(value %in% 'shell strength-middle') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  rename(
    var = Hole, 
    val = Mpa
  ) %>% 
  mutate(
    var = tolower(var) %>% substr(1, 3),
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  )

##
# total lipids
lip <- dat %>% 
  filter(value %in% 'total lipids') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  mutate(
    var = 'lip',
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  ) %>% 
  rename(
    val = lipidspergDW, 
    Species = species
  )

##
# fatty acids
fat <- dat %>% 
  filter(value %in% 'fatty acids') %>% 
  pull(data) %>% 
  .[[1]]

# separate mussels
fatmus <- fat %>% 
  filter(Species %in% 'mussel') %>% 
  gather('var', 'val', -Species, -`Depth (m)`, -Season)

# separate scallops
fatsca <- fat %>% 
  filter(!Species %in% 'mussel')
names(fatsca) <- c(names(fatsca)[1:3], fatsca[1, -c(1:3)]) %>% unlist
fatsca <- fatsca[-1, !is.na(names(fatsca))] %>% 
  gather('var', 'val', -Species, -`Depth (m)`, -Season)

# rejoin
fat <- bind_rows(fatsca, fatmus) %>% 
  rename(
    season = Season
  ) %>% 
  mutate(
    val = as.numeric(val),
    var = paste0('fat', var),
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
    )

# join all physio data
phys <- bind_rows(gro, edg, mid, lip, fat) %>% 
  mutate(
    date = case_when(
      season == 'Baseline' ~ mdy('12/21/2017'),
      season == 'winter' ~ mdy('3/22/2017'),
      season == 'spring' ~ mdy('6/15/2017'),
    )
  )

# # a simple plot
# phys %>% 
#   filter(Species == 'scallop') %>% 
#   filter(grepl('^fat', var)) %>% 
#   na.omit() %>% 
#   ggplot(aes(x = season, y = val)) + 
#   geom_boxplot() + 
#   facet_grid(`Depth (m)` ~ var, scales = 'free')

# environmental data ------------------------------------------------------

##
# carbon chemistry
crb <- dat %>% 
  filter(value %in% 'carbon chemistry-bottle samples') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  rename(
    season = Season,
    date = Date,
    dic = `DIC (µmol/kg)`,
    ta = `TA (µmol/kg)`, 
    ph = `pH Total In-situ`, 
    pco2 = `pCO2 (uatm)`, 
    calcite = `calcite saturation state`, 
    aragonite = `aragonite saturation state`, 
    revelle = `Revelle factor`
  ) %>% 
  mutate(
    date = as.Date(date), 
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  ) %>% 
  gather('var', 'val', -`Depth (m)`, -season, -date)

##
# physical
phy <- dat %>% 
  filter(value %in% 'temp,DO,salinity') %>% 
  pull(data) %>% 
  .[[1]] %>% 
  rename(
    season = Season,
    date = Date,
    sal = Salinity, 
    temp = temperature,
    do = DO
  ) %>% 
  mutate(
    date = as.Date(date), 
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  ) %>% 
  gather('var', 'val', -`Depth (m)`, -season, -date)

env <- bind_rows(crb, phy)

# # a simple plot
# env %>%
#   ggplot(aes(x = date, y = val, group = `Depth (m)`, colour = `Depth (m)`)) +
#   geom_line() +
#   geom_point() + 
#   facet_grid(var ~ ., scales = 'free')


