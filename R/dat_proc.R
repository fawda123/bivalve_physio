# data processing ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(lubridate)
library(imputeTS)

source('R/funcs.R')

# depth levels labels
deplev <- c('Baseline', '5', '30')
deplab <- c('Baseline', '5m', '30m')

# import all as nested list
wkbk <- here::here("data/raw/", "Copy of project data for Nina.xlsx") 
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
      season == 'Baseline' ~ mdy('12/21/2016'),
      season == 'winter' ~ mdy('3/22/2017'),
      season == 'spring' ~ mdy('6/15/2017'),
    )
  )

# need to duplicate depth baseline, same beginning measurements apply to both 
physbs5 <- phys %>% 
  filter(`Depth (m)` %in% 'Baseline') %>% 
  mutate(
    `Depth (m)` = '5',
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
    )

physbs30 <- phys %>% 
  filter(`Depth (m)` %in% 'Baseline') %>% 
  mutate(
    `Depth (m)` = '30',
    `Depth (m)` = factor(`Depth (m)`, levels = deplev, labels = deplab)
  )

biodat <- phys %>% 
  filter(!`Depth (m)` %in% 'Baseline') %>% 
  bind_rows(physbs5, physbs30)

# # a simple plot
# biodat %>% 
#   filter(Species == 'scallop') %>% 
#   filter(grepl('^fat', var)) %>% 
#   na.omit() %>% 
#   ggplot(aes(x = season, y = val)) + 
#   geom_boxplot() + 
#   facet_grid(`Depth (m)` ~ var, scales = 'free')

save(biodat, file = 'data/biodat.RData', compress = 'xz')

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

envdat <- bind_rows(crb, phy)

# # a simple plot
# envdat %>%
#   ggplot(aes(x = date, y = val, group = `Depth (m)`, colour = `Depth (m)`)) +
#   geom_line() +
#   geom_point() + 
#   facet_grid(var ~ ., scales = 'free')

save(envdat, file = 'data/envdat.RData', compress = 'xz')

# bivalve extrapolation ---------------------------------------------------

# bivalve data are for three time periods: baseline (duplicated for each depth), winter, and spring (three dates)
# there are also only two locations (deep/shallow)
# so we can only compare n = 3 or n = 2 for each enviromental/physiological combination (some parameters don't have baseline)
# however, we might interpolate physiological data between baseline/winter/spring observations at dates where we have env data
# this might be a stretch, but perhaps okay if we use spearman which is rank based and assumes only monotonic

data(biodat)
data(envdat)

# environmental sampling dates to extrapolate
envdts <- envdat %>% 
  pull(date) %>% 
  unique 

# first take average of physio samples at each depth/sample date
physagg <- biodat %>% 
  group_by(`Depth (m)`, date, var, Species) %>% 
  summarise(val = mean(val)) %>% 
  na.omit # some fatty acids not shared betwen spp

# nest, fit individual mods, interpolate
physprd <- physagg %>% 
  group_by(`Depth (m)`, var, Species) %>% 
  nest %>% 
  mutate(
    data = purrr::map(data, function(x){
      
      # fit lm, predict with env dates, return predictions
      md <- lm(val ~ date, x)
      prd <- predict(md, newdata = data.frame(date = envdts))
      out <- tibble(
        date = envdts, val = prd
      )
      
      return(out)
      
    })
  ) %>% 
  unnest %>% 
  ungroup

# get correlations --------------------------------------------------------

# setup search grid, remove fats for now
corgrd <- physprd %>% 
  pull(var) %>% 
  grep('^fat', ., invert = T, value = T) %>% 
  unique %>%
  c(., unique(envdat$var)) %>% 
  combn(2) %>%
  t %>% 
  data.frame(stringsAsFactors = F) %>%
  crossing(c('5m', '30m'), .) %>% 
  t %>% 
  data.frame(stringsAsFactors = F) %>% 
  as.list

##
# get mussel cors

# get only mussels
physprdsub <- physprd %>% 
  filter(Species %in% 'mussel') %>% 
  dplyr::select(-Species)

# combine physio and env
tocor <- bind_rows(physprdsub, envdat) %>% 
  dplyr::select(-season)

# get corelations 
estsmuss <- corgrd %>% 
  enframe %>% 
  mutate(
    corest = purrr::map(value, function(x){
    
      # select data to cor, fill missing dates with na interp
      tocorsub <- tocor %>% 
        filter(`Depth (m)` %in% x[1]) %>% 
        filter(var %in% x[2:3]) %>% 
        mutate(`Depth (m)` = as.character(`Depth (m)`)) %>%
        complete(`Depth (m)`, var, date) %>%
        group_by(var) %>%
        mutate(val = na_interpolation(val)) %>%
        spread(var, val) 
      
      # cor
      cortst <- try({suppressWarnings(cor.test(tocorsub[[x[2]]], tocorsub[[x[3]]], method = 'spearman'))})
      
      if(inherits(cortst, 'try-error'))
        return(NA)
      
      est <- cortst$estimate
      sig <- p_ast(cortst$p.value)
      
      # output
      out <- data.frame(est = est, sig = sig)
      
      return(out)
  
    }), 
    value = purrr::map(value, function(x) tibble(`Depth (m)` = x[1], var1 = x[2], var2 = x[3])), 
    Biodat = 'mussel'
  ) %>% 
  unnest %>% 
  select(-name)

##
# get scallop cors

# get only scallop
physprdsub <- physprd %>% 
  filter(Species %in% 'scallop') %>% 
  dplyr::select(-Species)

# combine physio and env
tocor <- bind_rows(physprdsub, envdat) %>% 
  dplyr::select(-season)

# get corelations 
estsscal <- corgrd %>% 
  enframe %>% 
  mutate(
    corest = purrr::map(value, function(x){
      
      # select data to cor, fill missing dates with na interp
      tocorsub <- tocor %>% 
        filter(`Depth (m)` %in% x[1]) %>% 
        filter(var %in% x[2:3]) %>% 
        mutate(`Depth (m)` = as.character(`Depth (m)`)) %>%
        complete(`Depth (m)`, var, date) %>%
        group_by(var) %>%
        mutate(val = na_interpolation(val)) %>%
        spread(var, val) 
      
      # cor
      cortst <- try({suppressWarnings(cor.test(tocorsub[[x[2]]], tocorsub[[x[3]]], method = 'spearman'))})
      
      if(inherits(cortst, 'try-error'))
        return(NA)
      
      est <- cortst$estimate
      sig <- p_ast(cortst$p.value)
      
      # output
      out <- data.frame(est = est, sig = sig)
      
      return(out)
      
    }), 
    value = purrr::map(value, function(x) tibble(`Depth (m)` = x[1], var1 = x[2], var2 = x[3])), 
    Biodat = 'scallop'
  ) %>% 
  unnest %>% 
  select(-name)

##
# combine and save
levs <- c('aragonite', 'calcite', 'dic', 'pco2', 'ph', 'revelle', 'sal', 'ta', 'tmp', 'gro', 'lip', 'edg', 'mid')
labs <- c('Omega[ar]', 'Calcite', 'DIC', 'pco[2]', 'pH', 'revelle', 'salinity', 'Alkalinity', 'Temp', 'Growth', 'Lipids', 'Thickness[edge]', 'Thickness[mid]')

crsdat <- bind_rows(estsmuss, estsscal) %>% 
  mutate(
    var1 = factor(var1, levels = rev(levs), labels = rev(labs)), 
    var2 = factor(var2, levels = rev(levs), labels = rev(labs)), 
    sig = gsub('ns', '', sig)
  )

save(crsdat, file = 'data/crsdat.RData', compress = 'xz')