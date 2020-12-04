library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(googleVis)


csv_filenames = list.files(pattern = 'EIA923')
eia <- do.call(rbind, lapply(csv_filenames, read.csv, stringsAsFactors = FALSE, skip = 5))
eia <- eia %>% select(., Plant.State, Reported.Fuel.Type.Code, Elec.Fuel.Consumption.MMBtu, YEAR)
eia$Reported.Fuel.Type.Code <- gsub('BIT|ANT|LIG|SUB|RC|WC|CBL|SGC|SC', 'Coal', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('NG|BFG|OG|PG', 'Natural Gas', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('AB|MSW|MSB|OBS|WDS|OBL|SLW|BLQ|WDL|LFG|OBG|SUN|WND|GEO|WAT', 'Renewables', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('NUC', 'Nuclear', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('DFO|RFO|JF|KER|WO|PC|WH|MWH|TDF|OTH|SGP|MSN|PUR', 'Petroleum & Other', eia$Reported.Fuel.Type.Code)
eia$Elec.Fuel.Consumption.MMBtu <- gsub(',', '', eia$Elec.Fuel.Consumption.MMBtu)
eia$Elec.Fuel.Consumption.MMBtu <- as.numeric(eia$Elec.Fuel.Consumption.MMBtu)
eia <- eia %>% rename(., Elec.MMBtu = Elec.Fuel.Consumption.MMBtu)

pop_csv <- read.csv('nst-est2019.csv', stringsAsFactors = FALSE, skip = 3)
pop <- pop_csv %>% gather('year', 'population', 4:13) %>% select(., X, year, population)
pop$year <- gsub('X', '', pop$year)
pop$population <- gsub(',', '', pop$population)
pop$population <- as.numeric(pop$population)
colnames(pop) <- c('Plant.State', 'YEAR', 'Population')

gdp_csv <- read.csv('GDP.csv', stringsAsFactors = FALSE)
gdp <- gdp_csv %>% gather('year', 'gdp', 21:30) %>% select(., GeoName, year, gdp)
gdp$year <- gsub('X', '', gdp$year)
gdp$gdp <- gsub('\\..*', '', gdp$gdp)
gdp$gdp <- as.numeric(gdp$gdp)
colnames(gdp) <- c('Plant.State', 'YEAR', 'GDP')

gov_csv <- read.csv('gov.csv', stringsAsFactors = FALSE)
gov <- gov_csv %>% gather('year', 'party', 2:11)
gov$year <- gsub('X', '', gov$year)
colnames(gov) <- c('Plant.State', 'Overall.Party', 'YEAR', 'Party')


eia <- merge(eia, pop, by = c('Plant.State', 'YEAR')) %>% merge(., gdp, by = c('Plant.State', 'YEAR')) %>% merge(., gov, by = c('Plant.State', 'YEAR'))

eia$Elec.MMBtu.Pop <- eia$Elec.MMBtu / eia$Population
eia$Elec.MMBtu.GDP <- eia$Elec.MMBtu / eia$GDP
eia$Coal.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Elec.MMBtu, 0)
eia$NG.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Elec.MMBtu, 0)
eia$Renewables.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Elec.MMBtu, 0)
eia$Nuc.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Elec.MMBtu, 0)
eia$Petro.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Elec.MMBtu, 0)
eia$Coal.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Elec.MMBtu.Pop, 0)
eia$NG.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Elec.MMBtu.Pop, 0)
eia$Renewables.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Elec.MMBtu.Pop, 0)
eia$Nuc.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Elec.MMBtu.Pop, 0)
eia$Petro.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Elec.MMBtu.Pop, 0)
eia$Coal.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Elec.MMBtu.GDP, 0)
eia$NG.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Elec.MMBtu.GDP, 0)
eia$Renewables.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Elec.MMBtu.GDP, 0)
eia$Nuc.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Elec.MMBtu.GDP, 0)
eia$Petro.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Elec.MMBtu.GDP, 0)




# GeoStates <-
#   gvisGeoChart(
#     eia,
#     'Plant.State',
#     'Elec.MMBtu',
#     options = list(
#       region = 'US',
#       displayMode = 'regions',
#       resolution = 'provinces',
#       width = 600,
#       height = 400,
#       sizeAxis.maxValue = max(eia$Elec.MMBtu)
#     )
#   )
# plot(GeoStates)


