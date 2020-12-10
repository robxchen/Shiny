library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

csv_filenames = list.files(pattern = 'EIA923')
eia <- do.call(rbind, lapply(csv_filenames, read.csv, stringsAsFactors = FALSE, skip = 5))
eia <- eia %>% select(., Plant.State, Reported.Fuel.Type.Code, Elec.Fuel.Consumption.MMBtu, Net.Generation..Megawatthours., YEAR)
eia$Reported.Fuel.Type.Code <- gsub('BIT|ANT|LIG|SUB|RC|WC|CBL|SGC|SC', 'Coal', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('NG|BFG|OG|PG', 'Natural Gas', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('AB|MSW|MSB|OBS|WDS|OBL|SLW|BLQ|WDL|LFG|OBG|SUN|WND|GEO|WAT', 'Renewables', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('NUC', 'Nuclear', eia$Reported.Fuel.Type.Code)
eia$Reported.Fuel.Type.Code <- gsub('DFO|RFO|JF|KER|WO|PC|WH|MWH|TDF|OTH|SGP|MSN|PUR', 'Petroleum & Other', eia$Reported.Fuel.Type.Code)
eia$Elec.Fuel.Consumption.MMBtu <- gsub(',', '', eia$Elec.Fuel.Consumption.MMBtu)
eia$Elec.Fuel.Consumption.MMBtu <- as.numeric(eia$Elec.Fuel.Consumption.MMBtu)
eia$Net.Generation..Megawatthours. <- gsub(',', '', eia$Net.Generation..Megawatthours.)
eia$Net.Generation..Megawatthours. <- as.numeric(eia$Net.Generation..Megawatthours.)
eia <- eia %>% rename(., Consumption.MMBtu = Elec.Fuel.Consumption.MMBtu) %>% rename(., Generation.MWh = Net.Generation..Megawatthours.)


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


# create variables for calculating GDP, population, and fuel source breakdowns
eia$Consumption.MMBtu.Pop <- eia$Consumption.MMBtu / eia$Population
eia$Consumption.MMBtu.GDP <- eia$Consumption.MMBtu / eia$GDP
eia$Coal.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Consumption.MMBtu, 0)
eia$NG.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Consumption.MMBtu, 0)
eia$Renewables.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Consumption.MMBtu, 0)
eia$Nuc.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Consumption.MMBtu, 0)
eia$Petro.MMBtu <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Consumption.MMBtu, 0)
eia$Coal.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Consumption.MMBtu.Pop, 0)
eia$NG.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Consumption.MMBtu.Pop, 0)
eia$Renewables.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Consumption.MMBtu.Pop, 0)
eia$Nuc.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Consumption.MMBtu.Pop, 0)
eia$Petro.MMBtu.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Consumption.MMBtu.Pop, 0)
eia$Coal.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Consumption.MMBtu.GDP, 0)
eia$NG.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Consumption.MMBtu.GDP, 0)
eia$Renewables.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Consumption.MMBtu.GDP, 0)
eia$Nuc.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Consumption.MMBtu.GDP, 0)
eia$Petro.MMBtu.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Consumption.MMBtu.GDP, 0)


eia$Generation.MWh.Pop <- eia$Generation.MWh / eia$Population
eia$Generation.MWh.GDP <- eia$Generation.MWh / eia$GDP
eia$Coal.MWh <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Generation.MWh, 0)
eia$NG.MWh <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Generation.MWh, 0)
eia$Renewables.MWh <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Generation.MWh, 0)
eia$Nuc.MWh <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Generation.MWh, 0)
eia$Petro.MWh <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Generation.MWh, 0)
eia$Coal.MWh.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Generation.MWh.Pop, 0)
eia$NG.MWh.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Generation.MWh.Pop, 0)
eia$Renewables.MWh.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Generation.MWh.Pop, 0)
eia$Nuc.MWh.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Generation.MWh.Pop, 0)
eia$Petro.MWh.Pop <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Generation.MWh.Pop, 0)
eia$Coal.MWh.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Coal', eia$Generation.MWh.GDP, 0)
eia$NG.MWh.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Natural Gas', eia$Generation.MWh.GDP, 0)
eia$Renewables.MWh.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Renewables', eia$Generation.MWh.GDP, 0)
eia$Nuc.MWh.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Nuclear', eia$Generation.MWh.GDP, 0)
eia$Petro.MWh.GDP <- ifelse(eia$Reported.Fuel.Type.Code == 'Petroleum & Other', eia$Generation.MWh.GDP, 0)



# GeoStates <-
#   gvisGeoChart(
#     eia,
#     'Plant.State',
#     'Consumption.MMBtu',
#     options = list(
#       region = 'US',
#       displayMode = 'regions',
#       resolution = 'provinces',
#       width = 600,
#       height = 400,
#       sizeAxis.maxValue = max(eia$Consumption.MMBtu)
#     )
#   )
# plot(GeoStates)


