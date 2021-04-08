library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(DBI)
#library(measurements) #only necessary if don't use Rex's dataset for points
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(config)
library(readxl)

# Register RStudio Connect, don't need to do multiple times
#board_register("rsconnect", server = "http://deq-rstudio-prod.cov.virginia.gov:3939")

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


# Set up pool connection to production environment
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#   # Production Environment
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   UID = conn$UID_prod,
#   PWD = conn$PWD_prod,
#   #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#   # Test environment
#   #Server= "WSQ04151,50000",
#   #dbname = "ODS_test",
#   #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#   trusted_connection = "yes"
# )
onStop(function() {
  poolClose(pool)
})

# ## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

## For testing: Connect to ODS_test
# establish db connection locally
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQL Server",  # note the space between SQL and Server ( how MS named driver)
#  Server= "WSQ04151,50000",
#  dbname = "ODS_test"
#)
#onStop(function() {
#  poolClose(pool)
#})


#### Fish Data
fishes <- pin_get("ejones/fishes", board = "rsconnect")
fishesMasterTaxa <- pin_get("ejones/fishesMasterTaxa", board = "rsconnect")
fishSamps <- pin_get("ejones/fishSamps", board = "rsconnect")
fishStations <- pin_get("ejones/fishStations", board = "rsconnect")


#### Station Data 

# Retrieve Pins
WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf


yearsSampled <- fishSamps %>% 
  mutate(Year = year(`Collection Date`)) %>% 
  group_by(StationID) %>% 
  summarise(`Years Sampled` = paste0(Year,  collapse = ", "))


fishStations2 <- left_join(fishStations, WQM_Stations_Spatial, by = 'StationID')

#z <- filter(fishStations, ! StationID %in% WQM_Stations_Spatial$StationID)
#write.csv(z, 'stationIssues.csv', row.names = F)


# dumbed down dataset for fish distribution maps before real data cleanup happens
# raw fish counts (including anomalies, etc.)
rawFish <-  fishes %>% 
  group_by( FishSampID, FinalID) %>% 
  left_join(dplyr::select(fishesMasterTaxa, FinalID, Genus, Species, `Genus-Species`), by = 'FinalID') %>% 
  left_join(fishSamps, by = c('FishSampID', 'RepNum')) %>% 
  left_join(dplyr::select(fishStations, StationID, StreamName, Lat, Long,
                          Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                          Basin, Ecoregion, `Ecoregion Name`), by = 'StationID') %>% 
  dplyr::select(StationID, StreamName, Lat, Long, Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                Basin, Ecoregion, `Ecoregion Name`, FishSampID, RepNum, `Collection Date`, 
                CollMeth, Duration, `Reach Length`, everything()) %>% 
  
  # drop missing location data for now
  filter(!is.na(Lat) | !is.na(Long)) %>% 
  
  st_as_sf(coords = c("Long", "Lat"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326)

# cleaned up fish counts dropping different anomaly info
totalFish <- fishes %>% 
  group_by( FishSampID, FinalID) %>% 
  mutate(`Total Individuals` = sum(Individuals, na.rm = T)) %>% 
  dplyr::select(-c(`Anomaly Code`, Anomaly, Individuals, `Fishes Comments`, Hybrid, `Entered Date`)) %>% 
  ungroup() %>% 
  group_by(FishSampID, RepNum, FinalID) %>% 
  distinct(FinalID, .keep_all = T) %>% 
  left_join(dplyr::select(fishesMasterTaxa, FinalID, Genus, Species, `Genus-Species`), by = 'FinalID') %>% 
  left_join(fishSamps, by = c('FishSampID', 'RepNum')) %>% 
  left_join(dplyr::select(fishStations, StationID, StreamName, Lat, Long,
                          Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                          Basin, Ecoregion, `Ecoregion Name`), by = 'StationID') %>% 
  dplyr::select(StationID, StreamName, Lat, Long, Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                Basin, Ecoregion, `Ecoregion Name`, FishSampID, RepNum, `Collection Date`, 
                CollMeth, Duration, `Reach Length`, everything()) %>% 
  
  # drop missing location data for now
  filter(!is.na(Lat) | !is.na(Long)) %>% 
  
  st_as_sf(coords = c("Long", "Lat"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)


# organize total fish by taxa then station/year
#i = 'slender chub'#"telescope shiner" 
# stationBySpeciesUniqueList <- list()
# notFoundInDatabase <- data.frame(Species=NA)
# 
# for(i in unique(totalFish$FinalID)){
#   print(i)
#   stationsWithSpecies <- filter(totalFish, FinalID == i)
#   if(nrow(stationsWithSpecies) > 0){
#     speciesCount <- stationsWithSpecies %>%
#       arrange(`Collection Date`) %>% # rearrange to make Years Collected make sense
#       group_by(StationID) %>%
#       summarise(`Total Count` = sum(`Total Individuals`, na.rm = T),
#                 `Years Collected` = paste0(year(`Collection Date`),  collapse = ", ")) %>%
#       left_join(stationsWithSpecies %>% ungroup() %>%
#                   dplyr::select(StationID, StreamName, Lat, Long, Order, `Catchment Area sqMile`, Class, `Special Standards`) %>%
#                   distinct(StationID, .keep_all = T),
#                 by = 'StationID')
#     stationBySpeciesUniqueList[[i]] <- speciesCount
#   }else{
#     notFoundInDatabase <- rbind(notFoundInDatabase,i)
#   }
# }
# notFoundInDatabase <- na.omit(notFoundInDatabase)
# 
#saveRDS(stationBySpeciesUniqueList, 'data/fishStationsUnique.RDS')
fishStationsUnique <- readRDS('data/fishStationsUnique.RDS')
