library(httr)
# need to comment this next line to run locally now???
httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

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

# update each rerun of EDAS ###############################################################


# From fishDataOrganizationandMoveToRServer.Rmd
fishStationsUnique <- readRDS("data/fishStationsUnique_2022-07-11.RDS")
#fishStationsUnique <- readRDS("data/fishStationsUnique_2022-01-14.RDS")
taxaByHUC8 <- read_csv('data/taxaByHUC8_2022-07-11.csv')

###############################################################################################

# Register RStudio Connect, don't need to do multiple times
#board_register("rsconnect", server = "http://deq-rstudio-prod.cov.virginia.gov:3939")
#board_register("rsconnect", server = "https://rconnect.deq.virginia.gov")

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


# Set up pool connection to production environment
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  UID = conn$UID_prod,
  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
  trusted_connection = "yes"
)
onStop(function() {
  poolClose(pool)
})

# ## For testing: connect to ODS production
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   trusted_connection = "yes"
# )



#### Fish Data
fishes <- pin_get("ejones/fishes", board = "rsconnect")
fishesMasterTaxa <- pin_get("ejones/fishesMasterTaxa", board = "rsconnect")
fishSamps <- pin_get("ejones/fishSamps", board = "rsconnect")
fishStations <- pin_get("ejones/fishStations", board = "rsconnect")
fishBCG <- read_excel('data/MasterAttributeFish_2019_May9.xlsx', sheet = 'FishMasterAttributes') %>% 
  dplyr::select(AFSCommonName, Family, `BCG General`:`BCGatt Comment`, `BCGatt CnAp.oth`:`ELOHA Lithophil`)
bcgAttributeColors <- list(
  brks = c(1,2,3,4,5,6),
  clrs = c("#21a654","#55e607","#f7f720","#f2c602", "#f70000", "#c70610", 'gray'))

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
# rawFish <-  fishes %>% 
#   group_by( FishSampID, FinalID) %>% 
#   left_join(dplyr::select(fishesMasterTaxa, FinalID, Genus, Species, `Genus-Species`), by = 'FinalID') %>% 
#   left_join(fishSamps, by = c('FishSampID', 'RepNum')) %>% 
#   left_join(dplyr::select(fishStations, StationID, StreamName, Lat, Long,
#                           Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
#                           Basin, Ecoregion, `Ecoregion Name`), by = 'StationID') %>% 
#   dplyr::select(StationID, StreamName, Lat, Long, Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
#                 Basin, Ecoregion, `Ecoregion Name`, FishSampID, RepNum, `Collection Date`, 
#                 CollMeth, Duration, `Reach Length`, everything()) %>% 
#   
#   # drop missing location data for now
#   filter(!is.na(Lat) | !is.na(Long)) %>% 
#   
#   st_as_sf(coords = c("Long", "Lat"),  # make spatial layer using these columns
#            remove = T, # don't remove these lat/lon cols from df
#            crs = 4326)

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


