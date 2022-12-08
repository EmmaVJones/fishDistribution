library(httr)
httr::set_config(config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

# need to comment these two lines above to run locally now


library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(pins)
library(config)
library(readxl)


################ HEY HERE IS IMPORTANT INFORMATION TO PAY ATTENTION TO ##############################################################

# update each rerun of EDAS ############################################################################################


# From fishDataOrganizationandMoveToRServer.Rmd
fishStationsUnique <- readRDS("data/fishStationsUnique_2022-12-07.RDS")#fishStationsUnique_2022-07-11.RDS")#fishStationsUnique_2022-01-14.RDS")
taxaByHUC8 <- read_csv('data/taxaByHUC8_2022-12-07.csv')#taxaByHUC8_2022-07-11.csv')

###############################################################################################

################### END OF FUN NOTES FROM EMMA ######################################################################################


# get configuration settings to link up to pinned data on R server
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


#### Bring in pinned Fish Data
fishes <- pin_get("ejones/fishes", board = "rsconnect")
fishesMasterTaxa <- pin_get("ejones/fishesMasterTaxa", board = "rsconnect")
fishSamps <- pin_get("ejones/fishSamps", board = "rsconnect")
fishStations <- pin_get("ejones/fishStations", board = "rsconnect")
fishBCG <- read_excel('data/MasterAttributeFish_2019_May9.xlsx', sheet = 'FishMasterAttributes') %>% 
  dplyr::select(AFSCommonName, Family, `BCG General`:`BCGatt Comment`, `BCGatt CnAp.oth`:`ELOHA Lithophil`)
bcgAttributeColors <- list(
  brks = c(1,2,3,4,5,6),
  clrs = c("#21a654","#55e607","#f7f720","#f2c602", "#f70000", "#c70610", 'gray'))

#### Bring in pinned Station Data 

# Retrieve Pins
WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf




### Do some data manipulation stuff for the app


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


