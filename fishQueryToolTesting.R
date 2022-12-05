source('global.R')

# make query methods to filter stations to query by basin, ecoregion, HUC, wildcard, manual station query

# once station list established, make Total Number of fish query
stations <- c('2-HAZ006.91', '2-JMS330.79', '2-LMC001.37','4AFSF013.80', '1AGOO039.63')
dateRange <- c(as.Date('2000-01-01'), as.Date(Sys.Date()- 7))# as.Date('2019-01-01'))#
repFilter <- c('1', '2')
sumFish <- TRUE

fishSamps_Filter <- fishSamps %>% 
  {if(!is.null(stations))
    filter(., StationID %in% stations)
    else .} %>% 
  {if(!is.null(dateRange))
    filter(., between(as.Date(`Collection Date`), dateRange[1], dateRange[2] ))
    else .} %>% 
  {if(!is.null(repFilter))
    filter(., RepNum %in% repFilter)
    else . } %>% 
  left_join(dplyr::select(fishStations, StationID, StreamName, Order, Class, `Special Standards`, `Catchment Area sqMile`, Basin, Ecoregion, `Ecoregion Name`), by = 'StationID') %>% 
  dplyr::select(FSampIndex:StationID, StreamName, Order, Class, `Special Standards`, `Catchment Area sqMile`, Basin, Ecoregion, `Ecoregion Name`,  everything()) %>% 
  arrange(StationID, `Collection Date`)

# raw fish counts (including anomalies, etc.)
rawFish <-  filter(fishes, FishSampID %in% fishSamps_Filter$FishSampID) %>% 
  group_by( FishSampID, FinalID) %>% 
  left_join(dplyr::select(fishesMasterTaxa, FinalID, Genus, Species, `Genus-Species`), by = 'FinalID') %>% 
  left_join(fishSamps_Filter, by = c('FishSampID', 'RepNum')) %>% 
  dplyr::select(StationID, StreamName, Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                Basin, Ecoregion, `Ecoregion Name`, FishSampID, RepNum, `Collection Date`, 
                CollMeth, Duration, `Reach Length`, everything())

# cleaned up fish counts dropping different anomaly info
totalFish <- filter(fishes, FishSampID %in% fishSamps_Filter$FishSampID) %>% 
  group_by( FishSampID, FinalID) %>% 
  mutate(`Total Individuals` = sum(Individuals, na.rm = T)) %>% 
  dplyr::select(-c(`Anomaly Code`, Anomaly, Individuals, `Fishes Comments`, Hybrid, `Entered Date`)) %>% 
  ungroup() %>% 
  group_by(FishSampID, RepNum, FinalID) %>% 
  distinct(FinalID, .keep_all = T) %>% 
  left_join(dplyr::select(fishesMasterTaxa, FinalID, Genus, Species, `Genus-Species`), by = 'FinalID') %>% 
  left_join(fishSamps_Filter, by = c('FishSampID', 'RepNum')) %>% 
  dplyr::select(StationID, StreamName, Order,  Class, `Special Standards`, `Catchment Area sqMile`, 
                Basin, Ecoregion, `Ecoregion Name`, FishSampID, RepNum, `Collection Date`, 
                CollMeth, Duration, `Reach Length`, everything())
  
    


## Taxa Word Bank Tab

# first get a location from either the station provided or lat/lng
stationOrCoords <- "Station" # "Coordinates" # user radiobutton input to allow selectInput or numericInput x2

stationChoice <- '2-JKS023.61'
latitude <- 37.983
longitude <- -78.999

if(stationOrCoords == 'Station'){
  HUCSelected <- filter(WQM_Stations_Spatial, StationID %in% '2-JKS023.61') %>% 
    pull(HUC10) %>% 
    substr(1, 8) # extract first 8 characters to get HUC8 from HUC10
} else {
  siteLocation <- tibble(StationID = "New Site", Latitude = latitude, Longitude = longitude) %>% 
    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
            remove = T, # remove these lat/lon cols from df
            crs = 4326)  
  HUCSelected <- suppressWarnings(suppressMessages(st_intersection(siteLocation, huc8)) %>% #simmer down
    pull(HUC8) %>% 
    as.character())
}




taxaWordBank <- dplyr::select(taxaByHUC8, Taxa, !! HUCSelected)
colNameAdjustment <- paste0(filter(taxaWordBank, is.na(Taxa))[,2] %>% pull(),
                           " (", filter(taxaWordBank, is.na(Taxa))[,2] %>% names(), ")")

taxaWordBank <- taxaWordBank %>% 
  drop_na() %>%
  left_join(dplyr::select(fishesMasterTaxa, FinalID, Family, Genus, Species), by = c('Taxa' = 'FinalID')) %>% 
  dplyr::select(Family, Genus, Species, `Common Name` = Taxa, everything()) %>% 
  arrange(Family, Genus) %>% 
  mutate_at(vars(contains(HUCSelected)), funs(as.numeric(.)))
names(taxaWordBank)[5] <- paste0('n Collected in ', colNameAdjustment)
