source('global.R')

# make query methods to filter stations to query by basin, ecoregion, HUC, wildcard, manual station query

# once station list established, make Total Number of fish query
stations <- c('2-HAZ006.91', '2-JMS330.79', '2-LMC001.37','4AFSF013.80')
dateRange <- c(as.Date('2000-01-01'), as.Date(Sys.Date()- 7))


totalFishFunction <- function(stations, dateRange, fishStationsSelection, fishSampsSelection, fishes, fishesMasterTaxa){
  
}
