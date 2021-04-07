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
  {if(!is.null(RepFilter))
    filter(., RepNum %in% repFilter)
    else . } 

totalFish <- filter(fishes, FishSampID %in% fishSamps_Filter$FishSampID) %>% 
  {if(sumFish == TRUE)
    group_by(., FishSampID, FinalID) %>% 
      mutate(`Total Individuals` = sum(Individuals, na.rm = T)) %>% 
      dplyr::select(-c(`Anomaly Code`, Anomaly, Individuals, Comments, `Entered Date`)) %>% 
      ungroup() %>% 
      group_by(FishSampID, RepNum, FinalID) %>% 
      distinct(FinalID, .keep_all = T)
    else . }

left_join(fishSamps_Filter, by = c('FishSampID', 'RepNum')) %>% 
  left_join(fishesMasterTaxa, by = 'FinalID') %>% 
  
    


