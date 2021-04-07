
# read in 2020 IR probmon data
probArea <- read_csv('C:/HardDriveBackup/R/GitHub/ProbMon-Integrated-Reports/2020ProbChapter/processedData/Wadeable_ProbMon_2001-2018_Final_Final.csv')
conserveVAarea <- read_csv('C:/HardDriveBackup/R/GitHub/LandcoverAnalysis/ConserveVA/Results/forDCR/landcoverResults.csv')

# join available area to stations
fishStations1 <- left_join(fishStations, 
                           dplyr::select(probArea, StationID, totalArea_sqMile) %>% 
                             distinct(StationID, .keep_all = T), # drop prob trend duplicates 
                           by = 'StationID') %>% 
  left_join(dplyr::select(conserveVAarea, StationID, totalArea_sqMile) %>% 
              distinct(StationID, .keep_all = T),
            by = 'StationID') %>% 
  group_by(StationID) %>% 
  mutate(diff = abs(totalArea_sqMile.x - totalArea_sqMile.y),
         diffFlag = ifelse(diff > 0.001, 'flag', 'cool'),
         n = n())

# drop stations that have differing areas between sources
fishStations2 <- mutate(fishStations1, `Catchment Area sqMile` = case_when(diffFlag == 'flag' ~  as.numeric(NA),
                                                                    diffFlag == 'cool' ~ totalArea_sqMile.x,
                                                                    is.na(diffFlag) ~ totalArea_sqMile.x,
                                                                    TRUE ~ as.numeric(totalArea_sqMile.x))) %>% 
  mutate(`Catchment Acreage` = `Catchment Area sqMile` * 640) %>% 
  dplyr::select(-c(totalArea_sqMile.x, totalArea_sqMile.y, diff, diffFlag, n)) %>% 
  dplyr::select(StationIndex:`Catchment Acreage`, `Catchment Area sqMile`, everything())
  
fishStations <- fishStations2

rm(fishStations1); rm(fishStations2); rm(probArea); rm(conserveVAarea)

