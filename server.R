source('global.R')


assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  group_by(SUBBASIN) %>%
  summarize()
huc8 <- st_read('data/GIS/HUC8_EVJ.shp')


shinyServer(function(input,output,session){
  
  taxaLocations <- reactive({req(input$taxaSelection)
    fishStationsUnique[[input$taxaSelection]] })
  
  taxaData <- reactive({req(taxaLocations())
    filter(totalFish, StationID %in% taxaLocations()$StationID) %>% 
      filter(FinalID %in% input$taxaSelection) })
  
  output$distributionMap <- renderLeaflet({
    # color palette for assessment polygons
    pal <- colorFactor(
      palette = topo.colors(7),
      domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(
      palette = topo.colors(7),
      domain = ecoregion$US_L3NAME)
    palSubbasins <- colorFactor(
      palette = terrain.colors(17),
      domain = subbasins$SUBBASIN)
    palHUC8 <- colorFactor(
      palette = rainbow(51),
      domain = huc8$HUC8)
    
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      setView(-79.1, 37.7, zoom=7)  %>% 
      addPolygons(data= ecoregion,  color = 'gray', weight = 1,
                  fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
                  group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>% 
      addPolygons(data= subbasins,  color = 'black', weight = 1,
                  fillColor= ~palSubbasins(subbasins$SUBBASIN), fillOpacity = 0.5,stroke=0.1,
                  group="Subbasins", label = ~SUBBASIN) %>% hideGroup('Subbasins') %>%
      addPolygons(data= huc8,  color = 'black', weight = 1,
                  fillColor= ~palHUC8(huc8$HUC8), fillOpacity = 0.5,stroke=0.1,
                  group="HUC8", label = ~HUC8, 
                  labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "center")) %>% hideGroup('HUC8') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Level III Ecoregions", 'Assessment Regions','Subbasins','HUC8'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    })
  
  map_proxy <- leafletProxy("distributionMap")
  
  # Add layers to map as requested
  observe({req(nrow(taxaLocations()) > 0)
    palOrder <- colorFactor(c('red','orange','yellow','limegreen','blue','purple','gray'),domain = NULL, as.factor(c('1','2','3','4','5','6',NA)), ordered=T)
    
    map_proxy %>% clearMarkers() %>% clearControls() %>% #clearGroup('Capture Location') %>% 
      addCircleMarkers(data = taxaLocations(),
                       radius=6,color='black', fillColor =~palOrder(Order),fillOpacity = 1, 
                       opacity=1,weight = 2,stroke=T, group='Capture Location',
                       label = ~StationID, layerId=~StationID,
                       popup=leafpop::popupTable(taxaLocations(), zcol = c("StationID","StreamName", "Order" ,  
                                                                           "Catchment Area sqMile", "Class", "Special Standards",  
                                                                           "Total Count", "Years Collected")))%>%
      addLegend(data = taxaLocations(), "topright", pal = palOrder, values = ~Order,
                title = "Strahler Order", opacity = 1, group = 'Capture Location') %>% 
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Capture Location", "Level III Ecoregions", 'Assessment Regions','Subbasins','HUC8'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  output$taxaCollection <-  renderDataTable({ req(taxaData())
    datatable(taxaData(), rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
              options = list(dom = 'Bift', scrollY = '250px', scrollX = TRUE, pageLength = nrow(taxaData()),buttons=list('copy','colvis'))) })
  
  
  
  # Query side of things
  
  output$totalFishData <- renderDataTable({ req(input$stationSelection)
    
    fishData <- filter(totalFish, StationID %in% input$stationSelection) %>% 
      left_join(fishBCG, by = c('FinalID'= 'AFSCommonName'))
    
    
    datatable(fishData, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
              options = list(dom = 'Bift', scrollY = '500px', scrollX = TRUE, pageLength = nrow(fishData),
                             buttons=list('copy','colvis',
                                          list(
                                            extend = 'collection',
                                            buttons = c('csv', 'excel'),#, 'pdf'),
                                            text = 'Download')))) %>% 
      formatStyle(names(fishData)[c(38:48)], backgroundColor = styleInterval(bcgAttributeColors$brks, bcgAttributeColors$clrs))  })
  
  
  
  
  
  
  ## Taxa Word Bank Tab
  
  # step to identify HUC by either station or Coords saved as reactive
  
  siteLocation <- eventReactive(input$pullTaxaOptions, {#req(input$stationOrCoords, input$pullTaxaOptions)
    if(input$stationOrCoords == 'StationID'){
      filter(WQM_Stations_Spatial, StationID %in% input$stationChoice) %>% 
        st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                 remove = T, # remove these lat/lon cols from df
                 crs = 4326)  
      
    } else {
      tibble(StationID = "New Site", Latitude = input$latitude, Longitude = input$longitude) %>% 
        st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                 remove = T, # remove these lat/lon cols from df
                 crs = 4326)  
    } })
  
  
  
  HUCSelected <- reactive({req(siteLocation())
    if(input$stationOrCoords == 'StationID'){
      siteLocation() %>% 
        pull(HUC10) %>% 
        substr(1, 8) # extract first 8 characters to get HUC8 from HUC10
    } else {
      suppressWarnings(suppressMessages(st_intersection(siteLocation(), huc8)) %>% #simmer down
                         pull(HUC8) %>% 
                         as.character()) } })
  
  #output$test <- renderPrint(HUCSelected())
  
  # Site Preview map
  output$sitePreviewMap <- renderLeaflet({req(HUCSelected(), siteLocation())
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      #setView(-79.1, 37.7, zoom=7)  %>% 
      addPolygons(data= huc8 %>% 
                    filter(HUC8 %in% HUCSelected()),  color = 'black', weight = 1,
                  fillColor= 'grey', fillOpacity = 0.5,stroke=0.1,
                  group="HUC8", label = ~HUC8, 
                  labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "center")) %>% #hideGroup('HUC8') %>%
      addCircleMarkers(data = siteLocation(),
                       radius=6,color='black', fillColor ='red',fillOpacity = 1, 
                       opacity=1,weight = 2,stroke=T)  })
  
  
  
  output$taxaOptions <- renderDataTable({req(input$pullTaxaOptions, ! is.na(HUCSelected()))
    
    taxaWordBank <- dplyr::select(taxaByHUC8, Taxa, !! HUCSelected())
    colNameAdjustment <- paste0(filter(taxaWordBank, is.na(Taxa))[,2] %>% pull(),
                                " (", filter(taxaWordBank, is.na(Taxa))[,2] %>% names(), ")")
    
    taxaWordBank <- taxaWordBank %>% 
      drop_na() %>%
      left_join(dplyr::select(fishesMasterTaxa, FinalID, Family, Genus, Species), by = c('Taxa' = 'FinalID')) %>% 
      dplyr::select(Family, Genus, Species, `Common Name` = Taxa, everything()) %>% 
      arrange(Family, Genus) %>% 
      mutate_at(vars(contains(HUCSelected())), funs(as.numeric(.)))
    names(taxaWordBank)[5] <- paste0('n Collected in ', colNameAdjustment)
    
    
    datatable(taxaWordBank, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
              options = list(dom = 'Bift', scrollY = '500px', scrollX = TRUE, pageLength = nrow(taxaWordBank),
                             buttons=list('copy','colvis',
                                          list(
                                            extend = 'collection',
                                            buttons = c('csv', 'excel', 'pdf'),
                                            text = 'Download'))))
  })
  
  #output$test <- renderPrint({taxaLocations()})
  
  
})

