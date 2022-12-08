source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  group_by(SUBBASIN) %>%
  summarize()
huc8 <- st_read('data/GIS/HUC8_EVJ.shp')


#fishStationsUnique <- readRDS('data/fishStationsUnique.RDS')


ui <- fluidPage(theme= "yeti.css",
                navbarPage("Fish EDAS",
                           tabPanel('About',
                                    h4('This is a preliminary application that allows users to explore fish data collected by DEQ
                                       and a rough query tool. These tools will improve as Fish EDAS is cleaned up and moved into
                                       CEDS.'),
                                    h4('BCG attribute information can be found under the `Query Fish Data` tab.'),
                                    h4('Please contact Emma Jones (emma.jones@deq.virginia.gov) and Jason Hill (jason.hill@deq.virginia.gov)
                                       if you have any questions about the tool.')),
                           tabPanel('Taxa Distribution Map',
                                    fluidRow(column(4),
                                             column(4,selectInput('taxaSelection', 'Choose a taxa to plot on the distribution map',
                                                                  choices = sort(names(fishStationsUnique)))),
                                             column(4,helpText('On load the map is blank, use the drop down to choose a taxa to plot.'))),
                                    #verbatimTextOutput('test'),
                                    leafletOutput('distributionMap'),
                                    h4('Taxa Collection Information'),
                                    DT::dataTableOutput('taxaCollection'), br(), br(), br()),
                           tabPanel('Query Fish Data',
                                    helpText('This is pretty rough right now bc we need to do some database cleanup before building
                                             out a proper query tool is worth it. Here is simple query tool for now.'),
                                    selectInput('stationSelection', 'Select Station to Query Fish Data',
                                                choices = sort(unique(totalFish$StationID))),
                                    DT::dataTableOutput('totalFishData'), br(), br(), br()),
                           tabPanel('Taxa Word Bank',
                                    helpText('This section of Fish EDAS aims to assist biologists with taxa identification in the field.
                                             To optimize this information, please enter the StationID or coordinates of the site you intend to visit and
                                             press the `Pull Taxa Options` button. This will return a spreadsheet of all taxa DEQ staff
                                             have collected in the HUC8 where you site is located.'),
                                    helpText('By printing this information to PDF and referencing the available taxa choices in the field,
                                             staff should improve and expedite fish identification while streamside.'),
                                    helpText('A complimentary DEQ developed field guide is in development to offer better photographs
                                             and identification tips for reference in the field. Stay tuned on that project.'),
                                    
                                    radioButtons("stationOrCoords", "Identify Taxa List By StationID or Coordinates", 
                                                 choices = c('StationID', "Coordinates")),
                                    
                                    fluidRow(
                                      conditionalPanel(condition = "input.stationOrCoords == 'StationID'", 
                                                       column(4, selectInput('stationChoice', 'Enter DEQ StationID for Taxa list',
                                                                             choices = sort(unique(WQM_Stations_Spatial$StationID))))),
                                      conditionalPanel(condition = "input.stationOrCoords == 'Coordinates'", 
                                                       column(4, numericInput('latitude',  'Enter New Station Latitude', value = 37.983, min = -35, max = -40),
                                                                 numericInput('longitude', 'Enter New Station Longitude', value = -78.999, min = -73, max = -85))),
                                      column(4, actionButton('pullTaxaOptions', 'Pull Taxa Options'),
                                             helpText('If no map appears after clicking the `Pull Taxa Options` button, please check that your coordinates are valid.')),
                                      
                                      # tiny preview map
                                      column(4, leafletOutput('sitePreviewMap',height="30vh") ) ),
                                    
                                    #verbatimTextOutput('test'),
                                     
                                    DT::dataTableOutput('taxaOptions'), br(), br(), br() )
                              ) )
  

server <- function(input,output,session){
  
  taxaLocations <- reactive({req(input$taxaSelection)
    fishStationsUnique[[input$taxaSelection]] })# %>% 
     # st_as_sf(coords = c("Long", "Lat"),  # make spatial layer using these columns
      #         remove = T, # don't remove these lat/lon cols from df
        #       crs = 4326)  })
  
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
  
  
}

shinyApp(ui,server)
