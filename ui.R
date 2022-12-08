
shinyUI(fluidPage(theme= "yeti.css",
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
                                                 column(4, selectInput('stationChoice', 
                                                                       span(strong('Enter DEQ StationID for Taxa list'),br(),
                                                                       '(Pro Tip: Click on the drop down then use the backspace button to clear the field 
                                                                       and begin typing the StationID you want to autopopulate station options)'),
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
)