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


#### Station Data 

# Retrieve Pins
WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")

fishes <- pin_get("ejones/fishes", board = "rsconnect")
fishesMasterTaxa <- pin_get("ejones/fishesMasterTaxa", board = "rsconnect")
fishSamps <- pin_get("ejones/fishSamps", board = "rsconnect")
fishStations <- pin_get("ejones/fishStations", board = "rsconnect")
