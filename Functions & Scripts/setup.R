#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A SCRIPT that imports the packages required and setup

# Functions Required
# source("./Functions & Scripts/dataimport.R")   # dataimport.R only works inside Airbus server
data <- readRDS("./Data/fr24data.rds")
source("./Functions & Scripts/DP1.R")
source("./Functions & Scripts/DP2.R")
source("./Functions & Scripts/DP3.R")
source("./Functions & Scripts/airportchecks.R")
source("./Functions & Scripts/fr24datainterp.R")
source("./Functions & Scripts/smallfunctions.R")
source("./Functions & Scripts/oagimport.R")
# Loading packages
required_packages=c("RCurl", "RJSONIO", "dplyr", "lubridate","devtools", "sqldf", "ggplot2", "readxl","plyr","svDialogs","Rmisc","plotly",
                    "knitr","xtable","shiny","shinydashboard", "Hmisc", "scales", "rCharts", 
                    "leaflet", "dygraphs", "xts", "googleVis", "DT", "chron", "rmarkdown",
                    "stringr", "shinyGlobe", "threejs", "maps", "visNetwork", "shinyAce", "rgeos", "sp", 
                    "ggvis", "rgeos", "grid", "png")  # (threejs has to be loaded after shinyGlobe) RODBC not needed unless dataimport.R uncommented
install_load(required_packages)
# Loading Info and weather
load("./Data/Info.RData")
weather <- readRDS("./Data/WeatherData.rds")
# Other options
options(warn=-1)
options(shiny.maxRequestSize=2^30)
screenheight <- 850  # big screen: 850, laptop: 600