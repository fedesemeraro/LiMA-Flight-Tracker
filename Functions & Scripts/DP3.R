#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that processes data

DP3 <- function(oooi, weather) {
  
  ######################
  ### FLIGHT SUMMARY ###
  ######################
  # Deviding oooi dataframe
  OUTdata <- subset(oooi,oooi$OUTtimestamp > 0)
  OUTdata <- OUTdata[,c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "OUT_OFFtimestamp", "OUTtimestamp", "RowNum", "Latitude", "Longitude")]
  OFFdata <- subset(oooi, oooi$OFFtimestamp > 0)
  OFFdata <- OFFdata[,c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "OFFtimestamp", "RowNum", "Latitude", "Longitude")]
  ONdata <- subset(oooi,oooi$ONtimestamp > 0)
  ONdata <- ONdata[,c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "ON_OFFtimestamp", "ONtimestamp", "RowNum", "Latitude", "Longitude")]
  INdata <- subset(oooi, oooi$INtimestamp > 0)
  INdata <- INdata[,c("Registration", "IN_ONtimestamp", "INtimestamp", "RowNum", "Latitude", "Longitude")]
  ## Merge
  # OUT with OFF
  FlightSummary <- merge(OFFdata, OUTdata, by.x = c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "OFFtimestamp"),
                         by.y = c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "OUT_OFFtimestamp"),
                         all=TRUE, suffixes=c(".OFF", ".OUT"))
  # ON
  FlightSummary <- merge(FlightSummary, ONdata, by.x = c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "OFFtimestamp"),
                         by.y = c("Registration", "FlightNumber", "AC_type", "Departure", "Destination", "ON_OFFtimestamp"),
                         all=TRUE, suffixes=c("", ".ON"))
  FlightSummary <- plyr::rename(FlightSummary, c("RowNum"="RowNum.ON", "Latitude"="Latitude.ON", "Longitude"="Longitude.ON"))
  # IN
  # Since most of the OUTs will not have RegNum ecc in common
  FlightSummary <- merge(FlightSummary, INdata, by.x = c("Registration", "ONtimestamp"),by.y = c("Registration", "IN_ONtimestamp"),all=TRUE, suffixes=c("", ".IN"))
  FlightSummary <- plyr::rename(FlightSummary, c("RowNum"="RowNum.IN", "Latitude"="Latitude.IN", "Longitude"="Longitude.IN"))
  
  ## POST PROCESSING
  # Removing 0s from OFF column
  FlightSummary$OFFtimestamp <- ifelse(FlightSummary$OFFtimestamp == 0, NA, FlightSummary$OFFtimestamp)
  # Convert timestamps to Dates
  FlightSummary$OUT <- ifelse(is.na(FlightSummary$OUTtimestamp),FlightSummary$OUTtimestamp, format((as.POSIXct(as.numeric(FlightSummary$OUTtimestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S"))
  FlightSummary$OFF <- ifelse(is.na(FlightSummary$OFFtimestamp),FlightSummary$OFFtimestamp, format((as.POSIXct(as.numeric(FlightSummary$OFFtimestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S"))
  FlightSummary$ON <- ifelse(is.na(FlightSummary$ONtimestamp),FlightSummary$ONtimestamp, format((as.POSIXct(as.numeric(FlightSummary$ONtimestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S"))
  FlightSummary$IN <- ifelse(is.na(FlightSummary$INtimestamp),FlightSummary$INtimestamp, format((as.POSIXct(as.numeric(FlightSummary$INtimestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S"))
  # Cleaning Data
  FlightSummary <- subset(FlightSummary, !(FlightSummary$FlightNumber == "" & FlightSummary$Destination == "" &
                                             (is.na(FlightSummary$OUT) | is.na(FlightSummary$OFF) | is.na(FlightSummary$ON) | is.na(FlightSummary$IN))) & FlightSummary$Registration != "")
  ## Adding Airline Information
  # According to MSN
  FlightSummary <- merge(FlightSummary, MSNs, by="Registration", suffixes=c("", ""), all.x=TRUE)
  # If MSN method fails, link IATA Flight Number to Operator
  FlightSummary$IATA <- substr(as.character(FlightSummary$FlightNumber), 1, 2)
  FlightSummary <- merge(FlightSummary, MSNs, by="Registration", suffixes=c("", ""), all.x=TRUE)
  # Reorder columns
  FlightSummary <- FlightSummary[,c("Registration", "Operator", "IATA", "FlightNumber", "AC_type", "Departure", "Destination","OUT", "OFF", "ON", "IN",
                                    "OUTtimestamp", "RowNum.OUT", "Latitude.OUT", "Longitude.OUT",
                                    "OFFtimestamp", "RowNum.OFF", "Latitude.OFF", "Longitude.OFF",
                                    "ONtimestamp", "RowNum.ON", "Latitude.ON", "Longitude.ON",
                                    "INtimestamp", "RowNum.IN", "Latitude.IN", "Longitude.IN")]
  # Transforming the Blanks in NAs
  FlightSummary[FlightSummary == ""]  <- NA
  
  
  ### TS CALCULATIONS
  # Initialising Calculation Rows
  FlightSummary$FlightTime_hrs <- rep(NA, nrow(FlightSummary))
  FlightSummary$Taxi.OUT_mins <- rep(NA, nrow(FlightSummary))
  FlightSummary$Taxi.IN_mins <- rep(NA, nrow(FlightSummary))
  FlightSummary$TAT_hrs <- rep(NA, nrow(FlightSummary))
  FlightSummary$TAT_type <- rep(NA, nrow(FlightSummary))
  FlightSummary$GCD_km <- rep(NA, nrow(FlightSummary))
  ## Flight Duration (remove the ones that are less than 15 mins)
  FlightSummary$FlightTime_hrs <- ifelse(!is.na(FlightSummary$OFFtimestamp) & !is.na(FlightSummary$ONtimestamp), (FlightSummary$ONtimestamp - FlightSummary$OFFtimestamp)/(60*60), NA) # in hours
  FlightSummary <- subset(FlightSummary, FlightTime_hrs>0.25 | is.na(FlightTime_hrs))
  ## Taxi OUT
  FlightSummary$Taxi.OUT_mins <- ifelse(!is.na(FlightSummary$OUTtimestamp) & !is.na(FlightSummary$OFFtimestamp), (FlightSummary$OFFtimestamp - FlightSummary$OUTtimestamp)/(60), NA) # in minutes
  ## Taxi IN
  FlightSummary$Taxi.IN_mins <- ifelse(!is.na(FlightSummary$INtimestamp) & !is.na(FlightSummary$ONtimestamp), (FlightSummary$INtimestamp - FlightSummary$ONtimestamp)/(60), NA) # in minutes
  ## TAT
  FlightSummary$TAT_hrs[1:(nrow(FlightSummary)-1)] <- ifelse(!is.na(FlightSummary$OUTtimestamp[2:nrow(FlightSummary)]) & 
                                                               !is.na(FlightSummary$INtimestamp[1:(nrow(FlightSummary)-1)]) &
                                                               as.character(FlightSummary$Departure[2:nrow(FlightSummary)]) == as.character(FlightSummary$Destination[1:(nrow(FlightSummary)-1)]) &
                                                               FlightSummary$Registration[2:nrow(FlightSummary)] == FlightSummary$Registration[1:(nrow(FlightSummary)-1)], 
                                                             (FlightSummary$OUTtimestamp[2:nrow(FlightSummary)] - FlightSummary$INtimestamp[1:(nrow(FlightSummary)-1)])/(60*60), NA)
  FlightSummary$TAT_hrs <- ifelse(FlightSummary$TAT_hrs < 0, NA, FlightSummary$TAT_hrs)
  ## Stop Type
  FlightSummary$TAT_type <- ifelse(is.na(FlightSummary$TAT_hrs), NA, 
                                   ifelse(FlightSummary$TAT_hrs < 3, "LESS", 
                                          ifelse(FlightSummary$TAT_hrs > 12, "EXTENDED", "NORMAL")))
  FlightSummary$TAT_type <- ifelse(FlightSummary$TAT_hrs < 0, NA, FlightSummary$TAT_type)
  
  ## Great Circle Distance (GCD in km)
  gcd_slc <- function(long1, lat1, long2, lat2) {
    # Earth mean radius (km)
    R <- 6371 
    # Degrees to Radians
    long1 <- long1*pi/180
    lat1 <- lat1*pi/180
    long2 <- long2*pi/180
    lat2 <- lat2*pi/180
    # Spherical Law of Cosines
    d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
    return(d)
  }
  
  FlightSummary$GCD_km <- ifelse(!is.na(FlightSummary$FlightTime_hrs), 
                                 round(gcd_slc(FlightSummary$Longitude.OFF, FlightSummary$Latitude.OFF, 
                                               FlightSummary$Longitude.ON, FlightSummary$Latitude.ON), digits=2), NA)
  # WEATHER DATA
  FlightSummary$RowNum <- seq.int(nrow(FlightSummary))
  roundoff <- strptime(FlightSummary$OFF, "%d/%m/%Y %H:%M", tz="GMT")
  roundoff$min <- round(roundoff$min/30)*30
  FlightSummary$OFFround <- roundoff
  FlightSummary <- merge(FlightSummary, weather, by.x=c("Departure", "OFFround"), by.y=c("Airport", "DateUTC"), all.x=T, suffixes=c("",".OFF"))
  FlightSummary <- FlightSummary[order(FlightSummary$RowNum),] 
  roundon <- strptime(FlightSummary$OFF, "%d/%m/%Y %H:%M", tz="GMT")
  roundon$min <- round(roundon$min/30)*30
  FlightSummary$ONround <- roundon
  FlightSummary <- merge(FlightSummary, weather, by.x=c("Destination", "ONround"), by.y=c("Airport", "DateUTC"), all.x=T, suffixes=c("",".ON"))
  FlightSummary <- FlightSummary[order(FlightSummary$RowNum),] 
  
  FlightSummary$TempDiff <- abs(FlightSummary$TemperatureC - FlightSummary$TemperatureC.ON)
  FlightSummary$HumidDiff <- abs(as.numeric(FlightSummary$Humidity) - as.numeric(FlightSummary$Humidity.ON))
  
  ## POST PROCESSING
  # Showing only two decimal places
  FlightSummary$FlightTime_hrs <- round(FlightSummary$FlightTime_hrs, digits = 2)
  FlightSummary$Taxi.OUT_mins <- round(FlightSummary$Taxi.OUT_mins, digits = 2)
  FlightSummary$Taxi.IN_mins <- round(FlightSummary$Taxi.IN_mins, digits = 2)
  FlightSummary$TAT_hrs <- round(FlightSummary$TAT_hrs, digits = 2)
  # Eliminating the non-sensical ones
  FlightSummary$GCD_km[FlightSummary$GCD_km < 20]  <- NA 
  FlightSummary$Duration[FlightSummary$GCD_km < 20]  <- NA 
  FlightSummary$Taxi.OUT_mins[FlightSummary$GCD_km < 20]  <- NA 
  FlightSummary$Taxi.OUT_mins[FlightSummary$GCD_km < 20]  <- NA 
  FlightSummary$TAT_hrs[FlightSummary$GCD_km < 20]  <- NA 
  # Reorder columns
  FlightSummary <- FlightSummary[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", "GCD_km", 
                                    "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type", "OUT", "OFF", "ON", "IN",
                                    "OUTtimestamp", "RowNum.OUT", "Latitude.OUT", "Longitude.OUT", 
                                    "OFFtimestamp", "RowNum.OFF", "Latitude.OFF", "Longitude.OFF",
                                    "ONtimestamp", "RowNum.ON", "Latitude.ON", "Longitude.ON", 
                                    "INtimestamp", "RowNum.IN", "Latitude.IN", "Longitude.IN",
                                    "RowNum", "OFFround", "Time.Local", "TemperatureC", "Dew.PointC", "Humidity", "Sea.Level.PressurehPa", "VisibilityKm", 
                                    "Wind.Direction", "Wind.SpeedKm.h", "Gust.SpeedKm.h", "Precipitationmm", "Events", "Conditions", "WindDirDegrees",
                                    "ONround", "Time.Local.ON", "TemperatureC.ON", "Dew.PointC.ON", "Humidity.ON", "Sea.Level.PressurehPa.ON", "VisibilityKm.ON", 
                                    "Wind.Direction.ON", "Wind.SpeedKm.h.ON", "Gust.SpeedKm.h.ON", "Precipitationmm.ON", "Events.ON", "Conditions.ON", "WindDirDegrees.ON", 
                                    "TempDiff", "HumidDiff")]
  names(FlightSummary) <- c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", "GCD_km", 
                            "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type", "OUT", "OFF", "ON", "IN",
                            "OUTtimestamp", "RowNum.OUT", "Latitude.OUT", "Longitude.OUT", 
                            "OFFtimestamp", "RowNum.OFF", "Latitude.OFF", "Longitude.OFF",
                            "ONtimestamp", "RowNum.ON", "Latitude.ON", "Longitude.ON", 
                            "INtimestamp", "RowNum.IN", "Latitude.IN", "Longitude.IN",
                            "RowNum", "OFFround", "LocalTime.OFF", "Temperature_C.OFF", "DewPoint_C.OFF", "Humidity.OFF", "SeaLevelPressure_hPa.OFF", "Visibility_Km.OFF", 
                            "WindDir.OFF", "WindSpeed_Kmh.OFF", "GustSpeed_Kmh.OFF", "Precip_mm.OFF", "Events.OFF", "Conditions.OFF", "WindDir_Deg.OFF",
                            "ONround", "LocalTime.ON", "Temperature_C.ON", "DewPoint_C.ON", "Humidity.ON", "SeaLevelPressure_hPa.ON", "Visibility_Km.ON", 
                            "WindDir.ON", "WindSpeed_Kmh.ON", "GustSpeed_Kmh.ON", "Precip_mm.ON", "Events.ON", "Conditions.ON", "WindDir_Deg.ON",
                            "TempDiff", "HumidDiff")
  FlightSummary$OUT <- as.POSIXct(strptime(FlightSummary$OFF, "%d/%m/%Y %H:%M:%S", tz="GMT"))
  FlightSummary$OFF <- as.POSIXct(strptime(FlightSummary$OFF, "%d/%m/%Y %H:%M:%S", tz="GMT"))
  FlightSummary$ON <- as.POSIXct(strptime(FlightSummary$ON, "%d/%m/%Y %H:%M:%S", tz="GMT"))
  FlightSummary$IN <- as.POSIXct(strptime(FlightSummary$IN, "%d/%m/%Y %H:%M:%S", tz="GMT"))
  
  return(FlightSummary)
}