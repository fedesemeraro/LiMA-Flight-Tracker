#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that creates the SQL query for the OAG download and it imports it

oagimport <- function(FlightSummary) {
  
  flightnumberopcode <- substr(FlightSummary$FlightNumber, 1, 2)
  flightnumbernumber <- substr(FlightSummary$FlightNumber, 3, length(FlightSummary$FlightNumber))
  month <- unique(format(FlightSummary$OUT[!is.na(FlightSummary$OUT)], "%m"))
  month <- ifelse(substr(month, 1, 1)=="0", substr(month, 2, 2), month)
  
  # Connection
  oagchannel <-odbcConnect("OAG", uid="AIRBUS", pwd="OAG_MF30")
  
  # Query 
  ## Columns Available
  # "SELECT LEG_ID, SUPPLY_YEAR, SUPPLY_MONTH, DUPLICATE_NON_OPERATIONAL, ",
  # "AIRLINE_CODE, FLIGHT_NUMBER, OPERATOR, AIRCRAFT_OWNER, ",
  # "ITINERARY_VARIATION_ID, LEG_SEQUENCE_NUMBER, AIRPORT_DEPARTURE_CODE, ",
  # "CITY_DEPARTURE_CODE, COUNTRY_DEPARTURE_CODE, REGION_DEPARTURE_CODE, ",
  # "AIRPORT_ARRIVAL_CODE, CITY_ARRIVAL_CODE, COUNTRY_ARRIVAL_CODE, ",
  # "REGION_ARRIVAL_CODE, AIRCRAFT_FLAG, AIRCRAFT_CATEGORY_CODE, ",
  # "SUB_TYPE_CODE, SERVICE_CODE, BOOKING_CLASS_DESIGNATOR_PRBD, ",
  # "CABIN_CODE_FROM_PRBD, AIRCRAFT_CONFIGURATION_ORIGIN, ACCONF_SOURCE, ",
  # "AIRCRAFT_CONFIGURATION, CABIN_CODE, TOTAL_SEATS, FIRST_SEATS, ",
  # "BUSINESS_SEATS, ECOPREMIUM_SEATS, ECO_SEATS, LEG_DISTANCE_KM, ",
  # "LEG_FLYING_TIME_MINUTES, AGREEMENT_CODE, MULTI_CARRIER_DESIGNATION, ",
  # "DUPLICATING_FLIGHTS, CODE_SHARE_OPERATING_AL_CODE, CODE_SHARE_OPERATING_FLT_NB, ",
  # "GHOST_FLIGHT, LOCAL_DEPARTURE_TIME_HHMM, LOCAL_DEPARTURE_TIME_MINUTES, ",
  # "LOCAL_ARRIVAL_TIME_HHMM, LOCAL_ARRIVAL_TIME_MINUTES, LOCAL_ARRIVAL_DAY, ",
  # "UTC_DEPARTURE_TIME_HHMM, UTC_DEPARTURE_TIME_MINUTES, UTC_ARRIVAL_TIME_HHMM, ",
  # "UTC_ARRIVAL_TIME_MINUTES, UTC_ARRIVAL_DAY, UTC_LOCAL_TIME_VAR_DEP_TXT, ",
  # "UTC_LOCAL_TIME_VAR_DEP_NUM, UTC_LOCAL_TIME_VAR_ARR_TXT, UTC_LOCAL_TIME_VAR_ARR_NUM, ",
  # "DAYS_OF_OPERATION_ORIGIN, DAYS_OF_OPERATION_LOCAL, DAYS_OF_OPERATION_UTC, ",
  # "MONTHLY_FREQUENCY, FREQ_DAY1, FREQ_DAY2, FREQ_DAY3, ",
  # "FREQ_DAY4, FREQ_DAY5, FREQ_DAY6, FREQ_DAY7, FREQ_DAY8, ",
  # "FREQ_DAY9, FREQ_DAY10, FREQ_DAY11, FREQ_DAY12, FREQ_DAY13, ",
  # "FREQ_DAY14, FREQ_DAY15, FREQ_DAY16, FREQ_DAY17, FREQ_DAY18, ",
  # "FREQ_DAY19, FREQ_DAY20, FREQ_DAY21, FREQ_DAY22, FREQ_DAY23, ",
  # "FREQ_DAY24, FREQ_DAY25, FREQ_DAY26, FREQ_DAY27, FREQ_DAY28, ",
  # "FREQ_DAY29, FREQ_DAY30, FREQ_DAY31, FLIGHT_CIRCULAR_FLAG, ",
  # "FLIGHT_NUMBER_OF_LEG, FLIGHT_GROUND_TIME_MINUTES, FLIGHT_FLYING_TIME_MINUTES, ",
  # "FLIGHT_DISTANCE_KM, FLIGHT_AIRCRAFT_CHANGE, FLIGHT_AIRPORT_ROUTING, ",
  # "FLIGHT_COUNTRY_DEPARTURE_CODE, FLIGHT_COUNTRY_ARRIVAL_CODE, EFFECTIVE_FROM_DATE, ",
  # "EFFECTIVE_TO_DATE, TRAFFIC_RESTRICTION, PASSENGER_TERMINAL_DEP, ",
  # "PASSENGER_TERMINAL_ARR, DEI_DATA_050, DEI_DATA_113, DEI_DATA_127, ",
  # "SUB_TYPE_CODE_ORIGIN, FLIGHT_LOCAL_DEP_TIME_HHMM, FLIGHT_LOCAL_ARR_TIME_HHMM, ",
  # "FLIGHT_LOCAL_ARRIVAL_DAY, CODE_SHARING, COCKPIT_CREW_EMPLOYER, CABIN_CREW_EMPLOYER ",
  oagquery <- paste0("SELECT ", 
                     
                     "FREQ_DAY1, FREQ_DAY2, FREQ_DAY3, FREQ_DAY4, FREQ_DAY5, FREQ_DAY6, FREQ_DAY7, FREQ_DAY8, FREQ_DAY9, FREQ_DAY10, FREQ_DAY11, FREQ_DAY12, FREQ_DAY13, ",
                     "FREQ_DAY14, FREQ_DAY15, FREQ_DAY16, FREQ_DAY17, FREQ_DAY18, FREQ_DAY19, FREQ_DAY20, FREQ_DAY21, FREQ_DAY22, FREQ_DAY23, ",
                     "FREQ_DAY24, FREQ_DAY25, FREQ_DAY26, FREQ_DAY27, FREQ_DAY28, FREQ_DAY29, FREQ_DAY30, FREQ_DAY31, ",
                     "SUPPLY_YEAR, SUPPLY_MONTH, SUB_TYPE_CODE, AIRLINE_CODE, FLIGHT_NUMBER, AIRPORT_DEPARTURE_CODE, AIRPORT_ARRIVAL_CODE, ",
                     "UTC_DEPARTURE_TIME_MINUTES, UTC_ARRIVAL_TIME_MINUTES, LEG_FLYING_TIME_MINUTES ",
                     
                     "FROM MF30_PRO.MF30_LEG_MONTHLY MF30_LEG_MONTHLY  ",
                     "WHERE ((SUPPLY_YEAR=2015) AND (SUPPLY_MONTH=", month, ")) AND ")
  # FlightNumber
  oagquery <- paste(oagquery, "(", sep="")
  for (i in 1:length(flightnumberopcode)) {
    if (i == length(flightnumberopcode)) {
      oagquery <- paste(oagquery, "(AIRLINE_CODE='", flightnumberopcode[i], "' AND FLIGHT_NUMBER='", flightnumbernumber[i], "')", sep="")
    } else if (i < length(flightnumberopcode)) {
      oagquery <- paste(oagquery, "(AIRLINE_CODE='", flightnumberopcode[i], "' AND FLIGHT_NUMBER='", flightnumbernumber[i], "') OR ", sep="")
    }
  }
  oagquery <- paste(oagquery, ")", sep="")
  # Download
  oagdata <- sqlQuery(oagchannel, oagquery)
  odbcCloseAll()
  
  ## POST-PROCESSING
  oagdata <- data.frame(oagdata)
  oagdata$FN <- paste0(oagdata$AIRLINE_CODE, oagdata$FLIGHT_NUMBER)
  oagdata$AIRLINE_CODE <- NULL
  oagdata$FLIGHT_NUMBER <- NULL
  # Filtering columns and data 
  datenum <- as.numeric(unique(format(FlightSummary$OUT[!is.na(FlightSummary$OUT)], "%d")))
  oagdata <- subset(oagdata, oagdata[,c(datenum, 32:ncol(oagdata))]==1)
  oagdata <- oagdata[,c(32:ncol(oagdata))]
  oagdata <- oagdata[rowSums(is.na(oagdata))!=length(oagdata), ]
  # Merging Dataframes
  oagschedule <- merge(FlightSummary[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination",
                                         "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                         "OUT", "OFF", "ON", "IN", "RowNum")], oagdata, 
                       by.x=c("FlightNumber", "Departure", "Destination"), 
                       by.y=c("FN", "AIRPORT_DEPARTURE_CODE", "AIRPORT_ARRIVAL_CODE"), sort=FALSE, all.x=T)
  # Other adjustments
  oagschedule$Scheduled_OUT <- as.POSIXct(oagschedule$UTC_DEPARTURE_TIME_MINUTES*60, origin=as.Date(format(oagschedule$OUT, "%Y-%m-%d")),tz="GMT")
  oagschedule$Scheduled_ON <- oagschedule$Scheduled_OUT + oagschedule$LEG_FLYING_TIME_MINUTES*60
  oagschedule <- oagschedule[order(oagschedule$RowNum),]
  # Delays Calculation
  oagschedule$Dep_Delay_mins <- ifelse(!is.na(oagschedule$Scheduled_OUT) & !is.na(oagschedule$OUT), round(as.numeric((oagschedule$OUT - oagschedule$Scheduled_OUT), units = "mins"), digits = 2), NA)
  oagschedule$Arr_Delay_mins <- ifelse(!is.na(oagschedule$Scheduled_ON) & !is.na(oagschedule$ON), round(as.numeric((oagschedule$ON - oagschedule$Scheduled_ON), units = "mins"), digits = 2), NA)
  oagschedule <- oagschedule[,c("Registration", "Operator", "FlightNumber", "SUB_TYPE_CODE", "AC_type", "Departure", "Destination", "Scheduled_OUT", "OUT", "OFF",
                                "Scheduled_ON","ON", "IN", "Dep_Delay_mins", "Arr_Delay_mins", "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type")]
  names(oagschedule) <- c("Registration", "Operator", "FlightNumber", "Scheduled_AC_type", "AC_type", "Departure", "Destination", "Scheduled_OUT", "OUT", "OFF", 
                          "Scheduled_ON","ON", "IN", "Dep_Delay_mins", "Arr_Delay_mins", "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type")
  
  return(oagschedule)
}