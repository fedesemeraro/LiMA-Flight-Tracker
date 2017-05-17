#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that creates the SQL query for the data download and it imports it

dataimport <- function(altitude, groundspeed, latitude, longitude, fleetopt, RegNum_chosen, FlNum_chosen, Dep_chosen, Dest_chosen, airport, dateout) {
  
  fr24query <- "SELECT Timestamp, Registration, FlightNumber, Equipement, Departure, Destination, Latitude, Longitude, Altitude, GroundSpeed, Callsign, Track\n  FROM dbo.FR24_RAWDATA"
  
  if (!is.null(altitude) | !is.null(groundspeed) | !is.null(latitude) | !is.null(longitude) |
      !is.null(fleetopt) | (RegNum_chosen!="") | (FlNum_chosen!="") | !is.null(Dep_chosen) |
      !is.null(Dest_chosen) | !is.null(airport)) {
    fr24query <- paste(fr24query, "\n                  WHERE ", sep="")
  }
  
  # Check
  check = 0
  
  # Selecting Altitude
  if (!is.null(altitude)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (Altitude>", altitude[1], " AND Altitude<", altitude[2], ")", sep="")
    } else {
      fr24query <- paste(fr24query, "(Altitude>", altitude[1], " AND Altitude<", altitude[2], ")", sep="")
    }
    check = 1
  }
  
  # Selecting Ground Speed
  if (!is.null(groundspeed)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (GroundSpeed>", groundspeed[1], " AND GroundSpeed<", groundspeed[2], ")", sep="")
    } else {
      fr24query <- paste(fr24query, "(GroundSpeed>", groundspeed[1], " AND GroundSpeed<", groundspeed[2], ")", sep="")
    }
    check = 1
  }
  
  # Selecting Latitude
  if (!is.null(latitude)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (latitude>", latitude[1], " AND latitude<", latitude[2], ")", sep="")
    } else {
      fr24query <- paste(fr24query, "(latitude>", latitude[1], " AND latitude<", latitude[2], ")", sep="")
    }
    check = 1
  }
  
  # Selecting Longitude
  if (!is.null(longitude)){
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (longitude>", longitude[1], " AND longitude<", longitude[2], ")", sep="")
    } else {
      fr24query <- paste(fr24query, "(longitude>", longitude[1], " AND longitude<", longitude[2], ")", sep="")
    }
    check = 1
  }
  
  # Selecting Fleets
  if (!is.null(fleetopt)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    for (i in 1:length(fleetopt)) {
      if (i == length(fleetopt)) {
        fr24query <- paste(fr24query, "Equipement='", fleetopt[i], "')", sep="")
      } else if (i < length(fleetopt)) {
        fr24query <- paste(fr24query, "Equipement='", fleetopt[i], "' OR ", sep="")
      }
    }
    check = 1
  }
  
  # Selecting Registration
  if (!is.null(RegNum_chosen) & RegNum_chosen!="") {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    fr24query <- paste(fr24query, "Registration='", RegNum_chosen, "')", sep="")
    check = 1
  }
  
  # Selecting Flight Number LEFT(FlightNumber,2)
  if (!is.null(FlNum_chosen) & FlNum_chosen!="") {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    if (nchar(FlNum_chosen)==2){
      fr24query <- paste(fr24query, "LEFT(FlightNumber,2)='", FlNum_chosen, "')", sep="")
    } else {
      fr24query <- paste(fr24query, "FlightNumber='", FlNum_chosen, "')", sep="")
    }
    check = 1
  }
  
  # Selecting Departure Airport
  if (!is.null(Dep_chosen)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    for (i in 1:length(Dep_chosen)) {
      if (i == length(Dep_chosen)) {
        fr24query <- paste(fr24query, "Departure='", Dep_chosen[i], "')", sep="")
      } else if (i < length(Dep_chosen)) {
        fr24query <- paste(fr24query, "Departure='", Dep_chosen[i], "' OR ", sep="")
      }
    }
    check = 1
  }
  
  # Selecting Destination Airport
  if (!is.null(Dest_chosen)) {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    for (i in 1:length(Dest_chosen)) {
      if (i == length(Dest_chosen)) {
        fr24query <- paste(fr24query, "Destination='", Dest_chosen[i], "')", sep="")
      } else if (i < length(Dest_chosen)) {
        fr24query <- paste(fr24query, "Destination='", Dest_chosen[i], "' OR ", sep="")
      }
    }
    check = 1
  }
  
  # Selecting Airport (only one supported)
  if (airport!="") {
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (", sep="")
    } else {
      fr24query <- paste(fr24query, "(", sep="")
    }
    fr24query <- paste(fr24query, "Departure='", airport, "' OR Destination='", airport, "')", sep="")
    check = 1
  }
  
  # Selecting Dates
  if (!is.null(dateout)){
    if (check == 1) {
      fr24query <- paste(fr24query, "\n                  AND (Timestamp>",  as.numeric(as.POSIXct(dateout[1])), " AND Timestamp<",  as.numeric(as.POSIXct(dateout[2]))+86400, ")", sep="")
    } else {
      fr24query <- paste(fr24query, "(Timestamp>",  as.numeric(as.POSIXct(dateout[1])) , " AND Timestamp<", as.numeric(as.POSIXct(dateout[2]))+86400, ")", sep="")
    }
    check = 1
  }
  
  # SQL QUERY
  fr24channel <- odbcConnect(dsn="FR24",uid="FR24",pwd="Airbus01052016",readOnly="True")
  fr24data <- sqlQuery(fr24channel, fr24query)
  
  # Close Query
  odbcCloseAll()
  
  fr24data <- data.frame(fr24data)
  
  return(fr24data)
}