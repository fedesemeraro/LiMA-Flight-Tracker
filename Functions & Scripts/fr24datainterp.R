#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that creates the interpolation for map3

# FlightSummary <- read.csv("O:/ENGINEERING/EIO/UK_E86_R_and_T/DEGs_and_Interns/INT_FSemeraro/ShareRNet/FR24 - R Analysis Tool/WIP/FlightSummary.csv")
# fr24data <- read.csv("O:/ENGINEERING/EIO/UK_E86_R_and_T/DEGs_and_Interns/INT_FSemeraro/ShareRNet/FR24 - R Analysis Tool/WIP/RawData.csv")

fr24datainterp <- function(FlightSummary, fr24data) {
  
  # Filtering Data
  usableflights <- subset(FlightSummary, !is.na(FlightSummary$RowNum.OUT) & !is.na(FlightSummary$RowNum.IN))
  
  # Creating TimeFrame dataframe
  tsrange <- NA
  tsrange[1] <- min(usableflights$OUTtimestamp)
  tsrange[2] <- max(usableflights$INtimestamp)
  # Unit for Timestamp Sincronisation (1 minute)
  tstenmins <- round((tsrange[2]-tsrange[1])/60)
  timeframe <- matrix(nrow=tstenmins, ncol=1+2*nrow(usableflights))
  timeframe[,1] <- tsrange[1] + ((tsrange[2]-tsrange[1])/tstenmins)*(1:tstenmins)
  
  for (i in 1:nrow(usableflights)) {
    # Subsetting 1 Flight data
    flight <- subset(fr24data, fr24data$RowNum>usableflights$RowNum.OUT[i] & fr24data$RowNum<usableflights$RowNum.IN[i])
    # Unit for Timestamp Interpolation (10 seconds)
    flighttenmins <- round((flight$Timestamp[nrow(flight)]-flight$Timestamp[1])/10)
    flightinterp <- matrix(nrow=flighttenmins, ncol=3)
    # Timestamps Interpolation
    flightinterp[,1] <- flight$Timestamp[1] + ((flight$Timestamp[nrow(flight)]-flight$Timestamp[1])/flighttenmins)*(1:flighttenmins)
    # Latitude Interpolation
    flightinterp[,2] <- approx(flight$Timestamp, flight$Latitude, method="linear", n=nrow(flightinterp))$y
    # Longitude Interpolation
    flightinterp[,3] <- approx(flight$Timestamp, flight$Longitude, method="linear", n=nrow(flightinterp))$y
    
    # Sincronisation: start and end rows
    start <- which.min(abs(timeframe[,1] - flightinterp[1,1]))
    end <- which.min(abs(timeframe[,1] - flightinterp[nrow(flightinterp),1]))
    # Finding closest (interpolated) timestamp signal
    datarows <- apply(timeframe[start:end,], 1, function(x) which.min(abs(x[1] - flightinterp[,1])))
    # Transferring Lat and Lon
    timeframe[start:end,2*i] <- flightinterp[datarows,2]
    timeframe[start:end,2*i+1] <- flightinterp[datarows,3]
  }
  return(timeframe)
}