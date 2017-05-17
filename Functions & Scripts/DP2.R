#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that processes data

DP2 <- function(fr24data) {
  
  #######################
  ### OOOI ESTIMATION ###
  #######################
  # Assumptions
  stopassump <- 15
  # Isolating the important info from fr24data
  oooi <- subset(fr24data, fr24data$OFFtimestamp > 0 | fr24data$ONtimestamp > 0 | fr24data$OUTcandidate == 3 | fr24data$INcandidate == 3)
  # Passing OFF timestamp to ON time (already calculated the middle one)
  oooi$ON_OFFtimestamp[2:nrow(oooi)] <- ifelse(oooi$ONtimestamp[2:nrow(oooi)] > 0 & oooi$OFFtimestamp[1:(nrow(oooi)-1)] > 0 &
                                                 oooi$Registration[2:nrow(oooi)] == oooi$Registration[1:(nrow(oooi)-1)] & 
                                                 oooi$FlightNumber[2:nrow(oooi)] == oooi$FlightNumber[1:(nrow(oooi)-1)] & 
                                                 oooi$Departure[2:nrow(oooi)] == oooi$Departure[1:(nrow(oooi)-1)] & 
                                                 oooi$Destination[2:nrow(oooi)] == oooi$Destination[1:(nrow(oooi)-1)], 
                                               oooi$OFF[1:nrow(oooi)], 0)
  ## OUT Estimation
  oooi$OUTtimestamp[1:(nrow(oooi)-1)] <- ifelse(oooi$OFFtimestamp[2:nrow(oooi)] > 0 & oooi$OUTcandidate[1:(nrow(oooi)-1)] == 3 &
                                                  oooi$Registration[2:nrow(oooi)] == oooi$Registration[1:(nrow(oooi)-1)] & 
                                                  oooi$FlightNumber[2:nrow(oooi)] == oooi$FlightNumber[1:(nrow(oooi)-1)] & 
                                                  oooi$Departure[2:nrow(oooi)] == oooi$Departure[1:(nrow(oooi)-1)] & 
                                                  oooi$Destination[2:nrow(oooi)] == oooi$Destination[1:(nrow(oooi)-1)], 
                                                oooi$Timestamp[1:(nrow(oooi)-1)], 0)
  # Passing OFF timestamp to OUT time
  oooi$OUT_OFFtimestamp[1:(nrow(oooi)-1)] <- ifelse(oooi$OUTtimestamp[1:(nrow(oooi)-1)] > 0 & oooi$OFFtimestamp[2:nrow(oooi)] > 0 & 
                                                      oooi$OUTcandidate[1:(nrow(oooi)-1)] == 3 &
                                                      oooi$Registration[2:nrow(oooi)] == oooi$Registration[1:(nrow(oooi)-1)] & 
                                                      oooi$FlightNumber[2:nrow(oooi)] == oooi$FlightNumber[1:(nrow(oooi)-1)] & 
                                                      oooi$Departure[2:nrow(oooi)] == oooi$Departure[1:(nrow(oooi)-1)] & 
                                                      oooi$Destination[2:nrow(oooi)] == oooi$Destination[1:(nrow(oooi)-1)], 
                                                    oooi$OFFtimestamp[2:nrow(oooi)], 0)
  ## IN Estimation
  oooi$INtimestamp[2:nrow(oooi)] <- ifelse(oooi$ONtimestamp[1:(nrow(oooi)-1)] > 0 & oooi$INcandidate[2:nrow(oooi)] == 3 & 
                                             oooi$OUTtimestamp[1:(nrow(oooi)-1)] == 0 &
                                             oooi$Registration[2:nrow(oooi)] == oooi$Registration[1:(nrow(oooi)-1)], 
                                           oooi$Timestamp[2:nrow(oooi)], 0)
  # Passing ON timestamp to IN time
  oooi$IN_ONtimestamp[2:nrow(oooi)] <- ifelse(oooi$INtimestamp[2:nrow(oooi)] > 0 & oooi$ONtimestamp[1:(nrow(oooi)-1)] &
                                                oooi$Registration[2:nrow(oooi)] == oooi$Registration[1:(nrow(oooi)-1)],
                                              oooi$ONtimestamp[1:(nrow(oooi)-1)], 0)
  oooi <- oooi[,c("RowNum", "Timestamp", "Registration", "FlightNumber", "AC_type", "Departure", "Destination",
                  "Latitude", "Longitude", "Altitude", "GroundSpeed", "OnGround", "TimeDiff", "OUTtimestamp",  
                  "OFFtimestamp", "ONtimestamp", "INtimestamp","OUT_OFFtimestamp", "ON_OFFtimestamp", "IN_ONtimestamp")]
  oooi <- subset(oooi, oooi$OUTtimestamp > 0 | oooi$OFFtimestamp > 0 | oooi$ONtimestamp > 0 | oooi$INtimestamp > 0)
  
  return(oooi)
}