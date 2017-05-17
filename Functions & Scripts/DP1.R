#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that creates the SQL query for the data download and it imports it

DP1 <- function(fr24data) {
  
  ## DATA PREPARATION
  # Create a dataframe and sort by Registration and time
  attach(fr24data)
  fr24data <- fr24data[order(Registration,Timestamp),]
  detach(fr24data)
  
  # Creating new columns: row number, date-time
  fr24data$RowNum <- seq.int(nrow(fr24data))
  
  ############################
  ### ON-GROUND ESTIMATION ###
  ############################
  # Calculating Time Difference between signals (considering Reg#)
  fr24data$TimeDiff <- ifelse(c(0,diff(fr24data$Registration)) == 0, c(0,diff(fr24data$Timestamp/60)), 0) # Time Difference in minutes
  # Check Input
  fr24data <- fr24data[,c("RowNum", "Timestamp", "TimeDiff", "Registration", "FlightNumber", "Callsign", "Equipement", "Departure", "Destination",
                          "Latitude", "Longitude", "Altitude", "GroundSpeed")] # Track
  # Change Equipement to AC_type
  names(fr24data) <- c("RowNum", "Timestamp", "TimeDiff", "Registration", "FlightNumber", "Callsign", "AC_type", "Departure", "Destination",
                       "Latitude", "Longitude", "Altitude", "GroundSpeed")
  # Initialising OnGround
  fr24data$OnGround <- rep(0, nrow(fr24data))
  ## OnGround Estimation (based on in the air conditions)
  # Check 1
  fr24data$OnGround <- ifelse(fr24data$Altitude > 0, 0, 1) # CONDITION 1: if altitude > 0 always IN AIR
  #Check 2
  fr24data$OnGround <- c(fr24data$OnGround[1], ifelse(fr24data$OnGround[1:(nrow(fr24data)-2)] == fr24data$OnGround[3:nrow(fr24data)] &
                                                        fr24data$OnGround[1:(nrow(fr24data)-2)] != fr24data$OnGround[2:(nrow(fr24data)-1)],
                                                      fr24data$OnGround[1:(nrow(fr24data)-2)], fr24data$OnGround[2:(nrow(fr24data)-1)]),
                         fr24data$OnGround[nrow(fr24data)])
  
  
  
  #######################
  ### OOOI ESTIMATION ###
  #######################
  ## Assumptions
  stopassump <- 15
  # Initialising OUT, OFF, ON, IN columns and other parameters in fr24data
  fr24data$OUTtimestamp <- rep(0, nrow(fr24data))
  fr24data$OUTcandidate <- rep(0, nrow(fr24data))
  fr24data$OUT_OFFtimestamp <- rep(0, nrow(fr24data))
  fr24data$OFFtimestamp <- rep(0, nrow(fr24data))
  fr24data$ONtimestamp <- rep(0, nrow(fr24data))
  fr24data$ON_OFFtimestamp <- rep(0, nrow(fr24data))
  fr24data$INtimestamp <- rep(0, nrow(fr24data))
  fr24data$INcandidate <- rep(0, nrow(fr24data))
  fr24data$IN_ONtimestamp <- rep(0, nrow(fr24data))
  
  ## OFF Estimation
  fr24data$OFFtimestamp[2:nrow(fr24data)]  <- ifelse(fr24data$OnGround[2:nrow(fr24data)] == 0 & fr24data$OnGround[1:(nrow(fr24data)-1)] == 1 & 
                                                       fr24data$Registration[2:nrow(fr24data)] == fr24data$Registration[1:(nrow(fr24data)-1)] & 
                                                       fr24data$FlightNumber[2:nrow(fr24data)] == fr24data$FlightNumber[1:(nrow(fr24data)-1)] & 
                                                       fr24data$Departure[2:nrow(fr24data)] == fr24data$Departure[1:(nrow(fr24data)-1)] & 
                                                       fr24data$Destination[2:nrow(fr24data)] == fr24data$Destination[1:(nrow(fr24data)-1)] & 
                                                       fr24data$TimeDiff[2:nrow(fr24data)] < stopassump, fr24data$Timestamp[2:nrow(fr24data)], 0)
  ## ON Estimation
  fr24data$ONtimestamp[2:nrow(fr24data)] <- ifelse(fr24data$OnGround[2:nrow(fr24data)] == 1 & fr24data$OnGround[1:(nrow(fr24data)-1)] == 0 & 
                                                     fr24data$Registration[2:nrow(fr24data)] == fr24data$Registration[1:(nrow(fr24data)-1)] & 
                                                     fr24data$FlightNumber[2:nrow(fr24data)] == fr24data$FlightNumber[1:(nrow(fr24data)-1)] & 
                                                     fr24data$Departure[2:nrow(fr24data)] == fr24data$Departure[1:(nrow(fr24data)-1)] & 
                                                     fr24data$Destination[2:nrow(fr24data)] == fr24data$Destination[1:(nrow(fr24data)-1)] & 
                                                     fr24data$TimeDiff[2:nrow(fr24data)] < stopassump, fr24data$Timestamp[2:nrow(fr24data)] , 0)
  
  ## OUT-IN CANDIDATES
  # Putting 3 to the ones that might be in times, in order to keep them
  fr24data$OUTcandidate[2:nrow(fr24data)] <- ifelse(fr24data$TimeDiff[2:nrow(fr24data)] > stopassump & fr24data$OnGround[2:nrow(fr24data)] == 1 &
                                                      fr24data$Registration[2:nrow(fr24data)] == fr24data$Registration[1:(nrow(fr24data)-1)], 3, 
                                                    ifelse(fr24data$Registration[2:nrow(fr24data)] != fr24data$Registration[1:(nrow(fr24data)-1)] & 
                                                             fr24data$OnGround[2:nrow(fr24data)] == 1, 3, 0))
  
  fr24data$INcandidate[1:(nrow(fr24data)-1)] <- ifelse(fr24data$TimeDiff[2:nrow(fr24data)] > stopassump & fr24data$OnGround[1:(nrow(fr24data)-1)] == 1 &
                                                         fr24data$Registration[2:nrow(fr24data)] == fr24data$Registration[1:(nrow(fr24data)-1)], 3, 
                                                       ifelse(fr24data$Registration[2:nrow(fr24data)] != fr24data$Registration[1:(nrow(fr24data)-1)] & 
                                                                fr24data$OnGround[1:(nrow(fr24data)-1)] == 1, 3, 0))
  
  return(fr24data)
}