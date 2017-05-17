#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## A FUNCTION that checks the inpolygon

airportchecks <- function(feature, Coordinates, points, pointsdata, aerw_lines) {
  
  # when changing airport no error
  if (Coordinates == "") {check <- data.frame("NO DATA")} else {
    # initiating matrix
    check <- matrix(nrow = length(points), ncol = length(feature))
    # in polygon check for data with only 1 apron (stored differently)
    if (length(feature) == 1) {
      a = length(sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]])))/2+1
      b = length(sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]])))
      check[1:length(points),1] <- point.in.polygon(coordinates(points)[1:length(points),2],
                                                    coordinates(points)[1:length(points),1],
                                                    sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[1:a-1],
                                                    sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[a:b])
    } else if (length(feature) == 2 & deparse(substitute(feature))=="aeap_poly") { # (same as before, stored differently)
      a = length(sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[,1])/2+1
      b = length(sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[,1])
      for (j in 1:2) {
        check[1:length(points),j] <- point.in.polygon(coordinates(points)[1:length(points),2],
                                                      coordinates(points)[1:length(points),1],
                                                      sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[1:a-1,j],
                                                      sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]]))[a:b,j])
      }
    } else { # general case (using sapply for this case)
      check[1:length(points),] <- sapply(sapply(feature@polygons, function(x) coordinates(x@Polygons[[1]])), 
                                         function(y) point.in.polygon(coordinates(points)[1:length(points),2],
                                                                      coordinates(points)[1:length(points),1],y[,2], y[,1]))
    }
    # no data when no in polygon has been detected
    if (is.na(match(1, check))) {check <- data.frame("NO DATA")} else {
      check <- ifelse(check==1, 1:nrow(check), 0)
      check <- check[rowSums(check)>0,]
      # add aircraft data (error if only 1 is present)
      check[1:nrow(check),] <- sapply(as.numeric(check[1:nrow(check),]), 
                                      function(x) ifelse(x>0, paste(as.character(pointsdata$Registration[x]),
                                                                    as.character(pointsdata$FlightNumber[x]),
                                                                    as.character(pointsdata$AC_type[x]),
                                                                    as.character(ifelse(deparse(substitute(points))=="outpoints",
                                                                                        pointsdata$OUT[x],
                                                                                        ifelse(deparse(substitute(points))=="inpoints",
                                                                                               pointsdata$IN[x],
                                                                                               ifelse(deparse(substitute(points))=="offpoints",
                                                                                                      pointsdata$OFF[x],
                                                                                                      pointsdata$ON[x])))), sep=" # "), 0))
      check <- data.frame(check)
      check[check==0] <- NA
      # column names
      names(check)[1:length(feature)] <- paste0(ifelse(is.null(aerw_lines),"Apron","Runway"), 1:length(feature))#feature$id[1:length(feature)]
      # when only one apron then it becomes a factor therefore it has to be transformed back to dataframe
      if (length((colSums(is.na(check)) != nrow(check))[(colSums(is.na(check)) != nrow(check))==T])==1) {
        a <- names((colSums(is.na(check)) != nrow(check))[(colSums(is.na(check)) != nrow(check))==T])
        check <- data.frame(check[, colSums(is.na(check)) != nrow(check)])
        names(check) <- a
      } else {
        # remove empty columnsairportchecks(feature(), input$Coordinates, inpoints(), inpointsdata())
        check <- check[, colSums(is.na(check)) != nrow(check)]
        check <- data.frame(lapply(check, beetroot))
        # remove NAs
        check <- check[1:max(colSums(!is.na(check))),]
      }
    }
    
    
    if (!is.null(aerw_lines)) {
      m <- data.frame(gDistance(points, aerw_lines, byid=TRUE))
      m <- apply(m[,1:ncol(m)], 1, function(x) ifelse(x<10^(-4), x, NA))
      m <- ifelse(!is.na(m), 1:nrow(m), NA)
      if (length((colSums(is.na(m)) != nrow(m))[(colSums(is.na(m)) != nrow(m))==T])==1) {
        a <- names((colSums(is.na(m)) != nrow(m))[(colSums(is.na(m)) != nrow(m))==T])
        m <- data.frame(m[, colSums(is.na(m)) != nrow(m)])
      } else {
        m <- m[rowSums(is.na(m)) != ncol(m), colSums(is.na(m)) != nrow(m)]
        m <- data.frame(m)
      }
      m <- data.frame(lapply(m, beetroot))
      m <- m[1:max(colSums(!is.na(m))),]
      m <- as.matrix(m)
      m[1:nrow(m),] <- sapply(as.numeric(m[1:nrow(m),]),
                              function(x) ifelse(!is.na(x), paste(as.character(pointsdata$Registration[x]),
                                                                  as.character(pointsdata$FlightNumber[x]),
                                                                  as.character(pointsdata$AC_type[x]),
                                                                  ifelse(deparse(substitute(points))=="offpoints",
                                                                         pointsdata$OFF[x],
                                                                         pointsdata$ON[x]), sep=" # "), NA))
      m <- data.frame(m)
      if (ncol(m)==1) {
        names(m) <- a
      }
      if (check[1,1] == "NO DATA") { # if only lines in airport
        check <- m
      } else {  # if mixture of lines and poly in airport
        n <- check
        m <- data.frame(m)
        if (nrow(n)>nrow(m)) {
          m[(nrow(m)+1):nrow(n),] <- NA
        } else if (nrow(n)<nrow(m)) {
          n[(nrow(n)+1):nrow(m),] <- NA
        }
        check <- cbind(n, m)
      }
    }
    
  }
  
  return(check)
}