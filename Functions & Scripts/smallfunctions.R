#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------

## Some small FUNCTIONS used in the app

### TO RUN IN GOOGLE CHROME (it can be commented)
#launch.browser = function(appUrl) {shell(sprintf('"%s" --app=%s',file.path('C:/Users/FSemeraro/AppData/Local/Google/Chrome/Application/chrome.exe'),appUrl))}
# runApp('O:/ENGINEERING/EIO/UK_E86_R_and_T/DEGs_and_Interns/INT_FSemeraro/ShareRNet/FR24 - R Analysis Tool/FR24_AnalysisTool',launch.browser=launch.browser)


### TO CLEAR THE ENVIRONMENT IF NEEDED 
# closeAllConnections()
# rm(list=setdiff(ls(), "fr24data"))
# cat("\014")

# Function to Install and Load R Packages
install_load <- function(required_packages) {
  suppressMessages(require("devtools"))
  for (package_name in required_packages) {
    remaining_package <- package_name[!(package_name %in% installed.packages()[,"Package"])]
    if (length(remaining_package) > 0) {
      if (remaining_package == "rCharts") {
        install.packages("./Particular Packages/rCharts-master.tar.gz", repos = NULL, type="source")
      } else if (remaining_package == "leaflet") {
        install.packages("./Particular Packages/leaflet_1.0.1.tar.gz", repos = NULL, type="source")
      } else if (remaining_package == "shinyGlobe") {
        install_local("./Particular Packages/shinyGlobe-master.zip")
      } else {
        install.packages(remaining_package)
      }
    }
  }
  for(package_name in required_packages) {
    suppressMessages(require(package_name,character.only=TRUE,quietly=TRUE))
  }
}

## Small Functions
# Bring NA at the bottom
beetroot <- function(x) {
  num.na <- sum(is.na(x))
  x <- as.character(x[!is.na(x)])
  x <- c(x, rep(NA, num.na))
  return(x)
}
# Crunch data in one column
datacolcreation <- function(x) {
  data <- matrix(nrow=length(which(!is.na(x))), ncol=1)
  c <- 1
  for (j in 1:ncol(x)) {
    for (i in 1:nrow(x)) {
      if (!is.na(x[i,j])) {
        data[c,1] <- names(x)[j]
        c <- c + 1
      }
    }
  }
  return(data)
}
# STATS ggplot
statsplot <- function(out, ingate, off, on) {
  p1 <- ggplot(data.frame(out), aes(out)) +
    geom_bar(width=.75, fill="orange", colour="black") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.title.x=element_blank()) +
    ggtitle("OUT")
  p2 <- ggplot(data.frame(ingate), aes(ingate)) +
    geom_bar(width=.75, fill="green", colour="black") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.title.x=element_blank()) +
    ggtitle("IN")
  p3 <- ggplot(data.frame(off), aes(off)) +
    geom_bar(width=.75, fill="red", colour="black") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.title.x=element_blank()) +
    ggtitle("OFF")
  p4 <- ggplot(data.frame(on), aes(on)) +
    geom_bar(width=.75, fill="blue", colour="black") +
    theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.title.x=element_blank()) +
    ggtitle("ON")
  return(multiplot(p1, p3, p4, p2, cols=2))
}
