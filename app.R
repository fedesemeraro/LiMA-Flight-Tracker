#-------------------------------------------
### Flight Radar 24 - R Analysis Tool
### written by Federico Semeraro
#-------------------------------------------


source("./Functions & Scripts/setup.R")

######################
### USER INTERFACE ###
######################
ui <- dashboardPage(
  
  title = "LiMA",
  
  # Dashboard color
  skin = "black",
  # Title
  dashboardHeader(title = span(img(src='Airbusicon.png'), "Project LiMA")),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("DATA", tabName = "about", icon = icon("dashboard")),
      menuItem("MAPPING", tabName = "mapping", icon = icon("globe"),
               menuSubItem("OUT-OFF-ON-IN", tabName = "oooimap", icon = icon("map-marker")), 
               menuSubItem("Routes", tabName = "routes", icon = icon("plane")), 
               menuSubItem("Flight Path", tabName = "path", icon = icon("road")),
               menuSubItem("3D Map", tabName = "tdmap", icon = icon("paper-plane"))),
      menuItem("ANALYSIS", tabName = "graphs", icon = icon("area-chart"),
               menuSubItem("Plot Creation", tabName = "ggplot", icon = icon("pencil")), 
               menuSubItem("3D Plot", tabName = "plotly", icon = icon("line-chart")), 
               menuSubItem("Airport Network", tabName = "airpnet", icon = icon("arrows-alt")),
               menuSubItem("Weather", tabName = "weather", icon = icon("bolt")),
               menuSubItem("OTP", tabName = "otp", icon = icon("clock-o"))),
      menuItem("REPORT", tabName = "report", icon = icon("book"))
    ),
    
    # Loading Spinning Globe
    tags$head(tags$style(type="text/css", "#loadmessage {text-align: center;font-weight: bold;font-size: 100%}")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", hr()),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Loading...",id="loadmessage")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", h5("")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="Loading.gif", height="100%", width="100%")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')", hr())
  ),
  
  ## Body content
  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo {font-family: "Georgia", Times, "Times New Roman", serif;font-weight: bold;font-size: 20px;}'))),
    
    tabItems(
      
      #-------------------------------------------START
      
      ## ABOUT
      tabItem(tabName = "about",
              fluidRow(
                column(3, align="center",
                       wellPanel(style = paste0("height: ", as.character(screenheight+160),"px"),
                                 title = "", status = "primary", solidHeader = TRUE,
                                 img(src='Airbuslogo.png', width="70%"),
                                 h3("Welcome to the Airbus ADSB Mining Tool!"),
                                 h4("This is an application that uses raw data from",
                                    a(href = 'https://www.flightradar24.com', 'Flight Radar 24'), "in order to 
                                    study the airline usage of Airbus products."),
                                 h4("It was developed by Federico Semeraro, 
                                    Intern in Operability between July 2015 and July 2016, under the supervision of Andy Williams. "),
                                 hr(),
                                 fluidRow(
                                   column(1.5,
                                          uiOutput("flightBox"),
                                          uiOutput("routeBox"),
                                          uiOutput("airlBox")
                                   ),
                                   column(1.5,
                                          uiOutput("regBox"),
                                          uiOutput("airpBox"),
                                          uiOutput("fleetBox")
                                   )
                                 )
                       )
                ),
                
                column(2,
                       wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; height: ", as.character(screenheight+160),"px"),
                                 h3("DATA IMPORT"),
                                 # Loading Environment
                                 fileInput('file1', 'File Import (Raw Data .csv)',accept = c('.csv')),
                                 
                                 h3("Input Options"),
                                 helpText("See Options Tab for choices"),
                                 # Date Range
                                 dateRangeInput("dateout", label = h5("Date range (extremes included)"), start = "2015-06-28", 
                                                end="2015-06-29", min = "2015-06-28", max="2015-06-29"),
                                 fluidRow(
                                   column(12,
                                          # Fleet
                                          selectInput('fleetout',
                                                      label = h5("Aircaft Type"),
                                                      choices = setNames(as.character(AC_FR24$AC_FR24), as.character(AC_FR24$FleetName)),
                                                      selected = as.character(AC_FR24[18,1]), # test with A350 (17) or A380 (18)
                                                      multiple=TRUE, selectize=TRUE)
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          # MSN
                                          textInput("msnout", label = h5("MSN"), value = NULL)
                                   ),
                                   column(6,
                                          # Flight Number
                                          textInput("flnumout", label = h5("FN/Airl. iata"), value = NULL)
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          # Departure
                                          selectInput('depout',
                                                      label = h5("Dep. iata"),
                                                      choices = as.character(DepAirp_FR24[,1]),
                                                      multiple=TRUE, selectize=TRUE)
                                   ),
                                   column(6,
                                          # Destination
                                          selectInput('destout',
                                                      label = h5("Dest. iata"),
                                                      choices = as.character(DestAirp_FR24[,1]),
                                                      multiple=TRUE, selectize=TRUE)
                                   )
                                 ),
                                 fluidRow(
                                   column(6,
                                          # Airport
                                          selectInput('airpout',
                                                      label = h5("From & To"),
                                                      choices = c("", as.character(DestAirp_FR24[,1])),
                                                      selected = NULL,
                                                      multiple=F)
                                   ),
                                   column(6,
                                          br(),
                                          br(),
                                          # RETRIEVE DATA
                                          actionButton("click1", label = "Get Data", icon = icon("cloud-download"), 
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   )
                                 ),
                                 # Altitude
                                 checkboxInput("altitude", "Altitude Range", value = FALSE),
                                 uiOutput("ui1"),
                                 # GroundSpeed
                                 checkboxInput("groundspeed", "GroundSpeed Range", value = FALSE),
                                 uiOutput("ui2"),
                                 # Latitude
                                 checkboxInput("latitude", "Latitude Range", value = FALSE),
                                 uiOutput("ui3"),
                                 # Latitude
                                 checkboxInput("longitude", "Longitude Range", value = FALSE),
                                 uiOutput("ui4"),
                                 
                                 h3("DOWNLOAD"),
                                 selectInput("dataset", "Choose a dataset:", choices = c("FlightSummary", "OOOI", "RawData")),
                                 # Download .csv
                                 downloadButton('downloadData', 'Get .csv')
                       )
                ),
                
                column(7,
                       wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; height: ", as.character(screenheight+160),"px"),
                                 tabsetPanel(type = "tabs",
                                             
                                             tabPanel("Raw Data", 
                                                      wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", as.character(screenheight+160),"px"),
                                                                dataTableOutput("table1")
                                                      )
                                             ),
                                             tabPanel("OOOI", 
                                                      wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", as.character(screenheight+160),"px"),
                                                                dataTableOutput("table2")
                                                      )
                                             ),
                                             tabPanel("Flight Summary", 
                                                      wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", as.character(screenheight+160),"px"),
                                                                dataTableOutput("table3")  
                                                      )
                                             ),
                                             tabPanel("Options", 
                                                      wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", as.character(screenheight+160),"px"),
                                                                dataTableOutput("table4")  
                                                      )
                                             )
                                 )     
                       )
                )
              )
      ),
      #-------------------------------------------END
      
      
      
      #-------------------------------------------START
      # MAPPING
      tabItem(tabName = "oooimap",
              fluidRow(
                uiOutput("flightstatsBox"),
                uiOutput("routestatsBox"),
                uiOutput("airlstatsBox"),
                uiOutput("regstatsBox"),
                uiOutput("fntatsBox"),
                uiOutput("fleetstatsBox")
              ),
              tabPanel("Airport Selection", 
                       fluidRow(
                         column(width = 8,
                                box(width = NULL,
                                    leafletOutput("map1", height = screenheight),
                                    absolutePanel(
                                      bottom = 60, left = 0, width = 220, height = 50,
                                      draggable = F,
                                      box(
                                        img(src='oooiLegend.png')
                                      )
                                    )
                                )
                         ),
                         column(width = 4,
                                box(width = NULL, height = screenheight+23,
                                    fluidRow(
                                      column(width = 7,
                                             selectizeInput(inputId = "Coordinates", label = "Select Airport",multiple  = F, 
                                                            choices = c("", setNames(paste(AirportList$Latitude,AirportList$Longitude,sep="_"),
                                                                                     paste(AirportList$Name, " (", AirportList$IATA.Code, ")", sep=""))),
                                                            selected = setNames(paste(AirportList$Latitude,AirportList$Longitude,sep="_"),
                                                                                paste(AirportList$Name, " (", AirportList$IATA.Code, ")", sep=""))[2522])
                                      ),
                                      column(width = 5,
                                             radioButtons("radiochoice", label="View Choice", choices = list("World View" = 1, "Selected Airport" = 2), selected = 1)
                                      )
                                    ),
                                    fluidRow(
                                      column(width = 7,
                                             verbatimTextOutput('values')
                                      ),
                                      column(width = 5,
                                             actionButton("refresh", label = "Load Data", icon = icon("arrow-circle-right"), 
                                                          style="color: #fff; background-color: #FF0000; border-color: #2e6da4")
                                      )
                                    ),
                                    tabBox(width = NULL, height = 700,
                                           tabPanel("STATS",
                                                    wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                              plotOutput('plotcreationstats')
                                                    )
                                           ),
                                           tabPanel("CHECKS",
                                                    tabBox(width = NULL, height = 620,
                                                           tabPanel("Apron",
                                                                    tabsetPanel(
                                                                      tabPanel("OUT CHECK", 
                                                                               wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                                                         dataTableOutput("outaproncheck")
                                                                               )
                                                                      ),
                                                                      tabPanel("IN CHECK", 
                                                                               wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                                                         dataTableOutput("inaproncheck")
                                                                               )
                                                                      )
                                                                    )
                                                           ),
                                                           tabPanel("Runway",
                                                                    tabsetPanel(
                                                                      tabPanel("OFF CHECK", 
                                                                               wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                                                         dataTableOutput("offruncheck")
                                                                               )
                                                                      ),
                                                                      tabPanel("ON CHECK", 
                                                                               wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                                                         dataTableOutput("onruncheck")
                                                                               )
                                                                      )
                                                                    )
                                                           )
                                                           # tabPanel("Gate",
                                                           #          tabsetPanel(
                                                           #            tabPanel("OUT CHECK", 
                                                           #                     wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px")
                                                           #                               #dataTableOutput("outgatecheck")
                                                           #                     )
                                                           #            ),
                                                           #            tabPanel("IN CHECK", 
                                                           #                     wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px")
                                                           #                               #dataTableOutput("ingatecheck")
                                                           #                     )
                                                           #            )
                                                           #          )
                                                           # ),
                                                           # tabPanel("Terminal",
                                                           #          tabsetPanel(
                                                           #            tabPanel("OUT CHECK", 
                                                           #                     wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px")
                                                           #                               #dataTableOutput("outtermcheck")
                                                           #                     )
                                                           #            ),
                                                           #            tabPanel("IN CHECK", 
                                                           #                     wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px")
                                                           #                               #dataTableOutput("intermcheck")
                                                           #                     )
                                                           #            )
                                                           #          )
                                                           # )
                                                    )
                                           ),
                                           tabPanel("DATA",
                                                    wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", 525,"px"),
                                                              dataTableOutput('tabledatainbounds')
                                                    )
                                           )
                                    )
                                )
                         )
                       )
              )
      ),
      
      
      tabItem(tabName = "routes",
              h2("Route Visualisation"),
              hr(),
              fluidRow(
                leafletOutput("mapx", height = screenheight)
              ),
              absolutePanel(
                top = 210, left = 290, width = 250, height = 300,
                draggable = T,
                wellPanel(
                  h5(strong("Timer Speed")),
                  fluidRow(
                    column(6,
                           numericInput("timerspeed1", label = NULL, value = 0.01)
                    ),
                    column(6, 
                           actionButton("clickx", label = "Plot Data", icon = icon("arrow-circle-right"), 
                                        style="color: #fff; background-color: #000000; border-color: #2e6da4")
                    )
                  ),
                  uiOutput("animation1"),
                  h5(strong("Date-Time Variable")),
                  verbatimTextOutput("timevar")
                ),
                style = "opacity: 0.90"
              ),
              absolutePanel(
                top = 70, left = 600, width = 400,
                draggable = F,
                uiOutput("amountflightsBox")
              )
      ),
      
      
      # Flight Path
      tabItem(tabName = "path",
              fluidRow(
                column(7,
                       wellPanel(
                         leafletOutput("map3", height=screenheight*(2/3)),
                         plotOutput("altitudeplot"),
                         verbatimTextOutput("monitoring")
                       )
                ),
                column(5,
                       wellPanel(
                         leafletOutput("map77", height=screenheight*(2/3)),
                         fluidRow(
                           column(5, 
                                  uiOutput("animationsel")
                           ),
                           column(5,
                                  h5(strong("Time")),
                                  verbatimTextOutput("info11")
                           ),
                           column(2,
                                  numericInput("timerspeed2", label = h5(strong("Timer Speed")), value = 0.3)
                           )
                         ),
                         helpText("The playback might be slow for big datasets, scrolling is advised"),
                         uiOutput("animation2"),
                         fluidRow(
                           column(4,
                                  h4("Route"),
                                  verbatimTextOutput("info12"),
                                  h4("Fleet"),
                                  verbatimTextOutput("info23")
                           ),
                           column(4,
                                  h4("Operator"),
                                  verbatimTextOutput("info22"),
                                  h4("Flight Number"),
                                  verbatimTextOutput("info21")
                           ),
                           column(4,
                                  h4("Altitude"),
                                  verbatimTextOutput("info13"),
                                  h4("GroundSpeed"),
                                  verbatimTextOutput("info24")
                           )
                         )
                       )
                )
              )
      ),
      
      
      tabItem(tabName = "tdmap",
              h2("3D Visualisation"),
              hr(),
              tabPanel("MSN Selection", 
                       fluidRow(
                         globeOutput("globe", height = screenheight)
                       ),
                       absolutePanel(
                         top = 180, left = 300, width = 250, height = 300,
                         draggable = F,
                         wellPanel(
                           radioButtons("radiochoice2", label="View Choice",
                                        choices = list("TAT" = 1, "Taxi OUT"= 2, "Taxi IN"= 3, "Route" = 4), 
                                        selected = 1),
                           uiOutput("regselected2")
                         ),
                         style = "opacity: 0.90"
                       )
              )
      ),
      #-------------------------------------------END
      
      
      
      #-------------------------------------------START
      ### GRAPHS
      
      # Plot Creation
      tabItem(tabName = "ggplot",
              titlePanel("Plot Creation"),
              hr(),
              
              sidebarLayout(
                
                ## SIDEBAR ##
                sidebarPanel(
                  tabsetPanel(
                    tabPanel("Inputs", 
                             wellPanel(style = paste0("overflow-y:scroll; max-height: ", as.character(screenheight),"px"),
                                       ## DATA CHOICE
                                       fluidRow(
                                         column(6,
                                                fileInput("datafile", "Choose csv file to upload", multiple = FALSE, accept = c("csv"))
                                         ),
                                         column(6,
                                                selectInput("datachoice", "Choose a dataset:", choices = c("FlightSummary", "Raw Data"), selected = "FlightSummary()")
                                         )
                                       ),
                                       ## AXIS
                                       h4("Axis Options"),
                                       fluidRow(
                                         column(6, 
                                                uiOutput("xcol"),
                                                uiOutput("ycol")
                                         ),
                                         column(6, 
                                                uiOutput("slider"),
                                                uiOutput("slider2")
                                         )
                                       ),
                                       hr(),
                                       fluidRow(
                                         column(6, 
                                                ## GROUPING
                                                h4("Grouping"),
                                                uiOutput("colour"),
                                                uiOutput("fill"),
                                                uiOutput("pointsize"),
                                                hr()
                                         ),
                                         
                                         column(6,
                                                ## FILTERING
                                                h4("Filtering"),
                                                uiOutput("maxlevels"),
                                                uiOutput("filtervar1"),
                                                uiOutput("filtervar1values"),
                                                uiOutput("filtervar2"),
                                                uiOutput("filtervar2values")
                                         )
                                       )
                             )
                    ),
                    ## GRAPH Look
                    tabPanel("Graph Look",
                             wellPanel(style = paste0("overflow-y:scroll; max-height: ", as.character(screenheight), "px"),
                                       h4("Marker Type:"),
                                       fluidRow(
                                         column (5,
                                                 radioButtons("Points", "Points/Jitter:",
                                                              c("Points" = "Points",
                                                                "Jitter" = "Jitter",
                                                                "None" = "None"), selected = "Jitter"),
                                                 conditionalPanel( " input.Points== 'Points' ",
                                                                   numericInput('pointtypes','Points Type:',16, min = 1, max = 25))
                                         ),
                                         column(7,
                                                conditionalPanel( " input.points== 'Points' ",
                                                                  sliderInput("pointsizes", "Points Size:", min=0, max=4, value=c(1),step=0.1),
                                                                  sliderInput("pointstransparency", "Points Transp.:", min=0, max=1, value=c(0.5),step=0.01)
                                                ))
                                       ),
                                       fluidRow(
                                         column (5,
                                                 radioButtons("line", "Lines:",
                                                              c("Lines" = "Lines",
                                                                "None" = "None"),selected="None"),
                                                 conditionalPanel( " input.line== 'Lines' ",
                                                                   sliderInput("linestransparency", "Lines Transp.:", min=0, max=1, value=c(0.5),step=0.01)
                                                 )
                                         ),
                                         column(7, conditionalPanel( " input.line == 'Lines' ",
                                                                     sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                                                                     selectInput('linetypes','Lines Type:',c("solid","dotted")))
                                         )
                                       ),
                                       h4("Axis Labels:"),
                                       fluidRow(
                                         column (5, textInput('ylab', 'y-axis', value = "")),
                                         column (5,textInput('xlab', 'x-axis', value = ""))
                                       ),
                                       h4("Other Options:"),
                                       fluidRow(
                                         column (5, selectInput('backgroundcol', label ='Background Color',
                                                                choices=c("Gray" ="gray97","White"="white","Dark Gray"="grey90"),
                                                                multiple=FALSE, selectize=TRUE,selected="grey90")),
                                         column (5, selectInput('legendposition', label ='Legend Position',
                                                                choices=c("left", "right", "bottom", "top", "none"),
                                                                multiple=FALSE, selectize=TRUE,selected="none"))
                                       ),
                                       h4("Additional Themes Options"),
                                       checkboxInput('themebw', 'Black and White Theme')   , 
                                       checkboxInput('themeaspect', 'Custom Aspect Ratio')   ,  
                                       conditionalPanel(
                                         condition = "input.themeaspect" , 
                                         
                                         numericInput("aspectratio",label = "Y/x ratio",value = 1) ),
                                       checkboxInput('labelguides', 'Hide the Names of the Guides',value = FALSE)
                             )
                    ),
                    ## Analysis OPTIONS
                    tabPanel("Analysis Options",
                             h4("Logarithmic Scale:"),
                             fluidRow(
                               column (5, checkboxInput('logy', 'Log y-axis', value = FALSE)),
                               column (5, checkboxInput('logx', 'Log x-axis', value = FALSE))
                             ),
                             h4("Reference Lines"),
                             checkboxInput('identityline', 'Identity Line'),   
                             checkboxInput('horizontalzero', 'Horizontal Zero Line'),
                             checkboxInput('customline1', 'Vertical Line'),
                             conditionalPanel(
                               condition = "input.customline1" , 
                               numericInput("vline",label = "",value = 1)),
                             checkboxInput('customline2', 'Horizontal Line'),
                             conditionalPanel(
                               condition = "input.customline2" , 
                               numericInput("hline",label = "",value = 1)),
                             
                             # LOESS
                             fluidRow(
                               column(12,hr()),
                               column (4,
                                       radioButtons("Loess", "Loess:", c("Loess" = "Loess","None" = "None"), selected="None")
                               ),
                               column (4,
                                       sliderInput("loessens", "Loess Span:", min=0, max=1, value=c(0.75),step=0.05)
                               ),
                               column (4,
                                       selectInput('colloess', label ='Loess Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                               )
                             ),
                             
                             # MEAN
                             h4("Mean"),
                             helpText("This might trigger the debugger in RStudio; if so, stop it from within RStudio console"),
                             fluidRow(
                               column (3, checkboxInput('mean', 'Mean', value = FALSE)),
                               column (3, uiOutput("meanlinks")),
                               column (6, uiOutput("meancateg"))
                             ),
                             fluidRow(
                               dataTableOutput("meantable")
                             )
                    ),
                    
                    ## Code Editor
                    tabPanel("Code Editor",
                             # Code editor window.
                             aceEditor("plotCode",
                                       mode = "r",
                                       value = "ggplot(data = FlightSummary(),\n    aes(x = Destination, y = TAT_hrs)) + \n    geom_bar(aes(fill=Operator), stat='identity', position='identity')"),
                             downloadButton(
                               outputId = "downloadCode",
                               label    = "Download Code"),
                             hr(),
                             radioButtons("plotselected", "Which plot do you want to show?",
                                          c("Normal", "Edited"), selected="Normal"),
                             actionButton("plotButton", "Update Plot")
                    )
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plot"  , 
                             wellPanel(style = paste0("overflow-y:scroll; overflow-x:scroll; max-height: ", as.character(screenheight),"px"),
                                       ## PLOT OUTPUT
                                       uiOutput("plotting"),
                                       hr(),
                                       ## GRAPH SELECTION
                                       h4("Clicked points"),
                                       wellPanel(style = "overflow-y:scroll; overflow-x:scroll; max-height: 300px",
                                                 uiOutput("clickheader"),
                                                 tableOutput("plot_clickedpoints")
                                       ),
                                       h4("Brushed points"),
                                       wellPanel(style = "overflow-y:scroll; overflow-x:scroll; max-height: 300px",
                                                 uiOutput("brushheader"),
                                                 tableOutput("plot_brushedpoints")
                                       )
                             )
                             
                    ),#tabPanel1
                    # DOWNLOAD PANEL
                    tabPanel("Download", 
                             selectInput(
                               inputId = "downloadPlotType",
                               label   = h5("Select download file type"),
                               choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
                             # Allow the user to set the height and width of the plot download.
                             h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
                             numericInput(
                               inputId = "downloadPlotHeight",label = "Height (inches)",value = 7,min = 1,max = 100),
                             numericInput(
                               inputId = "downloadPlotWidth",label = "Width (inches)",value = 7,min = 1,max = 100),
                             # Choose download filename.
                             textInput(
                               inputId = "downloadPlotFileName",
                               label = h5("Enter file name for download")),
                             # File downloads when this button is clicked.
                             downloadButton(
                               outputId = "downloadPlot", 
                               label    = "Download Plot")
                    ),
                    
                    ## DATA PANEL
                    tabPanel("Data in Plot", fluidRow(wellPanel(dataTableOutput("filedata"), 
                                                                style = "overflow-x:scroll; max-width: 100%")))
                  )
                )
              )
      ),
      # 3D Plot
      tabItem(tabName = "plotly",
              titlePanel("3D Plot"),
              hr(),
              sidebarLayout(
                sidebarPanel(
                  h4("Axis Options"),
                  uiOutput("xcoly"),
                  uiOutput("ycoly"),
                  uiOutput("zcoly"),
                  uiOutput("coloury")
                ),
                mainPanel(
                  plotlyOutput("plotlyplot", height = paste0(as.character(screenheight), "px"))
                )
              )
      ),
      # Airport Network
      tabItem(tabName = "airpnet",
              titlePanel("Airport Network"),
              hr(),
              absolutePanel(
                top = 135, left = 245, width = 300, height = 300,
                draggable = F,
                wellPanel(
                  uiOutput("equipselected"),
                  uiOutput("regselected7"),
                  uiOutput("opselected")
                )
              ),
              wellPanel(
                visNetworkOutput("network_hello", width = "100%", height = paste0(as.character(screenheight), "px"))
              )
      ),
      # Weather
      tabItem(tabName = "weather",
              h2("METAR Data Integration"),
              hr(),
              
              fluidRow(
                column(7,
                       wellPanel(uiOutput("ggvis_ui"),
                                 ggvisOutput("ggvis")
                       )
                ),
                column(5,
                       DT::dataTableOutput("tableweather")
                )
              )
      ),
      # OTP
      tabItem(tabName = "otp",
              fluidRow(
                column(3,
                       h2("Comparison with OAG")),
                column(2,
                       h2(""),
                       actionButton("clickoag", label = "Get OAG Schedule", icon = icon("cloud-download"), 
                                    style="color: #fff; background-color: #FF6600; border-color: #2e6da4")
                )
              ),
              tabsetPanel(
                tabPanel("Plot",
                         fluidRow(
                           column(8,
                                  fluidRow(
                                    column(12,
                                           uiOutput("oagplot1"),
                                           plotOutput("oagplot2"),
                                           verbatimTextOutput("test200")
                                    )
                                  )
                           ),
                           column(4,
                                  fluidRow(
                                    column(6,
                                           h4("Overall Mean Departure Delay"),
                                           verbatimTextOutput("depdelaymean")
                                    ),
                                    column(6,
                                           h4("Overall Mean Arrival Delay"),
                                           verbatimTextOutput("arrdelaymean")
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           h4("Mean Departure/Arrival Delay by Operator"),
                                           wellPanel(style = paste0("overflow-y:scroll; max-height: ", as.character(screenheight), "px"),
                                           dataTableOutput("deparrdelaymeantable")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(12,
                                           h4("Selected Point"),
                                           wellPanel(style = paste0("overflow-y:scroll; max-height: ", as.character(screenheight), "px"),
                                           dataTableOutput("info_oag")
                                           )
                                    )
                                  )
                           )
                         )
                ),
                tabPanel("Data",
                         fluidRow(
                           column(12,
                                  wellPanel(style = paste0("overflow-x:scroll; overflow-y:scroll; max-height: ", as.character(screenheight+100),"px"),
                                            dataTableOutput("tableoag")
                                  )
                           )
                         )
                )
              )
      ),
      #-------------------------------------------END GRAPHS 
      
      
      
      #-------------------------------------------START
      ## REPORT
      tabItem(tabName = "report",
              h2("Report Creation"),
              hr(),
              fluidRow(
                column(6,
                       wellPanel(
                         textInput('filename', 'File Name', value = 'report'),
                         textInput('filetitle', 'File Title', value = 'FR24 Data Analysis'),
                         textInput('firstname', 'First name', value = 'Federico'),
                         textInput('lastname', 'Last name', value = 'Semeraro'),
                         textInput('guytitle', 'Your Position in Airbus', value = 'Intern in Airbus'),
                         hr(),
                         radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                      inline = TRUE),
                         downloadButton('downloadReport')
                       )
                )
              )
      )
      #-------------------------------------------END
    )
  )
)
#-------------------------------------------END UI




































##############
### SERVER ###
##############
server <- function(input, output, session) {
  
  #-------------------------------------------START
  ### ABOUT & DATA
  # Create the info boxes
  output$flightBox <- renderUI({
    valueBox(length(FlightSummary()$AC_type), "Flights", icon = icon("database"), color = "red")
  })
  output$fleetBox <- renderUI({
    valueBox(length(unique(FlightSummary()$AC_type[!is.na(FlightSummary()$AC_type)])), "Fleets", icon = icon("plane"), color = "blue")
  })
  output$airlBox <- renderUI({
    valueBox(length(unique(FlightSummary()$Operator[!is.na(FlightSummary()$Operator)])), "Airlines", icon = icon("fighter-jet"), color = "purple")
  })
  output$regBox <- renderUI({
    valueBox(length(unique(FlightSummary()$Registration[!is.na(FlightSummary()$Registration)])), "MSNs", icon = icon("list-ol"), color = "green")
  })
  output$routeBox <- renderUI({
    valueBox(length(na.omit(unique(cbind(as.character(FlightSummary()$Destination), as.character(FlightSummary()$Departure)))))/2, "Routes", icon = icon("globe"), color = "yellow")
  })
  output$airpBox <- renderUI({
    valueBox(length(unique(na.omit(append(as.character(FlightSummary()$Destination), as.character(FlightSummary()$Departure))))), "Airports", icon = icon("map-marker"), color = "teal")
  })
  
  
  # Download tab
  datasetInput <- reactive({
    switch(input$dataset,
           "FlightSummary" = FlightSummary(),
           "OOOI" = oooi(),
           "RawData" = fr24data())
  })
  output$table <- renderTable({
    datasetInput()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "csv", sep = ".")
    },
    content = function(file) {
      write.table(datasetInput(), file, sep = "," , row.names = FALSE)
    })
  
  
  ### IMPORT
  ## INPUTS
  # Altitude
  output$ui1 <- renderUI({
    if (is.null(input$altitude)) return()
    switch(input$altitude, 
           "altitude" = sliderInput("altout", label = NULL, min = 0,max = 60000, value = c(0, 100000)))})
  # GroundSpeed
  output$ui2 <- renderUI({
    if (is.null(input$groundspeed)) return()
    switch(input$groundspeed,
           "groundspeed" = sliderInput("grspout", label = NULL, min = 0, max = 700, value = c(0, 10000)))})
  # Latitude
  output$ui3 <- renderUI({
    if (is.null(input$latitude)) return()
    switch(input$latitude, "latitude" = sliderInput("latout", label = NULL, min = -90, max = 90, value = c(-90, 90)))})
  # Longitude
  output$ui4 <- renderUI({
    if (is.null(input$longitude)) return()
    switch(input$longitude, "longitude" = sliderInput("lonout", label = NULL, min = -180, max = 180, value = c(-180, 180)))})
  
  
  ### REACTIVE DATA ###
  #  Uncomment inside Airbus server to import with RODBC
  # rawdata <- eventReactive(input$click1, {
  #   if (is.null(input$file1)) {
  #     df <- dataimport(input$altout, input$grspout, input$latout, input$lonout, input$fleetout, 
  #                      input$msnout, input$flnumout, input$depout, input$destout, input$airpout, input$dateout)
  #   } else {
  #     return(NULL)
  #   }
  # })
  rawdata <- eventReactive(input$click1, {
    if (is.null(input$file1)) {
      df <- data
    } else {
      return(NULL)
    }
  })
  
  fr24data <- eventReactive(input$click1, {
    if (is.null(input$file1)) {
      DP1(rawdata())
    } else {
      inFile <- input$file1
      read.csv(inFile$datapath)
    }
  })
  
  oooi <- eventReactive(input$click1, {
    DP2(fr24data())
  })
  FlightSummary <- eventReactive(input$click1, {
    DP3(oooi(), weather)
  })
  
  output$table1 <- renderDataTable({
    fr24data()[,1:14]},
    options = list(pageLength = 18)
  )
  output$table2 <- renderDataTable({
    oooi()[,1:18]},
    options = list(pageLength = 18)
  )
  output$table3 <- renderDataTable({
    FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                       "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                       "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                       "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")]},
    options = list(pageLength = 10)
  )
  # Option List
  output$table4 <- renderDataTable(
    Utilisation_FR24,
    options = list(pageLength = 18)
  )
  #-------------------------------------------END
  
  
  
  #-------------------------------------------START MAPPING
  # OUT-OFF-ON-IN Map
  values <- reactiveValues()
  observe({if (input$Coordinates!="") {values$a <- input$Coordinates}})
  # Creating Points
  outpointsdata <- reactive({subset(FlightSummary(), !is.na(FlightSummary()$Longitude.OUT) | !is.na(FlightSummary()$Latitude.OUT))})
  outpoints <- reactive({SpatialPoints(outpointsdata()[,c("Longitude.OUT", "Latitude.OUT")])})
  offpointsdata <- reactive({subset(FlightSummary(), !is.na(FlightSummary()$Longitude.OFF) | !is.na(FlightSummary()$Latitude.OFF))})
  offpoints <- reactive({SpatialPoints(offpointsdata()[,c("Longitude.OFF", "Latitude.OFF")])})
  onpointsdata <- reactive({subset(FlightSummary(),!is.na(FlightSummary()$Longitude.ON) | !is.na(FlightSummary()$Latitude.ON))})
  onpoints <- reactive({SpatialPoints(onpointsdata()[,c("Longitude.ON", "Latitude.ON")])})
  inpointsdata <- reactive({subset(FlightSummary(),!is.na(FlightSummary()$Longitude.IN) | !is.na(FlightSummary()$Latitude.IN))})
  inpoints <- reactive({SpatialPoints(inpointsdata()[,c("Longitude.IN", "Latitude.IN")])})
  
  
  # Loading rds files
  aedr_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aedr_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                    AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  # aews_points <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aews_points", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
  #                                                               AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aeap_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aeap_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                    AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aegt_points <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aegt_points", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                        AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aeha_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aeha_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                    AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aehepa_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aehepa_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                        AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aehepo_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aehepo_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                        AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  # aenaid_points <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aenaid_points", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
  #                                                                 AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aerw_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aerw_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                    AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aerw_lines <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aerw_lines", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1),
                                                                                                      AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aetl_lines <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aetl_lines", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                      AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aetw_lines <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aetw_lines", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                      AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  aete_poly <- eventReactive(input$refresh, {readRDS(paste0("./Data/Airport/aete_poly", "_", subset(AirportList$Name, AirportList$Latitude == substr(values$a,1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1), 
                                                                                                    AirportList$Longitude == substr(values$a,str_locate_all(pattern ='_',values$a)[[1]][1,1]+1,nchar(values$a))), ".rds"))})
  
  oooiIcons <- iconList(
    outicon = makeIcon("outicon.png", iconWidth = 18, iconHeight = 24),
    officon = makeIcon("officon.png", iconWidth = 18, iconHeight = 24),
    onicon = makeIcon("onicon.png", iconWidth = 18, iconHeight = 24),
    inicon = makeIcon("inicon.png", iconWidth = 18, iconHeight = 24)
  )
  
  output$values <- renderPrint({
    list(Coordinates = input$Coordinates)
  })
  
  inbounds <- reactive({c(as.numeric(substr(values$a, str_locate_all(pattern ='_',values$a)[[1]][1,1]+1, nchar(values$a))) - 0.03012, # 1km is 0.015060 degrees 
                          as.numeric(substr(values$a, 1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1)) - 0.03012,
                          as.numeric(substr(values$a, str_locate_all(pattern ='_',values$a)[[1]][1,1]+1, nchar(values$a))) + 0.03012,
                          as.numeric(substr(values$a, 1, str_locate_all(pattern ='_',values$a)[[1]][1,1]-1)) + 0.03012)
  })
  
  output$map1 <- renderLeaflet({
    map1 <- leaflet() %>% addTiles() %>% 
      addProviderTiles("MapQuestOpen.OSM", group = "Default") %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c('Default','Dark Map', 'Satellite'), 
                       overlayGroups = c("OOOI", "Airport"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      
      fitBounds(ifelse(input$radiochoice == 1, -100, inbounds()[1]), ifelse(input$radiochoice == 1, -80, inbounds()[2]),
                ifelse(input$radiochoice == 1, 100, inbounds()[3]),ifelse(input$radiochoice == 1, 80, inbounds()[4]))
    
  })
  
  observeEvent(input$refresh, {
    leafletProxy("map1") %>%
      
      addMarkers(outpointsdata()$Longitude.OUT, outpointsdata()$Latitude.OUT, icon = oooiIcons[1],
                 popup = paste0("<strong>Date-Time: </strong>", outpointsdata()$OUT,
                                "<br><strong>Fleet: </strong>", outpointsdata()$AC_type,
                                "<br><strong>Registration: </strong>", outpointsdata()$Registration,
                                "<br><strong>Airline: </strong>", outpointsdata()$Operator,
                                "<br><strong>Flight Number: </strong>", outpointsdata()$FlightNumber,
                                "<br><strong>Route: </strong>", paste(outpointsdata()$Departure, outpointsdata()$Destination, sep="-")), group = "OOOI") %>%
      
      addMarkers(offpointsdata()$Longitude.OFF, offpointsdata()$Latitude.OFF, icon = oooiIcons[2],
                 popup = paste0("<strong>Date-Time: </strong>", offpointsdata()$OFF,
                                "<br><strong>Fleet: </strong>", offpointsdata()$AC_type,
                                "<br><strong>Registration: </strong>", offpointsdata()$Registration,
                                "<br><strong>Airline: </strong>", offpointsdata()$Operator,
                                "<br><strong>Flight Number: </strong>", offpointsdata()$FlightNumber,
                                "<br><strong>Route: </strong>", paste(offpointsdata()$Departure, offpointsdata()$Destination, sep="-")), group = "OOOI") %>%
      
      addMarkers(onpointsdata()$Longitude.ON, onpointsdata()$Latitude.ON, icon = oooiIcons[3],
                 popup = paste0("<strong>Date-Time: </strong>", onpointsdata()$ON,
                                "<br><strong>Fleet: </strong>", onpointsdata()$AC_type,
                                "<br><strong>Registration: </strong>", onpointsdata()$Registration,
                                "<br><strong>Airline: </strong>", onpointsdata()$Operator,
                                "<br><strong>Flight Number: </strong>", onpointsdata()$FlightNumber,
                                "<br><strong>Route: </strong>", paste(onpointsdata()$Departure, onpointsdata()$Destination, sep="-")), group = "OOOI") %>%
      
      addMarkers(inpointsdata()$Longitude.IN, inpointsdata()$Latitude.IN, icon = oooiIcons[4],
                 popup = paste0("<strong>Date-Time: </strong>", inpointsdata()$ON,
                                "<br><strong>Fleet: </strong>", inpointsdata()$AC_type,
                                "<br><strong>Registration: </strong>", inpointsdata()$Registration,
                                "<br><strong>Airline: </strong>", inpointsdata()$Operator,
                                "<br><strong>Flight Number: </strong>", inpointsdata()$FlightNumber,
                                "<br><strong>Route: </strong>", paste(inpointsdata()$Departure, inpointsdata()$Destination, sep="-")), group = "OOOI") %>%
      
      addPolygons(data = aedr_poly(), color = "grey", popup = paste0("<strong>Aerodrome</strong><br>OSM id: ", aedr_poly()$id), group = "Airport") %>% # Aerodrome
      addPolygons(data = aeap_poly(), color = "red", popup = paste0("<strong>Apron: </strong>", 1:length(aeap_poly())), group = "Airport") %>% # Apron    aeap_poly()$id
      addPolygons(data = aeha_poly(), color = "blue", popup = paste0("<strong>Hangar </strong><br>OSM id: ", aeha_poly()$id), group = "Airport") %>% # Hangar
      addPolygons(data = aehepa_poly(), color = "magenta", popup = paste0("<strong>Helipad</strong><br>OSM id: ", aehepa_poly()$id), group = "Airport") %>% # Helipad
      addPolygons(data = aehepo_poly(), color = "gold", popup = paste0("<strong>Heliport</strong><br>OSM id: ", aehepo_poly()$id), group = "Airport") %>% # Heliport
      addPolygons(data = aerw_poly(), color = "orange", popup = paste0("<strong>Runway </strong><br>OSM id: ", aerw_poly()$id), group = "Airport") %>% # Runway
      addPolylines(data = aerw_lines(), color = "orange", weight = 10, popup = paste0("<strong>Runway </strong><br>OSM id: ", aerw_lines()$id), group = "Airport") %>% # Runway
      addPolylines(data = aetl_lines(), color = "purple", weight = 5, popup = paste0("<strong>Taxilane</strong><br>OSM id: ", aetl_lines()$id), group = "Airport") %>% # Taxilane
      addPolylines(data = aetw_lines(), color = "purple", weight = 5, popup = paste0("<strong>Taxiway</strong><br>OSM id: ", aetw_lines()$id), group = "Airport") %>% # Taxiway
      addPolygons(data = aete_poly(), color = "yellow", popup = paste0("<strong>Terminal </strong><br>OSM id: ", aete_poly()$id), group = "Airport") %>% # Terminal
      addCircleMarkers(aegt_points()$lon, aegt_points()$lat, color = "green", radius = 2, popup = paste0("<strong>Gate </strong><br>OSM id: ", aegt_points()$id), group = "Airport") # Gate
    
    # Other Data not used
    #addPolygons(data = aero_poly) %>%
    #addPolylines(data = aero_lines) %>%
    #addCircleMarkers(aero_points$lon, aero_points$lat, color = "red", radius = 5) %>%
    #addCircleMarkers(aedr_points$lon, aedr_points$lat, color = "red", radius = 5) %>%
    #addPolylines(data = aeha_lines) %>%
    #addCircleMarkers(aehepa_point$lon, aehepa_point$lat, color = "red", radius = 5) %>%
    #addCircleMarkers(aehepo_points$lon, aehepo_points$lat, color = "red", radius = 5) %>%
    #addCircleMarkers(aetl_points$lon, aetl_points$lat, color = "red", radius = 5) %>%
    #addCircleMarkers(aetw_points$lon, aetw_points$lat, color = "red", radius = 5) %>%
    #addCircleMarkers(aete_points$lon, aete_points$lat, color = "red", radius = 5) %>%
    #addPolylines(data = aeud_lines, weight = 5 color = "steelblue1", popup = paste0("<strong>User Defined</strong>")) %>%
    #addCircleMarkers(aeud_points$lon, aeud_points$lat, color = "steelblue1", radius = 5, popup = paste0("<strong>User Defined</strong>")) %>%
    #addCircleMarkers(aews_points()$lon, aews_points()$lat, color = "black", radius = 5, popup = paste0("<strong>Windsock</strong>")) %>% # Windsock
    #addCircleMarkers(aenaid_points()$lon, aenaid_points()$lat, color = "red", radius = 5, popup = paste0("<strong>NavigationAid</strong>")) %>% # NavigationAid
  })
  # Data Table
  ## APRONS
  # OUT
  outaproncheckdata <- reactive({airportchecks(aeap_poly(), values$a, outpoints(), outpointsdata(), NULL)})
  output$outaproncheck <- renderDataTable({outaproncheckdata()})
  outaproncheckdatacolumn <- eventReactive(input$refresh, {datacolcreation(outaproncheckdata())})
  # IN
  inaproncheckdata <- reactive({airportchecks(aeap_poly(), values$a, inpoints(), inpointsdata(), NULL)})
  output$inaproncheck <- renderDataTable({inaproncheckdata()})
  inaproncheckdatacolumn <- eventReactive(input$refresh, {datacolcreation(inaproncheckdata())})
  ## RUNWAYs
  # OFF
  offruncheckdata <- reactive({airportchecks(aerw_poly(), values$a, offpoints(), offpointsdata(), aerw_lines())})
  output$offruncheck <- renderDataTable({offruncheckdata()})
  offruncheckdatacolumn <- eventReactive(input$refresh, {datacolcreation(offruncheckdata())})
  # ON
  onruncheckdata <- reactive({airportchecks(aerw_poly(), values$a, onpoints(), onpointsdata(), aerw_lines())})
  output$onruncheck <- renderDataTable({onruncheckdata()})
  onruncheckdatacolumn <- eventReactive(input$refresh, {datacolcreation(onruncheckdata())})
  ## PLOTS
  output$plotcreationstats <- renderPlot({statsplot(outaproncheckdatacolumn(), inaproncheckdatacolumn(), offruncheckdatacolumn(), onruncheckdatacolumn())})
  
  # Reactive data in bounds
  dataInBounds <- reactive({
    df <- FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                             "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                             "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                             "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")]
    bounds <- input$map1_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(df, (Latitude.OUT >= latRng[1] & Latitude.OUT <= latRng[2] & Longitude.OUT >= lngRng[1] & Longitude.OUT <= lngRng[2]) |
             (Latitude.OFF >= latRng[1] & Latitude.OFF <= latRng[2] & Longitude.OFF >= lngRng[1] & Longitude.OFF <= lngRng[2]) | 
             (Latitude.ON >= latRng[1] & Latitude.ON <= latRng[2] & Longitude.ON >= lngRng[1] & Longitude.ON <= lngRng[2]) | 
             (Latitude.IN >= latRng[1] & Latitude.IN <= latRng[2] & Longitude.IN >= lngRng[1] & Longitude.IN <= lngRng[2]))
  })
  # Create the info boxes
  output$flightstatsBox <- renderUI({
    valueBox(nrow(dataInBounds()), "Flights", icon = icon("database"), color = "red", width = 2)
  })
  output$fleetstatsBox <- renderUI({
    valueBox(length(unique(dataInBounds()$AC_type)), icon = icon("plane"), "Fleets", color = "blue", width = 2)
  })
  output$airlstatsBox <- renderUI({
    valueBox(length(unique(dataInBounds()$Operator)), "Airlines", icon = icon("fighter-jet"), color = "purple", width = 2)
  })
  output$regstatsBox <- renderUI({
    valueBox(length(unique(dataInBounds()$Registration)), "MSNs", icon = icon("list-ol"), color = "green", width = 2)
  })
  output$fntatsBox <- renderUI({
    valueBox(length(unique(dataInBounds()$FlightNumber)), "Flight Numbers", icon = icon("list-ol"), color = "black", width = 2)
  })
  output$routestatsBox <- renderUI({
    valueBox(length(na.omit(unique(cbind(as.character(dataInBounds()$Destination), as.character(dataInBounds()$Departure)))))/2, "Routes", icon = icon("globe"), color = "yellow", width = 2)
  })
  ## DATA
  output$tabledatainbounds <- DT::renderDataTable({dataInBounds()}, options = list(pageLength = 5))
  
  # observeEvent(input$tabledatainbounds_rows_selected, {
  #   if (!is.null(input$tabledatainbounds_rows_selected)) {
  #     if (!is.na(dataInBounds()$Longitude.OUT[as.numeric(input$tabledatainbounds_rows_selected)]) & !is.na(dataInBounds()$Latitude.OUT[as.numeric(input$tabledatainbounds_rows_selected)])) {
  #       leafletProxy("map1") %>% 
  #         addCircleMarkers(dataInBounds()$Longitude.OUT[as.numeric(input$tabledatainbounds_rows_selected)], dataInBounds()$Latitude.OUT[as.numeric(input$tabledatainbounds_rows_selected)], color="orange", radius=20, layerId="outmoment")
  #     }
  #     # if (!is.na(dataInBounds()$Longitude.OFF[as.numeric(input$tabledatainbounds_rows_selected)]) & !is.na(dataInBounds()$Latitude.OFF[as.numeric(input$tabledatainbounds_rows_selected)])) {
  #     #   leafletProxy("map1") %>% #removeMarker("offmoment") %>%
  #     #     addCircleMarkers(dataInBounds()$Longitude.OFF[as.numeric(input$tabledatainbounds_rows_selected)], dataInBounds()$Latitude.OFF[as.numeric(input$tabledatainbounds_rows_selected)], color="red", radius=20, group="moment")
  #     # }
  #     # if (!is.na(dataInBounds()$Longitude.ON[as.numeric(input$tabledatainbounds_rows_selected)]) & !is.na(dataInBounds()$Latitude.ON[as.numeric(input$tabledatainbounds_rows_selected)])) {
  #     #   leafletProxy("map1") %>% #removeMarker("onmoment") %>%
  #     #     addCircleMarkers(dataInBounds()$Longitude.ON[as.numeric(input$tabledatainbounds_rows_selected)], dataInBounds()$Latitude.ON[as.numeric(input$tabledatainbounds_rows_selected)], color="blue", radius=20, group="moment")
  #     # }
  #     # if (!is.na(dataInBounds()$Longitude.IN[as.numeric(input$tabledatainbounds_rows_selected)]) & !is.na(dataInBounds()$Latitude.IN[as.numeric(input$tabledatainbounds_rows_selected)])) {
  #     #   leafletProxy("map1") %>% #removeMarker("inmoment") %>%
  #     #     addCircleMarkers(dataInBounds()$Longitude.IN[as.numeric(input$tabledatainbounds_rows_selected)], dataInBounds()$Latitude.IN[as.numeric(input$tabledatainbounds_rows_selected)], color="green", radius=20, group="moment")
  #     # }
  #   #} else {
  #     #map1 <- map1 %>% removeMarker(layerId="outmoment")
  #   }
  # # })
  # # 
  # # observeEvent(input$tabledatainbounds_rows_selected, {
  #   # if (is.null(input$tabledatainbounds_rows_selected) & ciao==1) {
  #   #   leafletProxy("map1") %>% clearMarker(group="moment")
  #   #   ciao <- 0
  #   # }
  # })
  
  
  
  ## All Flights Map
  output$animation1 <- renderUI({
    sliderInput("animval1", "Looping Animation:", min = 1, max = nrow(timeframe()), step = 1, value = 1,
                animate = animationOptions(interval = (input$timerspeed1)*1000, loop=TRUE))
  })
  output$mapx <- renderLeaflet({
    leaflet() %>% addTiles() %>% fitBounds(-100, -80, 100, 80) %>% 
      addProviderTiles("MapQuestOpen.OSM", group = "Default") %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c('Dark Map','Default', 'Satellite'), 
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  timeframe <- eventReactive(input$clickx, {fr24datainterp(FlightSummary(), fr24data())})
  flights <- reactive({timeframe()[input$animval1,!is.na(timeframe()[input$animval1,])]})
  observe({
    if (!is.null(flights()) & length(flights())>1) {
      leafletProxy("mapx") %>% clearMarkers() %>% 
        addCircleMarkers(flights()[seq(3,length(flights()), by=2)], flights()[seq(2,length(flights()), by=2)], color = "yellow", radius = 0.01) 
    }
  })
  output$timevar <- renderText({format((as.POSIXct(as.numeric(flights()[1]), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S")})
  output$amountflightsBox <- renderUI({
    valueBox((length(flights())-1)/2, "Flights", icon = icon("database"), color = "black")
  })
  
  
  ## Flight Path
  # Timer
  output$animation2 <- renderUI({
    sliderInput("animval2", "Looping Animation:",
                min = 1,
                max = length(fr24data()$Latitude[!is.na(fr24data()$Longitude) & !is.na(fr24data()$Latitude) & fr24data()$Registration == input$regselected3]),
                value = 1,
                step = 1,
                animate = animationOptions(interval = (input$timerspeed2)*1000, loop=TRUE))
  })
  output$animationsel <- renderUI({
    selectizeInput(
      inputId = "regselected3",
      label = h5(strong("Select MSN")),
      multiple  = F,
      choices = as.character(sort(unique(fr24data()$Registration[!is.na(fr24data()$Registration)])))
    )
  })
  # Map
  output$map3 <- renderLeaflet({
    map3 <- leaflet() %>% addTiles() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("MapQuestOpen.OSM", group = "Default") %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "Dark Map") %>%
      addLayersControl(baseGroups = c("Satellite", 'Default','Dark Map'), overlayGroups = c("Static Route"), options = layersControlOptions(collapsed = TRUE)) %>%
      
      addCircles(fr24data()$Longitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                 fr24data()$Latitude[!is.na(fr24data()$Longitude) & !is.na(fr24data()$Latitude) & fr24data()$Registration == input$regselected3],
                 color = "orange", radius = 3, 
                 popup = paste0("<strong>Date-Time: </strong>", format((as.POSIXct(as.numeric(fr24data()$Timestamp[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3]), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S"),
                                "<br><strong>Fleet: </strong>", fr24data()$AC_type[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                                "<br><strong>Registration: </strong>", fr24data()$Registration[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                                "<br><strong>Flight Number: </strong>", fr24data()$FlightNumber[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                                "<br><strong>Route: </strong>", paste(fr24data()$Departure[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                                                                      fr24data()$Destination[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],sep="-"),
                                "<br><strong>Coord. (Lat-Lon): </strong>", paste(substr(fr24data()$Latitude[!is.na(fr24data()$Longitude) & !is.na(fr24data()$Latitude) & fr24data()$Registration == input$regselected3],1,6),
                                                                                 substr(fr24data()$Longitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],1,6),sep="-"),
                                "<br><strong>Speed: </strong>", fr24data()$GroundSpeed[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                                "<br><strong>Altitude: </strong>", fr24data()$Altitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3]), group = "Static Route")
  })
  
  animdata <- eventReactive(input$regselected3, {
    subset(fr24data(), !is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3)
  })
  
  output$map77 <- renderLeaflet({
    map77 <- leaflet() %>% addProviderTiles("Stamen.TonerLite") %>%
      addCircles(fr24data()$Longitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected3],
                 fr24data()$Latitude[!is.na(fr24data()$Longitude) & !is.na(fr24data()$Latitude) & fr24data()$Registration == input$regselected3],
                 color = "yellow", radius = 0.1)
  })
  
  observe({
    planepoint <- animdata()[input$animval2,]
    
    # direction of the icon
    iconchoice <- ifelse(planepoint$Track >= 337.5 |
                           planepoint$Track < 22.5,"planeicon0.png",
                         ifelse(planepoint$Track >= 22.5 &
                                  planepoint$Track < 67.5,"planeicon45.png",
                                ifelse(planepoint$Track >= 67.5 &
                                         planepoint$Track < 112.5,"planeicon90.png",
                                       ifelse(planepoint$Track >= 112.5 &
                                                planepoint$Track < 157.5,"planeicon135.png",
                                              ifelse(planepoint$Track >= 157.5 &
                                                       planepoint$Track < 202.5,"planeicon180.png",
                                                     ifelse(planepoint$Track >= 202.5 &
                                                              planepoint$Track < 247.5,"planeicon225.png",
                                                            ifelse(planepoint$Track >= 247.5 &
                                                                     planepoint$Track < 292.5,"planeicon270.png",
                                                                   ifelse(planepoint$Track >= 292.5 &
                                                                            planepoint$Track < 337.5,"planeicon315.png", "planeicon90.png"))))))))
    
    planeicon <- makeIcon(iconchoice, iconWidth = ifelse(iconchoice == "planeicon0.png" | iconchoice == "planeicon90.png" |
                                                           iconchoice == "planeicon180.png" | iconchoice == "planeicon270.png", 30, 40),
                          iconHeight = ifelse(iconchoice == "planeicon0.png" | iconchoice == "planeicon90.png" |
                                                iconchoice == "planeicon180.png" | iconchoice == "planeicon270.png", 30, 40))
    zoom <- ifelse(planepoint$Altitude==0,0.01,
                   ifelse(planepoint$Altitude>0 &
                            planepoint$Altitude<=7000, 0.1,
                          ifelse(planepoint$Altitude>7000 &
                                   planepoint$Altitude<=15000,1,4)))
    
    leafletProxy("map3") %>% clearMarkers() %>% addMarkers(planepoint$Longitude, planepoint$Latitude, icon = planeicon) %>% 
      fitBounds(planepoint$Longitude-zoom, planepoint$Latitude-zoom, planepoint$Longitude+zoom, planepoint$Latitude+zoom)
    
    leafletProxy("map77") %>% clearMarkers() %>% addMarkers(planepoint$Longitude, planepoint$Latitude)
    
    altitudedata <- animdata()
    altitudedata$datetime <- format((as.POSIXct(as.numeric(altitudedata$Timestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S")
    altitudedata$datetime <- as.POSIXct(strptime(altitudedata$datetime, "%d/%m/%Y %H:%M:%S", tz="GMT"))
    outtimedata <- subset(oooi(), Registration==input$regselected3 & OUTtimestamp != 0)
    outtimedata$datetime <- format((as.POSIXct(as.numeric(outtimedata$Timestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S")
    outtimedata$datetime <- as.POSIXct(strptime(outtimedata$datetime, "%d/%m/%Y %H:%M:%S", tz="GMT"))
    intimedata <- subset(oooi(), Registration==input$regselected3 & INtimestamp != 0)
    intimedata$datetime <- format((as.POSIXct(as.numeric(intimedata$Timestamp), origin="1970-01-01",tz="GMT")), "%d/%m/%Y %H:%M:%S")
    intimedata$datetime <- as.POSIXct(strptime(intimedata$datetime, "%d/%m/%Y %H:%M:%S", tz="GMT"))
    
    # Altitude
    output$altitudeplot <- renderPlot({
      p <- ggplot(altitudedata, aes(datetime, Altitude)) + 
        geom_point() + 
        geom_point(data=outtimedata, aes(datetime, Altitude), fill="orange", size=3, shape=21) +
        geom_point(data=intimedata, aes(datetime, Altitude), fill="green", size=3, shape=21) +
        geom_point(data=subset(altitudedata, OFFtimestamp!=0), aes(datetime, Altitude), fill="red", size=3, shape=21) + 
        geom_point(data=subset(altitudedata, ONtimestamp!=0), aes(datetime, Altitude), fill="blue", size=3, shape=21) +
        geom_point(data=altitudedata[input$animval2,], aes(datetime, Altitude), fill="yellow", size=5, shape=21) + 
        labs(x = "Time") +
        labs(y = "Altitude (ft)")
      p
    })
    
    # Showing info
    output$info11 <- renderText({as.character(altitudedata[input$animval2,25])}) # time
    output$info12 <- renderText({paste0(as.character(altitudedata[input$animval2,8]), "-", as.character(altitudedata[input$animval2,9]))}) # dep-dest
    output$info13 <- renderText({as.character(altitudedata[input$animval2,12])}) # alt
    output$info21 <- renderText({as.character(altitudedata[input$animval2,5])}) # fn
    output$info22 <- renderText({unique(subset(FlightSummary(), Registration==altitudedata[input$animval2,4] &
                                                 Departure==altitudedata[input$animval2,8] & Destination==altitudedata[input$animval2,9] &
                                                 FlightNumber==altitudedata[input$animval2,5])[,c("Operator")])}) # op
    output$info23 <- renderText({as.character(altitudedata[input$animval2,7])}) # ac
    output$info24 <- renderText({as.integer(altitudedata[input$animval2,13])}) # gs
    
    test <- FlightSummary()[,c(1:16)]
    test$chaincheck <- rep(NA, nrow(test))
    test$chaincheck[2:nrow(test)] <- ifelse(as.character(test$Registration[2:nrow(test)])==as.character(test$Registration[1:(nrow(test)-1)]) & 
                                              as.character(test$Departure[2:nrow(test)])!=as.character(test$Destination[1:(nrow(test)-1)]), 1, NA)
    output$monitoring <- renderText({as.character(unique(subset(test, chaincheck==1)[,"Registration"]))}) # gs
  })
  
  
  # 3D Globe
  output$globe <- renderGlobe({
    if (input$radiochoice2 == 1){ # TAT
      globejs(lat = FlightSummary()$Latitude.ON[!is.na(FlightSummary()$TAT_hrs)],
              long = FlightSummary()$Longitude.ON[!is.na(FlightSummary()$TAT_hrs)],
              value = FlightSummary()$TAT_hrs[!is.na(FlightSummary()$TAT_hrs)])
    } else if (input$radiochoice2 == 2){ # Taxi OUT
      globejs(lat = FlightSummary()$Latitude.OFF[!is.na(FlightSummary()$Taxi.OUT_mins)],
              long = FlightSummary()$Longitude.OFF[!is.na(FlightSummary()$Taxi.OUT_mins)],
              value = FlightSummary()$Taxi.OUT_mins[!is.na(FlightSummary()$Taxi.OUT_mins)])
    } else if (input$radiochoice2 == 3){ # Taxi IN
      globejs(lat = FlightSummary()$Latitude.ON[!is.na(FlightSummary()$Taxi.IN_mins)],
              long = FlightSummary()$Longitude.ON[!is.na(FlightSummary()$Taxi.IN_mins)],
              value = FlightSummary()$Taxi.IN_mins[!is.na(FlightSummary()$Taxi.IN_mins)])
    } else if (input$radiochoice2 == 4) { # Route
      globejs(lat = fr24data()$Latitude[!is.na(fr24data()$Longitude) & !is.na(fr24data()$Latitude) & fr24data()$Registration == input$regselected2],
              long = fr24data()$Longitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected2],
              value = fr24data()$Altitude[!is.na(fr24data()$Latitude) & !is.na(fr24data()$Longitude) & fr24data()$Registration == input$regselected2]/2000)
    }
  })
  
  output$regselected2 <- renderUI({
    if (input$radiochoice2==4) {
      selectizeInput(
        inputId = "regselected2",
        label = "Select MSN",
        multiple  = F,
        choices = as.character(sort(unique(fr24data()$Registration[!is.na(fr24data()$Registration)])))
      )} else {return(NULL)}
  })
  #-------------------------------------------END MAPPING
  
  
  
  #-------------------------------------------START
  ### GRAPHS
  
  ## SIDEBAR START ##
  
  ## DATA CHOICE
  # FlightSummary or imported?
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      infile <- input$datachoice
      if (infile == "FlightSummary") {
        FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                           "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                           "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                           "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN", "Temperature_C.OFF", "Humidity.OFF", "WindSpeed_Kmh.OFF", 
                           "Visibility_Km.OFF","Temperature_C.ON", "Humidity.ON", "WindSpeed_Kmh.ON", "Visibility_Km.ON")]
      } else {
        fr24data()[,1:14]
      }
    } else {
      read.csv(infile$datapath,na.strings = c("NA","."))
    }
  })
  
  ## DATA TABLE
  output$filedata <- renderDataTable(
    filedata(), 
    options = list(pageLength = 7)
  )
  
  ## AXIS
  # Axis Choices
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("x", "x-axis:", items)
  })
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y", "y-axis:", items)
  })
  output$slider <- renderUI({
    df <-filedata()
    xvariable<- input$x
    if (is.null(df)) return(NULL)
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    sliderInput("inSlider", paste(xvariable,"Range"),
                min=min(df[,xvariable],na.rm=T),
                max=max(df[,xvariable],na.rm=T),
                value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
    )
  })
  output$slider2 <- renderUI({
    df <-filedata()
    yvariable<- input$y
    if (is.null(df)) return(NULL)
    if (!is.numeric(df[,yvariable]) ) return(NULL)
    sliderInput("inSlider2", paste(yvariable,"Range"),
                min=min(df[,yvariable],na.rm=T),
                max=max(df[,yvariable],na.rm=T),
                value=c(min(df[,yvariable],na.rm=T),max(df[,yvariable],na.rm=T)) 
    )
  })
  
  ## FILTERING
  output$maxlevels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    numericInput( inputId = "inmaxlevels",label = "Max number of unique values for filter variables:",value = 250,min = 1,max = NA)
    
  })
  output$filtervar1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP <- names(df)[NUNIQUEDF<input$inmaxlevels]
    selectInput("infiltervar1" , "Filter variable 1:", c('None',NAMESTOKEEP))
  })
  output$filtervar1values <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if(input$infiltervar1=="None") {
      return(NULL)  
    }
    if(input$infiltervar1!="None" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Select values", input$infiltervar1),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  })  
  subsetdata  <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if(!is.numeric(input$inSlider[1])) {
      df 
    }
    if(!is.numeric(input$inSlider2[1])) {
      df 
    }
    if(is.numeric( input$inSlider[1]) & is.numeric(df[,input$x])) {
      df <- df[!is.na(df[,input$x]),]
      df <- df[df[,input$x] >= input$inSlider[1]&df[,input$x] <= input$inSlider[2],]
    }
    if(is.numeric( input$inSlider2[1])& is.numeric(df[,input$y])) {
      df<- df[!is.na(df[,input$y]),]
      df <- df[df[,input$y] >= input$inSlider2[1]&df[,input$y] <= input$inSlider2[2],]
    }  
    if(is.null(input$infiltervar1)) {
      df <- df 
    }
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None") {
      
      df <- df[is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    df
  })
  output$filtervar2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP <- names(df)[NUNIQUEDF<input$inmaxlevels]
    NAMESTOKEEP <-  NAMESTOKEEP[NAMESTOKEEP!=input$infiltervar1]
    
    selectInput("infiltervar2" , "Filter variable 2:", c('None', NAMESTOKEEP))
  })
  output$filtervar2values <- renderUI({
    df <- subsetdata()
    
    if (is.null(df)) return(NULL)
    if(input$infiltervar2=="None") {
      selectInput('infiltervar2valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="None"&!is.null(input$infiltervar2))  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectInput('infiltervar2valuesnotnull',
                  label = paste("Select values", input$infiltervar2),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=TRUE)   
    }
  })   
  subsetdata2  <- reactive({
    df <- subsetdata()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar2)&input$infiltervar2!="None") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    if(input$infiltervar2=="None") {
      df 
    }
    df
  })
  
  # Functions
  recodedata1  <- reactive({
    df <- subsetdata2()
    if (is.null(df)) return(NULL)
    if(length(input$catvarin)>=1) {
      for (i in 1:length(input$catvarin)) {
        varname <- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncutsin)
        df[,varname]   <- as.factor(df[,varname])
      }
    }
    df
  })
  recodedata2  <- reactive({
    df <- recodedata1()
    if (is.null(df)) return(NULL)
    if(length(input$catvar2in)>=1) {
      for (i in 1:length(input$catvar2in ) ) {
        varname <- input$catvar2in[i]
        df[,varname]   <- as.factor(df[,varname])
      }
    }
    df
  })
  
  ## GROUPING
  output$colour <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("colorin", "Colour By:", c("None",items))
  })
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("fillin", "Fill By:", c("None",items)
    )
  })
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("pointsizein", "Size By:", c("None",items)
    )
  })
  output$meancateg <- renderUI({
    df <-filedata()
    if (input$mean) {
      items=names(df)
      names(items)=items
      selectInput("meancat", "Cluster By:", items)
    } else {return(NULL)}
  })
  output$meanlinks <- renderUI({
    if (input$mean) {
      checkboxInput('meansegments', 'Links', value = FALSE)
    } else {return(NULL)}
  })
  
  # GGPLOT TRANSLATION
  plotObject <- reactive({
    plotdata <- recodedata2()
    if(!is.null(plotdata)) {
      
      p <- ggplot(plotdata, aes_string(x=input$x, y=input$y)) 
      
      if (input$Points=="Points"&input$pointsizein == 'None')
        p <- p + geom_point(alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
      if (input$Points=="Points"&input$pointsizein != 'None')
        p <- p + geom_point(alpha=input$pointstransparency,shape=input$pointtypes)  
      if (input$line=="Lines"&input$pointsizein == 'None')
        p <- p + geom_line(size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
      if (input$line=="Lines"&input$pointsizein != 'None')
        p <- p + geom_line(alpha=input$linestransparency,linetype=input$linetypes)
      if (input$pointsizein != 'None')
        p <- p  + aes_string(size=input$pointsizein)
      if (input$Points=="Jitter")
        p <- p + geom_jitter()
      if (input$colorin != 'None')
        p <- p + aes_string(color=input$colorin)
      if (input$fillin != 'None')
        p <- p + aes_string(fill=input$fillin)
      
      ## GRAPH Analysis & Look
      if (input$logy)
        p <- p + scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                               labels=trans_format("log10", math_format(10^.x)))
      if (input$logx)
        p <- p + scale_x_log10(breaks=trans_breaks("log10", function(x) 10^x),
                               labels=trans_format("log10", math_format(10^.x)))
      if (input$ylab!="")
        p <- p + ylab(input$ylab)
      if (input$xlab!="")
        p <- p + xlab(input$xlab)
      if (input$horizontalzero)
        p <- p + geom_hline(aes(yintercept=0))
      if (input$customline1)
        p <- p + geom_vline(xintercept=input$vline)
      if (input$customline2)
        p <- p + geom_hline(yintercept=input$hline)
      if (input$identityline)
        p <- p + geom_abline(intercept = 0, slope = 1)
      if (input$themebw)
        p <- p + theme_bw()
      p <- p + theme(
        legend.position=input$legendposition,
        panel.background = element_rect(fill=input$backgroundcol),
        axis.title.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16)
      )
      if (input$labelguides)
        p <- p + theme(legend.title=element_blank())
      if (input$themeaspect)
        p <- p + theme(aspect.ratio=input$aspectratio)
      
      # MEAN
      if (input$mean) {
        gg <- merge(FlightSummary(), aggregate(cbind(mean.x = FlightSummary()[[input$x]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])], 
                                                     mean.y = FlightSummary()[[input$y]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])]), 
                                               FUN=mean, by=list(FlightSummary()[[input$meancat]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])])), 
                    by.x = as.character(input$meancat), by.y = "Group.1")
        if (input$meansegments) {
          p <- p + geom_segment(color = "steelblue", data=gg, aes(x = mean.x, y = mean.y, xend = gg[[input$x]], yend = gg[[input$y]]), alpha=1/6)
        }
        p <- p + geom_point(data=gg, aes(x = mean.x, y = mean.y, fill = gg[[input$meancat]]), size=4, shape = 21, color = "Black")
      }
      
      # LOESS
      if (input$Loess=="Loess"){
        p <- p + geom_smooth(method="loess",size=1.5,se=F,span=input$loessens,aes(group=NULL), color=input$colloess)
      }
      
      p
    }
  })
  
  output$meantable <- renderDataTable({
    if (input$mean) {
      df <- aggregate(cbind(mean.x = FlightSummary()[[input$x]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])],
                            mean.y = FlightSummary()[[input$y]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])]),
                      FUN=mean, by=list(FlightSummary()[[input$meancat]][!is.na(FlightSummary()[[input$x]]) & !is.na(FlightSummary()[[input$y]])]))
      names(df) <- c(input$meancat, input$x, input$y)
      df}
    else {data.frame("NO MEAN DATA")}
  },
  options = list(pageLength = 5)
  )
  
  output$plotcreation <- renderPlot({
    plotObject()
  })
  
  
  ## GRAPH SELECTION
  output$plotinfo <- renderPrint({
    df <- recodedata2()  
    if (is.null(df)) return(NULL)
    nearPoints( recodedata2(), input$plot_click, threshold = 5, maxpoints = 5,
                addDist = TRUE,xvar=input$x, yvar=input$y)
  })
  output$clickheader <- renderUI({
    df <- recodedata2()
    if (is.null(df)) return(NULL)
  })
  output$brushheader <- renderUI({
    df <- recodedata2()
    if (is.null(df)) return(NULL)
  })
  output$plot_clickedpoints <- renderTable({
    df <- recodedata2()  
    if (is.null(df)) return(NULL)
    
    res <- nearPoints(recodedata2(), input$plot_click, input$x, input$y)
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  output$plot_brushedpoints <- renderTable({
    df<- recodedata2()
    if (is.null(df)) return(NULL)
    res <- brushedPoints(recodedata2(), input$plot_brush, input$x, input$y)
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  
  
  ## PLOT SELECTION
  plotCode <- reactive({
    input$plotCode
  })
  plotObject2 <- reactive({
    plotNo <- input$plotButton
    isolate(eval(parse(text = gsub("\\n", "", plotCode()))))
  })
  output$codeeditor <- renderPlot({
    print(plotObject2())
  })
  output$plotting <- renderUI({
    if (input$plotselected == "Normal") {
      plotOutput('plotcreation',  width = "100%" ,click = "plot_click",
                 hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                 brush = brushOpts(id = "plot_brush"))
    } else if (input$plotselected == "Edited") {
      plotOutput(outputId = "codeeditor")
    }
  })
  
  
  ## DOWNLOAD
  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)
  })
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    content = function(con) {
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  
  
  ## PLOTLY
  # Axis Choices and color
  output$xcoly <- renderUI({
    if (is.null(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])) return(NULL)
    items=names(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])
    names(items)=items
    selectInput("xy", "x-axis:",items)
  })
  output$ycoly <- renderUI({
    if (is.null(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])) return(NULL)
    items=names(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])
    names(items)=items
    selectInput("yy", "y-axis:",items)
  })
  output$zcoly <- renderUI({
    if (is.null(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])) return(NULL)
    items=names(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])
    names(items)=items
    selectInput("zy", "z-axis:",items)
  })
  output$coloury <- renderUI({
    if (is.null(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])) return(NULL)
    items=names(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                                   "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                                   "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                                   "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")])
    names(items)=items
    selectInput("coloryin", "Colour By:",c("None",items ) )
  })
  
  output$plotlyplot <- renderPlotly({
    plot_ly(FlightSummary()[,c("Registration", "Operator", "FlightNumber", "AC_type", "Departure", "Destination", 
                               "GCD_km", "FlightTime_hrs", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "TAT_type",
                               "OUT", "OFF", "ON", "IN","Latitude.OUT", "Longitude.OUT","Latitude.OFF", "Longitude.OFF",
                               "Latitude.ON", "Longitude.ON", "Latitude.IN", "Longitude.IN")], x = FlightSummary()[[input$xy]],
            y = FlightSummary()[[input$yy]], z = FlightSummary()[[input$zy]],
            type = "scatter3d", mode = "markers", color=FlightSummary()[[input$coloryin]]) %>%
      layout(xaxis = list(title = as.character(input$xy)),
             yaxis = list(title = as.character(input$yy)),
             zaxis = list(title = as.character(input$zy)))
    
  })
  
  
  ## Airport Network
  output$equipselected <- renderUI({
    selectizeInput(inputId = "equipselected", label = "Select Fleet", multiple  = F,
                   choices = c("",as.character(sort(unique(FlightSummary()$AC_type[!is.na(FlightSummary()$AC_type)])))))
  })
  output$regselected7 <- renderUI({
    selectizeInput(
      inputId = "regselected7",
      label = "Select MSN",
      multiple  = F,
      choices = c("",as.character(sort(unique(FlightSummary()$Registration[!is.na(FlightSummary()$Registration)])))))
  })
  output$opselected <- renderUI({
    selectizeInput(
      inputId = "opselected",
      label = "Select Operator (IATA)",
      multiple  = F,
      choices = c("",as.character(sort(unique(substr(FlightSummary()$FlightNumber[!is.na(FlightSummary()$FlightNumber)],1,2))))))
  })
  
  output$network_hello <- renderVisNetwork({
    nodes <- reactive({
      if (input$equipselected!="") {
        if (input$regselected7=="" & input$opselected=="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$AC_type == input$equipselected & !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7=="" & input$opselected!="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$AC_type == input$equipselected & 
                                                                      substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7!="" & input$opselected=="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$AC_type == input$equipselected & 
                                                                      FlightSummary()$Registration == input$regselected7 &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        } else {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$AC_type == input$equipselected & 
                                                                      FlightSummary()$Registration == input$regselected7 &
                                                                      substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        }
      } else {
        if (input$regselected7=="" & input$opselected=="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[!is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7=="" & input$opselected!="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7!="" & input$opselected=="") {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$Registration == input$regselected7 &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        } else {
          nodesdata <- data.frame(summary(FlightSummary()$Departure[FlightSummary()$Registration == input$regselected7 &
                                                                      substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                      !is.na(FlightSummary()$FlightNumber)]))
        }
      }
      nodesdata <- subset(nodesdata, nodesdata[,1]>0)
      nodesdata$Airport <- rownames(nodesdata)
      nodesdata <- subset(nodesdata, nodesdata$Airport != "" & !is.na(nodesdata$Airport))
      nodesdata$type <- "Departure"
      if (input$equipselected!="") {
        if (input$regselected7=="" & input$opselected=="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$AC_type == input$equipselected & !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7=="" & input$opselected!="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$AC_type == input$equipselected & 
                                                                         substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7!="" & input$opselected=="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$AC_type == input$equipselected & 
                                                                         FlightSummary()$Registration == input$regselected7 &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        } else {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$AC_type == input$equipselected & 
                                                                         FlightSummary()$Registration == input$regselected7 &
                                                                         substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        }
      } else {
        if (input$regselected7=="" & input$opselected=="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[!is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7=="" & input$opselected!="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        } else if (input$regselected7!="" & input$opselected=="") {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$Registration == input$regselected7 &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        } else {
          nodesdata2 <- data.frame(summary(FlightSummary()$Destination[FlightSummary()$Registration == input$regselected7 &
                                                                         substr(FlightSummary()$FlightNumber,1,2) == input$opselected &
                                                                         !is.na(FlightSummary()$FlightNumber)]))
        }
      }
      nodesdata2 <- subset(nodesdata2, nodesdata2[,1]>0)
      nodesdata2$Airport <- rownames(nodesdata2)
      nodesdata2 <- subset(nodesdata2, nodesdata2$Airport != "" & !is.na(nodesdata2$Airport))
      nodesdata2$type <- "Destination"
      nodesdata <- merge(nodesdata, nodesdata2, by=c(2,3), all=T)
      rm("nodesdata2")
      nodesdata[,3] <- ifelse(is.na(nodesdata[,3]), nodesdata[,4], nodesdata[,3])
      nodesdata[,4] <- ifelse(nodesdata[,2]=="Departure", paste0(nodesdata[,1], "dep"), paste0(nodesdata[,1], "dest"))
      
      data.frame(id = nodesdata[,4],label = nodesdata[,1], group = nodesdata[,2], value = nodesdata[,3],
                 title = paste0("<center><b>", nodesdata[,1],"</b><br>", nodesdata[,3], " flights"),shadow = T)
    })
    
    edges <- reactive({
      edgesdata <- data.frame(unique(FlightSummary()$Departure))
      edgesdata <- merge(edgesdata, unique(FlightSummary()[,c("Departure", "Destination")]), by.x=1, by.y="Departure", all.y=T)
      edgesdata[,1] <- paste0(edgesdata[,1], "dep")
      edgesdata[,2] <- paste0(edgesdata[,2], "dest")
      
      data.frame(from = edgesdata[,1], to = edgesdata[,2],arrows = "middle",smooth = TRUE, shadow = T,
                 title = paste0(substr(edgesdata[,1],1,3), "-", substr(edgesdata[,2],1,3)))
    })
    
    visNetwork(nodes(), edges()) %>% visLegend()
  })
  
  
  ## Weather
  observe({
    FlightSummary() %>%
      ggvis(x = input_select(c("OFF", "ON"), map = as.name, label = "x-axis", selected = "ON"),
            y = input_select(c("Registration", "Operator", "Departure", "Destination", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "Temperature_C.OFF", "Humidity.OFF", "WindSpeed_Kmh.OFF", 
                               "Visibility_Km.OFF","Temperature_C.ON", "Humidity.ON", "WindSpeed_Kmh.ON", "Visibility_Km.ON"), map = as.name, label = "y-axis", selected = "TAT_hrs") ,
            fill = input_select(c("Registration", "Operator", "Departure", "Destination", "Taxi.OUT_mins", "Taxi.IN_mins", "TAT_hrs", "Temperature_C.OFF", "Humidity.OFF", "WindSpeed_Kmh.OFF", 
                                  "Visibility_Km.OFF", "Temperature_C.ON", "Humidity.ON", "WindSpeed_Kmh.ON", "Visibility_Km.ON"), map = as.name, label = "Fill by", selected = "WindSpeed_Kmh.ON")) %>%
      layer_points() %>%
      add_axis("x", title = "Time") %>%
      add_axis("y", title = "Variable") %>%
      set_options(width = "auto") %>%
      bind_shiny("ggvis", "ggvis_ui")
  })
  
  output$tableweather <- DT::renderDataTable(DT::datatable({
    aggregate(cbind(HumidDiff.mean = HumidDiff, TempDiff.mean = TempDiff) ~ Registration, FlightSummary(), mean)
  }))
  
  
  ## OTP
  oagdata <- eventReactive(input$clickoag, {
    oagschedule <- oagimport(FlightSummary())
    # Filter the unreasonable ones
    oagschedule[!is.na(oagschedule$Dep_Delay_mins) & oagschedule$Dep_Delay_mins>-720 & !is.na(oagschedule$Arr_Delay_mins) & oagschedule$Arr_Delay_mins>-720,]
  })
  output$tableoag <- renderDataTable({
    oagdata()
  }, options = list(pageLength = 10)
  )
  # Plot 1
  observeEvent(selected_oag(), {
  output$plotting_oag <- renderPlot({
    p <- ggplot(oagdata(), aes(Dep_Delay_mins, Arr_Delay_mins)) +
      geom_smooth(method='lm', fill="orange", color="black") +
      geom_point(aes(fill = Operator), color= "black", shape=21, size = 4, alpha = 0.5) +
      geom_vline(xintercept = 0, colour="black") +
      geom_hline(yintercept = 0, colour="black") +
      xlim(-100,240) + ylim(-60,240) +
      xlab("Departure Delay (mins)") + ylab("Arrival Delay (mins)") +
      theme(legend.position="right") +
      ggtitle("Arrival vs Departure Delay by Operator") +
      labs(color ="Aircraft Type")
    p
  })
  })
  # Table of selected item
  selected_oag <- reactive({nearPoints(oagdata(), input$oagplot_hover, "Dep_Delay_mins", "Arr_Delay_mins")})
  output$info_oag <- renderDataTable({selected_oag()})
  output$oagplot1 <- renderUI({
    plotOutput('plotting_oag',  width = "100%", hover = "oagplot_hover", height=500)
  })
  # Operator Mean Table
  output$deparrdelaymeantable <- renderDataTable({
    df <- aggregate(cbind(mean.x = oagdata()$Dep_Delay_mins, mean.y = oagdata()$Arr_Delay_mins), FUN=mean, by=list(oagdata()$Operator))
    names(df) <- c("Operator", "Departure Delay (mins)", "Arrival Delay (mins)")
    df
  },
  options = list(pageLength = 7)
  )
  # Plot 2 & 3
  output$oagplot2 <- renderPlot({
    depmean <- mean(oagdata()[,"Dep_Delay_mins"])
    arrmean <- mean(oagdata()[,"Arr_Delay_mins"])
    p1 <- ggplot(oagdata(), aes(Dep_Delay_mins)) +
      geom_histogram(binwidth = 15, aes(fill = Operator), color="black") +
      geom_density() +
      geom_vline(xintercept = 0, colour="black") +
      geom_hline(yintercept = 0, colour="black") +
      geom_vline(xintercept = depmean, colour="red", linetype = "longdash") +
      xlim(-100,240) +
      ggtitle("Departure Delay by Operator") +
      xlab("Departure Delay (mins)") + ylab("Frequency of Flights") + 
      theme(legend.position="none")
    p2 <- ggplot(oagdata(), aes(Arr_Delay_mins)) +
      geom_histogram(binwidth = 15, aes(fill = Operator), color="black") +
      geom_vline(xintercept = 0, colour="black") +
      geom_hline(yintercept = 0, colour="black") +
      geom_vline(xintercept = arrmean, colour="red", linetype = "longdash") +
      xlim(-100,240) +
      ggtitle("Arrival Delay by Operator") +
      xlab("Arrival Delay (mins)") + ylab("Frequency of Flights") + 
      theme(legend.position="none")
    multiplot(p1, p2, cols=2)
  })
  output$depdelaymean <- renderText({paste(round(mean(oagdata()[,"Dep_Delay_mins"]),2), "mins")})
  output$arrdelaymean <- renderText({paste(round(mean(oagdata()[,"Arr_Delay_mins"]),2), "mins")})
  #-------------------------------------------END GRAPHS
  
  
  
  #-------------------------------------------START
  ### REPORTING
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(input$filename, sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      out <- render('./Functions & Scripts/report_template.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  #-------------------------------------------END
  
  #-------------------------------------------END SERVER
  # TO UNCOMMENT WHEN CALLED OUTSIDE RSTUDIO
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("yes")
  #   closeAllConnections()
  #   rm(list=ls())
  # })
}


shinyApp(ui = ui, server = server)