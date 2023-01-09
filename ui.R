source("global.R")

# Mobility Indicators
#### Modes per Region
#### Motives & Modes per Region
#### Timeframe Data: Travel Purpose
#### Timeframe Data: Travel Mode
#### Personal Characteristics

# Green Mobility

# Traffic Intensity

# Proximity to Amenities
#### Map
#### Plot

#About us/ Data


shinyUI(navbarPage("Mobility in the Northern Netherlands",
                   tabPanel("Mobility Indicators", fluidPage(
                     navlistPanel(
                       id = "tabset",
                       "Ideas", # subtitle of overall dashboard
                       tabPanel("Modes per regions", # title of tab
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId = "region", #for server side
                                                label = "Select region", # text displayed 
                                                choices = unique(data84710$RegionCharacteristics), # input to choose from
                                                selected = "Northern Netherlands") # define default
                                  ),
                                  mainPanel(
                                    plotlyOutput("plotidea1")
                                  )
                                )),
                       tabPanel("Motives & Modes per regions", # title of tab
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(
                                      inputId = "TravelMotives",
                                      label = "Select Reason for Travel",
                                      choices = unique(data84710$TravelMotives),
                                      multiple = FALSE,
                                      selected = "Total"
                                    )
                                  ),
                                  mainPanel(plotlyOutput("lineplottravelmotives"))
                                ),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(
                                      inputId = "TravelModes",
                                      label = "Select Mode of Travel",
                                      choices = unique(data84710$TravelModes),
                                      multiple = FALSE,
                                      selected = "Total"
                                    )
                                  ),
                                  mainPanel(plotlyOutput("lineplottravelmodes"))
                                )),
                       tabPanel("Timeframe data: Travel Purpose",
                                plotlyOutput("timeframedataplot"),
                                hr(),
                                fluidRow(
                                  column(3,
                                         selectInput(
                                           inputId = "Periods1",
                                           label = "Select Year",
                                           choices = unique(data85055$Periods),
                                           selected = "2021",
                                           multiple = FALSE
                                         )),
                                  column(3,
                                         selectInput(inputId = "TravelPurposes",
                                                     label = "Pick Purpose of Travel",
                                                     choices = unique(data85055$TravelPurposes),
                                                     multiple = FALSE,
                                                     selected = "Total"
                                         )),
                                  column(3,
                                         radioButtons("Timeframe1", "Pick a timeframe",
                                                      choiceNames = c("Month", "Departure time", "Day of the week"),
                                                      choiceValues = c("Month", "Departure time", "Day of the week")
                                         )))),
                       tabPanel("Timeframe data: Travel Mode",
                                plotlyOutput("secondtimeframedataplot"),
                                hr(),
                                fluidRow(
                                  column(3,
                                         selectInput(
                                           inputId = "Periods2",
                                           label = "Select Year",
                                           choices = unique(data85056$Periods),
                                           selected = "2021", 
                                           multiple = FALSE
                                         )),
                                  column(3,
                                         selectInput(inputId = "ModesOfTravel",
                                                     label = "Pick Mode of Travel",
                                                     choices = unique(data85056$ModesOfTravel),
                                                     multiple = FALSE,
                                                     selected = "Total"
                                         )),
                                  column(3,
                                         radioButtons("Timeframe2", "Pick a timeframe",
                                                      choiceNames = c("Month", "Departure time", "Day of the week"),
                                                      choiceValues = c("Month", "Departure time", "Day of the week")
                                         )))),         
                       tabPanel("Personal Characteristics", # title of tab
                                plotlyOutput("Persoonskenmerken"),
                                hr(),
                                fluidRow(
                                  column(3,
                                         selectInput(inputId = "Perioden_graph1", 
                                                     label = "Years",
                                                     choices = unique(data84709$Perioden),
                                                     multiple = FALSE, 
                                                     selected = "2021"
                                         )),
                                  column(3,
                                         selectInput(inputId = "Features", 
                                                     label = "Feature", 
                                                     choices = unique(data84709$Feature),
                                                     multiple = FALSE, 
                                                     selected = "All"
                                         )),
                                  column(3,
                                         selectInput(inputId = "Vervoerwijzen_graph1", 
                                                     label = "Vervoerwijzen",
                                                     choices = unique(data84709$Vervoerwijzen), 
                                                     multiple = FALSE, 
                                                     selected = "Totaal"
                                         )))),
                       tabPanel("Driving licenses",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput(inputId = "LicenseHolderAge",
                                                label = "License Holder Age",
                                                choices = unique(data83488$AgeDrivingLicenseHolder), # What to do here, because you use a different column right?
                                                multiple = FALSE,
                                                selected = "Total"),
                                    
                                    selectInput(inputId = "LicenseCategory",
                                                label = "License Category",
                                                choices = unique(data83488$CategoryDrivingLicence), # What to do here, because you use a different column right?
                                                multiple = FALSE,
                                                selected = "Total"),
                                    selectInput(inputId = "PeriodsLicense",
                                                label = "Year", 
                                                choices = unique(data83488$Periods),
                                                selected = "2022",
                                                multiple = FALSE)
                                    
                                  ),
                                  mainPanel(
                                    plotlyOutput("DrivingLicense1"),plotlyOutput("DrivingLicense2")
                                  )
                                ))
                       
                     ))),
                   tabPanel("Green Mobility"),
                   tabPanel("Traffic Intensity", fluidPage(
                     navlistPanel(
                       id = "tabset",
                       "Traffic/Infrastructure",
                       tabPanel("Traffic Intensity",
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("Years_traffic", "Year", 
                                                choices = unique(data83712$Years), 
                                                multiple = FALSE)
                                  ),
                                  mainPanel(plotlyOutput("traffic_barplot"))
                                )
                       ),
                       tabPanel("Length of Highways",
                                sidebarLayout(
                                  sidebarPanel(
                                  ),
                                  mainPanel(plotlyOutput("highways"))
                                )
                       ),
                     ))
                   ),
                   tabPanel("Proximity to Amenities", fluidPage(sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         inputId = "mapvariable", # for server side interactivity
                         label = "Average Distance to...", # text displayed 
                         choices = c( # input to choose from  
                           # this can likely be summarized now that the data has been filtered
                           "GP practice" = "DistanceToGPPractice_1",
                           "GP post" = "DistanceToGPPost_5",
                           "Pharmacy" = "DistanceToPharmacy_6",
                           "Hospital" = "DistanceToHospital_11",
                           "Large Supermarket" = "DistanceToLargeSupermarket_20",
                           "Shop for other daily food" = "DistanceToShopForOtherDailyFood_24",
                           "Department Store" = "DistanceToDepartmentStore_28",
                           "Cafe or similar" = "DistanceToCafeEtc_32",
                           "Restaurant" = "DistanceToRestaurant_40",
                           "Daycare Dentres" = "DistanceToDaycareCentres_48",
                           "Out-of-school Care" = "DistanceToOutOfSchoolCare_52",
                           "Primary school" = "DistanceToSchool_60",
                           "Secondary school (all types)" = "DistanceToSchool_64",
                           "Train Stations" = "DistanceToTrainStationsAllTypes_101",
                           "Library" = "DistanceToLibrary_103",
                           "Average of 15 indicators" = "Avg15"
                         ),
                         selected = "Large Supermarket"
                       ) 
                     ),
                     mainPanel(
                       fluidRow(
                         column(6, div(style = "height: 50vh;", girafeOutput("map", height = "100%"))),
                         column(6, div(style = "height: 50vh;", plotOutput("mapplot", height = "100%")))
                       )
                     )))),
                   navbarMenu("About us/data",
                              tabPanel("Datasets origin and details"),
                              tabPanel("Creators"))
))