source("global.R")

shinyUI(fluidPage(
  navlistPanel(
    id = "tabset",
    "Ideas", # subtitle of overall dashboard
    tabPanel("Bar Graph", # title of tab
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "RegioS",
                   label = "Select region",
                   choices = unique(data84709$RegioS),
                   multiple = TRUE,
                   selected = "Noord-Nederland (LD)"
                 ),
                 radioButtons(
                   inputId = "variable",
                   label = "Pick parameter",
                   choiceNames = c("Reisduur_3"),
                   choiceValues = c("Reisduur_3")
                 )
               ),
               mainPanel(plotOutput("plot"), plotOutput("plot2"))
             )),
    
    tabPanel("Bar Graph", # title of tab
             sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "region", #for server side
                    label = "Select region", # text displayed 
                    choices = unique(data84710$RegionCharacteristics), # input to choose from
                    selected = "Noord-Nederland (LD)") # define default
      ),
      mainPanel(
        plotOutput("plotidea1")
      )
    )),
    tabPanel("Line graph Travel Motives and Motives", # title of tab
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
               mainPanel(plotOutput("lineplottravelmotives"))
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
               mainPanel(plotOutput("lineplottravelmodes"))
             )),
    tabPanel("Timeframe data: Travel Purpose",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Periods",
                   label = "Select Year",
                   choices = unique(data85055$Periods),
                   multiple = FALSE,
                   selected = "2021"
                 ),
                 selectInput(inputId = "TravelPurposes",label = "Pick Purpose of Travel",
                             choices = unique(data85055$TravelPurposes),
                             multiple = FALSE,
                             selected = "Total"
                 ),
                 radioButtons("Timeframe", "Pick a timeframe",
                              choiceNames = c("Month", "Departure time", "Day of the week"),
                              choiceValues = c("Month", "Departure time", "Day of the week")
                 )
               ),
               mainPanel(plotlyOutput("timeframedataplot"))
             )),
    tabPanel("Timeframe data: Travel Mode",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "Periods",
                   label = "Select Year",
                   choices = unique(data85056$Periods),
                   multiple = FALSE,
                   selected = "2021"
                 ),
                 selectInput(inputId = "ModesOfTravel",label = "Pick Mode of Travel",
                             choices = unique(data85056$ModesOfTravel),
                             multiple = FALSE,
                             selected = "Total"
                 ),
                 radioButtons("Timeframe", "Pick a timeframe",
                              choiceNames = unique(data85056$Timeframe),
                              choiceValues = unique(data85056$Timeframe)
                 )
               ),
               mainPanel(plotlyOutput("secondtimeframedataplot"))
             )),
    
    "Maps", # second subtitle
    tabPanel("Map", # title of tab
             sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "mapvariable", # for server side interactivity
          label = "Average Distance to...", # text displayed 
          choices = c( # input to choose from
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
      mainPanel(fluidRow(
        verticalLayout(
          girafeOutput("map"),  # girafe is the tooltip package
          plotOutput("mapplot2")
        )))
    )),
    
  )
))