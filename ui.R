source("global.R")


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
                             selected = "Noord-Nederland (LD)") # define default
               ),
               mainPanel(
                 plotOutput("plotidea1")
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
            plotlyOutput("timeframedataplot"),
            hr(),
            fluidRow(
               column(3,
               sliderInput(
                 inputId = "Periods1",
                 label = "Select Year",
                 min = 2018,
                 max = 2021,
                 value = 2021,
                 sep = "",
                 ticks = FALSE
               )),
              column(3,
               selectInput(inputId = "TravelPurposes",label = "Pick Purpose of Travel",
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
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   inputId = "Periods2",
                   label = "Select Year",
                   min = 2018, max = 2021, sep = "", value = 2021, ticks = FALSE
                 ),
                 selectInput(inputId = "ModesOfTravel",label = "Pick Mode of Travel",
                             choices = unique(data85056$ModesOfTravel),
                             multiple = FALSE,
                             selected = "Total"
                 ),
                 radioButtons("Timeframe2", "Pick a timeframe",
                              choiceNames = c("Month", "Departure time", "Day of the week"),
                              choiceValues = c("Month", "Departure time", "Day of the week")
                 )
               ),
               mainPanel(plotlyOutput("secondtimeframedataplot"))
             )),
    tabPanel("What is this", # title of tab
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
             ))
  ))),
  tabPanel("Green Mobility"),
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
    mainPanel(fluidRow(
      verticalLayout(
        girafeOutput("map"),  # girafe is the tooltip package
        plotOutput("mapplot2")
      )))))),
  navbarMenu("About us/data",
             tabPanel("Datasets origin and details"),
             tabPanel("Creators"))
))
