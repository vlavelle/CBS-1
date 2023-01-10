source("global.R")

shinyUI(navbarPage(
  "Mobility in the Northern Netherlands",
  tabPanel(
    "Mobility Indicators",
    h3("Indicators of Mobility Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(
      navlistPanel(
        widths = c(3, 9),
        id = "tabset",
        "Travel Characteristics",
        tabPanel(
          "Purpose of travel",
          h4("Travel Characteristics: Purpose of Travel"),
          h5("some accompanying text about dataset(s)"),
          tabsetPanel(
            tabPanel(
              "Regions in years",
              fluidPage(
                plotlyOutput("lineplottravelmotives"),
                hr(),
                fluidRow(
                  column(
                    3,
                    selectInput(
                      inputId = "TravelMotives",
                      label = "Select Purpose for Travel",
                      choices = unique(data84710$TravelMotives),
                      multiple = FALSE,
                      selected = "Total"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Regions in years: for a period, in a timeframe",
              fluidPage(
                plotlyOutput("timeframedataplot"),
                hr(),
                fluidRow(
                  column(
                    3,
                    selectInput(
                      inputId = "TravelPurposes",
                      label = "Select Purpose of Travel",
                      choices = unique(data85055$TravelPurposes),
                      multiple = FALSE,
                      selected = "Total"
                    )
                  ),
                  column(
                    3,
                    selectInput(
                      inputId = "Periods1",
                      label = "Select Year",
                      choices = unique(data85055$Periods),
                      selected = "2021",
                      multiple = FALSE
                    )
                  ),
                  column(
                    3,
                    radioButtons("Timeframe1", "Pick a timeframe",
                                 choiceNames = c("Month", "Departure time", "Day of the week"),
                                 choiceValues = c("Month", "Departure time", "Day of the week")
                    )
                  )
                )
              )
            )
          ),
        ),
        tabPanel(
          "Mode of travel",
          h4("Travel Characteristics: Mode of Travel"),
          h5("some accompanying text about dataset(s)"),
          tabsetPanel(
            tabPanel(
              "Regions in years",
              plotlyOutput("lineplottravelmodes"),
              hr(),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "TravelModes",
                    label = "Select Mode of Travel",
                    choices = unique(data84710$TravelModes),
                    multiple = FALSE,
                    selected = "Total"
                  )
                )
              )
            ),
            tabPanel(
              "Regions in years: for a period, in a timeframe",
              plotlyOutput("secondtimeframedataplot"),
              hr(),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "ModesOfTravel",
                    label = "Select Mode of Travel",
                    choices = unique(data85056$ModesOfTravel),
                    multiple = FALSE,
                    selected = "Total"
                  )
                ),
                column(
                  3,
                  selectInput(
                    inputId = "Periods2",
                    label = "Select Year",
                    choices = unique(data85056$Periods),
                    selected = "2021",
                    multiple = FALSE
                  )
                ),
                column(
                  3,
                  radioButtons("Timeframe2", "Pick a timeframe",
                               choiceNames = c("Month", "Departure time", "Day of the week"),
                               choiceValues = c("Month", "Departure time", "Day of the week")
                  )
                )
              )
            )
          ),
        ),
        # tabPanel(
        # "do we want this one??",
        # plotlyOutput("plotidea1"),
        # hr(),
        # fluidRow(
        # column(
        # 3,
        # selectInput(
        # inputId = "region",
        # label = "Select region",
        # choices = unique(data84710$RegionCharacteristics),
        # selected = "Northern Netherlands"
        # )
        # )
        # )
        "Other Characteristics",
        tabPanel(
          "Personal Characteristics",
          h4("Other Characteristics: Personal"),
          h5("some accompanying text about dataset(s)"),
          plotlyOutput("Persoonskenmerken"),
          hr(),
          fluidRow(
            column(
              3,
              selectInput(
                inputId = "Perioden_graph1",
                label = "Years",
                choices = unique(data84709$Perioden),
                multiple = FALSE,
                selected = "2021"
              )
            ),
            column(
              3,
              selectInput(
                inputId = "Features",
                label = "Feature",
                choices = unique(data84709$Feature),
                multiple = FALSE,
                selected = "All"
              )
            ),
            column(
              3,
              selectInput(
                inputId = "Vervoerwijzen_graph1",
                label = "Vervoerwijzen",
                choices = unique(data84709$Vervoerwijzen),
                multiple = FALSE,
                selected = "Totaal"
              )
            )
          )
        ),
        tabPanel(
          "Driving licenses",
          h4("Other Characteristics: Driving Licences"),
          h5("some accompanying text about dataset(s)"),
          plotlyOutput("DrivingLicense1"),
          hr(),
          fluidRow(
            column(
              3,
              selectInput(
                inputId = "PeriodsLicense",
                label = "Year",
                choices = unique(data83488$Periods),
                selected = "2022",
                multiple = FALSE
              )
            ),
            column(
              3,
              selectInput(
                inputId = "LicenseHolderAge",
                label = "License Holder Age",
                choices = unique(data83488$AgeDrivingLicenseHolder),
                multiple = FALSE,
                selected = "Total"
              )
            ),
            column(
              6,
              selectInput(
                inputId = "LicenseCategory",
                label = "License Category",
                choices = unique(data83488$CategoryDrivingLicence),
                multiple = FALSE,
                selected = "Total"
              )
            )
          ),
          plotlyOutput("DrivingLicense2")
        )
      )
    ),
    hr()
  ),
  tabPanel(
    "Green Mobility",
    h3("Indicators of 'Green Mobility' Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(tabPanel(
      "Vehicle Types",
      h4("Types of Vehicles"),
      h5("some accompanying text about dataset(s)"),
      hr(),
      fluidRow(
        column(
          3,
          selectInput(
            inputId = "Region_combined",
            label = "Region",
            choices = unique(datacombined$Region), # What to do here, because you use a different column right?
            multiple = FALSE,
            selected = "Groningen"
          )
        ),
        column(
          3,
          selectInput(
            inputId = "Vehicles_combined",
            label = "Vehicles",
            choices = unique(datacombined$Vehicles), # What to do here, because you use a different column right?
            multiple = TRUE,
            selected = "Van"
          )
        ),
        column(
          6,
          selectInput(
            inputId = "Years_combined",
            label = "Years_combined",
            choices = unique(datacombined$Years), # What to do here, because you use a different column right?
            multiple = FALSE,
            selected = "2021"
          )
        )
      ),
      plotlyOutput("plotidea12"), 
      plotlyOutput("plotidea12.1"),
      plotlyOutput("plotidea12.2")
      
    ))
  ),
  tabPanel(
    "Traffic/Infrastructure",
    h3("Traffic and Built Infrastructure Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(fluidRow(
      column(
        4,
        h4("Traffic Intensity"),
        h5("some accompanying text about dataset(s)"),
        plotlyOutput("traffic_barplot"),
        fluidRow(
          column(
            3,
            selectInput("Years_traffic", "Year",
                        choices = unique(data83712$Years),
                        multiple = FALSE
            )
          )
        )
      ),
      column(
        1
      ),
      column(
        7,
        h4("Length of Highways"),
        h5("some accompanying text about dataset(s)"),
        plotlyOutput("highways")
      )
    )),
    hr()
  ),
  tabPanel(
    "Proximity to Amenities",
    h3("Proximity to Amenities Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(
      fluidRow(
        column(
          4,
          h5("some accompanying text about dataset(s)"),
          plotOutput("mapplot"),
          fluidRow(
            column(
              12,
              selectInput(
                inputId = "mapvariable",
                label = "Average Distance to...",
                choices = c(
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
            )
          )
        ),
        column(
          8,
          h5("some accompanying text about dataset(s)"),
          girafeOutput("map")
        )
      ),
      hr()
    )
  ),
  navbarMenu(
    "About us/data",
    tabPanel("Datasets origin and details"),
    tabPanel("Creators")
  )
))