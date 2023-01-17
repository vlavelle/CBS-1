source("global.R")

shinyUI(navbarPage(
  "Mobility in the Northern Netherlands",
  navbarMenu(
    "Home",
    tabPanel("About the Data",
             h3("About the Data")
             ),
    tabPanel("About the Team",
             h3("About the Data"))
  ),
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
                      label = "Select Purpose of Travel",
                      choices = unique(data84710$TravelMotives),
                      multiple = FALSE,
                      selected = "Total"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Regions in years: different timeframes",
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
                    radioButtons(
                      "Timeframe1",
                      "Pick a timeframe",
                      choiceNames = c("Departure time", "Day of the week", "Month"),
                      choiceValues = c("Departure time", "Day of the week", "Month")
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
                    label = "Select a Mode of Travel",
                    choices = unique(data84710$TravelModes),
                    multiple = FALSE,
                    selected = "Total"
                  )
                )
              )
            ),
            tabPanel(
              "Regions in years: different timeframes",
              plotlyOutput("secondtimeframedataplot"),
              hr(),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "ModesOfTravel",
                    label = "Select a Mode of Travel",
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
                  radioButtons(
                    "Timeframe2",
                    "Pick a timeframe",
                    choiceNames = c("Departure time", "Day of the week", "Month"),
                    choiceValues = c("Departure time", "Day of the week", "Month")
                  )
                )
              )
            )
          ),
        ),
        # tabPanel(
        ############        # "do we want this one??",
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
                inputId = "Transport_graph1",
                label = "Mode of Transport",
                choices = unique(data84709$Transport),
                multiple = FALSE,
                selected = "Total"
              )
            )
          )
        ),
        tabPanel(
          "Driving licenses",
          h4("Other Characteristics: Driving Licences"),
          h5("some accompanying text about dataset(s)"),
          
          tabsetPanel(
            tabPanel(
              "Regions in years",
              plotlyOutput("DrivingLicense1"),
              fluidRow(
                column(
                  5,
                  selectInput(
                    inputId = "LicenseCategory",
                    label = "License Category",
                    choices = unique(data83488$CategoryDrivingLicence),
                    multiple = FALSE,
                    selected = "Total"
                  )),
                column(
                  3,
                  selectInput(
                    inputId = "LicenseHolderAge",
                    label = "License Holder Age",
                    choices = unique(data83488$AgeDrivingLicenseHolder),
                    multiple = FALSE,
                    selected = "Total"
                  )))),
            tabPanel(
              "Regions in years",
              plotlyOutput("DrivingLicense2"),
              fluidRow(
                column(
                  5,
                  selectInput(
                    inputId = "LicenseCategory2",
                    label = "License Category",
                    choices = unique(data83488$CategoryDrivingLicence),
                    multiple = FALSE,
                    selected = "Total"
                  )),
                column(
                  4,
                  selectInput(
                    inputId = "PeriodsLicense",
                    label = "Year",
                    choices = unique(data83488$Periods),
                    selected = "2022",
                    multiple = FALSE
                  ))))),
          hr())))),
  tabPanel(
    "Green Mobility",
    h3("Indicators of 'Green Mobility' Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(
      navlistPanel(
        widths = c(3, 9),
        id = "tabset",
        tabPanel(
          "Vehicles and regions",
          h4("aaaaa"),
          h5("some accompanying text about dataset(s)"),
          fluidRow(
            column(
              9,
              selectInput(
                inputId = "Vehicles_combined",
                label = "Vehicles",
                choices = unique(datacombined$Vehicles),
                multiple = TRUE,
                selected = "Van"
              ))
          ),
          tabsetPanel(
            tabPanel(
              "In selected region per years",
              fluidPage(
                plotlyOutput("plotidea12"),
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
                  )
                ),
                #plotlyOutput("plotidea12.1"),
                hr()
              )
            ),
            tabPanel(
              "In selected year per each region",
              fluidPage(
                plotlyOutput("plotidea12.2"),
                hr(),
                fluidRow(
                  column(
                    6,
                    selectInput(
                      inputId = "Years_combined",
                      label = "Year",
                      choices = unique(datacombined$Years),
                      multiple = FALSE,
                      selected = "2021"
                    )
                  )
                )
              )
            )
          )),
        tabPanel(
          "Vehicles and fuels",
          h4("Vehicle (fuel) type used, given fuel (vehicle) type"),
          h5("some accompanying text about dataset(s)"),
          tabsetPanel(
            tabPanel(
              "Vehicle type used, given fuel type",
              plotlyOutput("plotidea13"),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "Fueltype",
                    label = "Fuel type",
                    choices = unique(datafueltypes1$Fueltype), # What to do here, because you use a different column right?
                    multiple = FALSE,
                    selected = "Benzine"
                  )
                )),
              hr()
            ),
            tabPanel(
              "Fuel type used, given vehicle type",
              plotlyOutput("plotidea13.1"),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "Vehicletype",
                    label = "Vehicle type",
                    choices = unique(datafueltypes1$Vehicletype), # What to do here, because you use a different column right?
                    multiple = FALSE,
                    selected = "Van"
                  )))),
            hr())
        )))),
  tabPanel(
    "Traffic/Infrastructure",
    h3("Traffic and Built Infrastructure Across Different Regions Within the Northern Netherlands"),
    hr(),
    fluidPage(
      navlistPanel(
        widths = c(3, 9),
        id = "tabset",
        tabPanel(
          "Traffic Intensity",
          h4("Traffic Intensity"),
          h5("some accompanying text about dataset(s)"),
          plotlyOutput("traffic_intensity_plot")
        ),
        tabPanel(
          "Length of Highways",
          h4("Length of Highways"),
          h5("some accompanying text about dataset(s)"),
          plotlyOutput("highway_map"),
          fluidRow(
            column(3, selectInput(
              inputId = "SoortRijbanen", # for server side interactivity
              label = "Type of Highway", # text displayed
              choices = unique(data70806$SoortRijbanen),
              selected = "Totale weglengte"
            )),
            column(3, selectInput(
              inputId = "Years_highways",
              label = "Year",
              choices = unique(data70806$Perioden),
              selected = "2021"
            ))
          )
        )
      ),
      hr()
    )
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
          plotlyOutput("proximityplot"),
          fluidRow(
            column(
              12,
              selectInput(
                inputId = "proxmapvariable",
                label = "Average Distance to:",
                choices = unique(mapDataproximity$name),
                selected = "Large Supermarket"
              )
            )
          )
        ),
        column(
          8,
          h5("some accompanying text about dataset(s)"),
          plotlyOutput("proximity_map")
        )
      ),
      hr()
    )
  ),
))
