source("global.R")

shinyUI(navbarPage(
  "Mobility in the Northern Netherlands",
  tabPanel(
    "Homepage",
    h3("Homepage"),
    hr(),
    fluidRow(
      column(
        3,
        fluidPage(
          h4("Creators"),
          p("The dashboard was created by five students following the Data-Wise
              minor at the University of Groningen. Over the last few months, we have
              spent many hours sourcing data, creating graphs and putting them
              into the dashboard created using Shiny. Creating this dashboard
              provides further insights into mobility in the Northern Netherlands.
              After much work, we are proud of what we have been able to accomplish."),
          hr(),
          h4("CBS"),
          p("Centraal Bureau Voor de Statistiek, Nederland (CBS) was founded in
              1899 in response to a growing demand for statistics. They are an independent
              firm that sources, publishes, and updates statistics in the country which are then
              be used for policy making and government decision-making, but also to inform the
              public."),
          hr(),
        )
      ),
      column(
        9,
        fluidPage(
          h4("This dashboard"),
          tabsetPanel(
            tabPanel(
              "Instructions for use",
              p(" "),
              p("All graphs in our dashboard make use of a combination of ggplot2
                and plotly. Plotly is a data visualisation source that can be used
                for graphs in both R and Python. Using Plotly allows us to add an
                additional layer of interactivity to our graphs with hover tooltips
                and interactive legends. Within our graphs, the following is possible
                  due to plotly:"),
              p(" -  Zoom: use the plotly toolbar to zoom in and out."),
              p(" -  Select: focus on specific data to zoom in on by drawing a
                  rectangle around it."),
              p(" -  Pan: Use the toolbar and select the pan option to move
                  around the graph."),
              p(" -  Interactive legends: click on elements of the legend to
                  remove them or add them back"),
              p(" -  Hover on information: hover above points on our graphs for
                  further insight and more specific data."),
              p(" -  Dynamic scales: our y-axes are dynamic. Zoom in or out and
                  the axis will change with you."),
              p(" -  Easy comparison: for any overlapping or hard to read data
                  points, choose the 'compare on hover' option available in the
                  plotly toolbar. This will let you view the information for all
                  data points at once.")
            ),
            tabPanel(
              "Packages used",
              fluidPage(
                p(" "),
                tableOutput("packagestable")
              )
            ),
            tabPanel(
              "Datasets details",
              fluidPage(
                p(" "),
                p(
                  "Most of the data used during the course of creating the dashboard
                    originates from the",
                  a("CBS Statine website",
                    href = "https://opendata.cbs.nl/statline#/CBS/nl/"
                  ),
                  ", with data used for plotting maps obtained from the",
                  a("Publieke Dienstverlening Op de Kaart (PDOK)",
                    href = "https://service.pdok.nl/kadaster/bestuurlijkegebieden/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson%3B%20subtype%3Dgeojson&typeName=bestuurlijkegebieden:Gemeentegebied"
                  ),
                  ". The datasets directly imported from the CBS website are
                  introduced in the table below (one dataset, electric personal 
                  vehicles, was only available as an ", 
                  a("excel file",
                    href = "https://www.cbs.nl/-/media/_excel/2021/40/elektrische_personenauto_provincie.xlsx"
                  ), ")."
                ),
                tableOutput("datatable")
              )
            )
          )
        )
      ),
      hr()
    )
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
          tabsetPanel(
            tabPanel(
              "Regions in years",
              fluidPage(
                h5("The graph below contains the mean distance travelled per trip from 2018 to 2021 in different Dutch provinces and regions. The provinces are represented as lines with the average distance present on the y-axis. the periods (years) are on the x-axis, and the purpose of travel is available as a drop down."),
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
                h5("The graph below contains the mean distance travelled per trip in different Dutch provinces and the Northern Netherlands. The mean distance per trip is present on the y-axis, with the provinces given as lines. The time frame in which the trips occurred lies on the x-axis and can be chosen by the user, with the options “Departure time”, “Day of the week” and “Month. Users may also choose for which year they want to view the data, and which purpose of travel they are interested in."),
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
          tabsetPanel(
            tabPanel(
              "Regions in years",
              fluidPage(
                h5("The graph below contains the mean distance travelled per trip from 2018 to 2021 in different Dutch provinces and regions. The provinces are represented as lines with the average distance present on the y-axis. the periods (years) are on the x-axis, and the mode of travel is available as a drop down. "),
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
              )
            ),
            tabPanel(
              "Regions in years: different timeframes",
              fluidPage(
                h5("The graph below contains the mean distance travelled per trip in different Dutch provinces and the Northern Netherlands. The mean distance per trip is present on the y-axis, with the provinces given as lines. The time frame in which the trips occurred lies on the x-axis and can be chosen by the user, with the options “Departure time”, “Day of the week” and “Month. Users may also choose for which year they want to view the data, and which mode of travel they are interested in."),
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
            )
          ),
        ),
        "Other Characteristics",
        tabPanel(
          "Personal Characteristics",
          h4("Other Characteristics: Personal"),
          h5("In this graph the average number of trips per person per day is shown. If multiple transport modes are used in one trip, the one in which the furthest distance was covered is used. Hence, a trip is not considered over if the transport mode changes midway. The y-axis shows this average number of trips, while the x-axis shows a characteristic. The regions are represented as bars. There is the option to view a different year, and choose different characteristics to view. Users can also choose a specific mode of transport."),
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
          tabsetPanel(
            tabPanel(
              "Regions in years given license holder age",
              h5("The graph below shows the number of people who have a driver’s license on the y-axis from the year 2014 to 2022 with the years on the x-axis.  The provinces Drenthe, Friesland, and Groningen are visible as lines. Users can choose to view different categories of driving licenses, as well as different age groups."),
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
                )
              )
            ),
            tabPanel(
              "Regions per license holder age given year",
              h5("The bar graph below shows the distribution of the age of driving license holders in the three Northern provinces. Users can choose a year to view, and may also choose a license category. The graph then presents all age groups and the number of license holders per age group, by region."),
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
                  )
                ),
                column(
                  4,
                  selectInput(
                    inputId = "PeriodsLicense",
                    label = "Year",
                    choices = unique(data83488$Periods),
                    selected = "2022",
                    multiple = FALSE
                  )
                )
              )
            )
          ),
          hr()
        )
      )
    )
  ),
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
          h4("Number of vehicles used in regions in years"),
          tabsetPanel(
            tabPanel(
              "In selected region by years",
              fluidPage(
                h5("The time-series graph shows the number of different types of vehicles on the y-axis, and years 2019 to 2022 on the x axis, giving an overview of the increase or decrease in use of certain vehicles. This graph is made from a combination of three datasets, hence the difference in time periods available for electric vehicles versus all other vehicle types."),
                plotlyOutput("vehicles_1"),
                fluidRow(
                  column(
                    3,
                    selectInput(
                      inputId = "Region_combined",
                      label = "Region",
                      choices = unique(datacombined$Region),
                      multiple = FALSE,
                      selected = "Groningen"
                    )
                  )
                ),
                hr()
              )
            ),
            tabPanel(
              "In selected year by region",
              fluidPage(
                h5("This bar graph shows the number of vehicles on the y-axis, grouped by region which shows on the x-axis. Users are able to choose which year to view by the drop down menu."),
                plotlyOutput("vehicles_2"),
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
                ),
                # column(
                #   6,
                #   selectInput(
                #     inputId = "Vehicles_combined2",
                #     label = "Vehicle type",
                #     choices = unique(datacombined$Vehicles),
                #     multiple = TRUE,
                #     selected = "Van"
                #   )
                # )
              )
            )
          )
        ),
        tabPanel(
          "Vehicles and fuels",
          h4("Vehicle (fuel) type used given fuel (vehicle) type"),
          tabsetPanel(
            tabPanel(
              "Vehicle type used given fuel type",
              h5("This graph shows the different vehicle types by fuel type used, where fuel type is selected by the user. Data is only available at a national level, hence regions cannot be selected."),
              plotlyOutput("fuel_1"),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "Fueltype",
                    label = "Fuel type",
                    choices = unique(datafueltypes1$Fueltype),
                    multiple = FALSE,
                    selected = "Benzine"
                  )
                )
              ),
              hr()
            ),
            tabPanel(
              "Fuel type used given vehicle type",
              h5("This graph shows the different fuel types used by vehicles in recent years, where vehicle type is selected by the user. Data is only available at a national level, hence regions cannot be selected. "),
              plotlyOutput("fuel_2"),
              fluidRow(
                column(
                  3,
                  selectInput(
                    inputId = "Vehicletype",
                    label = "Vehicle type",
                    choices = unique(datafueltypes1$Vehicletype),
                    multiple = FALSE,
                    selected = "Van"
                  )
                )
              )
            ),
            hr()
          )
        )
      )
    )
  ),
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
          h5("The following line graph shows the average hourly number of cars on roads on the y-axis, between the years 2011 and 2018 which are shown on the x-axis. Drenthe, Friesland, and Groningen are each represented by a line."),
          plotlyOutput("traffic_intensity_plot")
        ),
        tabPanel(
          "Length of Highways",
          h4("Length of Highways"),
          h5("The choropleth map shows the municipalities in the Northern Netherlands in terms of the kilometers of roads they have. The colour of each municipality is determined by the length of their highways in kilometeres. There is the option to view different years, and different types of highways and roads."),
          plotlyOutput("highway_map"),
          fluidRow(
            column(3, selectInput(
              inputId = "SoortRijbanen",
              label = "Type of Highway",
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
          h5("This bar chart shows the average distance to the nearest facilities, as requested by the user, in kilometers driven by car. There are 15 facilities to choose from, and an additional indicator which averages all 15 facilities."),
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
          h5("The choropleth proximity map shows the distance to the nearest facilities in kilometers driven by car in the municipalities of the Northern Netherlands. Municipalities are coloured based on their distance to the chosen facility. The same 15 facilities and average of all 15 are available as options to be chosen. The Data for both graphs is limited to 2020, due to changes in municialpal borders and incomplete data for more recent years."),
          plotlyOutput("proximity_map")
        )
      ),
      hr()
    )
  ),
))
