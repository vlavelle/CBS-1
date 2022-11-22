source("global.R")

shinyUI(fluidPage(
  navlistPanel(
    id = "tabset",
    "Ideas",
    tabPanel("Bar Graph",
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
    
    tabPanel("Bar Graph", sidebarLayout(
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
    tabPanel("Line graph", sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "RegionCharacteristics",
          label = "Select region",
          choices = unique(data84710$RegionCharacteristics),
          multiple = FALSE,
          selected = "Noord-Nederland (LD)"
        )
      ),
      mainPanel(plotOutput("lineplot1"), plotOutput("lineplot2"))
    )),
    
    tabPanel("Line graph Idea 6", sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
        plotOutput("plot85055") #I would like to have a second plot on this page
      )
    )),
    
    "Maps",
    tabPanel("Map", sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "periods",
          # for server side interactivity
          label = "Select Year",
          # text displayed
          choices = unique(data80305$Periods),
          # input to choose from
          selected = "2020"
        ) # This is the most current year with data
      ),
      mainPanel(plotOutput("mapreactive"))
    )),
    
  )
))