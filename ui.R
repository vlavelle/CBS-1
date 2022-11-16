source("global.R")

shinyUI(fluidPage(
  navlistPanel(
    id = "tabset",
    "Ideas",
    tabPanel("idea2",
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
    "Maps",
    tabPanel("panel 2", sidebarLayout(
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
    tabPanel("panel 3", sidebarLayout(
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
  )
))