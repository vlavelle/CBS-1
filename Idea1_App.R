library(shiny)
library(cbsodataR)
library(ggplot2)
library(tidyverse)

###Data1##################################################
#should be in global file
#It gives an error, but the data seems to be complete, so no clue why.
metadata1 <- cbs_get_meta("84710ENG") 

dataImport1 <- cbs_get_data(
  id = "84710ENG",
  Periods = has_substring("JJ"),
  RegionCharacteristics = c("NL01    ", "LD01    ", "PV20    ",
                            "PV21    ", "PV22    "),
  select = c("TravelMotives", "Population", "TravelModes",
             "RegionCharacteristics", "Periods", "Trips_4", 
             "DistanceTravelled_5")
)


#####Data Prep1#####
#First, we have to make temp tables from the Metadata, these include the
#Key and Title column. We will use those to "match" and replace the Keys
#with Titles in our Dataset.
tempPeriods1 <- metadata1$Periods
tempMotives1 <- metadata1$TravelMotives
tempModes1 <- metadata1$TravelModes
tempRegion1 <- metadata1$RegionCharacteristics

##These bits do the matching and replacing!
#Periods
dataImport1$Periods <- tempPeriods1$Title[
  match(dataImport1$Periods, tempPeriods1$Key)
]
#TravelMotives
dataImport1$TravelMotives <- tempMotives1$Title[
  match(dataImport1$TravelMotives, tempMotives1$Key)
]
#TravelModes
dataImport1$TravelModes <- tempModes1$Title[
  match(dataImport1$TravelModes, tempModes1$Key)
]
#RegionCharacteristics
dataImport1$RegionCharacteristics <- tempRegion1$Title[
  match(dataImport1$RegionCharacteristics, tempRegion1$Key)
]

###UI#IDEA#1##################################################
ui1 <-shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "region", #for server side
                    label = "Select region", # text displayed 
                    choices = unique(dataImport1$RegionCharacteristics), # input to choose from
                    selected = "Noord-Nederland (LD)") # define default
      ),
      mainPanel(
        plotOutput("plot1")
      )
    )
  )
)
###Server################################################
server1 <- function(input, output, session) {
  #Change happens here for interactivity
  data1 <- reactive(
    dataImport1 %>%
      filter(RegionCharacteristics == input$region) %>%
      filter(Periods == "2021") %>%
      filter(TravelMotives == "Total") %>%
      filter(TravelModes != "Total")
  )
  
  output$plot1 <- renderPlot(
    ggplot(data = data1(), aes(x = TravelModes, y = Trips_4, fill = TravelModes)) +
      geom_col() +
      labs(
        title = "Avg Amount of trips by Travel Mode in 2021 in Northern Netherlands",
        x = "Travel Mode",
        y = "Avg Trips per Person Per Year",
        caption = "Data Source: CBS  84710ENG") +
      theme_void()
  )
}

###Run#Idea#1######################################
shinyApp(ui = ui1, server = server1)
