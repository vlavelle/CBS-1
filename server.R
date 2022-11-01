library(shiny)
library(cbsodataR)
library(ggplot2)
library(tidyverse)


shinyServer(function(input, output) {
  
  #Import of filtered Data
  dataImport <- cbs_get_data_from_link(
    link = "https://opendata.cbs.nl/statline/#/CBS/en/dataset/84710ENG/table?dl=717D2")
  metadata <- cbs_get_meta("84710ENG") 
  
  ##Changing Keys to Strings##
  #First, temp tables of the needed metadata
  tempPeriods <- metadata$Periods
  tempMotives <- metadata$TravelMotives
  tempModes <- metadata$TravelModes
  tempRegion <- metadata$RegionCharacteristics
  
  #Then, Replace Keys, by matching keys of temp table and imported table
  dataImport$Periods <- tempPeriods$Title[match(dataImport$Periods, tempPeriods$Key)]
  dataImport$TravelMotives <- tempMotives$Title[match(dataImport$TravelMotives, tempMotives$Key)]
  dataImport$TravelModes <- tempModes$Title[match(dataImport$TravelModes, tempModes$Key)]
  dataImport$RegionCharacteristics <- tempRegion$Title[match(dataImport$RegionCharacteristics, tempRegion$Key)]
  
  #Change happens here for interactivity
  data <- reactive(
    dataImport %>%
      filter(RegionCharacteristics == input$region) %>%
      filter(Periods == "2021") %>%
      filter(TravelMotives == "Total") %>%
      filter(TravelModes != "Total")
  )
    
  output$plot <- renderPlot(
    ggplot(data = data(), aes(x = TravelModes, y = Trips_4, fill = TravelModes)) +
      geom_col(data = data()) +
      labs(
        title = "Avg Amount of trips by Travel Mode in 2021 in Northern Netherlands",
        x = "Travel Mode",
        y = "Avg Trips per Person Per Year",
        caption = "Data Source: CBS  84710ENG")
  )
})
