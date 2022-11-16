library(shiny)
library(cbsodataR)
library(ggplot2)
library(tidyverse)

metadata85055 <- cbs_get_meta("85055ENG")

data85055 <- cbs_get_data(
  id = "85055ENG", 
  TripCharacteristics = c(
    "2031090", "2031100", "2031110", "2031120", "2031130", "2031140", "2031150", 
    "2031160", "2031170", "2031180", "2031190", "2031200", "2031210", "2031220", 
    "2031230", "2031240", "2031250", "2031260", "2031270"
  ), 
  Population = "A048710", 
  TravelPurposes = "2030170", 
  Margins = "MW00000",     
  RegionCharacteristics = has_substring("NL") | 
    c("LD01    ",     "PV20    ", "PV21    ", "PV22    ")
)

#####Data Prep1#####
#temp tables
temp_Region85055 <- metadata85055$RegionCharacteristics
temp_Periods85055 <- metadata85055$Periods
temp_TripCharacteristics85055 <- metadata85055$TripCharacteristics
temp_TravelPurposes85055 <- metadata85055$TravelPurposes
temp_Population85055 <- metadata85055$Population

#Matching and replacing Keys for Keys
data85055$RegionCharacteristics <- temp_Region85055$Title[
  match(data85055$RegionCharacteristics, temp_Region85055$Key)
]
data85055$Periods <- temp_Periods85055$Title[
  match(data85055$Periods, temp_Periods85055$Key)
]
data85055$TripCharacteristics <- temp_TripCharacteristics85055$Title[
  match(data85055$TripCharacteristics, temp_TripCharacteristics85055$Key)
]
data85055$TravelPurposes <- temp_TravelPurposes85055$Title[
  match(data85055$TravelPurposes, temp_TravelPurposes85055$Key)
]
data85055$Population <- temp_Population85055$Title[
  match(data85055$Population, temp_Population85055$Key)
]

###UI#85055#Idea6#################################################
ui85055 <-shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
        plotOutput("plot85055") #I would like to have a second plot on this page
      )
    )
  )
)
###Server#85055###############################################
server85055 <- function(input, output, session) {
  data_85055 <- data85055 %>%
    filter(Periods == "2019") %>%
    filter(TripCharacteristics %in% c('Monday','Tuesday','Wednesday','Thursday','Friday')) %>%
    mutate(TripCharacteristics = fct_relevel(TripCharacteristics, 'Monday','Tuesday','Wednesday','Thursday','Friday' ))
  
  
  output$plot85055 <- renderPlot( #need to fix the colors and theme
    ggplot(data_85055, aes(
      x = TripCharacteristics, 
      y = AverageTravelTimePerTrip_2, 
      group = RegionCharacteristics,
      color = RegionCharacteristics)) +
      geom_line(size = 1) +
      geom_point() +
      ylim(20,36) +
      labs(title = "Daily Commutes in the Northern Netherlands",
           x = " ",
           y = "Average commuting time in minutes",
           caption = "Data Source: CBS  85055ENG") 
  )
  #I was thinking to have a second plot here showing the travel time per month
  #That Data is also included in the import
  
}

###Run#Idea#1######################################
shinyApp(ui = ui85055, server = server85055)
