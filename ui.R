library(shiny)
library(cbsodataR)
library(ggplot2)
library(tidyverse)

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "region", # essential! give the input a name that you will need on the server side
                    label = "Select region", # text displayed in your app
                    choices = unique(dataImport$RegionCharacteristics), # input to choose from, here all gapminder countries
                    selected = "Noord-Nederland (LD)") # define default
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  )
)