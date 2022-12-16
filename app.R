library(shiny)
library(ggplot2)
library(cbsodataR)
library(dplyr)
library(readxl)
library(sf)

## IMPORTANT: make sure the excel file elektrische_personen_auto_2 is saved 
# in the same location as where you download this file
# then set the working directory
# session -> set working directory -> choose directory -> find the file location on your pc

# FOR GLOBAL FILE
## DRIVERS LICENSE SHINY APP

data83488 <- cbs_get_data('83488ENG')
metadata83488 <- cbs_get_meta("83488ENG")
data83488 <- data83488 %>%
  filter(Region == "LD01  "|Region == "PV20  "| Region == "PV21  "| Region == "PV22  ")

# Dataprep drivers license  
tempCategoryDrivingLicense83488 <- metadata83488$DrivingLicence
tempAgeDrivingLicenseHolder83488 <- metadata83488$AgeDrivingLicenseHolder
tempRegion83488 <- metadata83488$Region
tempPeriods83488 <- metadata83488$Periods
data83488$CategoryDrivingLicence <- tempCategoryDrivingLicense83488$Title[match(data83488$CategoryDrivingLicence, tempCategoryDrivingLicense83488$Key)]
data83488$AgeDrivingLicenseHolder <- tempAgeDrivingLicenseHolder83488$Title[match(data83488$AgeDrivingLicenseHolder, tempAgeDrivingLicenseHolder83488$Key)]
data83488$Region <- tempRegion83488$Title[match(data83488$Region, tempRegion83488$Key)]
data83488$Periods <- tempPeriods83488$Title[match(data83488$Periods, tempPeriods83488$Key)]

# VERKEERSINTENSITEIT # this data import can be made more efficient
data83712 <- cbs_get_data("83712NED") 
data83712 <- data83712 %>% 
  filter(RegioS == "PV20"| RegioS == "LD01"| RegioS == "PV21"| RegioS == "PV22") %>%
  filter(Perioden == "2011JJ00"|Perioden == "2012JJ00"|Perioden == "2013JJ00"|Perioden == "2014JJ00"|Perioden == "2015JJ00"|Perioden == "2016JJ00"|Perioden == "2017JJ00"|Perioden == "2018JJ00")

metadata83712 <- cbs_get_meta("83712NED") 

tempRegioS83712 <- metadata83712$RegioS
tempPerioden83712 <- metadata83712$Perioden
data83712$Regios <- tempRegioS83712$Title[match(data83712$RegioS, tempRegioS83712$Key)]
data83712$Perioden <- tempPerioden83712$Title[match(data83712$Perioden, tempPerioden83712$Key)]

colnames(data83712)[2] = "Year"
colnames(data83712)[3] = "Roadintensity"
colnames(data83712)[1] = "Region"

# VOERTUIGEN SHINY APP
# Data import is via excel sheet, no meta_data available.  
elektrische_personenauto_provincie_2_ <- read_excel("elektrische_personenauto_provincie (2).xlsx", 
                                                    sheet = "Tabel 1")
data_elek_2 <- elektrische_personenauto_provincie_2_ %>%
  filter(Regio== "Friesland"|Regio== "Groningen"|Regio== "Drenthe")
colnames(data_elek_2)[2] = "Year"
colnames(data_elek_2)[3] = "Count"
colnames(data_elek_2)[1] = "Region"

# VOERTUIGEN 2 SHINY
metadata85240 <- cbs_get_meta("85240NED")
data85240 <- cbs_get_data("85240NED") 
data85240 <- data85240 %>%
  filter(Provincie == "PV20  "| Provincie == "PV21  "| Provincie == "PV22  ") %>%
  filter(TenaamstellingEnLeeftijdParticulier == "T001191") %>%
  filter(Bouwjaar == "T001378")

tempYears85240 <- metadata85240$Years
tempProvince85240 <- metadata85240$RegioS
data85240$Years <- tempYears85240$Title[match(data85240$Years, tempYears85240$Key)]
data85240$Province <- tempProvince85240$Title[match(data85240$Province, tempProvince85240$Key)]


dataVoertuigenmetbromfietskenteken <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, VoertuigenMetBromfietskenteken_1) %>% 
  mutate(Voertuigtype = "Voertuig met bromfietskenteken") %>% 
  rename(Count = VoertuigenMetBromfietskenteken_1)

dataSnorfiets <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, Snorfiets_2) %>% 
  mutate(Voertuigtype = "Snorfiets") %>% 
  rename(Count = Snorfiets_2)

dataBrommobiel <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, Bromfiets_3) %>% 
  mutate(Voertuigtype = "Bromfiets") %>% 
  rename(Count = Bromfiets_3)

dataElektrischebrommobiel <- data85240 %>%
  select(Perioden, Bouwjaar, Provincie, OverigeVoertuigenMetBromfietskenteken_5) %>% 
  mutate(Voertuigtype = "Overige voertuigen met bromfietskenteken") %>% 
  rename(Count = OverigeVoertuigenMetBromfietskenteken_5)


#Dataprep for combined lineplot
data85240new <- bind_rows(dataVoertuigenmetbromfietskenteken, dataSnorfiets, dataBrommobiel, dataElektrischebrommobiel)
data85240new <- data85240new %>% group_by(Perioden, Voertuigtype) %>% select(Perioden, Provincie, Count, Voertuigtype)

colnames(data85240new)[1] = "Years"
colnames(data85240new)[2] = "Province"
colnames(data85240new)[4] = "Vehicles"




ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    "Ideas", # subtitle of overall dashboard
    tabPanel("Driving licenses",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Region1",
                             label = "Region1",
                             choices = unique(data83488$Region), # What to do here, because you use a different column right?
                             multiple = FALSE,
                             selected = "Groningen"),
                 selectInput(inputId = "AgeDrivingLicenseHolder1",
                             label = "AgeDrivingLicenseHolder1",
                             choices = unique(data83488$AgeDrivingLicenseHolder), # What to do here, because you use a different column right?
                             multiple = FALSE,
                             selected = "10000"),
                 selectInput(inputId = "AgeDrivingLicenseHolder2",
                             label = "AgeDrivingLicenseHolder2",
                             choices = unique(data83488$AgeDrivingLicenseHolder), # What to do here, because you use a different column right?
                             multiple = FALSE,
                             selected = "Total"),
                 selectInput(inputId = "Region2",
                             label = "Region2",
                             choices = unique(data83488$Region), # What to do here, because you use a different column right?
                             multiple = FALSE,
                             selected = "Groningen (PV)")
               ),
               mainPanel(
                 plotOutput("plotidea11"),plotOutput("plotidea11.1"), plotOutput("plotidea11.2"), plotOutput("plotidea11.3")
               )
             )),
    tabPanel("Verkeersintensiteit", # title of tab
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Region1", # Specify a better name
                             label = "Region1",
                             choices = unique(data83712$Region),  
                             multiple = FALSE,
                             selected = "Groningen"),
                 selectInput(inputId = "Region2", # Specify a better name
                             label = "Region2",
                             choices = unique(data83712$Region),  
                             multiple = TRUE,    # Choose multiple Regions
                             selected = "Groningen")
               ),
               mainPanel(
                 plotOutput("plotidea10"), plotOutput("plotidea10.1"), plotOutput("plotidea10.2"), plotOutput("plotidea10.3")
               )
             )),
    tabPanel("Voertuigen", # title of tab
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Region1", #Specify a better name
                             label = "Region1",
                             choices = unique(data_elek_2$Region),  
                             multiple = FALSE,
                             selected = "Groningen"),
                 selectInput(inputId = "Region2", #Specify a better name
                             label = "Region2",
                             choices = unique(data_elek_2$Region),  
                             multiple = TRUE,    #Choose multiple Regionns
                             selected = "Groningen")
               ),
               mainPanel(
                 plotOutput("plotidea7"), plotOutput("plotidea7.1"), plotOutput("plotidea7.2"), plotOutput("plotidea7.3")
               )
             )),
    tabPanel("Voertuigen 2", # title of tab
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Province1",
                             label = "Province1",
                             choices = unique(data85240new$Province),  # Is this oke?
                             multiple = FALSE,
                             selected = "Groningen"),
                 selectInput(inputId = "Vehicles1",
                             label = "Vehicles1",
                             choices = unique(data85240new$Vehicles),  # Is this oke?
                             multiple = TRUE,
                             selected = "Bromfiets"),
                 selectInput(inputId = "Vehicles2",
                             label = "Vehicles2",
                             choices = unique(data85240new$Vehicles),  # Is this oke?
                             multiple = FALSE,
                             selected = "Bromfiets"),
                 selectInput(inputId = "Provinces2",
                             label = "Provinces2",
                             choices = unique(data85240new$Province),  # Is this oke?
                             multiple = TRUE,
                             selected = "Groningen")
               ),
               mainPanel(
                 plotOutput("plotidea9"), plotOutput("plotidea9.1"), plotOutput("plotidea9.2"), plotOutput("plotidea9.3")
               )
             ))
    
  )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  dataDrivingLicense1 <- reactive(
    data83488%>%
      filter(Region == input$Region1) %>%
      filter(AgeDrivingLicenseHolder == input$AgeDrivingLicenseHolder1)
  )
  dataDrivingLicense2 <- reactive(
    data83488 %>%
      filter(Region == input$Region2) %>%
      filter(AgeDrivingLicenseHolder == input$AgeDrivingLicenseHolder2)
  )
  output$plotidea11 <- renderPlot(
    ggplot(dataDrivingLicense1(), aes(x= Periods, y = PeopleWithADrivingLicence_1, group = AgeDrivingLicenseHolder, colour= factor(AgeDrivingLicenseHolder))) + 
      geom_point() +
      geom_line() +
      ylab("People with a driving license") + # Specific Region
      xlab("Periods")
    
  )
  output$plotidea11.1 <- renderPlot(
    ggplot(dataDrivingLicense1(), aes(x= Periods, y = PeopleWithADrivingLicence_1, group = 1 )) + 
      geom_col() +
      ylab("People with a driving license") + # Specific Region
      xlab("Periods") # Region--> Input$Region?
    
  )
  
  output$plotidea11.2 <- renderPlot(
    ggplot(dataDrivingLicense2(),aes(x = Periods, y = PeopleWithADrivingLicence_1, colour = factor(Region), group = Region)) +
      geom_point() +
      geom_line() +
      ylab("People with a driving license") +
      xlab("Periods")
  )
  
  output$plotidea11.3 <- renderPlot(
    ggplot(dataDrivingLicense2(), aes(x= Periods, y = PeopleWithADrivingLicence_1, fill = AgeDrivingLicenseHolder)) + 
      geom_bar(stat = "summary", fun = "median") +
      ylab("median value")
  )
  
## VERKEERSINTENSITEIT
  data_83712_1 <- reactive(
    data83712 %>%
      filter(Region == input$Region1)
  )
  data_83712_2 <- reactive(
    data83712 %>%
      filter(Region == input$Region2)
  )
  output$plotidea10 <- renderPlot(
    ggplot(data_83712_1(), aes(x= Year, y = Roadintensity, group = 1)) + 
      geom_point() +
      geom_line() +
      ylab("Road intensity in Netherlands") + #Specific Regionn
      xlab("Year")
    
  )
  output$plotidea10.1 <- renderPlot(
    ggplot(data_83712_1(), aes(x= Year, y = Roadintensity)) + 
      geom_col() +
      ylab("Road intensity in Netherlands")  #Region--> Input$Region?
    
  )
  
  output$plotidea10.2 <- renderPlot(
    ggplot(data_83712_2(), aes(x = Year, y = Roadintensity, colour = factor(Region), group = Region)) +
      geom_point() +
      geom_line() +
      ylab("Road intensity")
  )
  
  output$plotidea10.3 <- renderPlot(
    ggplot(data_83712_2(), aes(x= Year, y = Roadintensity)) + #, fill = Region)) + 
      geom_col() +
      ylab("Road Intensity")
  )
  
  ## VOERTUIGEN 
  dataVoertuigen <- reactive(
    data_elek_2 %>%
      filter(Region == input$Region1)
  )
  dataVoertuigen2 <- reactive(
    data_elek_2 %>%
      filter(Region == input$Region2)
  )
  output$plotidea7 <- renderPlot(
    ggplot(dataVoertuigen(), aes(x= Year, y = Count, group = 1
    )) + 
      geom_point() +
      geom_line() +
      ylab("Number of electric vehicles in ") + #Specific Regionn
      xlab("Year")
    
  )
  output$plotidea7.1 <- renderPlot(
    ggplot(dataVoertuigen(), aes(x= Year, y = Count)) + 
      geom_col() +
      ylab("Number of electric vehicles in ....")  #Region--> Input$Region?
    
  )
  
  output$plotidea7.2 <- renderPlot(
    ggplot(dataVoertuigen2(),aes(x = Year, y = Count, color = Region)) +
      geom_point() +
      geom_line() +
      ylab("Number of electric vehicles in Nothern Netherlands")
  )
  
  output$plotidea7.3 <- renderPlot(
    ggplot(dataVoertuigen2(), aes(x= Year, y = Count, fill = Region)) + 
      geom_bar(stat = "summary", fun = "median") +
      ylab("median value")
  )
  #Nog even naar kijken
  
  ## VOERTUIGEN 2
  dataVoertuigen2.1 <- reactive(
    data85240new %>%
      filter(Province == input$Province1) %>%
      filter(Vehicles == input$Vehicles1)
  )
  
  dataVoertuigen2.2 <- reactive(
    data85240new %>%
      filter(Vehicles == input$Vehicles2) %>%
      filter(Province == input$Province2)
  )
  output$plotidea9 <- renderPlot(
    ggplot(dataVoertuigen2.1() , aes(x = Years, y = Count, colour = factor(Vehicles), group = Vehicles)) +
      geom_point() +
      geom_line()
    
  )
  output$plotidea9.1 <- renderPlot(
    ggplot(dataVoertuigen2.1(), aes(x= Years, y = Count, fill = Vehicles)) + 
      geom_col()
  )
  
  output$plotidea9.2 <- renderPlot(
    ggplot(dataVoertuigen2.2(),aes(x=Years, y= Count, colour = factor(Province), group = Province)) +
      geom_point() +
      geom_line()
  )
  
  output$plotidea9.3 <- renderPlot(
    ggplot(dataVoertuigen2.2(), aes(x= Years, y = Count, fill = Province)) + 
      geom_bar(stat = "summary", fun = "median") +
      ylab("median value") +
      scale_y_continuous(labels = scales::comma)
  )

}

# Run the application 
shinyApp(ui = ui, server = server)
