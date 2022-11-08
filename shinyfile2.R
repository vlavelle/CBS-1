library(shiny)
library(DT)
library(tidyverse)

### DATAFILTERED ############################# 
datafiltered <- data709 %>%
  filter (Perioden == "2021JJ00") %>% 
  filter(RegioS == "PV20    " | RegioS == "PV21    " | RegioS == "PV22    ")

### DATAPLOT1 ############################# 
dataplot1 <- datafiltered %>%
  filter(Persoonskenmerken %in% c("51511  ","52020  ","53105  ","53500  ","53705  ","53850  ","53925  ","21600  "))%>% 
  mutate(Persoonskenmerken = case_when(
    Persoonskenmerken =="51511  "~ "Age 6 to 12 years",
    Persoonskenmerken =="52020  "~"Age 12 to 18 years",
    Persoonskenmerken =="53105  "~"Age 18 to 25 years",
    Persoonskenmerken =="53500  "~"Age 25 to 35 years",
    Persoonskenmerken =="53705  "~"Age 35 to 50 years",
    Persoonskenmerken =="53850  "~"Age 50 to 65 years" ,
    Persoonskenmerken =="53925  "~"Age 65 to 75 years",
    Persoonskenmerken =="21600  "~"Age 75 yearsor older"))

### DATAPLOT2 ############################# 
dataplot2 <- datafiltered %>%
  filter(Persoonskenmerken %in% c("1012600", "2012655","2012657")) %>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="1012600","Netherlands"))%>%
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012655","Western"))%>% 
  mutate(Persoonskenmerken = replace(Persoonskenmerken, Persoonskenmerken =="2012657","Not Western"))

### UI START ############################# 
ui <- fluidPage(
  navlistPanel(
    id = "tabset",
    "Heading 1",
    tabPanel("panel 1",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "RegioS",label = "Select region",choices = unique(dataplot1$RegioS),multiple = TRUE,selected = "PV21    "),
                 radioButtons(inputId = "variable",label = "Pick parameter", choiceNames = c("Reisduur_3"), choiceValues = c("Reisduur_3"))),
               mainPanel(plotOutput("plot"), plotOutput("plot2"))
             )),
    "Heading 2",
    tabPanel("panel 2", "Panel two contents"),
    tabPanel("panel 3", "Panel three contents")
  ))

### SERVER ############################# 
    server <- shinyServer(function(input, output) {
      dataused <- reactive(
        dataplot1 %>%
          filter(RegioS == input$RegioS))
      dataused2 <- reactive(
        dataplot2 %>%
          filter(RegioS == input$RegioS))
      output$plot <- renderPlot(
        ggplot(dataplot1, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS")) +
          geom_col(data = dataused(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"), size = 1.5) +
          labs(fill = "RegioS", colour = "RegioS") +
          theme_minimal()
        )
      output$plot2 <- renderPlot(
        ggplot(dataplot2, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS"))+
          geom_col(data = dataused2(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"))) 
    })
    shinyApp(ui, server)
