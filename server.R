source("global.R", local = TRUE)


# this is how your server side should look like
shinyServer(function(input, output) {
  dataused <- reactive(
    data84709 %>%
      filter(RegioS == input$RegioS))
  

  data_80305 <- reactive(
    data80305 %>%
      filter(Periods == input$periods)
  )
  

  output$plot <- renderPlot(
    ggplot(data84709, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS")) +
      geom_col(data = dataused(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"), size = 1.5) +
      labs(fill = "RegioS", colour = "RegioS") +
      theme_minimal())
  
  output$plot2 <- renderPlot(
    ggplot(data84709, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS"))+
      geom_col(data = dataused(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"))+
      theme_minimal())
  
  output$mapreactive <- renderPlot(
    data_80305() %>%
      ggplot() +
      #geom_sf(aes(color = ligtInProvincieCode)) + (Will have to figure out how to do neat provincial borders)
      geom_sf(aes(fill = DistanceToGPPost_5)) +
      scale_fill_viridis_c() +
      labs(title = "Distance to GP", fill = "") +
      theme_void()
  )
  # idea 1
  data_84710 <- reactive(
    data84710 %>%
      filter(RegionCharacteristics == input$region) %>%
      filter(Periods == "2021") %>%
      filter(TravelMotives == "Total") %>%
      filter(TravelModes != "Total")
  )
  
  output$plotidea1 <- renderPlot(
    ggplot(data = data_84710(), aes(x = TravelModes, y = Trips_4, fill = TravelModes)) +
      geom_col() +
      labs(
        title = "Avg Amount of trips by Travel Mode in 2021 in Northern Netherlands",
        x = "Travel Mode",
        y = "Avg Trips per Person Per Year",
        caption = "Data Source: CBS 84710ENG") +
      theme_void()
  )
  
  
})


