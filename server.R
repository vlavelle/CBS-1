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
      theme_minimal() + labs(caption = "CBS dataset 84709"))
  
  output$plot2 <- renderPlot(
    ggplot(data84709, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS"))+
      geom_col(data = dataused(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"))+
      theme_minimal() + labs(caption = "CBS dataset 84709"))
  
  output$mapreactive <- renderPlot(
    data_80305() %>%
      ggplot() +
      #geom_sf(aes(color = ligtInProvincieCode)) + (Will have to figure out how to do neat provincial borders)
      geom_sf(aes(fill = DistanceToGPPost_5)) +
      scale_fill_viridis_c() +
      labs(title = "Distance to GP", fill = "", caption = "CBS dataset 80305") +
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
        caption = "Data Source: CBS 84710ENG")
  )
  
  data_8471 <- reactive(data84710  %>% 
                          filter(TravelModes == "Total") %>% 
                          filter(TravelMotives != "Total") %>% 
                          filter(RegionCharacteristics == input$RegionCharacteristics) %>% 
                          group_by(Periods, TravelMotives) %>% 
                          select(TravelMotives, RegionCharacteristics, Periods, DistanceTravelled_5)  %>% 
                          mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>% 
                          select(TravelMotives, RegionCharacteristics, Periods, mean_distance_travelled) %>% 
                          distinct()
  )
  output$lineplot1 <- renderPlot(ggplot(data_8471(), aes(x = Periods, y = mean_distance_travelled, group = TravelMotives, color = TravelMotives)) +
                                   geom_line() + geom_point() + theme_minimal() +
                                   labs(caption = "CBS dataset 84710"))
  
  
  
  data_8472 <- reactive(data84710 %>% 
                          filter(TravelModes != "Total") %>% 
                          filter(TravelMotives == "Total") %>% 
                          filter(RegionCharacteristics == input$RegionCharacteristics) %>% 
                          group_by(Periods, TravelModes) %>% 
                          select(TravelModes, RegionCharacteristics, Periods, DistanceTravelled_5)%>%
                          mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>% 
                          select(TravelModes, RegionCharacteristics, Periods, mean_distance_travelled) %>% 
                          distinct())
  
  output$lineplot2 <- renderPlot(ggplot(data_8472(), aes(x = Periods, y = mean_distance_travelled, group = TravelModes, color = TravelModes)) +
                                   geom_line() + geom_point() + theme_minimal()+
                                   labs(caption = "CBS dataset 84710"))
  
  # Idea 6
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
  
  
})


