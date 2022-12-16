source("global.R", local = TRUE)


shinyServer(function(input, output) {
  data_84709 <- reactive(
    data84709 %>%
      filter(RegioS == input$RegioS))
  

  data_80305 <- reactive(
    data80305 %>%
      filter(Periods == input$periods)
  )
  

  output$plot <- renderPlot(
    ggplot(data84709, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS")) +
      geom_col(data = data_84709(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"), size = 1.5) +
      labs(fill = "RegioS", colour = "RegioS") +
      theme_minimal() + labs(caption = "CBS dataset 84709"))
  
  output$plot2 <- renderPlot(
    ggplot(data84709, aes_string("Persoonskenmerken", input$variable, group = "RegioS", fill = "RegioS"))+
      geom_col(data = data_84709(), aes_string("Persoonskenmerken", input$variable, group = "RegioS", colour = "RegioS"))+
      theme_minimal() + labs(caption = "CBS dataset 84709"))
  
  
  ## Map data
  MapData1 <- reactive({mapData}) # all the data/municipal
  MapData2 <- provincialBoundaries # only provinical boundaries and names
  PlotData <- reactive({data80305 %>% filter(Regions == c("PV20  " ,"PV21  ", "PV22  "))})
  
  
  output$map <- renderGirafe({ # map is made first, then called within girafe function to create the output
    map <- ggplot() +
      geom_sf_interactive(            # This is the Municipal polygons
        data = MapData1(),
        aes_string(
          geometry = "geometry",
          fill = input$mapvariable,
          tooltip = "Municipality" # It only takes one argument, tried to glue, but aes_string is a pain.
        )
      ) +
      scale_fill_gradient( # This is for the map colours.
        low = "#B3EFFF", 
        high = "#1C304A"
      ) +
      geom_sf(            # This is the Provincial polygons
        data = MapData2,  
        aes(
          color = provinces,
          geometry = geometry
        ),
        alpha = 0,
        size = 1.2,
        show.legend = FALSE
      ) +
      labs(
        title = " ", 
        caption = "Source: CBS Statistics Netherlands (80305ENG) and Dutch National Georegistry",
        fill = "Average Distance in km") +
      scale_colour_manual( # this is the colours for the provincial boundaries
        values = c(
          "grey20","grey20", "grey20"
        )
      ) + 
      theme_void() + 
      theme(
        legend.title = element_text( 
          size=10, 
          face = "bold"),
        legend.key.width = unit(
          2, "cm"      # this might have to be adjusted to work  with the rest.
        ),
        legend.position = "bottom"
      )
    
    girafe(ggobj = map)
    
  }
  )
  
  output$mapplot2 <- renderPlot({ 
    PlotData()  %>%
      ggplot(aes(x = Municipality, fill = Regions))+
      geom_col(aes_string(y = input$mapvariable), show.legend = FALSE) +
      labs(
        tite = "Average distance by Province",
        x= "Provinces",
        y="Average Distance in km",
        caption = "Source: CBS 80305ENG"
      ) +
      theme_minimal()
  }
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
  
  data84710_1 <- reactive(data84710 %>% filter(TravelModes == "Total") %>%
                            filter(TravelMotives == input$TravelMotives) %>%
                            group_by(Periods, TravelMotives, RegionCharacteristics) %>%
                            select(TravelMotives,
                                   RegionCharacteristics,
                                   Periods,
                                   DistanceTravelled_5)  %>%
                            mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
                            select(TravelMotives,
                                   RegionCharacteristics,
                                   Periods,
                                   mean_distance_travelled) %>%
                            distinct())
  
  output$lineplottravelmotives <- renderPlot(ggplot(
    data84710_1(),
    aes(
      x =  Periods,
      y = mean_distance_travelled,
      group = interaction(RegionCharacteristics, TravelMotives),
      colour = RegionCharacteristics
    )
  ) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(caption = "CBS 84710",
         colour = "Region:"))
  
  
  data84710_2 <- reactive(data84710 %>% 
                            filter(TravelMotives == "Total") %>%
                            filter(TravelModes == input$TravelModes) %>%
                            group_by(Periods, TravelModes, RegionCharacteristics) %>%
                            select(TravelModes,
                                   RegionCharacteristics,
                                   Periods,
                                   DistanceTravelled_5)  %>%
                            mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
                            select(TravelModes,
                                   RegionCharacteristics,
                                   Periods,
                                   mean_distance_travelled) %>%
                            distinct())
  
  output$lineplottravelmodes <- renderPlot(ggplot(
    data84710_2(),
    aes(
      x =  Periods,
      y = mean_distance_travelled,
      group = interaction(RegionCharacteristics, TravelModes),
      colour = RegionCharacteristics
    )
  ) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(caption = "CBS 84710",
         colour = "Region:"))
  
  # Idea 6
  
  data85055_1 <- data85055 %>% mutate(
    Timeframe = case_when(
      grepl("Distance:", data85055$TripCharacteristics) ~ "Distance",
      grepl("Time travelled:", data85055$TripCharacteristics) ~ "Time travelled",
      grepl("Trip in", data85055$TripCharacteristics) ~ "Month",
      grepl("Departure time:", data85055$TripCharacteristics) ~ "Departure time",
      grepl("day", data85055$TripCharacteristics) ~ "Day of the week"
    )
  ) %>% select(TripCharacteristics, Timeframe, TravelPurposes, RegionCharacteristics, Periods, AverageDistanceTravelledPerTrip_1, AverageTravelTimePerTrip_2)  
  data_85055_1 <- reactive(data85055_1 %>% filter(TravelPurposes == input$TravelPurposes) %>% 
                          filter(Periods == input$Periods) %>% 
                          filter(Timeframe == input$Timeframe))
  
  output$timeframedataplot <- renderPlot(ggplot(data_85055_1(), 
                                                aes(x =  TripCharacteristics, 
                                                    y = AverageDistanceTravelledPerTrip_1, 
                                                    group = interaction(RegionCharacteristics, TravelPurposes), 
                                                    colour = RegionCharacteristics)) +
                                           geom_line() +
                                           geom_point() +
                                           theme_minimal() +
                                           theme(axis.text.x.bottom = element_text(angle = 45, hjust = 1),
                                                 legend.position = "bottom") +
                                           labs(x = "Time frame", 
                                                y = "Average Distance Travelled Per Trip", 
                                                caption = "CBS 85055", 
                                                colour = "Region:")
  )
  
})


