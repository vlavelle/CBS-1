source("global.R", local = TRUE)


shinyServer(function(input, output) {
  data_84709 <- reactive(data84709 %>% 
                           filter(Perioden == input$Perioden_graph1) %>% 
                           filter(Feature == input$Features) %>% 
                           filter(Vervoerwijzen == input$Vervoerwijzen_graph1) )
  
  output$Persoonskenmerken <- renderPlotly({
    Persoonskenmerken_plot <- ggplot(data_84709(), 
                                     aes(x = Persoonskenmerken, 
                                         y = Verplaatsingen_1, 
                                         fill = RegioS,
                                         text1 = Perioden,
                                         text2 = Feature,
                                         text3 = Vervoerwijzen)) +
      geom_col(position = position_dodge()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    ggplotly(Persoonskenmerken_plot, tooltip = c("text1", "text2", "text3"))
  })
  
  data_80305 <- reactive(
    data80305 %>%
      filter(Periods == input$periods)
  )
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
        caption = "Source: CBS 80305ENG and Dutch National Georegistry",
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
  data_85055 <- reactive(data85055 %>% filter(TravelPurposes == input$TravelPurposes) %>% 
                           filter(Periods == input$Periods1) %>% 
                           filter(Timeframe == input$Timeframe1))
  
  output$timeframedataplot <- renderPlotly({data85055plot <- ggplot(
    data_85055(),
    aes(
      x =  factor(TripCharacteristics, levels = unique(TripCharacteristics)),
      y = AverageDistanceTravelledPerTrip_1,
      group = interaction(RegionCharacteristics, TravelPurposes),
      colour = RegionCharacteristics,
      text1 = RegionCharacteristics,
      text2 = TripCharacteristics,
      text3 = AverageDistanceTravelledPerTrip_1
    )
  ) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Time frame",
      y = "Average Distance Travelled Per Trip",
      caption = "CBS 85055",
      colour = "Region:"
    )
  data85055plotly <- ggplotly(data85055plot, tooltip = c("text1", "text2", "text3"))
  data85055plotly
  })
  
  data_85056 <- reactive(
    data85056 %>% filter(ModesOfTravel == input$ModesOfTravel) %>%
      filter(Periods == input$Periods2) %>%
      filter(Timeframe == input$Timeframe2))
  
  
  output$secondtimeframedataplot <- renderPlotly({data85056plot <- ggplot(
    data_85056(),
    aes(
      x =  factor(TripCharacteristics, levels = unique(TripCharacteristics)),
      y = AverageDistanceTravelledPerTrip_1,
      group = interaction(RegionCharacteristics, ModesOfTravel),
      colour = RegionCharacteristics,
      text1 = RegionCharacteristics,
      text2 = TripCharacteristics,
      text3 = AverageDistanceTravelledPerTrip_1)
  ) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      x = "Time frame",
      y = "Average Distance Travelled Per Trip",
      caption = "CBS 85055",
      colour = "Region:"
    )
  data85056plotly <- ggplotly(data85056plot, tooltip = c("text1", "text2", "text3"))
  data85056plotly
  }
  )
  
}
)


  



