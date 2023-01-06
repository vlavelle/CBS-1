source("global.R", local = TRUE)

##### Mobility Indicators Tab
### Modes per Region
#Data
#Plot
### Motives & Modes per Region
#Data 1
#Plot 1
#Data 2
#Plot 2
### Timeframe Data: Travel Purpose
#Data
#Plot
#Plotly
### Timeframe Data: Travel Mode
#Data
#Plot
#Plotly
### Personal Characteristics
#Data
#Plot

##### Green Mobility Tab

##### Traffic Intensity Tab

##### Proximity to Amenities Tab
#Data
#Map
#Plot


shinyServer(function(input, output) {
  
  # define a vector for the colours for the Region (colourblind safe)
  # Not yet implemented
  regioncolours <- c("The Netherlands"="#E7298A", "Northern Netherlands"="#7570B3", 
                     "Groningen"="#1B9E77", "Drenthe"="#D95F02", "Friesland"="#E6AB02")
  regioncolours_prox <- c("Groningen "="#1B9E77", "Drenthe"="#D95F02", "Friesland"="#E6AB02")
  
  
  
  #Data - Not sure where the plot for this one is
  data_80305 <- reactive({
    data80305 %>%
      filter(Periods == input$periods)
  })
  
  ###### Mobility Indicators Tab
  ### Modes per Region
  # Data
  data_84710 <- reactive({
    data84710 %>%
      filter(RegionCharacteristics == input$region) %>%
      filter(Periods == "2021") %>%
      filter(TravelMotives == "Total") %>% #only value, no confidence interval
      filter(TravelModes != "Total") %>%
      select(TravelMotives,
             TravelModes,
             RegionCharacteristics,
             Periods,
             Trips_4)
  })
  
  #Plot 1
  output$plotidea1 <- renderPlotly({
    modesperregionplot <- ggplot(
      data = data_84710(), 
      aes(
        x = TravelModes, 
        y = Trips_4, #yearly avg
        fill = TravelModes,
        text1 = Trips_4 #yearly avg
      )
    ) +
      geom_col() +
      theme_minimal() +
      labs(
        title = "Average Yearly Trips in 2021",
        x = "Travel Mode",
        y = "Avg Trips per Person Per Year",
        caption = "Data Source: CBS 84710ENG",
        fill = "Travel Mode:"
      )
    modesperregionplotly <- ggplotly(modesperregionplot, tooltip = c("text1"))
    modesperregionplotly
  })
  
  
  
  ### Motives & Modes per regions
  #Data 1
  data84710_1 <- reactive({
    data84710 %>% 
      filter(TravelModes == "Total") %>%
      filter(TravelMotives == input$TravelMotives) %>%
      group_by(Periods, TravelMotives, RegionCharacteristics) %>%
      select(TravelMotives, RegionCharacteristics, Periods, DistanceTravelled_5) %>%
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelMotives, RegionCharacteristics, Periods, mean_distance_travelled) %>%
      distinct()
  })
  
  #Plot 1
  
  output$lineplottravelmotives <- renderPlotly({
    lineplot_1 <- ggplot(
      data = data84710_1(), 
      aes(
        x =  Periods, 
        y = mean_distance_travelled,
        group = interaction(RegionCharacteristics, TravelMotives),
        colour = RegionCharacteristics
      )
    ) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = regioncolours) + #for unified colours
      theme_minimal() +
      labs(
        y = "Mean Distance Travelled",
        caption = "Data Source: CBS 84710",
        colour = "Region:"
      )
    lineplotly_1 <- ggplotly(lineplot_1, tooltip = c("text1"))
    lineplotly_1
  })
  
  #Data 2
  data84710_2 <- reactive({
    data84710 %>% 
      filter(TravelMotives == "Total") %>%
      filter(TravelModes == input$TravelModes) %>%
      group_by(Periods, TravelModes, RegionCharacteristics) %>%
      select(TravelModes, RegionCharacteristics, Periods, DistanceTravelled_5)  %>%
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelModes, RegionCharacteristics, Periods, mean_distance_travelled) %>%
      distinct()
  })
  
  #Plot 2
  output$lineplottravelmodes <- renderPlotly({
    lineplot_2 <- ggplot(
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
      scale_colour_manual(values = regioncolours) + #for unified colours
      theme_minimal() +
      labs(
        y = "Mean Distance Travelled",
        caption = "Data Source: CBS 84710",
        colour = "Region:")
    
    lineplotly_2 <- ggplotly(lineplot_2, tooltip = c("text1"))
    lineplotly_2
  })
  
  ###Timeframe Data: Travel Purpose
  #Data
  data_85055 <- reactive({
    data85055 %>% 
      filter(TravelPurposes == input$TravelPurposes) %>% 
      filter(Periods == input$Periods1) %>% 
      filter(Timeframe == input$Timeframe1)
  })
  
  #Plot
  output$timeframedataplot <- renderPlotly({
    data85055plot <- ggplot(
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
      scale_colour_manual(values = regioncolours) + #for unified colours
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Time frame",
        y = "Average Distance Travelled Per Trip",
        caption = "Data Source: CBS 85055",
        colour = "Region:"
      )
    #Plotly
    data85055plotly <- ggplotly(data85055plot, tooltip = c("text1", "text2", "text3"))
    data85055plotly
  })
  
  ###Timeframe Data: Travel Mode
  #Data
  data_85056 <- reactive({
    data85056 %>% 
      filter(ModesOfTravel == input$ModesOfTravel) %>%
      filter(Periods == input$Periods2) %>%
      filter(Timeframe == input$Timeframe2)
  })
  
  #Plot
  output$secondtimeframedataplot <- renderPlotly({
    data85056plot <- ggplot(
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
      scale_colour_manual(values = regioncolours) + #for unified colours
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Time frame",
        y = "Average Distance Travelled Per Trip",
        caption = "Data Source: CBS 85055",
        colour = "Region:"
      )
    
    #Plotly
    data85056plotly <- ggplotly(data85056plot, tooltip = c("text1", "text2", "text3"))
    data85056plotly
  }
  )
  
  ###Personal Characteristics
  #Data
  data_84709 <- reactive({
    data84709 %>%
      filter(Perioden == input$Perioden_graph1) %>% 
      filter(Feature == input$Features) %>% 
      filter(Vervoerwijzen == input$Vervoerwijzen_graph1) 
  })
  
  #Plot
  output$Persoonskenmerken <- renderPlotly({
    Persoonskenmerken_plot <- ggplot(
      data_84709(),
      aes(
        x = Persoonskenmerken, 
        y = Verplaatsingen_1, 
        fill = RegioS,
        text1 = Perioden,
        text2 = Feature,
        text3 = Vervoerwijzen)
    ) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      labs(
        caption = "Data Source: CBS 84709",
        colour = "Region:")
    
    ggplotly(Persoonskenmerken_plot, tooltip = c("text1", "text2", "text3"))
  })
  
  #####Green Mobility Tab 
  #Data
  
  #Plot
  
  
  ###Traffic Intensity Tab
  #Data
  
  #Plot
  
  
  #####Proximity to Amenities Tab 
  #Data
  MapData1 <- reactive({mapData}) # all the data/municipal
  MapData2 <- provincialBoundaries # only provinical boundaries and names
  PlotData <- reactive({
    data80305 %>% 
      filter(Regions == c("PV20  " ,"PV21  ", "PV22  "))
  })
  
  #Map
  output$map <- renderGirafe({ # map is made first, then called within girafe function to create the output
    map <- ggplot() +
      geom_sf_interactive(  # This is the Municipal polygons
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
  })
  
  #Plot
  output$mapplot <- renderPlot({ 
    PlotData()  %>%
      ggplot(aes(
        x = Municipality, 
        fill = Municipality)
      )+
      geom_col(aes_string(y = input$mapvariable),
               width = 0.5,
               show.legend = FALSE) +
      labs(
        title = "Average distance by Province",
        x= "Provinces",
        y="Average Distance in km",
        caption = "Data Source: CBS 80305ENG"
      ) +
      scale_fill_manual(values = regioncolours_prox) + #for unified colours
      theme_minimal()
  })
  
  # Driving license
  dataDrivingLicense1 <- reactive(
    data83488%>%
      filter(AgeDrivingLicenseHolder == input$LicenseHolderAge) %>% 
      filter(CategoryDrivingLicence == input$LicenseCategory) %>%
      group_by(Periods, Region)
  )
  
  output$DrivingLicense1 <- renderPlotly({
    DrivingLicense1 <- ggplot(dataDrivingLicense1(), 
                              aes(x= Periods, 
                                  y = PeopleWithADrivingLicence_1, 
                                  group = interaction(Region, AgeDrivingLicenseHolder),
                                  colour = Region)) + 
      geom_point() +
      geom_line(aes(group = interaction(Region, AgeDrivingLicenseHolder))
      ) + 
      theme_minimal() +
      labs(caption = "CBS 83488",
           x = "Years",
           y = "Number of people with driver's licenses")
    ggplotly(DrivingLicense1)
    
  })
  
  dataDrivingLicense2 <- reactive(data83488 %>% 
                                    filter(Periods == input$PeriodsLicense) %>% 
                                    filter(CategoryDrivingLicence == input$LicenseCategory) %>% 
                                    filter(AgeDrivingLicenseHolder != "Total"))
  
  output$DrivingLicense2 <- renderPlotly({
    DrivingLicense2 <- ggplot(dataDrivingLicense2(), aes(x= AgeDrivingLicenseHolder, y = PeopleWithADrivingLicence_1, fill = Region )) + 
      geom_col(position = position_dodge()) +
      ylab("People with a driving license") + 
      xlab("Periods") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(DrivingLicense2)
  }  
  )
  
  # Traffic Intensity
  data_83712 <- reactive(data83712 %>% 
                           filter(Years == input$Years_traffic))
  
  output$traffic_barplot <- renderPlotly({
    trafficintensity <- ggplot(data_83712(), 
                               aes(x = provinces, 
                                   y = VerkeersintensiteitenRijkswegen_1, 
                                   fill = provinces)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Province",
           y = "Hourly Average Number of Cars on Highways", 
           caption = "source: CBS 83712")
      ylim(0, 1270)
    
    ggplotly(trafficintensity)
  })
  
  # Length of highways
  Highway <- mapDatarijbanen$Weglengte_1
  Region <- mapDatarijbanen$naam
  data70806 <- data70806 %>% filter(SoortRijbanen == "Totale weglengte") # option for interactivity
  
  output$highways <- renderPlotly({
    maprijbaan <- ggplot(mapDatarijbanen, aes(
    fill = Highway,
    text = Region)) +
    geom_sf(            # This is the Municipal polygons
      data = mapDatarijbanen,
      aes_string(
        geometry = "geometry")
    ) +
    scale_fill_gradient( # This is for the map colours.
      low = "white", 
      high = "red",
      lim = c(0, 1400)
    )  +
    labs(
      title = "Lengte ", 
      caption = "Source: CBS Statistics Netherlands () and Dutch National Georegistry") +
    scale_colour_manual( # this is the colours for the provincial boundaries
      values = c(
        "grey20","grey20", "grey20"
      )
    ) + 
    theme_void() + 
    theme(legend.title = element_blank(),
          legend.key.width = unit(
            2, "cm"      # this might have to be adjusted to work  with the rest.
          ),
          legend.position = "bottom"
    ) 
  ggplotly(maprijbaan) %>% style(hoveron = c("text"))
  })
  
})