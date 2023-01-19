source("global.R", local = TRUE)

##### Mobility Indicators Tab
### Modes per Region
# Data
# Plot
### Motives & Modes per Region
# Data 1
# Plot 1
# Data 2
# Plot 2
### Timeframe Data: Travel Purpose
# Data
# Plot
# Plotly
### Timeframe Data: Travel Mode
# Data
# Plot
# Plotly
### Personal Characteristics
# Data
# Plot

##### Green Mobility Tab

##### Traffic Intensity Tab

##### Proximity to Amenities Tab
# Data
# Map
# Plot


shinyServer(function(input, output) {
  
  # define a vector for the colours for the Region (colourblind safe)
  # Not yet implemented
  regioncolours <-
    c(
      "The Netherlands" = "#E7298A",
      "Northern Netherlands" = "#7570B3",
      "Groningen" = "#1B9E77",
      "Drenthe" = "#D95F02",
      "Friesland" = "#E6AB02"
    )
  regioncolours_prox <-
    c(
      "Groningen " = "#1B9E77",
      "Drenthe" = "#D95F02",
      "Friesland" = "#E6AB02"
    )
  
  
  
  # Data - Not sure where the plot for this one is
  data_80305 <- reactive({
    data80305 %>%
      filter(Periods == input$periods)
  })
  
  ##### Mobility Indicators Tab  #####
  ### Modes per Region
  # Data
  data_84710 <- reactive({
    data84710 %>%
      filter(RegionCharacteristics == input$region) %>%
      filter(Periods == "2021") %>%
      filter(TravelMotives == "Total") %>% # only value, no confidence interval
      filter(TravelModes != "Total") %>%
      select(
        TravelMotives,
        TravelModes,
        RegionCharacteristics,
        Periods,
        Trips_4
      ) %>%
      mutate(tooltip_text = paste(TravelModes, "\n", "Region: ", RegionCharacteristics, "\n", Trips_4))
  })
  
  # Plot 1
  output$plotidea1 <- renderPlotly({
    modesperregionplot <- ggplot(
      data = data_84710(),
      aes(
        x = TravelModes,
        y = Trips_4, # yearly avg
        fill = TravelModes,
        text = tooltip_text # yearly avg
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
    
    ggplotly(modesperregionplot, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 84710", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  
  ### Motives & Modes per regions
  # Data 1
  data84710_1 <- reactive({
    data84710 %>%
      filter(TravelModes == "Total") %>%
      filter(TravelMotives == input$TravelMotives) %>%
      group_by(Periods, TravelMotives, RegionCharacteristics) %>%
      select(TravelMotives, RegionCharacteristics, Periods, DistanceTravelled_5) %>%
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelMotives, RegionCharacteristics, Periods, mean_distance_travelled) %>%
      distinct() %>%
      mutate(tooltip_text = paste0("Region: ", RegionCharacteristics, "\n", "Distance: ", mean_distance_travelled))
  })
  
  # Plot 1
  output$lineplottravelmotives <- renderPlotly({
    lineplot_1 <- ggplot(
      data = data84710_1(),
      aes(
        x = Periods,
        y = mean_distance_travelled,
        group = interaction(RegionCharacteristics, TravelMotives),
        colour = RegionCharacteristics,
        text = tooltip_text
      )
    ) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = regioncolours) + # for unified colours
      theme_minimal() +
      labs(
        y = "Mean Distance Travelled",
        caption = "Data Source: CBS 84710",
        colour = "Region:"
      )
    
    ggplotly(lineplot_1, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 84710", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  # Data 2
  data84710_2 <- reactive({
    data84710 %>%
      filter(TravelMotives == "Total") %>%
      filter(TravelModes == input$TravelModes) %>%
      group_by(Periods, TravelModes, RegionCharacteristics) %>%
      select(TravelModes, RegionCharacteristics, Periods, DistanceTravelled_5) %>%
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelModes, RegionCharacteristics, Periods, mean_distance_travelled) %>%
      distinct() %>%
      mutate(tooltip_text = paste0("Region: ", RegionCharacteristics, "\n", "Distance:", mean_distance_travelled))
  })
  
  # Plot 2
  output$lineplottravelmodes <- renderPlotly({
    lineplot_2 <- ggplot(
      data84710_2(),
      aes(
        x = Periods,
        y = mean_distance_travelled,
        group = interaction(RegionCharacteristics, TravelModes),
        colour = RegionCharacteristics,
        text = tooltip_text
      )
    ) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = regioncolours) + # for unified colours
      theme_minimal() +
      labs(
        y = "Mean Distance Travelled",
        caption = "Data Source: CBS 84710",
        colour = "Region:"
      )
    
    ggplotly(lineplot_2, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 84710", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  ### Timeframe Data: Travel Purpose
  # Data
  data_85055 <- reactive({
    data85055 %>%
      filter(TravelPurposes == input$TravelPurposes) %>%
      filter(Periods == input$Periods1) %>%
      filter(Timeframe == input$Timeframe1) %>%
      mutate(tooltip_text = paste0(
        "Region: ", RegionCharacteristics, "\n",
        "Avg Distance: ", AverageDistanceTravelledPerTrip_1, "\n",
        TripCharacteristics
      ))
  })
  
  # Plot
  output$timeframedataplot <- renderPlotly({
    data85055plot <- ggplot(
      data_85055(),
      aes(
        x = factor(TripCharacteristics, levels = unique(TripCharacteristics)),
        y = AverageDistanceTravelledPerTrip_1,
        group = interaction(RegionCharacteristics, TravelPurposes),
        colour = RegionCharacteristics,
        text = tooltip_text
      )
    ) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = regioncolours) + # for unified colours
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Time frame",
        y = "Average Distance Travelled Per Trip",
        caption = "Data Source: CBS 85055",
        colour = "Region:"
      )
    # Plotly
    ggplotly(data85055plot, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 85055", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  ### Timeframe Data: Travel Mode
  # Data
  data_85056 <- reactive({
    data85056 %>%
      filter(ModesOfTravel == input$ModesOfTravel) %>%
      filter(Periods == input$Periods2) %>%
      filter(Timeframe == input$Timeframe2) %>%
      mutate(tooltip_text = paste0(
        "Region: ", RegionCharacteristics, "\n",
        "Avg Distance: ", AverageDistanceTravelledPerTrip_1, "\n",
        TripCharacteristics
      ))
  })
  
  # Plot
  output$secondtimeframedataplot <- renderPlotly({
    data85056plot <- ggplot(
      data_85056(),
      aes(
        x = factor(TripCharacteristics, levels = unique(TripCharacteristics)),
        y = AverageDistanceTravelledPerTrip_1,
        group = interaction(RegionCharacteristics, ModesOfTravel),
        colour = RegionCharacteristics,
        text = tooltip_text
      )
    ) +
      geom_point() +
      geom_line() +
      scale_colour_manual(values = regioncolours) + # for unified colours
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = "Time frame",
        y = "Average Distance Travelled Per Trip",
        caption = "Data Source: CBS 85055",
        colour = "Region:"
      )
    
    # Plotly
    ggplotly(data85056plot, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 85056", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  ### Personal Characteristics
  # Data
  data_84709 <- reactive({
    data84709 %>%
      filter(Perioden == input$Perioden_graph1) %>%
      filter(Feature == input$Features) %>%
      filter(Transport == input$Transport_graph1) %>%
      mutate(tooltip_text = paste0(
        "Feature: ", Feature, "\n",
        "Transport Mode: ", Transport, "\n",
        "Region: ", RegioS
      ))
  })
  
  # Plot
  output$Persoonskenmerken <- renderPlotly({
    Persoonskenmerken_plot <- ggplot(
      data_84709(),
      aes(
        x = Personal_Characteristics,
        y = Verplaatsingen_1,
        fill = RegioS,
        text = tooltip_text
      )
    ) +
      geom_col(position = position_dodge()) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
      labs(
        x = "Characteristic",
        y = "Average number of trips per day, per person",
        caption = "Data Source: CBS 84709",
        fill = "Region:"
      ) +
      scale_fill_manual(values = regioncolours)
    
    ggplotly(Persoonskenmerken_plot, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 84709", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
  })
  
  
  # Driving license
  dataDrivingLicense1 <- reactive(
    data83488 %>%
      filter(AgeDrivingLicenseHolder == input$LicenseHolderAge) %>%
      filter(CategoryDrivingLicence == input$LicenseCategory) %>%
      group_by(Periods, Region) %>%
      mutate(tooltip_text = paste0(
        "Count: ", PeopleWithADrivingLicence_1, "\n",
        "Region: ", Region, "\n"
      ))
  )
  
  output$DrivingLicense1 <- renderPlotly({
    DrivingLicense1 <- ggplot(
      dataDrivingLicense1(),
      aes(
        x = Periods,
        y = PeopleWithADrivingLicence_1,
        group = interaction(Region, AgeDrivingLicenseHolder),
        colour = Region,
        text = tooltip_text
      )
    ) +
      geom_point() +
      geom_line(aes(group = interaction(Region, AgeDrivingLicenseHolder))) +
      theme_minimal() +
      scale_colour_manual(values = regioncolours) +
      labs(
        caption = "CBS 83488",
        x = "Years",
        y = "Number of people with driver's licenses"
      )
    
    ggplotly(DrivingLicense1, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 83488", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  
  dataDrivingLicense2 <- reactive(data83488 %>%
                                    filter(Periods == input$PeriodsLicense) %>%
                                    filter(CategoryDrivingLicence == input$LicenseCategory2) %>%
                                    filter(AgeDrivingLicenseHolder != "Total") %>%
                                    mutate(tooltip_text = paste0(
                                      "Count: ", PeopleWithADrivingLicence_1, "\n",
                                      "Region: ", Region, "\n",
                                      "Category: ", CategoryDrivingLicence
                                    )))
  
  output$DrivingLicense2 <- renderPlotly({
    DrivingLicense2 <- ggplot(
      dataDrivingLicense2(),
      aes(
        x = AgeDrivingLicenseHolder,
        y = PeopleWithADrivingLicence_1,
        fill = Region,
        text = tooltip_text
      )
    ) +
      geom_col(position = position_dodge()) +
      ylab("People with a driving license") +
      xlab("Periods") +
      theme_minimal() +
      scale_fill_manual(values = regioncolours) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(DrivingLicense2, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 83488", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  
  
  ##### Green Mobility Tab  #####
  ## VEHICLES
  # Data
  data_vehicles <- reactive(
    datacombined %>%
      filter(Region == input$Region_combined) %>%
      group_by(Vehicles, Years)
  )
  data_vehicles_2 <- reactive(
    datacombined %>%
      filter(Years == input$Years_combined) 
  )
  # Plot
  output$plotidea12 <- renderPlotly({
    plot12 <- ggplot(
      data_vehicles(),
      aes(
        x = Years,
        y = Count,
        group = factor(Vehicles),
        colour = factor(Vehicles)
      )
    ) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      ylim(0, 50000) +
      ylab("Number of vehicles") +
      xlab("Years") 
    
    ggplotly(plot12, dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = -0.2, 
                    text = paste0("CBS: 85237, 85240,", "\n", "Electric Personal Vehicles"), 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  # Plot
  # has no tooltip as of yet
  # I think this graph is not in the dashboard anymore
  # output$plotidea12.1 <- renderPlotly({
  #   plot12.1 <-
  #     ggplot(data_vehicles(), aes(x = Years, y = Count, fill = Vehicles)) +
  #     geom_col(width = 0.8) +
  #     ylab("Number of vehicles") + # Specific Region
  #     xlab("Years") +
  #     theme_minimal()
  #   
  #   ggplotly(plot12.1) %>% 
  #     layout(annotations = # adds caption to plot
  #              list(x = 1.2, y = -0.18, 
  #                   text = paste0("85237, 85240,", "\n", "Electric Personal Vehicles"), 
  #                   showarrow = F, 
  #                   # sets the x and y id to the proportional to the edge of the graph:
  #                   xref = 'paper', 
  #                   yref = 'paper', 
  #                   font = list(size = 12)))
  # })
  
  # Plot
  output$plotidea12.2 <- renderPlotly({
      plot12.2 <-
        ggplot(datacombined %>%
                 filter(Years == "2021") , aes(x = Region, y = Count, fill = Vehicles)) +
        geom_bar(stat ="identity", position = "stack", width = 0.5) +
        ylab("Number of vehicles") + # Specific Region
        xlab("Years") +
        theme_minimal()
      
      plotly12.2 <- ggplotly(plot12.2, dynamicTicks = TRUE) %>%
        layout(
          annotations = # adds caption to plot
            list(
              x = 1.2,
              y = -0.15,
              text = paste0("CBS: 85237, 85240,", "\n", "Electric Personal Vehicles"),
              showarrow = F,
              # sets the x and y id to the proportional to the edge of the graph:
              xref = 'paper',
              yref = 'paper',
              font = list(size = 12)
            )
        )
      for (i in 1:length(plotly12.2$x$data)) {
        plotly12.2$x$data[[i]]$base <- NULL
        temporary <- plotly12.2$x$data[[i]]
        plotly12.2$x$data[[i]] <- plotly12.2$x$data[[length(plotly12.2$x$data) - i + 1]]
        plotly12.2$x$data[[length(plotly12.2$x$data) - i + 1]] <- temporary
      }
      plotly12.2
      
    })
  ## FUEL TYPE
  # Data
  dataFueltypes2 <- reactive(
    datafueltypes1 %>%
      filter(Fueltype == input$Fueltype) %>%
      mutate(tooltip_fuel = paste0(
        "Count: ", Count, "\n",
        "Vehicle Type: ", Vehicletype, "\n",
        "Fuel Type: ", Fueltype
      ))
  )
  dataFueltypes3 <- reactive(
    datafueltypes1 %>%
      filter(Vehicletype == input$Vehicletype) %>%
      mutate(tooltip_fuel = paste0(
        "Count: ", Count, "\n",
        "Vehicle Type: ", Vehicletype, "\n",
        "Fuel Type: ", Fueltype
      ))
  )
  # Plot
  output$plotidea13 <- renderPlotly({
    plot1 <-
      ggplot(
        dataFueltypes2(),
        aes(
          x = Years,
          y = Count,
          colour = factor(Vehicletype),
          group = Vehicletype,
          text = tooltip_fuel
        )
      ) + # Possible to use different Y-indicators?
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(y = "Number of vehicles",
           x = "Years",
           colour = "Vehicle type")
    
    ggplotly(plot1, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 85239", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  # Plot
  output$plotidea13.1 <- renderPlotly({
    plot2 <-
      ggplot(
        dataFueltypes3(),
        aes(
          x = Years,
          y = Count,
          colour = factor(Fueltype),
          group = Fueltype,
          text = tooltip_fuel
        )
      ) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(y = "Vehicles using this fuel type",
           x = "Years",
           colour = "Fuel type")
    
    ggplotly(plot2, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.2, y = 0, 
                    text = "CBS 85239", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  
  
  ##### Traffic and Infrastructure Tab  #####
  # Traffic Intensity
  # Data
  data_83712 <- data83712 %>%
    mutate(tooltip_traffic = paste0(
      "Region: ", provinces, "\n",
      "No. cars: ", VerkeersintensiteitenRijkswegen_1
    ))
  # Plot
  output$traffic_intensity_plot <- renderPlotly({
    trafficintensity <- ggplot(
      data_83712,
      aes(
        x = Years,
        y = VerkeersintensiteitenRijkswegen_1,
        group = provinces,
        colour = provinces,
        text = tooltip_traffic
      )
    ) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      scale_colour_manual(values = regioncolours) +
      labs(
        x = "Years",
        y = "Hourly Average Number of Cars on Highways",
        caption = "source: CBS 83712",
        colour = "Provinces"
      )
    ylim(0, 1270)
    
    ggplotly(trafficintensity, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1, y = 0, 
                    text = "CBS 83712", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
  
  # Data
  # Length of highways
  # making data reactive by type of highway
  mapData_rijbanen <- reactive({
    mapDatarijbanen %>%
      filter(Perioden == input$Years_highways) %>%
      mutate(col1.1 = paste0(naam, ": ", Weglengte_1)) %>% # creating text for hover
      mutate(col1.2 = paste0("Province: ", ligtInProvincieNaam)) %>% # hover text
      mutate(tooltip_text = paste(naam, ": ", Weglengte_1, "\n", "Province: ", ligtInProvincieNaam)) %>% # combined
      filter(SoortRijbanen == input$SoortRijbanen)
  })
  
  # Plot
  output$highway_map <- renderPlotly({
    plotted <- ggplot(mapData_rijbanen()) +
      geom_sf(aes(
        fill = Weglengte_1,
        colour = naam,
        text = tooltip_text
      )) + # tooltip_text new custom column
      guides(colour = "none") +
      scale_fill_gradient(
        name = "in km",
        low = "white", # make an option somewhere for NA vals
        high = "red"
      ) +
      scale_colour_manual(values = rep("grey40", 40)) +
      theme_void() +
      theme(legend.position = "bottom") # plotly ignores this
    
    gg_1 <- ggplotly(plotted, tooltip = "text") %>% 
      layout(annotations = # adds caption to plot
               list(x = 1, y = 0, 
                    text = "CBS 70806 & PDOK", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
    gg_1 %>%
      style(
        hoveron = "text",
        # override the color mapping
        # line.color = toRGB("darkgrey"),
        traces = seq.int(2, length(gg_1$x$data))
      )
    # config(modeBarButtonsToRemove = c("comparedataonhover")) # check this
  })
  
  ## accompanying bar graph
  # provincial_highways <- reactive(
  #   data70806_2 %>%
  #   filter(SoortRijbanen == input$SoortRijbanen) %>%
  #   filter(Perioden == input$Years_highways)
  # )
  #
  # output$highway_bargraph <- renderPlotly({
  #   ggplot(provincial_highways) +
  #   geom_col(aes(x = RegioS, y = Weglengte_1)) +
  #   theme_minimal()
  # })
  
  
  
  ##### Proximity to Amenities Tab  #####
  # Data
  proximity_data <- reactive({
    mapDataproximity %>%
      mutate(column1.1 = paste0(naam, ": ", distances)) %>%
      mutate(column1.2 = paste0("Province: ", ligtInProvincieNaam)) %>%
      mutate(proximity_tooltip = paste(column1.1, column1.2, sep = "\n")) %>%
      filter(name == input$proxmapvariable)
  })
  # Map
  output$proximity_map <- renderPlotly({ 
    proximity_map_plot <- ggplot(proximity_data()) +
      geom_sf(aes(
        fill = distances,
        colour = naam,
        text = proximity_tooltip
      )) +
      guides(colour = "none") +
      scale_fill_gradient( # This is for the map colours.
        low = "#B3EFFF",
        high = "#1C304A"
      ) +
      scale_colour_manual(values = rep("grey40", 40)) +
      theme_void() +
      labs(fill = "in km")
    
    gg_2 <- ggplotly(proximity_map_plot, tooltip = "text") %>% 
      layout(annotations = # adds caption to plot
               list(x = 1, y = 0, 
                    text = "CBS 80305 & PDOK", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
    
    gg_2 %>%
      style(
        hoveron = "text",
        # # change colour outline of province to grey
        # line.color = toRGB("darkgrey"),
        traces = seq.int(2, length(gg_2$x$data))
      )
  })
  
  # Plot accompanying proximity map
  provincial_data <- reactive({
    longformdata80305 %>%
      mutate(tooltip_prox_plot = paste0("Distance: ", distances)) %>%
      filter(Regions %in% c("PV20  ", "PV21  ", "PV22  ")) %>%
      filter(name == input$proxmapvariable)
  })
  output$proximityplot <- renderPlotly({
    prox_barplot <- ggplot(provincial_data()) +
      geom_col(
        aes(
          x = Municipality,
          y = distances,
          fill = Municipality,
          text = tooltip_prox_plot
        ),
        width = 0.5,
        show.legend = FALSE
      ) +
      labs(
        title = "Average distance by Province",
        x = "Provinces",
        y = "Average Distance in km",
        caption = "Data Source: CBS 80305ENG"
      ) +
      scale_fill_manual(values = regioncolours_prox) + # for unified colours
      theme_minimal()
    
    ggplotly(prox_barplot, tooltip = c("text"), dynamicTicks = TRUE) %>% 
      layout(annotations = # adds caption to plot
               list(x = 1.4, y = 0, 
                    text = "CBS 80305", 
                    showarrow = F, 
                    # sets the x and y id to the proportional to the edge of the graph:
                    xref = 'paper', 
                    yref = 'paper', 
                    font = list(size = 12)))
  })
})
