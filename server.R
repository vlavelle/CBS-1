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
  
  # defining a vector for the colours for the Region (colourblind safe)
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
  
  
  
  ##### Mobility Indicators Tab  #####
  
  ### Modes per Region
  # Data reactivity
  # this is all not in the dashboard @seb delete?
  # data_84710 <- reactive({
  #   data84710 %>%
  #     filter(RegionCharacteristics == input$region) %>%
  #     filter(Periods == "2021") %>%
  #     filter(TravelMotives == "Total") %>% # only value, no confidence interval
  #     filter(TravelModes != "Total") %>%
  #     select(
  #       TravelMotives,
  #       TravelModes,
  #       RegionCharacteristics,
  #       Periods,
  #       Trips_4
  #     ) %>%
  #     mutate(tooltip_text = paste(TravelModes, "\n", "Region: ", RegionCharacteristics, "\n", Trips_4))
  # })
  # 
  # # Plot 1
  # output$plotidea1 <- renderPlotly({
  #   modesperregionplot <- ggplot(
  #     data = data_84710(),
  #     aes(
  #       x = TravelModes,
  #       y = Trips_4, # yearly avg
  #       fill = TravelModes,
  #       text = tooltip_text # yearly avg
  #     )
  #   ) +
  #     geom_col() +
  #     theme_minimal() +
  #     labs(
  #       title = "Average Yearly Trips in 2021",
  #       x = "Travel Mode",
  #       y = "Avg Trips per Person Per Year",
  #       caption = "Data Source: CBS 84710ENG",
  #       fill = "Travel Mode:"
  #     )
  #   
  #   ggplotly(modesperregionplot, tooltip = c("text"), dynamicTicks = TRUE) %>%
  #     layout(
  #       annotations = # adds caption to plot
  #         list(
  #           x = 1.2, y = 0,
  #           text = "CBS 84710",
  #           showarrow = F,
  #           # sets the x and y id to the proportional to the edge of the graph:
  #           xref = "paper",
  #           yref = "paper",
  #           font = list(size = 12)
  #         )
  #     )
  # })
  
  
  ### Motives & Modes per regions
  # Data reactivity
  data84710_1 <- reactive({
    data84710 %>%
      filter(TravelModes == "Total") %>%
      filter(TravelMotives == input$TravelMotives) %>%
      group_by(Periods, TravelMotives, RegionCharacteristics) %>%
      select(TravelMotives,
             RegionCharacteristics,
             Periods,
             DistanceTravelled_5) %>%
      # create a new column for average distace
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelMotives,
             RegionCharacteristics,
             Periods,
             mean_distance_travelled) %>%
      distinct() %>% # removes any duplicate rows
      mutate( # creates text for the hover tooltip
        tooltip_text = paste0(
          "Region: ",
          RegionCharacteristics,
          "\n",
          "Distance: ",
          mean_distance_travelled
        )
      )
  })
  
  # Plot output
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
    # putting into plotly for extra interactivity
    # dynamic ticks allows for y axis to change with selections
    ggplotly(lineplot_1, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2, y = 0, # coordinates
            text = "CBS 84710", # caption text
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12) 
          )
      )
  })
  
  # Data reactivity plot 2
  data84710_2 <- reactive({
    data84710 %>%
      filter(TravelMotives == "Total") %>%
      filter(TravelModes == input$TravelModes) %>%
      group_by(Periods, TravelModes, RegionCharacteristics) %>%
      select(TravelModes,
             RegionCharacteristics,
             Periods,
             DistanceTravelled_5) %>%
      # create new column with average distances travelled
      mutate(mean_distance_travelled = mean(DistanceTravelled_5, na.rm = TRUE)) %>%
      select(TravelModes,
             RegionCharacteristics,
             Periods,
             mean_distance_travelled) %>%
      distinct() %>% # removes duplicate rows
      mutate( # creates text for hover tooltip
        tooltip_text = paste0(
          "Region: ",
          RegionCharacteristics,
          "\n",
          "Distance:",
          mean_distance_travelled
        )
      )
  })
  
  # Plot 2 output
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
    # Putting plot into plotly for extra interactivity
    ggplotly(lineplot_2, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2, y = 0, # coordinates of caption
            text = "CBS 84710", # caption text
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  ### Timeframe Data: Travel Purpose
  # Data reactivity
  data_85055 <- reactive({
    data85055 %>%
      filter(TravelPurposes == input$TravelPurposes) %>%
      filter(Periods == input$Periods1) %>%
      filter(Timeframe == input$Timeframe1) %>%
      # create text for hover tooltip:
      mutate(tooltip_text = paste0(
        "Region: ", RegionCharacteristics, "\n",
        "Avg Distance: ", AverageDistanceTravelledPerTrip_1, "\n",
        TripCharacteristics
      ))
  })
  
  # Plot output
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
    
    # Plotly for extra interactivity
    # Dynamic ticks for dyanmic y axis
    ggplotly(data85055plot, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2, y = 0,
            text = "CBS 85055", # caption text
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  ### Timeframe Data: Travel Mode
  # Data reactivity
  data_85056 <- reactive({
    data85056 %>%
      filter(ModesOfTravel == input$ModesOfTravel) %>%
      filter(Periods == input$Periods2) %>%
      filter(Timeframe == input$Timeframe2) %>%
      mutate(tooltip_text = paste0(
        "Region: ", RegionCharacteristics, "\n",
        "Avg Distance: ", AverageDistanceTravelledPerTrip_1, "\n",
        TripCharacteristics
      )
      )
  })
  
  # Plot output
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
    
    # Plotly creation
    ggplotly(data85056plot,
             tooltip = c("text"),
             dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2,
            y = 0,
            text = "CBS 85056",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  ### Personal Characteristics
  # Data reactivity
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
  
  # Plot output
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
      theme(axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8
      )) +
      labs(
        x = "Characteristic",
        y = "Average number of trips per day, per person",
        caption = "Data Source: CBS 84709",
        fill = "Region:"
      ) +
      scale_fill_manual(values = regioncolours)
    
    # Plotly output
    ggplotly(Persoonskenmerken_plot,
             tooltip = c("text"),
             dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2,
            y = 0,
            text = "CBS 84709",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  
  # Driving license
  # Data reactivity
  dataDrivingLicense1 <- reactive(
    data83488 %>%
      filter(AgeDrivingLicenseHolder == input$LicenseHolderAge) %>%
      filter(CategoryDrivingLicence == input$LicenseCategory) %>%
      group_by(Periods, Region) %>%
      mutate(
        tooltip_text = paste0(
          "Count: ",
          PeopleWithADrivingLicence_1,
          "\n",
          "Region: ",
          Region,
          "\n"
        )
      )
  )
  # Plot output
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
      labs(caption = "CBS 83488",
           x = "Years",
           y = "Number of people with driver's licenses")
    
    # Plotly creation
    ggplotly(DrivingLicense1,
             tooltip = c("text"),
             dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2,
            y = 0,
            text = "CBS 83488",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  # Data reactivity for second driver's license graph
  dataDrivingLicense2 <- reactive(
    data83488 %>%
      filter(Periods == input$PeriodsLicense) %>%
      filter(CategoryDrivingLicence == input$LicenseCategory2) %>%
      filter(AgeDrivingLicenseHolder != "Total") %>%
      mutate(
        tooltip_text = paste0(
          "Count: ",
          PeopleWithADrivingLicence_1,
          "\n",
          "Region: ",
          Region,
          "\n",
          "Category: ",
          CategoryDrivingLicence
        )
      )
  )
  
  # second plot for driver's licenses
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
    
    # Plotly creation
    ggplotly(DrivingLicense2, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1, y = 0,
            text = "CBS 83488",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  
  
  ##### Green Mobility Tab  #####
  ## VEHICLES
  # colourblind friendly palette for vehicle types
  vehicle_colours <- c(
    "Electric vehicle" =  "#000000",       
    "All mopeds" = "#004949",     
    "Moped(25km/h)" = "#009292",        
    "Moped(45km/h)" = "#ff6db6",  
    "Moped (45km/h and <350kg)" = "#ffb6db",
    "Remaining mopeds" = "#490092",       
    "All commercial vehicles"  = "#006ddb", 
    "Van" = "#b66dff",                    
    "Truck" = "#6db6ff",                  
    "Tractor" = "#b6dbff",                  
    "Special vehicle" = "#920000",         
    "Bus" = "#924900",                      
    "Trailer" = "#db6d00",                 
    "Semi trailer" = "#24ff24",             
    "Normal car" = "#ffff6d"  
  )
  # Data reactivity
  data_vehicles <- reactive(
    datacombined %>%
      filter(Region == input$Region_combined) %>%
      group_by(Vehicles, Years) %>% 
      mutate(tooltip_text = paste0("Vehicle: ", Vehicles,
                                   "\n", "Count: ", Count,
                                   "\n", "Region: ", Region))
  )
  
  # Plot output
  output$vehicles_1 <- renderPlotly({
    plot_vehicles_1 <- ggplot(
      data_vehicles(),
      aes(
        x = Years,
        y = Count,
        group = factor(Vehicles),
        colour = factor(Vehicles),
        text = tooltip_text
      )
    ) +
      scale_colour_manual(values = vehicle_colours) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      ylim(0, 50000) +
      labs(y = "Number of vehicles",
           x = "Years",
           colour = "Vehicle type")
    
    # Plotly creation
    ggplotly(plot_vehicles_1, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2,
            y = -0.2,
            text = paste0("CBS: 85237, 85240,", "\n", "Electric Personal Vehicles"),
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = 'paper',
            yref = 'paper',
            font = list(size = 12)
          )
      )
  })
  
  # Plot 2 for Vehicles
  output$vehicles_2 <- renderPlotly({
    data_vehicles_2 <- reactive(
      datacombined %>%
        filter(Years == input$Years_combined) %>% 
        # filter(Vehicles == input$Vehicles_combined2) %>% 
        mutate(tooltip_text = paste0("Vehicle: ", Vehicles,
                                     "\n", "Count: ", Count,
                                     "\n", "Region: ", Region))
    )
    plot_vehicles_2 <-
      ggplot(data_vehicles_2(),
             aes(
               x = Region,
               y = Count,
               fill = factor(Vehicles),
               text = tooltip_text
             )) +
      geom_col(
               position = "stack",
               width = 0.5) +
      scale_fill_manual(values = vehicle_colours) +
      labs(x = "Region",
           y = "Number of vehicles",
           fill = "Vehicle type") +
      theme_minimal()
    
    # Plotly creation
    plotly_vehicles_2 <- ggplotly(plot_vehicles_2,
             tooltip = c("text"),
             dynamicTicks = TRUE) %>%
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
          )#,
        # showlegend = FALSE
      )
    # this is not working entirely
    for (i in 1:length(plotly_vehicles_2$x$data)) {
      plotly_vehicles_2$x$data[[i]]$base <- NULL
      temporary <- plotly_vehicles_2$x$data[[i]]
      plotly_vehicles_2$x$data[[i]] <- plotly_vehicles_2$x$data[[length(plotly_vehicles_2$x$data) - i + 1]]
      plotly_vehicles_2$x$data[[length(plotly_vehicles_2$x$data) - i + 1]] <- temporary
    }
    plotly_vehicles_2
    
  })
  
  
  ## FUEL TYPE
  # Data reactivity
  dataFueltypes2 <- reactive(
    datafueltypes1 %>%
      filter(Fueltype == input$Fueltype) %>%
      mutate(tooltip_fuel = paste0(
        "Count: ", Count, "\n",
        "Vehicle Type: ", Vehicletype, "\n",
        "Fuel Type: ", Fueltype
      )) %>%
      group_by(Vehicletype, Years)
    
  )
  
  # Plot output
  output$fuel_1 <- renderPlotly({
    plot1 <-
      ggplot(
        dataFueltypes2(),
        aes(
          x = Years,
          y = Count,
          colour = Vehicletype,
          group = Vehicletype,
          text = tooltip_fuel
        )
      ) + 
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(y = "Number of vehicles",
           x = "Years",
           colour = "Vehicle type")
    
    # Plotly output
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
  
  # Data reactivity plot 2
  dataFueltypes3 <- reactive(
    datafueltypes1 %>%
      filter(Vehicletype == input$Vehicletype) %>%
      mutate(tooltip_fuel = paste0(
        "Count: ", Count, "\n",
        "Vehicle Type: ", Vehicletype, "\n",
        "Fuel Type: ", Fueltype
      )) %>% 
      group_by(Fueltype, Years)
  )
  
  # Plot
  output$fuel_2 <- renderPlotly({
    plot2 <-
      ggplot(
        dataFueltypes3(),
        aes(
          x = Years,
          y = Count,
          colour = factor(Fueltype),
          group = factor(Fueltype),
          text = tooltip_fuel
        )
      ) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      labs(y = "Vehicles using this fuel type",
           x = "Years",
           colour = "Fuel type")
    
    # Plotly output
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
  
  # Plot output
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
    
    # Plotly creation
    ggplotly(trafficintensity, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.2, y = 0,
            text = "CBS 83712",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
  

  ## Length of highways
  # making data reactive by type of highway
  mapData_rijbanen <- reactive({
    mapDatarijbanen %>%
      filter(Perioden == input$Years_highways) %>%
      # creating text for hover tooltip
      mutate(tooltip_text = paste(
        naam,
        ": ",
        Weglengte_1,
        "\n",
        "Province: ",
        ligtInProvincieNaam
      )) %>%
      filter(SoortRijbanen == input$SoortRijbanen)
  })
  
  # Plot output
  output$highway_map <- renderPlotly({
    plotted <- ggplot(mapData_rijbanen()) +
      geom_sf(aes(
        fill = Weglengte_1,
        colour = naam,
        text = tooltip_text
      )) + # tooltip_text new custom column
      guides(colour = "none") +
      scale_fill_gradient( # fill colours for map
        name = "in km",
        low = "white", 
        high = "red"
      ) +
      # turn outline of all 40 municipalities grey:
      scale_colour_manual(values = rep("grey40", 40)) +
      theme_void() 
    
    # plotly
    gg_1 <- ggplotly(plotted, tooltip = "text") %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1, y = 0,
            text = "CBS 70806 & PDOK",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
    
    gg_1 %>%
      style(
        hoveron = "text",
        # define how may unique traces are needed for the map
        traces = seq.int(2, length(gg_1$x$data))
      )
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
  # Data reactivity
  proximity_data <- reactive({
    mapDataproximity %>%
      mutate(proximity_tooltip = paste0(naam, ": ", distances, "\n", 
                                        "Province: ", ligtInProvincieNaam)) %>%
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
      # changes the outline of all 40 municipalities to grey:
      scale_colour_manual(values = rep("grey40", 40)) +
      theme_void() +
      labs(fill = "in km")
    
    gg_2 <- ggplotly(proximity_map_plot, tooltip = "text") %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1, y = 0,
            text = "CBS 80305 & PDOK",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
    
    gg_2 %>%
      style(
        hoveron = "text",
        # define how may unique traces are needed for the map
        traces = seq.int(2, length(gg_2$x$data))
      )
  })
  
  # Plot accompanying proximity map
  # making the data reactive
  provincial_data <- reactive({
    longformdata80305 %>%
      mutate(tooltip_prox_plot = paste0("Distance: ", distances)) %>%
      filter(Regions %in% c("PV20  ", "PV21  ", "PV22  ")) %>%
      filter(name == input$proxmapvariable)
  })
  # Plot creation
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
    
    # Creating plotly
    ggplotly(prox_barplot, tooltip = c("text"), dynamicTicks = TRUE) %>%
      layout(
        annotations = # adds caption to plot
          list(
            x = 1.4, y = 0,
            text = "CBS 80305",
            showarrow = F,
            # sets the x and y id to the proportional to the edge of the graph:
            xref = "paper",
            yref = "paper",
            font = list(size = 12)
          )
      )
  })
})
