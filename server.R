options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)

# Define server that analyzes flight delays
shinyServer(function(input, output) {
  
  # Create a descriptive table for all flights
  output$table1 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target1 <- c(input$Departure)
    target2 <- c(input$Arrival)
    target3 <- c(input$Airline)
    delay_df <- filter(mydata, FINAL_DEP_DELAY %in% target1 & FINAL_ARR_DELAY %in% target2 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    airline_perf <- filter(mydata, CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2] )
      
    #Create a table 
      #Performance Count (by Airline)
      flightcount <- table(delay_df$CARRIER)
    
      #Performance Percentage (by Airline)
      airline_count <- table(delay_df$CARRIER)
      airline_percent <- airline_count / table(airline_perf$CARRIER)*100
      flightperc <- round(airline_percent,2)
    
      #Count of All Flights (by Airlines)
      flightotal <- table(airline_perf$CARRIER)
    
    performance <- matrix(c(flightperc,flightcount,flightotal),ncol=3,nrow=3)
    colnames(performance) <- c("   Performance Index (%)","   # On Time and/or Delayed","      # Flights")
    rownames(performance) <- c("American Airlines","Delta Airlines","United Airlines")
    performance <- as.table(performance)
    performance
    
  })
  
  # Create a descriptive table for flight delay reasons
  output$table2 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file

    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target1 <- c(input$Departure)
    target2 <- c(input$Arrival)
    target3 <- c(input$Airline)
    carrier_df <- filter(mydata, FINAL_DEP_DELAY %in% target1 & FINAL_ARR_DELAY %in% target2 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    
    #Compute Delay Values
    carrier <- sum(as.numeric(carrier_df$CARRIER_DELAY), na.rm = TRUE)
    weather <- sum(as.numeric(carrier_df$WEATHER_DELAY), na.rm = TRUE)
    nas <- sum(as.numeric(carrier_df$NAS_DELAY), na.rm = TRUE)
    security <- sum(as.numeric(carrier_df$SECURITY_DELAY), na.rm = TRUE)
    lateac <- sum(as.numeric(carrier_df$LATE_AIRCRAFT_DELAY), na.rm = TRUE)
    delaysum <- (carrier+weather+nas+security+lateac)
    carrier_perc <- carrier/delaysum*100
    weather_perc <- weather/delaysum*100
    nas_perc <- nas/delaysum*100
    security_perc <- security/delaysum*100
    lateac_perc <- lateac/delaysum*100
    delayreason <- matrix(c(carrier,carrier_perc,weather,weather_perc,nas,nas_perc,security,security_perc,lateac,lateac_perc),ncol=5,nrow=2)
    rownames(delayreason) <- c("delay caused (in minutes)","percentage of total delays")
    colnames(delayreason) <- c("   Carrier","   Weather","   Air Traffic","   Security"," Late Aircraft Arrival")
    delayreason <- round(as.table(delayreason),1)
    delayreason
  })
  
  #ARRIVAL DELAY Plot
  output$plot1 <- renderPlot({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target2 <- c(input$Arrival)
    target3 <- c(input$Airline)
    plot_df <- filter(mydata, FINAL_ARR_DELAY %in% target2 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    
    #Compute Delay Values
    alldelays <- aggregate(list(delay=plot_df$ARR_DELAY_NEW), by=list(date=plot_df$DATE, carrier=plot_df$CARRIER), FUN=sum,na.rm = TRUE)
    
    p <- ggplot(alldelays, aes(x=date, y=delay, group=carrier)) +
      geom_line(aes(color=carrier))+
      geom_point(aes(color=carrier))+
      labs(title="Airline Delays (Arrivals)",x="Date", y = "Total Delays (min)")
    p + scale_color_manual(values=c("red", "blue", "purple"))
  })
  
  #DEPARTURE DELAY Plot
  output$plot2 <- renderPlot({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return("Please Upload A File For Analysis")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target1 <- c(input$Departure)
    target3 <- c(input$Airline)
    plot_df2 <- filter(mydata, FINAL_DEP_DELAY %in% target1 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    
    #Compute Delay Values
    alldelays <- aggregate(list(delay=plot_df2$DEP_DELAY_NEW), by=list(date=plot_df2$DATE, carrier=plot_df2$CARRIER), FUN=sum,na.rm = TRUE)
    
    p <- ggplot(alldelays, aes(x=date, y=delay, group=carrier)) +
      geom_line(aes(color=carrier))+
      geom_point(aes(color=carrier))+
      labs(title="Airline Delays (Departures)",x="Date", y = "Total Delays (min)")
    p + scale_color_manual(values=c("red", "blue", "purple"))
  })
  
  
  # Create a map output variable - State Delays
  output$map1 <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target1 <- c(input$Departure)
    target2 <- c(input$Arrival)
    target3 <- c(input$Airline)
    map_df <- filter(mydata, FINAL_DEP_DELAY %in% target1 & FINAL_ARR_DELAY %in% target2 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    mydftable <- table(map_df$ORIGIN_STATE)
    arrivaldelays <- aggregate(map_df$ARR_DELAY_NEW, by=list(State=map_df$DEST_STATE_NAME), FUN=sum,na.rm = TRUE)

    #Import JSON
    states <- geojsonio::geojson_read("http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json", what = "sp")
    
    #Create DF with JSON state names 
    states_names <- data.frame(states$NAME)
    colnames(states_names) <- "State"
    
    #Merge JSON DF with Arrival DF (Outter Join to include missing states)
    newdelays <- join(states_names, arrivaldelays, by="State")
    
    #Add data to JSON file
    states$arr_delay <- newdelays$x
    
    # Create colors with a categorical color function
    bins <- c(0, 100, 200, 500, 1000, 2000, 5000, 10000, Inf)
    pal <- colorBin("Reds", domain = states$arr_delay, bins = bins)
    
    #Add Labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%g minutes",
      states$NAME, states$arr_delay
    ) %>% lapply(htmltools::HTML)
    
    # Create the leaflet function for data
    m <- leaflet(states) %>%

      # Set the default view
       setView(-96, 37.8, 4) %>%
      
      # Provide tiles
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
      
      # Add shading
      
     m<- m %>% addPolygons(
       fillColor = pal(states$arr_delay),
       weight = 2,
       opacity = 1,
       color = "white",
       dashArray = "3",
       fillOpacity = 0.7,
       highlight = highlightOptions(
         weight = 5,
         color = "#666",
         dashArray = "",
         fillOpacity = 0.7,
         bringToFront = TRUE),
       label = labels,
       labelOptions = labelOptions(
         style = list("font-weight" = "normal", padding = "3px 8px"),
         textsize = "15px",
         direction = "auto")
     )
     m
  })
  
  #Create Map for Arrival Airports (in minute delays)
  output$map2 <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target2 <- c(input$Arrival)
    target3 <- c(input$Airline)
    map2_df <- filter(mydata, FINAL_ARR_DELAY %in% target2 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    
    #Aggregate Data
    airportdelay <- aggregate(list(delay=map2_df$ARR_DELAY_NEW), by=list(name=map2_df$DEST), FUN=sum,na.rm = TRUE)
    airportlat <- aggregate(list(lat = map2_df$DEST_LAT), by=list(name=map2_df$DEST), FUN=mean,na.rm = TRUE)
    airportlon <- aggregate(list(lon = map2_df$DEST_LON), by=list(name=map2_df$DEST), FUN=mean,na.rm = TRUE)
    allairport <- cbind(airportdelay,airportlat,airportlon)
    
    # Create the leaflet function for data
    leaflet(allairport) %>%
      
      # Set the default view
      setView(-96, 37.8, 4) %>%
      
      # Provide tiles
      addTiles() %>%
      
      # Add circles
      addCircles(
        radius =100+(allairport$delay)*3,
        lng= (allairport$lon),
        lat= (allairport$lat),
        stroke= FALSE,
        fillOpacity=0.5,
        color="Red",
        label=sprintf("%s: %g minutes", allairport$name, allairport$delay)
      )
  })
  
  #Create Map for DEPARTURE Airports (in minute delays)
  output$map3 <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different time slots, and different airlines, and categories
    target1 <- c(input$Departure)
    target3 <- c(input$Airline)
    map3_df <- filter(mydata, FINAL_DEP_DELAY %in% target1 & CARRIER %in% target3 & as.Date(DATE)>=input$Date[1] & as.Date(DATE)<=input$Date[2])
    
    #Aggregate Data
    airportdelay1 <- aggregate(list(delay=map3_df$DEP_DELAY_NEW), by=list(name=map3_df$ORIGIN), FUN=sum,na.rm = TRUE)
    airportlat1 <- aggregate(list(lat = map3_df$ORIGIN_LAT), by=list(name=map3_df$ORIGIN), FUN=mean,na.rm = TRUE)
    airportlon1 <- aggregate(list(lon = map3_df$ORIGIN_LON), by=list(name=map3_df$ORIGIN), FUN=mean,na.rm = TRUE)
    allairport1 <- cbind(airportdelay1,airportlat1,airportlon1)
    
    # Create the leaflet function for data
    leaflet(allairport1) %>%
      
      # Set the default view
      setView(-96, 37.8, 4) %>%
      
      # Provide tiles
      addTiles() %>%
      
      # Add circles
      addCircles(
        radius =100+(allairport1$delay)*3,
        lng= (allairport1$lon),
        lat= (allairport1$lat),
        stroke= FALSE,
        fillOpacity=0.5,
        color="Purple",
        label=sprintf("%s: %g minutes", allairport1$name, allairport1$delay)
      ) 
  })
  
})