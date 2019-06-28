library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that analyzes the patterns of crimes in DC
shinyUI(fluidPage(
  
  # Change the theme to united - Pun Intended
  theme = shinytheme("simplex"),
  
  # Application title
  titlePanel("US Flight Delays for Major US Airlines (Holiday Season 2017)"),
  hr(),
  
  # Three sidebars for uploading files, selecting time slots and districts
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Upload CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Create a multiple checkbox input for performance - departure
      checkboxGroupInput("Departure",
                         "Departure Type:",
                         choices = list("Delayed Departure"=1,"On-Time Departure"=0),
                         selected=c(1,0)
      ),
      
      hr(),
      
      # Create a multiple checkbox input for performance - arrival
      checkboxGroupInput("Arrival",
                         "Arrival Type:",
                         choices = list("Delayed Arrival"=1,"On-Time Arrival"=0),
                         selected=c(1,0)
      ),
      
      hr(),
      

      # Create a multiple checkbox input for airline
      checkboxGroupInput("Airline",
                         "Airline:",
                         choices = list("United Airlines (UA)"="UA","American Airlines (AA)"="AA","Delta Airlines (DL)"="DL"),
                         selected=c("UA","AA","DL")
      ),
      
      hr(),
      
      # Create a date range
      dateRangeInput("Date","Date Range:",start="2017-12-24", end="2017-12-30", min="2017-12-24", max="2017-12-31"
      )
      
    ),
    
    # Make the sidebar on the right of the webpage
    position = "left",
    fluid = TRUE,
    
    # Create two tabs
    mainPanel(
      tabsetPanel(type="tabs",
                  
                  #Add a tab for problem description
                  tabPanel("Problem Description",
                    p(),       
                    p("Flight Delays are a source of frustration for anybody's travel experience. The Department of Transportation estimates that in 2017, approximately 20% of all flights experience some sort of delay or cancellation. This not only negatively impacts a customer's perception of an airline but leads to an exhorbitatnt amount of costs (the FAA estimates that delays costs the airline industry upwards of $8 billion each year)."),
                    p("This project uses the dataset 'On-Time : Reporting Carrier On-Time Performance (1987-present)' from the Bureau of Transportation statistics. Given the rather large size of the dataset, it has been filtered to show flights that occured between December 24, 2017 and December 31, 2017 operated by:"),
                    tags$ul(
                      tags$li("United Airlines (UA)"),
                      tags$li("American Airlines (AA)"),
                      tags$li("Delta Airlines (DL)")
                      ),
                    p("The dataset includes the following variables (only the relevant variables are shown:"),
                    tags$ul(
                      tags$li("Flight Date"),
                      tags$li("Operating Carrier"),
                      tags$li("Origin Airport (ID, City, State, Longitude, Lattitude)"),
                      tags$li("Destination Airport (ID, City, State, Longitude, Lattitude)"),
                      tags$li("Stated & Actual Departure Times"),
                      tags$li("Stated & Actual Arrival Times"),
                      tags$li("Delay"),
                      tags$li("Reason for Delay")
                      ),
                    p("Question(s): 
                    How are airplane delays distributed amongst the United States? Are there any particular airports and/or majors airlines that are more likely to experience them? What are the main causes for these delays? This dashboard will provide insight into these questions",
                    p("Data Source: http://www.transtats.bts.gov/Fields.asp?Table_ID=272"),
                    p("Data Information: https://jblevins.org/notes/airline-data") 
                           
                  )),
                  
                  #Add a tab for decriptive table
                  tabPanel("Descriptive Analysis",
                           
                           #Add two subtabs
                           tabsetPanel(
                             tabPanel("Airline Performance",verbatimTextOutput("table1")),
                             tabPanel("Causes of Delays",verbatimTextOutput("table2")),
                             tabPanel("Delay Visualization (Departure)",plotOutput("plot2")),
                             tabPanel("Delay Visualization (Arrivals)",plotOutput("plot1"))
                           )
                  ),
                                       
                  
                  #Tab for the Map
                  tabPanel("Maps",
                          
                           #Add two subtabs
                          tabsetPanel(
                            tabPanel("Total Delays by Airport (Departures)",leafletOutput("map3", height=450)),
                            tabPanel("Total Delays by Airport (Arrivals)",leafletOutput("map2", height=450)),
                            tabPanel("Arrival Delays by State",leafletOutput("map1", height=450))
                          )
                  )
                  
      )
    )
  )
))