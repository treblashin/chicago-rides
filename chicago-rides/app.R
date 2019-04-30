# installing the necessary packages

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(leaflet)

# read in the csv that was created through write_csv in the .Rmd documeent.
# Assign that data to y.

y <- read_csv("www/y.csv")

# Define UI for application that draws a graph. Used navbarPage to create a page
# with a top-level navigation bar.

ui <- navbarPage(
   
   # Application title. used shinytheme to change the layout of the shiny app to
   # something more aesthetically pleasing.
  
   title = "Chicago Ride-Sharing Data",
   theme = shinytheme("yeti"),
   
   # Create a set of tabs on the navigation bar. Title the tab as "Home". Have a
   # title called "Ride Share Locations".

   tabPanel(
     title = "Home",
     h4("Ride Share Locations"),
     sidebarLayout(
       sidebarPanel(

         # Create a slider with input that allows users to sift between hours of
         # the day. min = 0 (for 12AM), max = 23 (for 11PM). Value = 0 for the
         # initial time setting of 12AM. step = 1 to increment the slider by one
         # hour for each tick mark. sep = no separator. animate = TRUE to enable
         # the map to animate.
         
         sliderInput(inputId = "hour",
                     label = "Hour of Day:",
                     min = 0,
                     max = 23,
                     value = 0,
                     step = 1,
                     sep = "", 
                     animate = TRUE),
         
         # Use checkboxGroupInput in order for users to check which dates they
         # want to see on the map. There are two different dates (as we are
         # looking at 12/4/18 and 12/25/18). The pre-selected options are both
         # days, but users are able to toggle the dates based on what days they
         # want to see visualized.
         
         checkboxGroupInput(inputId = "startdateleaflet",
                            label = "Select Ride Start Date:",
                            choices = c("December 04, 2018", "December 25, 2018"),
                            selected = c("December 04, 2018", "December 25, 2018"))
       ),
       
       # In the mainPanel(), create a set of two tabs that are titled pickup and
       # dropoff locations. There are two maps: one for the pickup locations of
       # the ride-sharing trips, and one for the dropoff locations of the
       # ridesharing trips. Users can click on either tab to view either plot.
       
       mainPanel(
         tabsetPanel(
           tabPanel(title = "Pickup Locations", 
                    leafletOutput("leafletpickupPlot")),
           tabPanel(title = "Dropoff Locations", 
                    leafletOutput("leafletdropoffPlot"))
         )
       )
     )
   ),

  # The purpose of the insights tab is to provide important context and
  # background on the data that otherwise might not be noticeable, especially on
  # the other plots.
   
  tabPanel(
    title = "Insights", 
    h4("Insights"),
    mainPanel(
      
      # PlotOutput that creates a plot showing the number of observations based
      # on the start date.
      # I used h6() to insert text and an interpretation of the data.s
      
      plotOutput("nPlot"),
      h6("Though it is difficult to see in other graphics, the data indicates 
         there is a significant difference in the number of rides started on 
         December 4th, 2018 and December 25th, 2018. The number of rides on 
         December 4th is over double the amount of rides on December 25th.
         A possible explanation (especially as both days are on a Tuesday, so
         there is no day of the week variation or bias) is that people are less
         likely to be traveling on Christmas day.")
    )
  ),
   
  # Next tab, Distribution information, contains distribution histograms of
  # variables in the data specifically the frequency of certain trip mileages
  # and trip fares.
   
  tabPanel(
     title = "Distribution Information",
     h4("Distribution Information"),
     sidebarLayout(
       sidebarPanel(
         
         # Use checkboxGroupInput in order for users to check which dates they
         # want to see on the map. There are two different dates (as we are
         # looking at 12/4/18 and 12/25/18). The pre-selected options are both
         # days, but users are able to toggle the dates based on what days they
         # want to see visualized. 
         
         checkboxGroupInput(inputId = "startdatebasic",
                            label = "Select Ride Start Date:",
                            choices = c("December 04, 2018", "December 25, 2018"),
                            selected = "December 04, 2018"),
         
         # sliderInput to determine the number of bins that should be in the
         # histogram. The minimum is 1, and the maximum is 50. The default value
         # is set at 30 when the app opens.
         
         sliderInput(inputId = "basicbins",
                     label = "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
       ),
       mainPanel(
         
         # Create two tabs for both of the variables, "Trip Miles" and "Trip
         # Fares". create two plots, milesDist (for miles distribution plot) and
         # faresDist (for fares distribution plot)
         
         tabsetPanel(
           tabPanel(
             title = "Trip Miles",
             plotOutput("milesDist")),
           tabPanel(
             title = "Trip Fares",
             plotOutput("faresDist"))
         )
       )
     )
  ),

  # Next tab displays tip data from the rides. The tab has scatterplots
  # comparing the tip amounts with other ride variables.
  
  tabPanel(
    title = "Tip Data",
    h4("Comparing Tip Amounts with Ride Variables"),
    sidebarLayout(
      sidebarPanel(
        
        # Use checkboxGroupInput in order for users to check which dates they
        # want to see on the map. There are two different dates (as we are
        # looking at 12/4/18 and 12/25/18). The pre-selected options are both
        # days, but users are able to toggle the dates based on what days they
        # want to see visualized.
        
        checkboxGroupInput(inputId = "startdatetips",
                           label = "Select Ride Start Date:",
                           choices = c("December 04, 2018", "December 25, 2018"),
                           selected = c("December 04, 2018", "December 25, 2018"))
      ),
      mainPanel(
        tabsetPanel(
          
          # Create three tabs for three variables that are being compared with
          # the tip amount: fares, additional charges, and trip distane in
          # miles. Each of these tabs has a scatterplot, so users can click on
          # the tabs and alternate between plots. Thanks to the
          # checkboxGroupInput, users can toggle certain dates they want to look
          # at.
          
          tabPanel(title = "Fares",
                   plotOutput("faretipPlot")),
          
          tabPanel(title = "Additional Charges",
                   plotOutput("additionaltipPlot")),
          
          tabPanel(title = "Trip Distance (Miles)",
                   plotOutput("milestipPlot"))
        )
      )
    )
  ),
  
  # Tab outlining information about the project, and acknowledgments (sources,
  # thank yous, etc.). Used html tags (h4(), h6(), to create varying sizes) and
  # br() to cleanly divide the text sections. Used a/href in order to link
  # GitHub and the source of the data.
  
  tabPanel(
    title = "About/Acknowledgments",
    h4("About/Acknowledgments"),
    mainPanel(
      h4("Goals and Objectives"),
      h6("The goal of this project is to look at ride-sharing data in the city 
         of Chicago, specifically at rides with a start date of December 4th, 2018
         and December 25th, 2018, to compare ride-sharing on the Christmas holiday 
         and a non-holiday day in December that is the same day of the week as 
         Christmas."),
      br(),
      h4("Methodology"),
      h6("The original dataset contains over 17 million rows of data, spanning 
         dates from November to December 2018. Due to the volume of data, I decided
         to look at specific dates — in particular, December 4th and December 25th.
         When selecting for rows that have a start date of December 4th or December 
         25th, there are still over 300 thousand rows. For the sake of clarity in the 
         plots and graphs used in this app, I used a random sample of 25 thousand rows.
         This provides enough data points to provide a reasonable representation of the 
         data, while simultaneously making certain plots and graphs easier to read."),
      br(),
      h4("Disclaimer"),
      h6("The Shiny App may take a while to load due to the volume of data points."),
      br(),
      h4("Sources"),
      h6("Data from the city of Chicago data portal can be found", 
         a("here.", href="https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips/m6dm-c72p/data"),
         "The GitHub repository can be found",
         a("here.", href="https://github.com/treblashin/chicago-rides"), 
         "Shiny App created by Albert Shin ('22)."),
      h6("Questions about the project? Contact me at albertshin@college.harvard.edu."),
      br(), 
      h4("Thank You"),
      h6("Thank you Preceptor, Albert, Jacob, and everyone in Gov 1005 for an amazing 
         semester — this project would not have been possible without your help.")
    )
  )
)

# Define server logic required to create plots

server <- function(input, output) {

  # reactive allows the date selection of the tips tab to be interactive. req()
  # ensures that values are avilable before proceeding, making sure that there
  # is an input before running the scatterplots. Filter y for the dates that
  # are selected for by the user (through checking the desired date boxes). Feed
  # all of this into start_date_tips, which will be used later when plotting.
  
  start_date_tips <- reactive({
    req(input$startdatetips)
    filter(y, format_start_date %in% input$startdatetips)
  })
  
  # reactive allows the date selection of the basic information tab to be
  # interactive. req() ensures that values are avilable before proceeding,
  # making sure that there is an input before running the scatterplots. Filter y
  # for the dates that are selected for by the user (through checking the
  # desired date boxes). Feed all of this into start_date, which will be used
  # later when creating histogram graphics in the basic information tab.
  
  start_date <- reactive({
    req(input$startdatebasic)
    filter(y, format_start_date %in% input$startdatebasic)
  })

  output$nPlot <- renderPlot({
    
    # the data in use is the regular dataset y. We assign it to z after creating
    # so transformations. We group by the start date of the trip. We then create
    # a total column that counts up the number of rows. Finally we ungroup.
    
    z <- y %>% 
      group_by(format_start_date) %>% 
      mutate(total = n()) %>% 
      ungroup()
    
    ggplot(data = z, aes(x = format_start_date)) + 
      
      # Use geom_histogram() to create a histogram plot. Make the bins dynamic
      # using input$basicbins, which takes the number that the user inputs for
      # the number of bins.
      
      geom_bar() + 
      
      # use labs() to title the graph. 
      
      labs(
        title = "Number of Trips by Start Date", 
        x = "Trip Start Date",
        y = "Number of Trips"
      )
  })
  
  # Creates a ggplot histogram using renderplot. This is the output of the miles
  # distribution plot in the basic information tab.
  
  output$milesDist <- renderPlot({
    
    # the data in use is start_date(), which is the reactive data. Map the x
    # axis values to trip_miles, the various mile lengths of the rides.
    
    ggplot(data = start_date(), aes(x = trip_miles)) + 
      
      # Use geom_histogram() to create a histogram plot. Make the bins dynamic
      # using input$basicbins, which takes the number that the user inputs for
      # the number of bins.
      
      geom_histogram(bins = input$basicbins) + 
      
      # use labs() to title the graph. 
      
      labs(
        title = "Distribution of Trip Miles", 
        x = "Length of Trip in Miles",
        y = "Number of Trips"
      )
  })
  
  # Creates a ggplot histogram using renderplot. This is the output of the fares
  # distribution plot in the basic information tab.
  
  output$faresDist <- renderPlot({
    
    # the data in use is start_date(), which is the reactive data. Map the x
    # axis values to fare, the various fare amounts of the rides.
    
    ggplot(data = start_date(), aes(x = fare)) + 
      
      # Use geom_histogram() to create a histogram plot. Make the bins dynamic
      # using input$basicbins, which takes the number that the user inputs for
      # the number of bins. Use labs() to title the graph.
      
      geom_histogram(bins = input$basicbins) + 
      labs(
        title = "Distribution of Trip Fares", 
        x = "Cost of Trip in Dollars",
        y = "Number of Trips"
      )
  })
   
   # Output leafletplot, for the mapping of pickup points in Chicago for
   # ride-sharing rides. mutate y so that there are columns called longitude and
   # latitude, essentially renaming the columns (pickup_centroid_longitude) and
   # (pickup_centroid_latitude) to those names (leaflet looks for columns named
   # latitude and longitude to plot the points).
  
   output$leafletpickupPlot <- renderLeaflet({
     y <- y %>% 
       mutate(longitude = as.numeric(pickup_centroid_longitude),
              latitude = as.numeric(pickup_centroid_latitude))
     
     # Subset the data based on the user-inputted desired hour of the day and
     # the day of the month they want to view.
     
     data <- subset(y, start_hour == input$hour)
     data <- subset(data, format_start_date == input$startdateleaflet)
     
     # Create a leaflet plot using leaflet(), addTiles() to add the map,
     # addCircleMarkers() to add the dots on the map (arguments radius and
     # opacity change the size of the dots and make the dots more transparent,
     # allowing for easier and more comfortable visualization of the data,
     # allowing one to notice multiple points that overlap). setView to set a
     # default view that the user sees, with a specific latitude, longitude, and
     # zoom level.
     
     map <- data %>%
       leaflet() %>%
       addTiles() %>%
       addCircleMarkers(radius = 0.5, opacity = 0.25) %>%
       setView(lat = 41.835, lng = -87.722, zoom = 10.25) 
   })

   # Output leafletplot, for the mapping of dropoff points in Chicago for
   # ride-sharing rides. mutate y so that there are columns called longitude and
   # latitude, essentially renaming the columns (pickup_centroid_longitude) and
   # (pickup_centroid_latitude) to those names (leaflet looks for columns named
   # latitude and longitude to plot the points).
   
   output$leafletdropoffPlot <- renderLeaflet({
     y <- y %>% 
       mutate(longitude = as.numeric(dropoff_centroid_longitude),
              latitude = as.numeric(dropoff_centroid_latitude))
     
     # Subset the data based on the user-inputted desired hour of the day and
     # the day of the month they want to view.
     
     data <- subset(y, start_hour == input$hour)
     data <- subset(data, format_start_date == input$startdateleaflet)
     
     # Create a leaflet plot using leaflet(), addTiles() to add the map,
     # addCircleMarkers() to add the dots on the map (arguments radius and
     # opacity change the size of the dots and make the dots more transparent,
     # allowing for easier and more comfortable visualization of the data,
     # allowing one to notice multiple points that overlap). setView to set a
     # default view that the user sees, with a specific latitude, longitude, and
     # zoom level.
     
     map <- data %>%
       leaflet() %>%
       addTiles() %>%
       addCircleMarkers(radius = 0.5, opacity = 0.25) %>%
       setView(lat = 41.835, lng = -87.722, zoom = 10.25) 
   })
   
   # output$faretipPlot creates a scatterplot in the "Tip Data" tab of the Shiny
   # App. ggplot in order to create the graph. The start_date_tips() is
   # reactive, which is why there is a set of parentheses around it. Map the x
   # axis to fare, y axis to tip, and color as format_start_date (different
   # color for different start date of the trip).

   output$faretipPlot <- renderPlot({
      ggplot(data = start_date_tips(), aes(x = fare, y = tip, color = format_start_date)) + 
       
       # geom_point() to get the points on the scatterplot, and set alpha to set
       # the transparency of the points. labs() in  order to name the title, x
       # axis, and y axis, and legend title.
       
       geom_point(alpha = 0.25) + 
       labs(
         title = "Relationship between Fare Price and Tip Amounts",
         x = "Fare Amount",
         y = "Tip Amount",
         color = "Ride Start Date")
   })
   
   # output$additionaltipPlot creates a scatterplot in the "Tip Data" tab of the
   # Shiny App. the plot graphs the relationship between additional charges and
   # tip amounts.ggplot in order to create the graph. The start_date_tips() is
   # reactive, which is why there is a set of parentheses around it. Map the x
   # axis to additional_charges, y axis to tip, and color as format_start_date
   # (different color for different start date of the trip).
   
   output$additionaltipPlot <- renderPlot({
     ggplot(data = start_date_tips(), aes(x = additional_charges, y = tip, color = format_start_date)) + 
       
       # geom_point() to get the points on the scatterplot, and set alpha to set
       # the transparency of the points. labs() in  order to name the title, x
       # axis, and y axis, and legend title.
       
       geom_point(alpha = 0.25) + 
       labs(
         title = "Relationship between Additional Charges and Tip Amounts",
         x = "Additional Charges",
         y = "Tip Amount", 
         color = "Ride Start Date")
   })
   
   # output$milestipPlot creates a scatterplot in the "Tip Data" tab of the
   # Shiny App. the plot graphs the relationship between miles driven and tip
   # amounts.ggplot in order to create the graph. The start_date_tips() is
   # reactive, which is why there is a set of parentheses around it. Map the x
   # axis to trip_miles, y axis to tip, and color as format_start_date
   # (different color for different start date of the trip).
   
   output$milestipPlot <- renderPlot({
     ggplot(data = start_date_tips(), aes(x = trip_miles, y = tip, color = format_start_date)) + 
       
       # geom_point() to get the points on the scatterplot, and set alpha to set
       # the transparency of the points. labs() in  order to name the title, x
       # axis, and y axis, and legend title.
       
       geom_point(alpha = 0.25) + 
       labs(
         title = "Relationship between Trip Distance (in Miles) and Tip Amounts",
         x = "Miles Driven",
         y = "Tip Amount", 
         color = "Ride Start Date")
   })
   
}

# Run the application 

shinyApp(ui = ui, server = server)