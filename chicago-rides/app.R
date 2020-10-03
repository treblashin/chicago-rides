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
           tabPanel(title = "Pickup Locations", 
                    leafletOutput("leafletpickupPlot")),
           tabPanel(title = "Dropoff Locations", 
                    leafletOutput("leafletdropoffPlot")
      h6('Adjust the slider on the map to see the number of rides at specific locations
         throughout the day. Use the check box to select the date of interest.
         Per the data, darker blue dots show locations throughout the day with a high volume of rides.
         As the data shows, on both days, there are "hot spots" where rides are constantly
         being called to—one such location are the airports in Chicago—even during late hours
         like 3 and 4 AM, rides are still a constant there.')
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
      h6("Though it is difficult to see in other graphics, the histogram indicates 
         there is a significant difference in the number of rides started on 
         December 4th, 2018 and December 25th, 2018. The number of rides on 
         December 4th is over double the amount of rides on December 25th.
         A possible explanation (especially as both days are on a Tuesday, so
         there is no day of the week variation or bias) is that people are less
         likely to be traveling on Christmas day."), 
      br(),
      plotOutput("tipfreqPlot"),
      h6('The difference in number of rides throughout December 4th and December 25th 
         can be seen even more clearly in the line graph above. At every hour of the day,
         there are more rides recorded for December 4th compared to December 25th.
         This graph shows interesting patterns on December 4th and December 25th throughout
         the day. On December 25th, for instance, the line is relatively smooth—rides increase
         in volume from 12AM to 10-11PM, where there is a slight dip as times go back to 12AM.
         Peak rideshare calls are in the evening.
         On the other hand, on December 4th, there are significant "peaks" that can be seen throughout
         the day. December 4th is a Tuesday, and unlike the 25th, which is a Federal Holiday, many people
         are going to work. This could be a major reason why there are spikes in ride share calls in hours
         7-8AM and 5-6 PM, as people are going to work in the morning and are coming from work in the 
         afternoon/evening. The data is even more suggestive of this because the 25th does not have comparable
         spikes in rides at these times.')
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
         ),
         
         h6("These distribution tabs show the distribution of number of miles in a trip, 
              and the total fares of a trip. As the distributions both show, there are large
            clusters of trip distances within 0 and 5 miles, and trip fares of 0 and 20 
            dollars. The bins slider can be adjusted to see finer details in then numbers 
            as well. Overall, the data is skewed right for both dates.")
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
        ),
      h6("Tip data correlations confirm the natural correlation one might expect  
         between fares, additional charges, miles and tips. For fare distance
         and trip miles, fare  distance/trip mile amounts and amount tipped seem 
         are more spread out. On the other hand, additional charges has a high
         volume of points on the line where tip amount equals 0. This indicates
         that riders may not tip as much if additional charges are charged to 
         the ride in question. If one is charged ten dollars in additional charges,
         they might be less likely to tip the ride in addition to paying those
         additional charges.")
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
      h4("Other Variables/Demographic Information"),
      h6("The original dataset unfortunately does not contain demographic information,
         including but not limited to driver/rider gender, age, race/ethnicity, etc."),
      br(),
      h4("Disclaimer"),
      h6("The Shiny App may take a while to load due to the volume of data points."),
      br(),
      h4("Sources"),
      h6("I first stumbled on this data from sifting through an email newsletter called
         Data is Plural by Jeremy Singer Vine. Data is plural sends out a number of interesting 
         datasets every week to those subscribed to the newsletter. More information 
         regarding Data is Plural can be found", 
         a("here.", href = "https://tinyletter.com/data-is-plural")),
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
  
# Create a plot called tipfreqPlot that shows the number of rides called per
# hour on the different days. Created two datasets called a and b that contain
# information regarding each date, and then created a "Date" variable for both,
# that allows you to bind rows but have a differentiating variable. You use this
# differentiating variable to color the lines by Date, and then create a legend
# that shows which line is which date.
  
output$tipfreqPlot <- renderPlot({
  
  a <- y %>% 
    filter(start_date == "2018-12-04") %>% 
    select(start_hour, start_date) %>% 
    group_by(start_hour) %>% 
    count()
  b <- y %>% 
    filter(start_date == "2018-12-25") %>% 
    select(start_hour) %>% 
    group_by(start_hour) %>% 
    count()
  
  c <- b %>%  mutate(Date = "December 25th, 2018") %>%
    bind_rows(a %>%
                mutate(Date = "December 4th, 2018"))
  
# Took some advice from Healy's chapters and used a geom_line() plot to show the
# distribution of rides by hour for each date. As the data shows, there are
# interesting patterns that emerge from these times.
    
  ggplot(data = c, aes(x = start_hour, y = n, color = Date)) + 
    geom_line() + 
    labs(title = "Number of Rides Start at each Hour of the Day",
         subtitle = "Chicago, IL",
         caption = 'Source: Chicago Data Portal') + 
    xlab(label = "Trip Start Hour (24 Hr Time)") + 
    ylab(label = "Number of Rides")
  
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
