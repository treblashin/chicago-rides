library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(leaflet)

y <- read_csv("www/y.csv")

# Define UI for application that draws a graph
ui <- navbarPage(
   
   # Application title
   title = "Chicago Ride-Sharing Data",
   theme = shinytheme("yeti"),
   
   # Sidebar with a slider input for number of bins 
   tabPanel(
     title = "Home", 

       # Show a plot of the generated distribution
       mainPanel(
         h4("Project Home"),
         h6("The goal of this project is to look at ride-sharing data in the city of Chicago, specifically at rides with a 
            start date of December 4th, 2018 and December 25th, 2018, to compare ride-sharing on the Christmas holiday and 
            a non-holiday day in December that is the same day of the week as Christmas."),
         br(),
         h4("Methodology"),
         h6("The original dataset contains over 17 million rows of data, spanning dates from November to December 2018. Due
            to the volume of data, I decided to look at specific dates —— in particular, December 4th and December 25th.
            When selecting for rows that have a start date of December 4th or December 25th, there are still over 300 thousand
            rows. For the sake of clarity in the plots and graphs used in this app, I used a random sample of 25 thousand rows.
            This provides enough data points to provide a reasonable representation of the data, while simultaneously making 
            certain plots and graphs easier to read."),
         br(),
         h4("Disclaimer"),
         h6("The Shiny App may take a while to load due to the volume of data points.")
       )
   ),

  tabPanel(
     title = "Basic Information",
       # Show a plot of the generated distribution
     h4("Basic Information"),
     sidebarLayout(
       sidebarPanel(
         checkboxGroupInput(inputId = "startdatebasic",
                            label = "Select Ride Start Date:",
                            choices = c("December 04, 2018", "December 25, 2018"),
                            selected = "December 04, 2018"),
         sliderInput(inputId = "basicbins",
                     label = "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
       ),
       mainPanel(
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

  tabPanel(
    title = "Tip Data",
    h4("Comparing Tip Amounts with Ride Variables"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "startdatetips",
                           label = "Select Ride Start Date:",
                           choices = c("December 04, 2018", "December 25, 2018"),
                           selected = c("December 04, 2018", "December 25, 2018"))
      ),
      mainPanel(
        tabsetPanel(
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

  tabPanel(
    title = "Trip Locations",
    h4("Ride Share Pickup Locations"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "hour",
                    label = "Hour of Day:",
                    min = 0,
                    max = 23,
                    value = 0,
                    step = 1,
                    sep = "", 
                    animate = TRUE),
        checkboxGroupInput(inputId = "startdateleaflet",
                           label = "Select Ride Start Date:",
                           choices = c("December 04, 2018", "December 25, 2018"),
                           selected = c("December 04, 2018", "December 25, 2018"))
      ),
      mainPanel(
        leafletOutput("leafletPlot")
      )
    )
  ),
  tabPanel(
    title = "Acknowledgments/Sources",
    h4("Acknowledgments/Sources"),
    mainPanel(
      h4("Sources"),
      h6("Data from the city of Chicago data portal can be found", a("here.", href="https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips/m6dm-c72p/data"),
         "The GitHub repository can be found",
         a("here.", href="https://github.com/treblashin/chicago-rides")),
      br(), 
      h4("Thank You"),
      h6("Thank you Preceptor, Albert, Jacob, and everyone in Gov 1005 for an amazing semester — 
         this project would not have been possible without you!")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  start_date_tips <- reactive({
    req(input$startdatetips)
    filter(y, format_start_date %in% input$startdatetips)
  })
  
  start_date <- reactive({
    req(input$startdatebasic)
    filter(y, format_start_date %in% input$startdatebasic)
  })
  
  output$milesDist <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = start_date(), aes(x = trip_miles)) + 
      geom_histogram(bins = input$basicbins) + 
      labs(
        title = "Distribution of Trip Miles", 
        x = "Length of Trip in Miles",
        y = "Number of Trips"
      )
  })
  
  output$faresDist <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = start_date(), aes(x = fare)) + 
      geom_histogram(bins = input$basicbins) + 
      labs(
        title = "Distribution of Trip Fares", 
        x = "Cost of Trip in Dollars",
        y = "Number of Trips"
      )
  })
    
   output$leafletPlot <- renderLeaflet({
     y <- y %>% 
       select(pickup_centroid_longitude, pickup_centroid_latitude, start_hour, start_date, format_start_date) %>% 
       mutate(longitude = as.numeric(pickup_centroid_longitude),
              latitude = as.numeric(pickup_centroid_latitude))
     data <- subset(y, start_hour == input$hour)
     data <- subset(data, format_start_date == input$startdateleaflet)
     
     map <- data %>%
       leaflet() %>%
       addTiles() %>%
       addCircleMarkers(radius = 0.5, opacity = 0.25) %>%
       setView(lat = 41.835, lng = -87.722, zoom = 10.25) 
   })

   output$faretipPlot <- renderPlot({
      ggplot(data = start_date_tips(), aes(x = fare, y = tip, color = format_start_date)) + 
       geom_point(alpha = 0.5) + 
       labs(
         title = "Relationship between Fare Price and Tip Amounts",
         x = "Fare Amount",
         y = "Tip Amount",
         color = "Ride Start Date")
   })
   
   output$additionaltipPlot <- renderPlot({
     ggplot(data = start_date_tips(), aes(x = additional_charges, y = tip, color = format_start_date)) + 
       geom_point(alpha = 0.25) + 
       labs(
         title = "Relationship between Additional Charges and Tip Amounts",
         x = "Additional Charges",
         y = "Tip Amount", 
         color = "Ride Start Date")
   })
   
   output$milestipPlot <- renderPlot({
     ggplot(data = start_date_tips(), aes(x = trip_miles, y = tip, color = format_start_date)) + 
       geom_point(alpha = 0.5) + 
       labs(
         title = "Relationship between Trip Distance (in Miles) and Tip Amounts",
         x = "Miles Driven",
         y = "Tip Amount", 
         color = "Ride Start Date")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)