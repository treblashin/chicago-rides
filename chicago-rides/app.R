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
         h3("Project Home"),
         h5("The goal of this project is to look at ride-sharing data in the city of Chicago, specifically at rides with a 
            start date of December 4th, 2018 and December 25th, 2018, to compare ride-sharing on the Christmas holiday and 
            a non-holiday day in December that is the same day of the week as Christmas."),
         h4("Disclaimer"),
         h6("The Shiny App will take a while to load due to the volume of data points."),
         h4("Sources"),
         h6("Data from the city of Chicago data portal can be found", a("here.", href="https://data.cityofchicago.org/Transportation/Transportation-Network-Providers-Trips/m6dm-c72p/data"),
            "The GitHub repository can be found",
            a("here.", href="https://github.com/treblashin/chicago-rides")), 
         h4("Thank You"),
         h6("Thank you Preceptor, Albert, Jacob, and Gov 1005 for an amazing semester — this project would not have been possible without you!")
       )
   ),

  tabPanel(
     title = "Basic Information",
       # Show a plot of the generated distribution
       mainPanel(
         h2("Basic Information")
       )
  ),

  tabPanel(
    title = "Tip Data",
    h4("Comparing Tip Amounts with Ride Variables"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "startdate",
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
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  start_date <- reactive({
    req(input$startdate)
    filter(y, format_start_date %in% input$startdate)
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
      ggplot(data = start_date(), aes(x = fare, y = tip, color = format_start_date)) + 
       geom_point(alpha = 0.5) + 
       labs(
         title = "Relationship between Fare Price and Tip Amounts",
         x = "Fare Amount",
         y = "Tip Amount",
         color = "Ride Start Date")
   })
   
   output$additionaltipPlot <- renderPlot({
     ggplot(data = start_date(), aes(x = additional_charges, y = tip, color = format_start_date)) + 
       geom_point(alpha = 0.5) + 
       labs(
         title = "Relationship between Additional Charges and Tip Amounts",
         x = "Additional Charges",
         y = "Tip Amount", 
         color = "Ride Start Date")
   })
   
   output$milestipPlot <- renderPlot({
     ggplot(data = start_date(), aes(x = trip_miles, y = tip, color = format_start_date)) + 
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