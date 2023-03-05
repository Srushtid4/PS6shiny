#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#load data
library (tidyverse)
library(plotly)
library(ggplot2)

airbnbData <- read_delim ("seattle_01.csv")

NRows=nrow(airbnbData)
NCols=ncol(airbnbData)
data <- head(airbnbData, n = 500)
selectedData<-head(data[c("address","reviews","bedrooms")],10)
udata<-unique(airbnbData$address)
roomType<-unique(airbnbData$room_type)
roomType_listings <- airbnbData %>%
  group_by(room_type,address) %>%
  summarize(listings_count = n())

cities = airbnbData %>%
  select(address) %>%
  unique()

listings_count <- airbnbData %>%
  group_by(address) %>%
  summarize(listings = n())


ui <- fluidPage(
  # Create a tabsetPanel with two tabs
  tabsetPanel(
    # About tab content
    tabPanel("About",
             tags$h1("This App uses", em("Seattle Airbnb Listings Data")),
             tags$h2("Airbnb Listings Data for Seattle and Surrounding Regions"),
             tags$p("This data set is from the site", strong("Kaggle"), "and displays the number of bedrooms, address, and reviews", em("for each of the listings")),
             tags$p("This Data set has", NRows, "rows & ", NCols, "columns"),
             tags$p(strong("Here is some sample data")),
             # Display data in a table
             tableOutput("about_table")
    ),
    # Plot tab content
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 # Add radio buttons
                 radioButtons("color_input", "Select color:",
                              choices = c("Red", "Purple", "Blue", "Green"),
                              selected = "Red"),
                 checkboxGroupInput("Cities",
                                    "Select Cities",
                                    choices = unique(airbnbData$address),
                                    selected = unique(airbnbData$address),
                 )
               ),
               mainPanel(
                 plotOutput("Plot"),
                 htmlOutput("PlotSummaryText")
               )
             )
    ),
    # Tables tab content
    tabPanel("Tables",
             sidebarLayout(
               
               # Define sidebar panel
               sidebarPanel(
                 # Display multiple sentences
                 tags$p("This panel gives listing counts and address for a selected Room Type"),
                 tags$h2("Room Type:"),
                 # Add radio buttons
                 radioButtons("radio_input", "Select type:",
                              choices = c(roomType),
                              selected = head(roomType, n=1))
               ),
               # Define main panel
               mainPanel(
                 # Add table
                 tableOutput("roomtype_listingdata"),
                 htmlOutput("tableSummaryText")
               )
             )
    )
    
  )
)

server <- function(input, output) {
  # Define a reactive object to manipulate the data for the table
  about_data <- reactive({
    filtered_data <- selectedData
    filtered_data
  })
  
  # Define an output to display the table for about page
  output$about_table <- renderTable({
    about_data()
  })
  
  output$Plot <- renderPlot({
    fitlered_listing <- listings_count %>%
      filter(address %in% input$Cities)
    ggplot(fitlered_listing, aes(x = listings, y = address)) + #like groupby, but you don't need to use it twice
      geom_bar(stat = "identity", width = 0.6, fill = input$color_input) +
      labs(title = "Number of Airbnb Listings by City in Seattle Area",
           x = "Number of Listings",
           y = "Cities")
  })
  
  output$PlotSummaryText <- renderText({
    fitlered_listing <- listings_count %>%
      filter(address %in% input$Cities)
    HTML("Count of Selected Addresses : ", "<b>",length(input$Cities), "</b>", "Total Listings are ", "<b>",sum(fitlered_listing$listings), "</b>")
    
  })
  
  output$roomtype_listingdata <- renderTable({
    roomslection = input$radio_input;
    # Filter data based on radio button input
    filtered_data <- roomType_listings %>%
      filter (room_type == roomslection)
  })
  
  output$tableSummaryText <- renderText({
    roomslection = input$radio_input;
    filtered_data <- roomType_listings %>%
      filter (room_type == roomslection)
    HTML("For Room Type", "<b>",roomslection, "</b>", " Number of Listings : ", "<b>",(sum(filtered_data$listings_count)),"</b>")
  })
  
  
}

shinyApp(ui, server)