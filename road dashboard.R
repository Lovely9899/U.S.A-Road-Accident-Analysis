library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

# Load data
data <- read_excel(file.choose())

# UI
iu <- fluidPage(
  titlePanel("Road Accidents Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select Location:", choices = c("All", unique(data$Location))),
      selectInput("severity", "Select Severity:", choices = c("All", unique(data$Severity))),
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(data$Date),
                     end = max(data$Date))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 fluidRow(
                   column(6, plotOutput("severityPlot")),
                   column(6, plotOutput("vehiclePlot"))
                 )
        ),
        tabPanel("Map",
                 leafletOutput("accidentMap", height = 600)
        ),
        tabPanel("Table",
                 DTOutput("dataTable")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  filteredData <- reactive({
    df <- data
    if (input$location != "All") {
      df <- df %>% filter(Location == input$location)
    }
    if (input$severity != "All") {
      df <- df %>% filter(Severity == input$severity)
    }
    df <- df %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    df
  })
  
  output$severityPlot <- renderPlot({
    filteredData() %>%
      count(Severity) %>%
      ggplot(aes(x = Severity, y = n, fill = Severity)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +  # <-- Add this line
      theme_minimal() +
      labs(title = "Accidents by Severity", y = "Count")
  
  
  })
  
  output$vehiclePlot <- renderPlot({
    filteredData() %>%
      count(Vehicle_Type) %>%
      ggplot(aes(x = Vehicle_Type, y = n, fill = Vehicle_Type)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.5) +  # <-- Add this line
      theme_minimal() +
      labs(title = "Accidents by Vehicle Type", y = "Count")
  
  
  })
  
  output$accidentMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       popup = ~paste("Date:", Date, "<br>Severity:", Severity),
                       color = ~case_when(
                         Severity == "Fatal" ~ "red",
                         Severity == "Serious" ~ "orange",
                         TRUE ~ "green"
                       ),
                       radius = 5,
                       fillOpacity = 0.7)
  })
  
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = iu, server = server)
