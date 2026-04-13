################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

rm(list = ls())

load('sewanee_weather.rds')

# Clean data
sewanee_rain$year <- as.numeric(as.character(sewanee_rain$year))
sewanee_rain$inches <- as.numeric(as.character(sewanee_rain$inches))

sewanee_rain$month <- factor(
  sewanee_rain$month,
  levels = c("January","February","March","April","May","June",
             "July","August","September","October","November","December")
)

################################################################################
# UI
################################################################################

ui <- fluidPage(
  titlePanel('Rain'),
  helpText('Rain in Sewanee & Mesification'),
  
  tabsetPanel(
    
    tabPanel("Time Series",
             fluidRow(
               column(4,
                      sliderInput(
                        inputId = 'year', 
                        label = 'Select years',
                        min = min(sewanee_rain$year, na.rm = TRUE), 
                        max = max(sewanee_rain$year, na.rm = TRUE),
                        value = range(sewanee_rain$year, na.rm = TRUE)
                      ),
                      
                      checkboxInput("show_points", "Show points", TRUE),
                      
                      selectInput("highlight_year", "Highlight a year:",
                                  choices = unique(sewanee_rain$year),
                                  selected = min(sewanee_rain$year))
               )
             ),
             
             fluidRow(
               column(12, plotOutput("rainplot"))
             )
    ),
    
    tabPanel("Data Viewer",
             DTOutput('dt1')
    )
  )
)

################################################################################
# SERVER
################################################################################

server <- function(input, output) {
  
  rv <- reactiveValues(data = NULL)
  
  observe({
    req(input$year)
    
    rv$data <- sewanee_rain %>%
      filter(year >= input$year[1],
             year <= input$year[2],
             !is.na(inches))
  })
  
  #####################################
  # Plot
  #####################################
  
  output$rainplot <- renderPlot({
    req(rv$data)
    
    validate(
      need(nrow(rv$data) > 0, "No data available for selected years")
    )
    
    # base plot
    p <- ggplot(rv$data,
                aes(x = month, y = inches, group = year)) +
      geom_line(color = "gray70", alpha = 0.6)
    
    # highlight selected year
    highlight_data <- rv$data %>%
      filter(year == input$highlight_year)
    
    p <- p +
      geom_line(data = highlight_data,
                color = "blue", size = 1.2)
    # optional points
    if (input$show_points) {
      p <- p + geom_point(size = 1.5, alpha = 0.7)
    }
    
    p +
      labs(
        title = "Monthly Rainfall in Sewanee",
        subtitle = paste("Highlighted year:", input$highlight_year),
        x = "Month",
        y = "Rainfall (inches)"
      ) 
  })
  
  #####################################
  # Table
  #####################################
  
  output$dt1 <- renderDT({
    req(rv$data)
    rv$data
  })
}

################################################################################

shinyApp(ui = ui, server = server)