################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)

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
               column(12, plotlyOutput("rainplot"))
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
  
  output$rainplot <- renderPlotly({
    req(rv$data)
    
    validate(
      need(nrow(rv$data) > 0, "No data available for selected years")
    )
    
    p <- ggplot(rv$data,
                aes(x = month, y = inches, group = year,
                    text = paste("Year:", year,
                                 "<br>Rain:", inches, "inches"))) +
      geom_line(color = "gray70", alpha = 0.6)
    
    # Highlight selected year
    highlight_data <- rv$data %>%
      filter(year == input$highlight_year)
    
    p <- p +
      geom_line(data = highlight_data,
                color = "blue", size = 1.2)
    
    # Optional points
    if (input$show_points) {
      p <- p + geom_point(aes(text = paste("Year:", year,
                                           "<br>Rain:", inches)),
                          size = 2)
    }
    
    p <- p +
      labs(
        title = "Monthly Rainfall in Sewanee",
        subtitle = paste("Highlighted year:", input$highlight_year),
        x = "Month",
        y = "Rainfall (inches)"
      )
    ggplotly(p, tooltip = "text")
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