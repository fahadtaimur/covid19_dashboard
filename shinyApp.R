# LIBRARIES ----
library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)
library(shinythemes)
library(shinyjs)
library(readr)
library(lubridate)
library(tidyquant)
library(ggthemes)
library(shinyWidgets)
library(DT)
library(leaflet)
library(leafpop)
library(purrr)


source(file = "00_analysis/scripts.R")

data <- get_data()

# Define UI for application that draws a histogram
ui <- navbarPage(
  
  
  # title
  title = "COVID19 - Dashboard",
  inverse = FALSE,
  collapsible = TRUE,
  
  theme = shinytheme("sandstone"),
  
  tabPanel(
  # 1.0 Header ----
  title = "Country Profile",
  
  # 2.0 APPLICATION UI ----
  # Selection filters
  div(
    class = "container",
    column(
      width = 4, 
      # wellPanel containing list of countries and action button
      shiny::wellPanel(
        # country picker
        pickerInput(inputId = "country_selection",
                    label = "Select a Country",
                    choices = data$Country.Region %>% unique(),
                    multiple = FALSE,
                    selected = "US",
                    options = pickerOptions(
                      actionsBox = FALSE,
                      liveSearch = TRUE,
                      size = 10)),
        
        br(),
        
        # type picker
        pickerInput(inputId = "type_selection",
                    label = "Choose an Outcome",
                    choices = data$type %>% unique(),
                    multiple = FALSE,
                    selected = "confirmed",
                    options = pickerOptions(
                      actionsBox = FALSE,
                      liveSearch = TRUE,
                      size = 10)),
        
        # Date 1 Picker
        dateInput(inputId = "date1", 
                  label = "Chart beginning date", 
                  value = data$date %>% head(1)),
        
        # Date 2 Picket
        dateInput(inputId = "date2", 
                  label = "Chart End Date", 
                  value = data$date %>% tail(1)),
        
        # Analyze button
        actionButton(inputId = "analyze", label = "Submit", icon("download")),
        verbatimTextOutput(outputId = "outcome_text")
        
        
        
      )
    ),
    
    column(
      width = 8,
      
      # encapsulate the plot
      div(
        # Text above plot
        div(class = "text-center", h4("COVID19 - Time Series / Data.Table")),
        # Plot
        tabsetPanel(id = "global",
                    
                    tabPanel(title = "Plot",
                             plotlyOutput(outputId = "plotly_plot")),
                    
                    tabPanel(title = "Data.Table",
                             dataTableOutput("Table"))
                    

                    ))
        
      )
    
    ),
  
  
  # 3.0 Text Summary ----
  div(
    class = "container",
    id = "summary",
    column(
      width = 12,
      div(
        class = "panel",
        div(class = "panel-header", h4("Highlights")),
        div(
          class = "panel-body"
          #textOutput(outputId = "analyst_commentary")
        )
      )
    )
  )
  
  ),
  
  # MAP Display ----

  tabPanel(
    title = "Map",
    leafletOutput("map", width = "100%", height = 650)
        
      )
 
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Event Reactive for Country Selected
  country_reactive <- eventReactive(eventExpr = input$analyze, {
    input$country_selection
  }, ignoreNULL = FALSE)
  
  
  # Event Reactive for Outcome
  outcome_reactive <- eventReactive(eventExpr = input$analyze, {
    input$type_selection
  }, ignoreNULL = FALSE)
  
  # Date1
  date1_reactive <- eventReactive(eventExpr = input$analyze, {
    input$date1
  }, ignoreNULL = FALSE)
  
  # Date2
  date2_reactive <- eventReactive(eventExpr = input$analyze, {
    input$date2
  }, ignoreNULL = FALSE)
  
  
  # Use this country_reactive and outcome_reactive to reactively generate the plot
  output$plotly_plot <- renderPlotly({
    plotly_function(data, country = country_reactive(),
                              outcome = outcome_reactive(), 
                              date1 = date1_reactive(),
                              date2 = date2_reactive()) 
  })
  
  # Table
  output$Table <- renderDataTable(
    datatable(data %>%
                filter(date == date %>% tail(1)) %>%
                group_by(Country.Region, type) %>%
                summarise(Cases = sum(number_of_cases)) %>%
                ungroup() %>%
                select(Country = Country.Region, 
                       Outcome = type,
                       Cases) %>%
                spread(key = Outcome, value = Cases) %>%
                rename(Confirmed = confirmed, Deaths = deaths), 
                rownames = FALSE,
                options = list(searching = TRUE,
                               pageLength = 8,
                               lengthMenu = c(5, 10), 
                               scrollX = T))
  )
  
  # Map
  output$map <- renderLeaflet({
    leaflet_map_plot(data = data)
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
