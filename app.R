

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(ggiraph)
library(reshape2)
library(rnaturalearth)
library(reactable)
library(sf)
library(plotly)
library(rgeos)
library(dplyr)
library(tidyverse)
library(echarts4r)
library(gapminder)
library(rsconnect)
library(devtools)

# Reading in Political Terror Scale data (which I have saved in my Github repo)
data <- data.table::fread("https://raw.githubusercontent.com/akrishnamurthy97/Political-Terror-Scale/main/PTS-2020.csv")

# Reshaping data into long form
data <-
  reshape2::melt(data,
                 measure.vars = c("PTS_A", "PTS_H", "PTS_S"),
                 variable.name = "Report Type")

# Changing names of factors (to represent full sources of reports)
levels(data$`Report Type`) <- c("Amnesty International", "Human Rights Watch", "U.S. State Department")

# Turning regions into a factor, adjusting the factor levels, and getting a list of unique regions
data$Region <- as.factor(data$Region)
levels(data$Region) <-
  c(
    "East Asia and Pacific",
    "Europe and Central Asia",
    "Latin America and Caribbean",
    "Middle East and North Africa",
    "North America",
    "South Asia",
    "Sub-Saharan Africa"
  )
regions <- prepend(sort(unique(as.character(data$Region))), "All")



ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(sidebarMenu(
                      menuItem("World Map", tabName = "worldmap"),
                      menuItem("Charts", tabName = "charts")
                    )),
                    dashboardBody(tabItems(
                      tabItem(tabName = "worldmap",
                              uiOutput("map_selections"),
                              fluidRow(
                                box("World Map", echarts4rOutput("map", height = 800), width = 12)
                              )),
                      tabItem(tabName = "charts",
                              uiOutput("chart_selections"),
                              fluidRow(tabBox(
                                tabPanel("Scores by Report", plotlyOutput("chart", height = 800), width = 12),
                                tabPanel("Trends over Time", plotlyOutput("trends", height = 800), width = 12),
                                tabPanel("Trends over Time (Smoothed)",
                                         uiOutput("smoothness"),
                                         plotlyOutput("trends_smoothed", height = 800), width = 12),
                                width = 12
                              )))
                    )))

server <- function(input, output) {
  
  
  # generate UI for the world map
  output$map_selections <- renderUI({
    tagList(
      selectInput(
        "report",
        "Report Type",
        choices =
          c(
            "Amnesty International" = "Amnesty International",
            "Human Rights Watch" = "Human Rights Watch",
            "U.S. State Department" = "U.S. State Department"
          )
      )
    )
    
  })
  
  
  # filter PTS data for the world map based on selections
  get_PTS_data <- reactive({
    
    req(input$report)
    
    chart_data <- data %>%
      filter(`Report Type` == input$report)
    
    return(chart_data)
    
  })
  
  
  # generate world map
  output$map <- renderEcharts4r({
    
    world_map <- get_PTS_data()
    
    world_map %>%
      group_by(Year) %>%
      e_charts(Country, timeline = TRUE) %>%
      e_map(value) %>%
      e_visual_map(value) %>%
      e_title("Political Terror Scale by Country and Year", left = "center") %>%
      e_timeline_opts(
        playInterval = 200
      ) %>%
      e_tooltip(
        trigger = "item",
        formatter = e_tooltip_choro_formatter()
      )
    
  })
  
  
  # generate UI for chart tab
  output$chart_selections <- renderUI({
    tagList(selectInput(
      "region",
      "Choose a region:",
      choices = regions,
      selected = "All"
      ),
      radioButtons(
        "include_NAs",
        "Include countries for which no score is available:",
        choices = c("Yes" = TRUE, "No" = FALSE),
        selected = FALSE
      ),
      sliderInput(
        "year_range",
        "Select time range to be displayed:",
        min = 1976,
        max = 2019,
        value = c(2010, 2019),
        sep = "",
        dragRange = TRUE
      )
    )
  })
  
  chart_data_values <- reactive({
    
    req(input$region)
    req(input$include_NAs)
    
    chart_data_values <- data %>%
      {
        if (input$region != "All")
          filter(., Region == input$region) else filter(.)
      } %>%
      {
        if (input$include_NAs == FALSE)
          filter(., !is.na(value)) else filter(.)
      }
    
    return(chart_data_values)
    
  })
  
  # generate stacked bar chart
  output$chart <- renderPlotly({
    
    req(input$year_range)
    
    chart_data <- chart_data_values() %>%
      group_by(Year, `Report Type`, value) %>%
      summarise("Count" = n())
    
    graph <-
      ggplot(chart_data,
             aes(fill = value,
                 x = Year,
                 y = Count)) + geom_bar(position = "fill", stat = "identity") + facet_wrap( ~ `Report Type`) + 
      xlim(input$year_range[1], input$year_range[2])
    
    return(ggplotly(graph))
    
  })
  
  # generate trend lines chart
  output$trends <- renderPlotly({
    
    req(input$year_range)
    
    chart_data <- chart_data_values() %>%
      group_by(Year, `Report Type`) %>%
      summarise("Mean PTS Score" = mean(value, na.rm = TRUE))
    
    graph <-
      ggplot(chart_data,
             aes(x = Year,
                 y = `Mean PTS Score`,
                 group = `Report Type`,
                 color = `Report Type`)) + geom_line() + geom_point() + xlim(input$year_range[1], input$year_range[2])
    
    return(graph)
    
  })
  
  # get user's desired smoothness
  output$smoothness <- renderUI({
    
    tagList(
      sliderInput(
        "smoothness",
        "Choose smoothness of fit:",
        min = 0.60,
        max = 1,
        value = 0.75,
        step = 0.05
      )
    )
    
  })
  
  output$trends_smoothed <- renderPlotly({
    
    req(input$year_range)
    req(input$smoothness)
    
    chart_data <- chart_data_values() %>%
      group_by(Year, `Report Type`) %>%
      summarise("Mean PTS Score" = mean(value, na.rm = TRUE))
    
    graph <-
      ggplot(
        chart_data,
        aes(
          x = Year,
          y = `Mean PTS Score`,
          group = `Report Type`,
          color = `Report Type`
        )
      ) + geom_smooth(
        method = "loess",
        span = input$smoothness,
        alpha = 0.3,
        size = 0.7
      ) + xlim(input$year_range[1], input$year_range[2])
    
    return(graph)
    
  })
  
  # reactable table with expandable rows for list of countries
  
}

shinyApp(ui = ui, server = server)
