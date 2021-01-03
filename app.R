

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
library(scales)

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

# Get full list of countries
countries <- sort(unique(as.character(data$Country)))


ui <- dashboardPage(dashboardHeader(title = "PTS Dashboard"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("World Map", tabName = "worldmap"),
                      menuItem("Global Overview", tabName = "charts"),
                      menuItem("Country Drilldown", tabName = "country")
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
                              ))),
                      tabItem(tabName = "country",
                              uiOutput("country_selections"),
                              fluidRow(tabBox(
                                tabPanel("Country Scores over Time", plotlyOutput("country_chart", height = 800), width = 12),
                                tabPanel("Summary Table", reactableOutput("country_table", height = 800), width = 12),
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
    
    req(input$report)
    
    world_map <- get_PTS_data()
    
    world_map %>%
      group_by(Year) %>%
      e_charts(Country, timeline = TRUE) %>%
      e_map(value) %>%
      e_visual_map(value) %>%
      e_title(paste0("Political Terror Scale by Country and Year (", input$report, ")"), left = "center") %>%
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
    
    chart_data_values$value <- as.factor(chart_data_values$value)
    
    return(chart_data_values)
    
  })
  
  # generate stacked bar chart
  output$chart <- renderPlotly({
    
    req(input$year_range)
    req(input$region)
    
    chart_data <- chart_data_values() %>%
      group_by(Year, `Report Type`, value) %>%
      summarise("Count" = n())
    
    graph <-
      ggplot(chart_data,
             aes(fill = value,
                 x = Year,
                 y = Count,
                 text = paste(
                   "<br> Region: ",
                   input$region,
                   "<br> Year: ",
                   Year,
                   "<br> Count: ",
                   Count
                 ))) + geom_bar(position = "fill", stat = "identity") + facet_wrap( ~ `Report Type`) + 
      scale_x_continuous(
        limits = c(input$year_range[1] - 1, input$year_range[2] + 1),
        labels = scales::number_format(accuracy = 1, big.mark = ""),
        breaks = scales::pretty_breaks()
      ) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1L)) + 
      theme(axis.title.y = element_blank())
    
    return(ggplotly(graph, tooltip = "text"))
    
  })
  
  # generate trend lines chart
  output$trends <- renderPlotly({
    
    req(input$year_range)
    req(input$region)
    
    chart_data <- chart_data_values() %>%
      mutate(value = as.numeric(value)) %>%
      group_by(Year, `Report Type`) %>%
      summarise("Mean PTS Score" = mean(value, na.rm = TRUE))
    
    graph <-
      ggplot(chart_data,
             aes(x = Year,
                 y = `Mean PTS Score`,
                 group = `Report Type`,
                 color = `Report Type`,
                 text = paste(
                   "<br> Region: ",
                   input$region,
                   "<br> Year: ",
                   Year,
                   "<br> Mean PTS Score: ",
                   formatC(`Mean PTS Score`, digits = 2, format = "f")
                 ))) + geom_line() + geom_point() + 
      scale_x_continuous(
        limits = c(input$year_range[1] - 1, input$year_range[2] + 1),
        labels = scales::number_format(accuracy = 1, big.mark = ""),
        breaks = scales::pretty_breaks()
      ) +
      scale_y_continuous(labels = number_format(accuracy = 0.1)) + ylab("Mean PTS Score")
    
    return(ggplotly(graph, tooltip = "text"))
    
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
  
  # generate smoothed chart
  output$trends_smoothed <- renderPlotly({
    
    req(input$year_range)
    req(input$smoothness)
    req(input$region)
    
    chart_data <- chart_data_values() %>%
      mutate(value = as.numeric(value)) %>%
      group_by(Year, `Report Type`) %>%
      summarise("Mean PTS Score" = mean(value, na.rm = TRUE))
    
    graph <-
      ggplot(
        chart_data,
        aes(
          x = Year,
          y = `Mean PTS Score`,
          group = `Report Type`,
          color = `Report Type`,
          text = paste(
            "<br> Region: ",
            input$region,
            "<br> Year: ",
            Year,
            "<br> Mean PTS Score: ",
            `Mean PTS Score`
          )
        )
      ) + geom_point() +
      geom_smooth(
        method = "loess",
        span = input$smoothness,
        alpha = 0.3,
        size = 0.7
      ) + 
      scale_x_continuous(
        limits = c(input$year_range[1] - 1, input$year_range[2] + 1),
        labels = scales::number_format(accuracy = 1, big.mark = ""),
        breaks = scales::pretty_breaks()
      ) + scale_y_continuous(labels = number_format(accuracy = 0.1))
    
    return(ggplotly(graph, tooltip = "text"))
    
  })
  
  # generate UI for Country tab
  output$country_selections <- renderUI({
    tagList(
      selectInput(
        "country",
        "Select a country: ",
        choices = countries,
        selected = "China"
      ),
      radioButtons(
        "show_regional_or_world_averages",
        "Show regional or world average PTS scores by report type: ",
        choices = c("Region" = "Region", "World" = "World", "None" = "None"),
        selected = "None"
      ),
      sliderInput(
        "year_range_country",
        "Select time range to be displayed:",
        min = 1976,
        max = 2019,
        value = c(2010, 2019),
        sep = "",
        dragRange = TRUE
      )
    )
    
  })
  
  # enerate data for country chart
  country_data_values <- reactive({

    req(input$country)
    req(input$show_regional_or_world_averages)

    country_data_values <- data
    selected_country_region <-
      as.character(unique(country_data_values$Region[match(input$country, country_data_values$Country)]))

    if (input$show_regional_or_world_averages == "Region") {
      country_data_values <- filter(country_data_values, Region == selected_country_region)
    }

    return(country_data_values)

  })
  
  # generate plotly chart
  output$country_chart <- renderPlotly({
    
    req(input$year_range_country)
    req(input$country)
    req(input$show_regional_or_world_averages)
    
    chart_data <- country_data_values() %>%
      group_by(Year, `Report Type`) 
    
    more_countries <- chart_data %>%
      group_by(Year, `Report Type`) %>%
      summarise("Mean PTS Score" = mean(value, na.rm = TRUE))
    
    graph <-
      ggplot() + geom_point(
        data = chart_data[chart_data$Country == input$country,], # limiting data to selected country
        aes(
          x = Year,
          y = value,
          group = `Report Type`,
          color = `Report Type`,
          text = paste(
            "<br> Country: ",
            Country,
            "<br> Region: ",
            Region,
            "<br> Year: ",
            Year,
            "<br> PTS Score: ",
            formatC(value, digits = 0, format = "f")
          )
        ),
        position = position_jitter(h = 0.05, w = 0.05),
        shape = 15,
        alpha = 0.8,
        size = 3
      ) + 
      scale_x_continuous(
        limits = c(input$year_range_country[1] - 1, input$year_range_country[2] + 1),
        labels = scales::number_format(accuracy = 1, big.mark = ""),
        breaks = scales::pretty_breaks()
      ) +
      scale_y_continuous(labels = number_format(accuracy = 0.1)) + ylab("PTS Score")
    
    if (input$show_regional_or_world_averages != "None") { # adding in regional or world averages
      graph <-
        graph + geom_smooth(
          data = more_countries,
          aes(
            x = Year,
            y = `Mean PTS Score`,
            group = `Report Type`,
            color = `Report Type`,
            text = paste(
              "<br> Report Type: ",
              `Report Type`,
              "<br> Year: ",
              Year,
              "<br> PTS Score: ",
              formatC(`Mean PTS Score`, digits = 2, format = "f")
            )
          ),
          size = 1,
          method = "loess",
          se = TRUE
        )
    }
    
    return(ggplotly(graph, tooltip = "text"))
    
  })
  
  # generate summary table
  summary_table <- reactive({
    
    req(input$country)
    req(input$year_range_country)
    
    table_data <- country_data_values() %>%
      filter(Country == input$country, Year >= input$year_range_country[1], Year <= input$year_range_country[2])
    
    summary_table <-
      data.frame("Country" = c(
        "Mean",
        "Median",
        "Standard Deviation",
        "Min",
        "Max",
        "Observations"
      ))
    
    reports <- c("Amnesty International", "Human Rights Watch", "U.S. State Department", "All")
    
    for (report in reports) {
      filtered_data <- table_data
      if (report != "All") {
        filtered_data <- filter(table_data, `Report Type` == report)
      }
      mean <- formatC(mean(filtered_data$value, na.rm = TRUE), digits = 2, format = "f")
      median <- formatC(median(filtered_data$value, na.rm = TRUE), digits = 2, format = "f")
      ssd <- formatC(sd(filtered_data$value, na.rm = TRUE), digits = 2, format = "f")
      min <- formatC(min(filtered_data$value, na.rm = TRUE), digits = 2, format = "f")
      max <- formatC(min(filtered_data$value, na.rm = TRUE), digits = 2, format = "f")
      obs <- formatC(nrow(filtered_data), digits = 0, format = "f")
      summary_table[, paste0(report)] <- c(mean, median, ssd, min, max, obs)
    }
    
    return(summary_table)
    
  })
  
  # output summary table
  output$country_table <- renderReactable({
    
    reactable(summary_table(), defaultColDef = colDef(minWidth = 100))
    
  })
  
  
}

shinyApp(ui = ui, server = server)
