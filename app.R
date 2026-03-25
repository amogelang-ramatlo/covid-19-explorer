#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# Load required libraries
library(shiny)
library(tidyverse)
library(bslib)
library(DT)
#library(leaflet)
library(plotly)


# COVID data
covid_original <- read.csv("data/countries-aggregated.csv")

# Modify the data to include daily change
covid_modified <- covid_original %>% mutate(Date = ymd(Date)) %>%
  group_by(Country) %>% arrange(Date) %>%
  # Calculate daily cases, i.e, the daily change in the total
  mutate(DailyConfirmed = Confirmed - lag(Confirmed, default = 0),
         DailyRecovered = Recovered - lag(Recovered, default = 0),
         DailyDeaths = Deaths - lag(Deaths, default = 0)) %>%
  # Replace negative daily values with 0 (these could be due to data correction)
  mutate(DailyConfirmed = ifelse(DailyConfirmed < 0, 0, DailyConfirmed),
         DailyRecovered = ifelse(DailyRecovered < 0, 0, DailyRecovered),
         DailyDeaths = ifelse(DailyDeaths < 0, 0, DailyDeaths)) %>% 
  ungroup() %>% arrange(Country, Date)


# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("🌍 COVID-19 Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("🔎 Filter Options"),
      
      #_______________________________________________________________
      # --- CONTROLS FOR TOTAL PLOTS ---
      #_______________________________________________________________
      conditionalPanel(
        condition = "input.mainTab == '📈 Time Series Plot' || 
                     input.mainTab == '📊 Summary Table'",
        tagList(
          selectizeInput("selected_countries", "Choose Countries (Max 3):",
                         choices = unique(covid_modified$Country), 
                         multiple = TRUE,
                         options = list(maxItems = 3),
                         selected = "South Africa"),
          
          checkboxGroupInput("selected_metrics", "Select Metric:", 
                             choices = c("Confirmed", "Recovered", "Deaths"),
                             selected = "Confirmed"),
          
          dateRangeInput("date_range", "Date Range:",
                         start = min(covid_modified$Date),
                         end = max(covid_modified$Date)),
          
          checkboxInput("log_scale", "Logarithmic Scale", value = FALSE),
          
          # Facet options for TOTAL
          conditionalPanel(
            condition = "input.selected_countries.length > 1 && input.selected_metrics.length == 1",
            tagList(hr(), h5("Facet Options"), checkboxInput("facet_by_country", "Facet by Country", value = FALSE))
          ),
          conditionalPanel(
            condition = "input.selected_countries.length == 1 && input.selected_metrics.length > 1",
            tagList(hr(), h5("Facet Options"), checkboxInput("facet_by_metric", "Facet by Metric", value = FALSE))
          ),
          conditionalPanel(
            condition = "input.selected_countries.length > 1 && input.selected_metrics.length > 1",
            tagList(hr(), h5("Facet Options"),
                    radioButtons("facet_choice", "Choose Facet Dimension:",
                                 choices = c("Country", "Metric"),
                                 selected = "Country"))
          ),
          hr()
        )
      ),    # End of Sidebar panel for the cumulative (total) cases
      
      #_______________________________________________________________
      # --- CONTROLS FOR DAILY CHANGE ---
      #_______________________________________________________________
      
      conditionalPanel(
        condition = "input.mainTab == '📉 Daily Change Plot' || 
                     input.mainTab == '📊 Daily Table'",
        tagList(
          selectizeInput("daily_countries", "Choose Countries (Max 3):",
                         choices = unique(covid_modified$Country),
                         multiple = TRUE,
                         options = list(maxItems = 3),
                         selected = "South Africa"),
          
          checkboxGroupInput("daily_metrics", "Select Daily Metric:", 
                             choices = c("Confirmed", "Recovered", "Deaths"),
                             selected = "Confirmed"),
          
          dateRangeInput("daily_date_range", "Date Range:",
                         start = min(covid_modified$Date),
                         end = max(covid_modified$Date)),
          
          checkboxInput("daily_log_scale", "Logarithmic Scale", value = FALSE),
          
          # Facet options for DAILY
          conditionalPanel(
            condition = "input.daily_countries.length > 1 && input.daily_metrics.length == 1",
            tagList(hr(), h5("Facet Options"), checkboxInput("daily_facet_by_country", "Facet by Country", value = FALSE))
          ),
          conditionalPanel(
            condition = "input.daily_countries.length == 1 && input.daily_metrics.length > 1",
            tagList(hr(), h5("Facet Options"), checkboxInput("daily_facet_by_metric", "Facet by Metric", value = FALSE))
          ),
          conditionalPanel(
            condition = "input.daily_countries.length > 1 && input.daily_metrics.length > 1",
            tagList(hr(), h5("Facet Options"),
                    radioButtons("daily_facet_choice", "Choose Facet Dimension:",
                                 choices = c("Country", "Metric"),
                                 selected = "Country"))
          ),
          hr()
        )
      ),  # End of Sidebar panel for daily
      
      #_______________________________________________________________
      # --- CONTROLS FOR WORLD MAP ---
      #_______________________________________________________________
      
      conditionalPanel(condition = "input.mainTab == '🌐 COVID-19 Map'",
        tagList(
          
          selectInput("map_metric", "Select Daily Metric:", 
                      choices = c("Confirmed", "Deaths", "Recovered"),
                      selected = "Confirmed"),
          
          dateRangeInput("map_date", "Date Range:",
                         start = min(covid_modified$Date),
                         end = max(covid_modified$Date)),
          
          # Projection options for Plotly globe
          selectInput("projection", "Map Projection:",
                      choices = c("equirectangular", "orthographic", "natural earth", 
                                  "mercator", "miller", "kavrayskiy7", "robinson", 
                                  "sinusoidal", "mollweide"),
                      selected = "natural earth"),
          hr()
        )
      )
      
    ),   # End of Sidebar panel
    
    mainPanel(
      fluidRow(
        column(4, uiOutput("confirmedBox")),
        column(4, uiOutput("recoveredBox")),
        column(4, uiOutput("deathsBox"))
      ),
      br(),
      tabsetPanel(id = "mainTab",
                  tabPanel("📈 Time Series Plot", plotOutput("trendPlot", height = "400px")),
                  tabPanel("📊 Summary Table", DTOutput("summaryTable")),
                  tabPanel("📉 Daily Change Plot", plotOutput("dailyPlot", height = "400px")),
                  tabPanel("📊 Daily Table", DTOutput("dailyTable")),
                  tabPanel("🌐 COVID-19 Map", plotlyOutput("covidMap"))
      )
    )
  )
)



# Define Server
server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  filtered_data1 <- reactive({
    req(input$selected_countries)  # Ensure input is available
    covid_modified %>% 
      filter(Country %in% input$selected_countries) %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
  })
  
  #_______________________________________________________________
  # Value boxes
  #_______________________________________________________________
  create_value_box <- function(output_id, title, value_column, color) {
    output[[output_id]] <- renderUI({
      total <- sum(filtered_data2()[[value_column]], na.rm = TRUE)
      wellPanel(
        style = paste("background-color:#f8f9fa; border-left: 5px solid", color, ";"),
        h5(title),
        h3(formatC(total, format = "d", big.mark = ",")),
        style = "text-align:center;"
      )
    })
  }
  # Create value boxes with respective color and data
  create_value_box("confirmedBox", "Total Daily Confirmed", "DailyConfirmed", "#0d6efd")
  create_value_box("recoveredBox", "Total Daily Recovered", "DailyRecovered", "#198754")
  create_value_box("deathsBox", "Total Daily Deaths", "DailyDeaths", "#dc3545")
  
  
  #_______________________________________________________________
  # Summary Table for Total
  #_______________________________________________________________  
  output$summaryTable <- renderDT({
    # Validate user input
    validate(
      need(length(input$selected_metrics) > 0 && length(input$selected_countries) > 0, 
           "Please select at least one metric and at least one country.")
    )
    
    filtered_data1() %>%
      arrange(Country, Date) %>%
      select(Date, Country, sort(input$selected_metrics)) %>% # Filter by selected metrics
      datatable(options = list(pageLength = 10))
  })
  
  
  #_______________________________________________________________
  # Time series plot
  #_______________________________________________________________
  faceting_mode <- function(selected_countries, selected_metrics) {
    countries <- selected_countries
    metrics <- selected_metrics
    if (length(countries) < 1 || length(metrics) < 1) return("invalid")
    # Handle the case where faceting by country or metric is required
    if (length(countries) > 1 && length(metrics) > 1) return("both")
    if (length(countries) > 1) return("country")
    if (length(metrics) > 1) return("metric")
    return("none")
  }
  
  facet_mode <- reactive({
    faceting_mode(input$selected_countries, input$selected_metrics)
  })
  
  render_plot <- function(data, selected_countries, selected_metrics, facet_by) {
    # Filter and reshape data
    data_long <- data %>%
      filter(Country %in% selected_countries) %>%
      select(Date, all_of(selected_metrics), Country) %>%
      pivot_longer(cols = selected_metrics, names_to = "Metric", values_to = "Count")
    
    # Build titles
    metric_title <- paste(selected_metrics, collapse = ", ")
    
    # Start base ggplot
    p <- ggplot(data_long, aes(x = Date, y = Count))
    
    # Case 1: multiple metrics, one country
    if (length(selected_metrics) > 1 && length(selected_countries) == 1) {
      if (!is.null(facet_by)) {
        p <- p + geom_line(size = 1, na.rm = TRUE)
        p <- p + facet_wrap(as.formula(paste("~", facet_by)))
      } else {
        p <- p + geom_line(aes(color = Metric), size = 1, na.rm = TRUE)
      }
      
      # Case 2: multiple countries, one metric
    } else if (length(selected_countries) > 1 && length(selected_metrics) == 1) {
      if (!is.null(facet_by)) {
        p <- p + geom_line(size = 1, na.rm = TRUE)
        p <- p + facet_wrap(as.formula(paste("~", facet_by)))
      } else {
        p <- p + geom_line(aes(color = Country), size = 1, na.rm = TRUE)
      }
      
      # Case 3: multiple countries & metrics
    } else if (length(selected_countries) > 1 && length(selected_metrics) > 1) {
      color_var <- if (facet_by == "Country") "Metric" else "Country"
      p <- p + geom_line(aes_string(color = color_var), size = 1, na.rm = TRUE)
      if (!is.null(facet_by)) {
        p <- p + facet_wrap(as.formula(paste("~", facet_by)))
      }
      p <- p + labs(color = color_var)  # Manually set legend title
      
      # Case 4: one country, one metric (default startup case)
    } else {
      p <- p + geom_line(size = 1, na.rm = TRUE)
    }
    
    # Add theme and labels
    p <- p + scale_y_continuous(labels = scales::comma) +
      labs(title = paste("COVID-19 Metrics for", metric_title), x = "Date", y = "Count") +
      theme_bw()
    
    return(p)
  }
  
  # Server logic for trendPlot
  output$trendPlot <- renderPlot({
    validate(
      need(length(input$selected_metrics) > 0 && length(input$selected_countries) > 0, 
           "Please select at least one metric and at least one country.")
    )
    # Facet logic based on selections
    mode <- facet_mode()
    facet_by <- NULL  # Default is no faceting
    if (mode == "both") {
      facet_by <- input$facet_choice
    } else if (mode == "country" && isTRUE(input$facet_by_country)) {
      facet_by <- "Country"
    } else if (mode == "metric" && isTRUE(input$facet_by_metric)) {
      facet_by <- "Metric"
    }
    #Add log scale if selected
    temp <- filtered_data1
    if (input$log_scale) {
      render_plot(temp(), input$selected_countries, input$selected_metrics, facet_by) + 
        scale_y_log10(labels = scales::comma_format())
    } else {
      render_plot(temp(), input$selected_countries, input$selected_metrics, facet_by)
    }
  })
  
  
  #_______________________________________________________________
  # Daily Time series plot
  #_______________________________________________________________
  filtered_data2 <- reactive({
    req(input$daily_countries)  # Ensure input is available
    covid_modified %>% 
      filter(Country %in% input$daily_countries) %>%
      filter(Date >= input$daily_date_range[1], Date <= input$daily_date_range[2])
  })
  
  
  
  
  
  
  # Server logic for dailyPlot
  output$dailyPlot <- renderPlot({
    validate(
      need(length(input$daily_metrics) > 0 && length(input$daily_countries) > 0, 
           "Please select at least one metric and at least one country.")
    )
    # Facet logic based on selections
    mode <- reactive({faceting_mode(input$daily_countries, input$daily_metrics)})
    facet_by <- NULL  # Default is no faceting
    if (mode() == "both") {
      facet_by <- input$daily_facet_choice
    } else if (mode() == "country" && isTRUE(input$daily_facet_by_country)) {
      facet_by <- "Country"
    } else if (mode() == "metric" && isTRUE(input$daily_facet_by_metric)) {
      facet_by <- "Metric"
    }
    
    #Add log scale if selected
    temp <- filtered_data2
    if (input$daily_log_scale) {
      render_plot(temp(), input$daily_countries, paste0("Daily", input$daily_metrics), facet_by) + 
        scale_y_log10(labels = scales::comma_format())
    } else {
      render_plot(temp(), input$daily_countries, paste0("Daily", input$daily_metrics), facet_by)
    }
  })
  
  
  #_______________________________________________________________
  # Summary Table for Daily Change
  #_______________________________________________________________  
  output$dailyTable <- renderDT({
    # Validate user input
    validate(
      need(length(input$daily_metrics) > 0 && length(input$daily_countries) > 0, 
           "Please select at least one metric and at least one country.")
    )
    filtered_data2() %>%
      arrange(Country, Date) %>%
      # Filter by selected metrics
      select(Date, Country, sort(paste0("Daily", input$daily_metrics))) %>% 
      datatable(options = list(pageLength = 10))
  })
  
  #_______________________________________________________________
  # Covid Map
  #_______________________________________________________________ 
  output$covidMap <- renderPlotly({
    temp <- paste0("Daily", input$map_metric)
    covid_data <- covid_modified %>% 
      filter(Date >= input$map_date[1], Date <= input$map_date[2]) %>%
      group_by(Country) %>%
      summarise(Value = sum(!!sym(temp), na.rm = TRUE))
    
    plot_geo(covid_data) %>%
      add_trace(
        z = ~Value,
        color = ~Value,
        colors = "Blues",
        text = ~paste0(Country, paste0('\n', input$map_metric, ": "), Value),
        locations = ~Country,
        locationmode = "country names",
        hoverinfo = "text"
      ) %>%
      colorbar(title = paste("Daily", input$map_metric, "Cases")) %>%
      layout(
        #title = "Global COVID-19 Cases",
        geo = list(
          showframe = TRUE,
          showcoastlines = FALSE,
          projection = list(type = input$projection)
        )
      )
  })
  
} # End of server

# Run the app
shinyApp(ui, server)
