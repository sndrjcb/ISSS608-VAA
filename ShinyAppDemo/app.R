
library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(DT)
library(ggraph)
library(visNetwork)

ui <- fluidPage(
  titlePanel("Temporal Trend of Oceanus Folk Influence"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = 2000, max = 2040,
                  value = c(2010, 2040), sep = "", step = 1),
      checkboxGroupInput("edge_types", "Select Edge Types:",
                         choices = c("InspiredBy", "InterpolatesFrom", "InStyleOf"),
                         selected = c("InspiredBy", "InterpolatesFrom")),
      checkboxInput("show_trend", "Show Trendline", value = TRUE),
      actionButton("run_regression", "Run Trend Test")
    ),
    
    mainPanel(
      plotOutput("influence_trend"),
      verbatimTextOutput("trend_summary")
    )
  )
)

server <- function(input, output, session) {
  library(tidyverse)
  library(jsonlite)
  
  # Load data
  graph <- fromJSON("data/MC1_graph.json")
  nodes <- as_tibble(graph$nodes)
  edges <- as_tibble(graph$links)
  
  # Clean and prepare
  nodes <- janitor::clean_names(nodes)
  edges <- janitor::clean_names(edges)
  
  # Oceanus Folk songs
  of_songs <- nodes %>%
    filter(node_type == "Song", genre == "Oceanus Folk", !is.na(release_date)) %>%
    mutate(release_year = floor(as.numeric(release_date)))
  
  of_ids <- of_songs$id
  
  # Filter influence edges
  influence_data <- reactive({
    edges %>%
      filter(edge_type %in% input$edge_types, source %in% of_ids) %>%
      left_join(of_songs %>% select(id, release_year), by = c("source" = "id")) %>%
      filter(!is.na(release_year)) %>%
      count(release_year, name = "count") %>%
      filter(release_year >= input$year_range[1], release_year <= input$year_range[2])
  })
  
  # Trend plot
  output$influence_trend <- renderPlot({
    df <- influence_data()
    p <- ggplot(df, aes(x = release_year, y = count)) +
      geom_line(color = "#2C3E50") +
      geom_point(color = "#18BC9C") +
      labs(x = "Year", y = "Number of Influenced Songs", title = "Oceanus Folk Influence Over Time") +
      theme_minimal()
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#E74C3C")
    }
    p
  })
  
  # Linear regression test
  output$trend_summary <- renderPrint({
    input$run_regression
    isolate({
      df <- influence_data()
      if (nrow(df) > 1) {
        model <- lm(count ~ release_year, data = df)
        summary(model)
      } else {
        "Not enough data points to perform regression."
      }
    })
  })
}



shinyApp(ui=ui, server=server)