# Shiny Dashboard: Statistical Analysis of Oceanus Folk Influence

# This Shiny app shows how the influence of "Oceanus Folk" music spreads using statistical analysis (out-degree centrality & chi-square test)

library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(DT)
library(ggraph)
library(visNetwork)

ui <- fluidPage(
  titlePanel("Exploratory Data Analysis: Oceanus Folk Knowledge Graph"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("genre_select", "Select Genre:",
                  choices = NULL, multiple = TRUE),
      
      sliderInput("year_range", "Select Year Range:",
                  min = 2000, max = 2040, value = c(2020, 2040), sep = "")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Charts",
                 plotOutput("songs_by_year"),
                 plotOutput("genres_over_time")
        ),
        
        tabPanel("Composition",
                 plotOutput("node_composition")
        ),
        
        tabPanel("Network Graph",
                 visNetworkOutput("network_graph", height = "600px")
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  # Dynamic filtering based on genre and year
  filtered_nodes <- reactive({
    req(nodes)
    nodes %>%
      filter(is.na(genre) | genre %in% input$genre_selector) %>%
      filter(is.na(release_date) | between(as.numeric(release_date), input$year_range[1], input$year_range[2]))
  })
  
  filtered_edges <- reactive({
    req(edges)
    edges %>%
      filter(source %in% filtered_nodes()$id & target %in% filtered_nodes()$id)
  })
  
  # Oceanus Folk Songs by Year
  output$song_release_plot <- renderPlot({
    oceanus_songs <- nodes %>%
      filter(genre == "Oceanus Folk", !is.na(release_date)) %>%
      mutate(year = as.integer(release_date)) %>%
      count(year)
    
    ggplot(oceanus_songs, aes(x = year, y = n)) +
      geom_col(fill = "#1f78b4") +
      labs(title = "Oceanus Folk Songs Released by Year", x = "Year", y = "Number of Songs") +
      theme_minimal()
  })
  
  # Genre Distribution Over Time
  output$genre_dist_plot <- renderPlot({
    genre_dist <- nodes %>%
      filter(!is.na(genre), !is.na(release_date)) %>%
      mutate(year = as.integer(release_date)) %>%
      count(year, genre)
    
    ggplot(genre_dist, aes(x = year, y = n, fill = genre)) +
      geom_col() +
      labs(title = "Genre Distribution Over Time", x = "Year", y = "Count") +
      theme_minimal()
  })
  
  # Node Type Composition Pie Chart
  output$node_type_pie <- renderPlot({
    node_type_count <- nodes %>%
      count(node_type)
    
    ggplot(node_type_count, aes(x = "", y = n, fill = node_type)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Node Type Composition")
  })
  
  # Interactive visNetwork Graph
  output$eda_network <- renderVisNetwork({
    req(filtered_nodes(), filtered_edges())
    
    visNetwork(filtered_nodes(), filtered_edges()) %>%
      visNodes(shape = "dot", size = 10) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
}


shinyApp(ui, server)