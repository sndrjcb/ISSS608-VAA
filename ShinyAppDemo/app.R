
library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(DT)
library(ggraph)
library(visNetwork)

ui <- navbarPage("Oceanus Folk Dashboard",
                 
                 tabPanel("Statistical Analysis",
                          tabsetPanel(
                            
                            tabPanel("Temporal Trend of Oceanus Folk Influence",
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
                            ),
                          
                              tabPanel("Group Differences (ANOVA/Kruskal-Wallis)",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("group_var", "Group By:",
                                                     choices = c("genre", "node_type"),
                                                     selected = "genre"),
                                         radioButtons("test_type", "Choose Test:",
                                                      choices = c("ANOVA", "Kruskal-Wallis"),
                                                      selected = "ANOVA"),
                                         actionButton("run_group_test", "Run Test")
                                       ),
                                       mainPanel(
                                         plotOutput("group_boxplot"),
                                         verbatimTextOutput("group_test_result"),
                                         verbatimTextOutput("top_genres_output")
                                       )
                                     )
                            )
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
  
  # Data reactive: create summary data grouped by selected variable
  group_data <- reactive({
    req(nodes)  # from your existing dataset
    nodes %>%
      filter(node_type == "Song", !is.na(release_date), !is.na(genre)) %>%
      mutate(release_year = as.numeric(release_date)) %>%
      group_by(!!sym(input$group_var)) %>%
      summarise(mean_year = mean(release_year, na.rm = TRUE),
                n = n()) %>%
      ungroup()
  })
  
  # Boxplot for groups
  output$group_boxplot <- renderPlot({
    df <- nodes %>%
      filter(node_type == "Song", !is.na(release_date), !is.na(genre)) %>%
      mutate(release_year = as.numeric(release_date))
    
    ggplot(df, aes_string(x = input$group_var, y = "release_year")) +
      geom_boxplot(fill = "#2C3E50", alpha = 0.7) +
      labs(x = input$group_var, y = "Release Year",
           title = paste("Distribution of Release Years by", input$group_var)) +
      theme_minimal()
  })
  
  # Run statistical test
  output$group_test_result <- renderPrint({
    input$run_group_test  # re-run when button is clicked
    isolate({
      df <- nodes %>%
        filter(node_type == "Song", !is.na(release_date), !is.na(genre)) %>%
        mutate(release_year = as.numeric(release_date))
      
      if (input$test_type == "ANOVA") {
        model <- aov(release_year ~ get(input$group_var), data = df)
        summary(model)
      } else {
        kruskal.test(release_year ~ get(input$group_var), data = df)
      }
    })
  })
  
  output$top_genres_output <- renderPrint({
    # 1) Find Sailor Shift node
    sailor_node <- nodes %>%
      filter(str_to_lower(name) == "sailor shift") %>%
      pull(id)
    
    if (length(sailor_node) == 0) {
      return("Sailor Shift node not found in dataset.")
    }
    
    # 2) Find Sailor Shift's songs
    sailor_songs <- nodes %>%
      filter(node_type == "Song", str_detect(str_to_lower(name), "sailor shift")) %>%
      pull(id)
    
    if (length(sailor_songs) == 0) {
      return("No songs associated with Sailor Shift found in dataset.")
    }
    
    # 3) Get edges where Sailor's songs are the source of influence
    influenced <- edges %>%
      filter(source %in% sailor_songs) %>%
      left_join(nodes, by = c("target" = "id")) %>%
      filter(!is.na(genre)) %>%
      count(genre, sort = TRUE)
    
    # 4) Display
    if (nrow(influenced) == 0) {
      "No influenced genres found from Sailor Shift's songs."
    } else {
      cat("Top Genres Influenced by Sailor Shift's Songs:\n")
      print(head(influenced, 5))
    }
  })
  
  
  
}



shinyApp(ui=ui, server=server)