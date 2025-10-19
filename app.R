# -------------
# Attempt 1
# -------------


library(shiny)
library(ggplot2)
library(tidyr)

# Directory where all species CSVs are stored
data_dir <- "data"

# Loop to read all CSV files in directory
# Returns a named list where names are species (from filename without .csv)
species_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
species_list <- lapply(species_files, read.csv, header = TRUE, stringsAsFactors = FALSE)
names(species_list) <- tools::file_path_sans_ext(basename(species_files)) # species name from filename


ui <- fluidPage(
  titlePanel("KZFP Cluster Conservation Across Species"),

  # Search bar on top
  selectizeInput(
    "selected_species",
    "Select or type species name:",
    choices = names(species_list),
    multiple = FALSE,
    options = list(
      placeholder = 'Type to search species...',
      maxItems = 1
    )
  ),

  # Plot below
  plotOutput("clusterPlot", width = "1200px", height = "600px")
)


# Shiny Server
server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$selected_species)  # ensure a species is selected

    # Load the species CSV
    df <- species_list[[input$selected_species]]

    # Factor ordering
    df$cluster <- factor(df$cluster, levels = rev(unique(df$cluster)), ordered = TRUE)
    df$Species <- factor(df$Species, levels = unique(df$Species), ordered = TRUE)

    df
  })

  output$clusterPlot <- renderPlot({
    df <- filtered_data()
    req(df)

    ggplot(df, aes(x = Species, y = factor(cluster))) +
      geom_tile(aes(fill = present), alpha = 0.5) +  # alpha outside aes()
      scale_fill_manual(values = c("lightgrey", "violetred4")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}

# Run the app
shinyApp(ui, server)



# 
# # -------------
# # Attempt 2
# # -------------
# library(shiny)
# library(ggplot2)
# library(tidyr)
# library(plotly)  # <-- for interactive hover
# 
# # Directory where all species CSVs are stored
# data_dir <- "C:/Users/danie/Documents/nih_postbac/ShinyAppTronoData/species_kzfp_cluster_conservation_dfs"
# 
# # Loop to read all CSV files in directory
# species_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
# species_list <- lapply(species_files, read.csv, header = TRUE, stringsAsFactors = FALSE)
# names(species_list) <- tools::file_path_sans_ext(basename(species_files)) # species name from filename
# 
# ui <- fluidPage(
#   titlePanel("KZFP Cluster Conservation Across Species"),
#   
#   # Search bar on top
#   selectizeInput(
#     "selected_species",
#     "Select or type species name:",
#     choices = names(species_list),
#     multiple = FALSE,
#     options = list(
#       placeholder = 'Type to search species...',
#       maxItems = 1
#     )
#   ),
#   
#   # Interactive plot below
#   plotlyOutput("clusterPlot", width = "1200px", height = "600px")
# )
# 
# server <- function(input, output, session) {
#   
#   filtered_data <- reactive({
#     req(input$selected_species)  # ensure a species is selected
#     
#     # Load the species CSV
#     df <- species_list[[input$selected_species]]
#     
#     # Factor ordering
#     df$cluster <- factor(df$cluster, levels = rev(unique(df$cluster)), ordered = TRUE)
#     df$Species <- factor(df$Species, levels = unique(df$Species), ordered = TRUE)
#     
#     df
#   })
#   
#   output$clusterPlot <- renderPlotly({
#     df <- filtered_data()
#     req(df)
#     
#     # ggplot with hover text
#     p <- ggplot(df, aes(x = Species, y = factor(cluster))) +
#       geom_tile(aes(fill = present,
#                     text = paste0("Species: ", Species,
#                                   "<br>Cluster: ", cluster,
#                                   "<br>Present: ", present)),
#                 alpha = 0.5) +
#       scale_fill_manual(values = c("lightgrey", "violetred4")) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     
#     # Convert ggplot to plotly for interactive hover
#     ggplotly(p, tooltip = "text")
#   })
# }
# 
# # Run the app
# shinyApp(ui, server)

# 
# # -------------
# # # Attempt 3
# # # -------------
# 
# 
# library(shiny)
# library(ggplot2)
# library(tidyr)
# library(plotly)
# 
# # Directory where all species CSVs are stored
# data_dir <- "C:/Users/danie/Documents/nih_postbac/ShinyAppTronoData/species_kzfp_cluster_conservation_dfs"
# 
# # Load all CSVs into a named list
# species_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)
# species_list <- lapply(species_files, read.csv, header = TRUE, stringsAsFactors = FALSE)
# names(species_list) <- tools::file_path_sans_ext(basename(species_files)) # species name from filename
# 
# ui <- fluidPage(
#   titlePanel("KZFP Cluster Conservation Across Species"),
#   
#   # Search bar on top
#   selectizeInput(
#     "selected_species",
#     "Select or type species name:",
#     choices = names(species_list),
#     multiple = FALSE,
#     options = list(
#       placeholder = 'Type to search species...',
#       maxItems = 1
#     )
#   ),
#   
#   # Interactive plot below
#   plotlyOutput("clusterPlot", width = "1200px", height = "600px")
# )
# 
# server <- function(input, output, session) {
#   
#   filtered_data <- reactive({
#     req(input$selected_species)  # ensure a species is selected
#     
#     # Load the species CSV
#     df <- species_list[[input$selected_species]]
#     
#     # Factor ordering
#     df$cluster <- factor(df$cluster, levels = rev(unique(df$cluster)), ordered = TRUE)
#     df$Species <- factor(df$Species, levels = unique(df$Species), ordered = TRUE)
#     
#     df
#   })
#   
#   output$clusterPlot <- renderPlotly({
#     df <- filtered_data()
#     req(df)
#     
#     # Create hover text as a mini HTML table
#     df$hover_text <- paste0(
#       "<b>Species:</b> ", df$Species, "<br>",
#       "<b>Cluster:</b> ", df$cluster, "<br>",
#       "<b>Present:</b> ", df$present
#     )
#     
#     # ggplot with hover text
#     p <- ggplot(df, aes(x = Species, y = factor(cluster))) +
#       geom_tile(aes(fill = present, text = hover_text), alpha = 0.5) +
#       scale_fill_manual(values = c("lightgrey", "violetred4")) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     
#     # Convert ggplot to plotly with formatted hover
#     ggplotly(p, tooltip = "text") %>%
#       layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
#   })
# }
# 
# # Run the app
# shinyApp(ui, server)
