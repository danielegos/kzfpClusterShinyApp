library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# =======================================================
# Load both datasets once at startup
# =======================================================

# Dataset 1: Species-based clusters
df_species <- read.csv(
  "data/df_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
bool_cols <- 6:ncol(df_species)
df_species[bool_cols] <- lapply(df_species[bool_cols], function(x) x == "True")

# Dataset 2: Label/gene-based clusters
df_label <- read.csv(
  "data/df_znf_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
bool_cols2 <- 6:ncol(df_label)
df_label[bool_cols2] <- lapply(df_label[bool_cols2], function(x) x == "True")

# =======================================================
# Shared setup
# =======================================================
species_choices <- df_species %>%
  mutate(label = paste(Species, "—", CommonName)) %>%
  select(Species, label)

label_choices <- colnames(df_label)[6:ncol(df_label)]

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  titlePanel("KZFP Conservation Viewer"),
  
  tabsetPanel(
    id = "tabs",
    type = "tabs",
    
    # ---------------------------------------------------
    # TAB 1: View by Species
    # ---------------------------------------------------
    tabPanel(
      title = "View by Species",
      br(),
      selectizeInput(
        "selected_species",
        "Select or type species name or common name:",
        choices = setNames(species_choices$Species, species_choices$label),
        multiple = FALSE,
        options = list(
          placeholder = 'Type species or common name...',
          maxItems = 1
        )
      ),
      br(),
      plotlyOutput("clusterPlot", width = "1500px", height = "600px")
    ),
    
    # ---------------------------------------------------
    # TAB 2: View by Label / Gene
    # ---------------------------------------------------
    tabPanel(
      title = "View by Label / Gene",
      br(),
      selectizeInput(
        "selected_labels",
        "Select one or more Label(s):",
        choices = label_choices,
        selected = "ZNF777",
        multiple = TRUE,
        options = list(
          placeholder = "Type to search for clusters or genes...",
          maxItems = NULL
        )
      ),
      br(),
      plotlyOutput("labelPlot", width = "1500px", height = "600px")
    )
  )
)

# =======================================================
# SERVER
# =======================================================
server <- function(input, output, session) {
  
  # -------------------------------
  # Tab 1: View by Species
  # -------------------------------
  filtered_species_data <- reactive({
    req(input$selected_species)
    
    # Step 1: Get selected species row
    species_row <- df_species %>% filter(Species == input$selected_species)
    cluster_cols <- colnames(df_species)[6:ncol(df_species)]
    clusters_true <- cluster_cols[as.logical(species_row[1, cluster_cols])]
    
    # Step 2: Subset all species for those clusters
    sub <- df_species %>%
      select(Species, Order, Class, CommonName, timeFromHuman_MY, all_of(clusters_true))
    
    # Step 3: Melt wide → long
    df_long <- sub %>%
      pivot_longer(
        cols = all_of(clusters_true),
        names_to = "cluster",
        values_to = "present"
      )
    
    # Step 4–7: Sort and order
    cluster_freq <- df_long %>%
      filter(present == TRUE) %>%
      count(cluster, name = "Frequency_T") %>%
      arrange(desc(Frequency_T), cluster)
    
    df_sorted <- df_long %>%
      arrange(desc(timeFromHuman_MY), Species, cluster)
    
    df_sorted$cluster <- factor(df_sorted$cluster, levels = rev(cluster_freq$cluster), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    df_sorted
  })
  
  output$clusterPlot <- renderPlotly({
    df <- filtered_species_data()
    req(df)
    df$present_num <- as.numeric(df$present)
    
    plot_ly(
      data = df,
      x = ~Species,
      y = ~cluster,
      z = ~present_num,
      type = "heatmap",
      colors = c("lightgrey", "violetred4"),
      opacity = 0.8,
      text = ~paste(
        "Species:", Species,
        "<br>Order:", Order,
        "<br>Class:", Class,
        "<br>Common Name:", CommonName,
        "<br>Cluster:", cluster,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        title = list(
          text = paste("Cluster Conservation for", input$selected_species),
          x = 0.05,
          font = list(size = 20)
        ),
        xaxis = list(title = "Species", tickangle = 90),
        yaxis = list(title = "Cluster"),
        margin = list(l = 100, r = 20, b = 150, t = 80)
      )
  })
  
  # -------------------------------
  # Tab 2: View by Label / Gene
  # -------------------------------
  filtered_label_data <- reactive({
    req(input$selected_labels)
    selected <- input$selected_labels
    
    sub <- df_label %>%
      select(Species, Order, Class, CommonName, timeFromHuman_MY, all_of(selected))
    
    df_long <- sub %>%
      pivot_longer(
        cols = all_of(selected),
        names_to = "Label",
        values_to = "present"
      )
    
    label_freq <- df_long %>%
      filter(present == TRUE) %>%
      count(Label, name = "Frequency_T") %>%
      arrange(desc(Frequency_T), Label)
    
    df_sorted <- df_long %>%
      arrange(desc(timeFromHuman_MY), Species, Label)
    
    df_sorted$Label <- factor(df_sorted$Label, levels = rev(label_freq$Label), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    df_sorted
  })
  
  output$labelPlot <- renderPlotly({
    df <- filtered_label_data()
    req(df)
    df$present_num <- as.numeric(df$present)
    
    plot_ly(
      data = df,
      x = ~Species,
      y = ~Label,
      z = ~present_num,
      type = "heatmap",
      colors = c("lightgrey", "violetred4"),
      opacity = 0.8,
      text = ~paste(
        "Species:", Species,
        "<br>Order:", Order,
        "<br>Class:", Class,
        "<br>Common Name:", CommonName,
        "<br>Label:", Label,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        title = list(
          text = "Conservation of Selected Labels / Genes",
          x = 0.05,
          font = list(size = 20)
        ),
        xaxis = list(title = "Species", tickangle = 90),
        yaxis = list(title = "Label"),
        margin = list(l = 100, r = 20, b = 150, t = 80)
      )
  })
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)

