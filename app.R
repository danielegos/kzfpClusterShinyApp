library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# =======================================================
# Load dataset once at startup
# =======================================================
df_label <- read.csv(
  "data/df_znf_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
bool_cols2 <- 6:ncol(df_label)
df_label[bool_cols2] <- lapply(df_label[bool_cols2], function(x) x == "True")

# Shared setup
species_choices <- df_label %>%
  mutate(label = paste(Species, "—", CommonName)) %>%
  select(Species, label)

label_choices <- colnames(df_label)[6:ncol(df_label)]

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  titlePanel(
    HTML("KZFP Conservation Viewer<br>
          <span style='font-size:16px; color:gray; font-style:italic;'>
          Data adapted from Imbeault et al. (2017), DOI: 
          <a href='https://doi.org/10.1038/nature21683' target='_blank' style='color:#8B008B; text-decoration:none;'>
          https://doi.org/10.1038/nature21683
          </a>
          </span>")
  ),
  
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
      uiOutput("dynamicClusterPlotUI")   # dynamic height
    ),
    
    # ---------------------------------------------------
    # TAB 2: View by Label / Gene
    # ---------------------------------------------------
    tabPanel(
      title = "View by Label/Gene",
      br(),
      selectizeInput(
        "selected_labels",
        "Select one or more Label(s):",
        choices = label_choices,
        selected = "ZNF777",
        multiple = TRUE,
        options = list(
          placeholder = "Type to search for labels or genes...",
          maxItems = NULL
        )
      ),
      br(),
      uiOutput("dynamicLabelPlotUI")   # dynamic height
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
    
    species_row <- df_label %>% filter(Species == input$selected_species)
    label_cols <- colnames(df_label)[6:ncol(df_label)]
    labels_true <- label_cols[as.logical(species_row[1, label_cols])]
    
    # If no labels are TRUE, return NULL
    if(length(labels_true) == 0) return(NULL)
    
    sub <- df_label %>%
      select(Species, Order, Class, CommonName, timeFromHuman_MY, all_of(labels_true))
    
    df_long <- sub %>%
      pivot_longer(
        cols = all_of(labels_true),
        names_to = "Label",
        values_to = "present"
      )
    
    label_freq <- df_long %>%
      filter(present == TRUE) %>%
      count(Label, name = "Frequency_T") %>%
      arrange(desc(Frequency_T), Label)
    
    # Define the desired order of classes
    class_order <- c("Sarcopterygii", "Reptilia", "Aves", "Marsupial", "Mammalia")
    
    # Convert Class column to an ordered factor first
    df_long$Class <- factor(df_long$Class, levels = class_order, ordered = TRUE)
    
    
    df_sorted <- df_long %>%
      # arrange(desc(timeFromHuman_MY), Species, Label)
      # Change to sort by Class first
      arrange(Class, desc(timeFromHuman_MY), Species, Label)
    
    df_sorted$Label <- factor(df_sorted$Label, levels = rev(label_freq$Label), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    
    df_sorted
  })
  
  # Dynamic UI for species plot height
  output$dynamicClusterPlotUI <- renderUI({
    df <- filtered_species_data()
    n_rows <- length(unique(df$Label))
    plot_height <- max(400, n_rows * 20)  # 15px per row, minimum 400px
    plotlyOutput("clusterPlot", width = "1500px", height = paste0(plot_height, "px"))
  })
  
  output$clusterPlot <- renderPlotly({
    df <- filtered_species_data()
    index <- which(df$Species == input$selected_species)
    
    
    # Display message if no genes/clusters for this species
    validate(
      need(!is.null(df), paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    # req(df)
    df$present_num <- as.numeric(df$present)
    
    # --- Class strip ---
    # Example: df is your filtered_species_data()
    # df$Species <- factor(df$Species, levels = unique(df$Species))
    # 
    # # Define class colors
    # class_levels <- sort(unique(df$Class))
    # class_colors <- c("green", "blue", "red", "purple", "orange")[1:length(class_levels)]
    # names(class_colors) <- class_levels
    # 
    # # # Map species to class colors
    # # species_colors <- class_colors[df$Class[match(levels(df$Species), df$Species)]]
    # 
    # # Ensure species are in the right order
    # species_levels <- unique(df$Species)
    # 
    # # Map each species to its class color
    # species_colors <- class_colors[match(df$Class[match(species_levels, df$Species)], names(class_colors))]
    
    # Define class colors manually
    # Create a named vector
    # class_colors <- c("Sarcopterygii" = "green", "Reptilia" = "blue", "Aves" = "red", "Marsupial" = "purple", "Mammalia" = "orange")
    
    # --- Map classes to numeric codes ---
    class_levels <- sort(unique(df$Class))
    class_colors <- c("#7BD151", "#24A884", "#2A788E", "#414387", "#440154")[1:length(class_levels)]
    # 7BD151 , 24A884, 2A788E, 414387, 440154
    names(class_colors) <- class_levels
    class_codes <- setNames(seq_along(class_levels), class_levels)
    
    # --- Assign numeric value for each tile ---
    df$tile_value <- ifelse(df$present,
                            max(class_codes) + 1,            # TRUE tiles
                            class_codes[df$Class])           # FALSE tiles
    
    # --- Create discrete colorscale ---
    # map each code to a color: class_colors + violetred4 for TRUE
    tile_colors <- c(class_colors, "#FDE725")
    discrete_colorscale <- lapply(seq_along(tile_colors), function(i) {
      list((i-1)/ (length(tile_colors)-1), tile_colors[i])
    })
    
    
    
    # --- Plot ---
    plot_ly(
      data = df,
      x = ~Species,
      y = ~Label,
      z = ~tile_value,
      type = "heatmap",
      # colorscale = "Viridis",
      colorscale = discrete_colorscale,
      showscale = TRUE,
      colorbar = list(
        title = "Class / Presence",
        tickvals = c(class_codes, max(class_codes) + 1),
        ticktext = c(names(class_codes), "Present"),
        orientation = "v", 
        # x = 0.5,               # x position (0 left → 1 right)
        # y = 1.05,              # y position (0 bottom → 1 top)
        # xanchor = "center",    # anchor point for x
        # yanchor = "top",    # anchor point for y
        len = .5             # fraction of plot width (horizontal) or height (vertical)
        # thickness = 10         # thickness in pixels
      ),
      text = ~paste("Species:", Species,
                    # "<br>Class:", Class,
                    # "<br>Label:", Label,
                    # "<br>Present:", present
                    # "Species:", Species,
                    "<br>Order:", Order,
                    "<br>Class:", Class,
                    "<br>Common Name:", CommonName,
                    "<br>Label:", Label,
                    "<br>Present:", present,
                    "<br>Time from Human (MY):", timeFromHuman_MY
                    
      ),
      hoverinfo = "text"
    ) %>%
      # layout(
      #   xaxis = list(title = "Species", tickangle = 60),
      #   yaxis = list(title = "Label")
      # )
      
      layout(
        title = list(
          text = paste0("KZFP Gene Conservation for <i>", input$selected_species, "</i> — ", df$CommonName[index]),
          x = 0.05,
          font = list(size = 20)
        ),
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(
            size = 5
            # color = class_colors[Aves]
          ),
          automargin = TRUE
        ),
        yaxis = list(
          title = "Label",
          tickfont = list(
            size = 10
            # color = class_colors[Aves]
          )
        ),
        margin = list(l = 100, r = 20, b = 0, t = 80)
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
  
  # Dynamic UI for label plot height
  output$dynamicLabelPlotUI <- renderUI({
    df <- filtered_label_data()
    n_rows <- length(unique(df$Label))
    plot_height <- max(600, n_rows * 50)  # 15px per row, minimum 400px
    plotlyOutput("labelPlot", width = "1500px", height = paste0(plot_height, "px"))
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
      opacity = 1,
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
          text = "Conservation of Selected KZFP Labels / Genes",
          x = 0.05,
          font = list(size = 20)
        ),
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(size = 5),
          automargin = TRUE
        ),
        yaxis = list(
          title = "Label"
        ),
        margin = list(l = 100, r = 20, b = 0, t = 80)
      )
  })
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)