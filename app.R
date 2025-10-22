library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# =======================================================
# Load datasets once at startup
# =======================================================
df_label <- read.csv(
  "data/df_clade.csv",
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
    HTML("<h1>KZFP Conservation Viewer</h1><br>
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
        ),
        width = '300px'
      ),
      br(),
     
      # Two-column layout
      fluidRow(
        column(
          width = 8,    # left side (e.g., plot)
          uiOutput("dynamicClusterTableUI")
        ),
        column(
          width = 4,    # right side (e.g., summary/statistics)
          br(),
          br(),
          br(),
          br(),
          
          uiOutput("staticClusterPlotUI")
        )
      ),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      # Place Dynamic Plot below below
      fluidRow(
        column(
          width = 12,
          uiOutput("dynamicClusterPlotUI")
        )
      )
    ),
    
    # ---------------------------------------------------
    # TAB 2: View by Label / Gene
    # ---------------------------------------------------
    tabPanel(
      title = "View by Gene",
      br(),
      selectizeInput(
        "selected_labels",
        "Select one or more Gene(s):",
        choices = label_choices,
        selected = "ZNF777",
        multiple = TRUE,
        options = list(
          placeholder = "Type to search for genes...",
          maxItems = NULL
        ),
        width = '1500px'  # or '100%', '50%', etc.
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      uiOutput("dynamicLabelPlotUI")   # dynamic height
    )
  ),
  
  # Only show this absolutePanel when on the "View by Species" tab
  conditionalPanel(
    condition = "input.tabs == 'View by Species'",
    
    absolutePanel(
      top = 605, left = 165,
      img(src = "kzfp_phylogeny.png", height = "133px"),
      style = "z-index: 9999;"  # high z-index ensures it's on top
      
    ),
    
    absolutePanel(
      top = 324, left = 1046,
      img(src = "kzfp_phylogeny.png", height = "44px")
      # style = "z-index: 9999;"  # high z-index ensures it's on top
      
    ),
    
    absolutePanel(
      top = 155, left = 315,
      uiOutput("speciesInfoPanel"),
      style = "z-index: 9999; background-color: rgba(255,255,255,0.9); padding: 0px; border-radius: 8px;"
    ),
    
    uiOutput("speciesImagePanel")
  ),
  
  # Only show this absolutePanel when on the "View by Species" tab
  conditionalPanel(
    condition = "input.tabs == 'View by Gene'",
    
    absolutePanel(
      top = 250, left = 165,
      img(src = "kzfp_phylogeny.png", height = "133px")
      # style = "z-index: 9999;"  # high z-index ensures it's on top
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
      select(Species, Order, Clade, CommonName, timeFromHuman_MY, all_of(labels_true))
    
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
    
    # Define the desired order of Cladees
    Clade_order <- c("Coelacanth", "Amphibia", "Reptiles", "Birds", "Monotremes", "Marsupials", "Eutheria", "Primates")
    
    # Convert Clade column to an ordered factor first
    df_long$Clade <- factor(df_long$Clade, levels = Clade_order, ordered = TRUE)
    
    
    df_sorted <- df_long %>%
      # arrange(desc(timeFromHuman_MY), Species, Label)
      # Change to sort by Clade first
      arrange(Clade, desc(timeFromHuman_MY), Species, Label)
    
    df_sorted$Label <- factor(df_sorted$Label, levels = rev(label_freq$Label), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    df_sorted
  })
  
  output$speciesInfoPanel <- renderUI({
    df <- filtered_species_data()
    req(input$selected_species)
    
    species_row <- df[df$Species == input$selected_species, ]
    
    validate(
      need(nrow(species_row) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    div(
      style = "width: 900px; max-width: 90%; margin: 0 auto;",  # fixed width + responsive max + centered
      tagList(
        h2(HTML(paste("KZFP Gene Conservation for <i>", species_row$Species[1], "</i> — ", species_row$CommonName[1]))),
        p(strong("Clade:"), species_row$Clade[1], strong("| Order:"), species_row$Order[1], strong("| Time from Human:"), species_row$timeFromHuman_MY[1], "million years")
        # img(src = "kzfp_phylogeny.png", height = "40px", style = "display:block;margin:auto;")
      )
    )
  })
  
  # Species image panel
  output$speciesImagePanel <- renderUI({
    req(input$selected_species)
    
    # Construct image filename
    img_file <- paste0(input$selected_species, ".png")
    
    # Check if the file exists in the www/ folder
    # (Shiny serves files from ./www automatically)
    img_path <- file.path("www", img_file)
    if (!file.exists(img_path)) {
      img_file <- "default_species.png"  # fallback image
    }
    
    # Display only on the "View by Species" tab
    if (input$tabs == "View by Species") {
      absolutePanel(
        top = 160, left = 1220,
        img(src = img_file, height = "150px"),
        style = "z-index: 9999;"
      )
    }
  })
  
  # Dynamic UI for species plot height
  output$dynamicClusterPlotUI <- renderUI({
    df <- filtered_species_data()
    n_rows <- length(unique(df$Label))
    plot_height <- max(400, n_rows * 15)  # 15px per row, minimum 400px
    plotlyOutput("clusterPlot", width = "1500px", height = paste0(plot_height, "px"))
  })
  
  # Dynamic UI for species table height
  output$dynamicClusterTableUI <- renderUI({
    df <- filtered_species_data()
    validate(
      need(!is.null(df), "No data available for selected species.")
    )
    
    n_rows <- nrow(df)
    table_height <- 330  # 20px per row, min 200px
    div(
      style = paste0("height:", table_height, "px; overflow-y:auto;"),
      DT::dataTableOutput("clusterTable", width = "100%")
    )
  })
  
  # Static UI for species plot height
  output$staticClusterPlotUI <- renderUI({
    df <- filtered_species_data()
    n_rows <- length(unique(df$Label))
    # plot_height <- max(400, n_rows * 15)  # 15px per row, minimum 400px
    plotlyOutput("staticClusterPlot", width = "100%", height = "200px")
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
    
    # --- Main heatmap ---
    # heatmap_plot <-
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
        "<br>Clade:", Clade,
        "<br>Common Name:", CommonName,
        "<br>Label:", Label,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        # title = list(
        #   text = paste0("KZFP Gene Conservation for <i>", input$selected_species, "</i> — ", df$CommonName[index]),
        #   x = 0.05,
        #   font = list(size = 20)
        # ),
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(
            size = 5
            # color = Clade_colors[Aves]
          ),
          automargin = TRUE
        ),
        yaxis = list(
          title = "Label",
          tickfont = list(size = 10),
          automargin = TRUE
        ),
        margin = list(l = 150, r = 20, b = 0, t = 0)
      )
  })
  
  output$clusterTable <- DT::renderDataTable({
    df <- filtered_species_data()
    
    validate(
      need(!is.null(df), paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    # --- Summarize by Label ---
    df_summary <- df %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        # Species = first(Species),
        # CommonName = first(CommonName),
        # Order = first(Order),
        # Clade = first(Clade),
        PresentCount = sum(present == TRUE, na.rm = TRUE),
        # timeFromHuman_MY = mean(timeFromHuman_MY, na.rm = TRUE)
        PercentConserved = paste0(round(100 * PresentCount / n(), 1), "%")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(PresentCount), Label)  # ✅ sort by conservation, then alphabetically by gene
    
    # --- Reorder/select columns ---
    df_display <- df_summary %>%
      dplyr::select(
        `Gene Label` = Label,
        `Number of Species with Gene` = PresentCount,
        `Percent Conserved - All Species` = PercentConserved
        # Label,
        # # Species,
        # # CommonName,
        # # Order,
        # # Clade,
        # PresentCount
        # # timeFromHuman_MY
      )
    
    # --- Display as a datatable ---
    DT::datatable(
      df_display,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold; color: #333333;',
        'Table 1: KZFP Gene Conservation Summary'
      ),
      
      rownames = FALSE,
      options = list(
        scrollY = TRUE,
        pageLength = 5,
        autoWidth = TRUE,
        dom = 'tp',
        order = list(list(1, 'desc'), list(0, 'asc')),  # ✅ UI sort: PresentCount desc, Label asc
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      )
    ) 
    # |>
    #   DT::formatRound(columns = "timeFromHuman_MY", digits = 2)
  })
  
  output$staticClusterPlot <- renderPlotly({
    df <- filtered_species_data()
    index <- which(df$Species == input$selected_species)
    
    # Display message if no genes/clusters for this species
    validate(
      need(!is.null(df), paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    # req(df)
    df$present_num <- as.numeric(df$present)
    
    # --- Main heatmap ---
    # heatmap_plot <-
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
        "<br>Clade:", Clade,
        "<br>Common Name:", CommonName,
        "<br>Label:", Label,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        # title = list(
        #   text = paste0("KZFP Gene Conservation for <i>", input$selected_species, "</i> — ", df$CommonName[index]),
        #   x = 0.05,
        #   font = list(size = 20)
        # ),
        xaxis = list(
          title = "Species",
          showticklabels = FALSE
          # showgrid = FALSE,
          # tickangle = 60,
          # tickfont = list(
          #   size = 5
          #   # color = Clade_colors[Aves]
          # ),
          # automargin = TRUE
        ),
        yaxis = list(
          title = "Gene",
          showticklabels = FALSE
          
          # tickfont = list(size = 10),
          # automargin = TRUE
        ),
        margin = list(l = 0, r = 20, b = 0, t = 0)
      )
  })
  
  # -------------------------------
  # Tab 2: View by Label / Gene
  # -------------------------------
  filtered_label_data <- reactive({
    req(input$selected_labels)
    selected <- input$selected_labels
    
    sub <- df_label %>%
      select(Species, Order, Clade, CommonName, timeFromHuman_MY, all_of(selected))
    
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
    
    # Define the desired order of Cladees
    Clade_order <- c("Coelacanth", "Amphibia", "Reptiles", "Birds", "Monotremes", "Marsupials", "Eutheria", "Primates")
    
    # Convert Clade column to an ordered factor first
    df_long$Clade <- factor(df_long$Clade, levels = Clade_order, ordered = TRUE)
    
    
    df_sorted <- df_long %>%
      # arrange(desc(timeFromHuman_MY), Species, Label)
      # Change to sort by Clade first
      arrange(Clade, desc(timeFromHuman_MY), Species, Label)
    
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
        "<br>Clade:", Clade,
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
          text = "Conservation of Selected KZFP Genes",
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
          title = "Gene",
          tickfont = list(size = 10),
          automargin = TRUE
        ),
        margin = list(l = 150, r = 20, b = 0, t = 40)
      )
  })
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)

