# ATTEMPT FOR GITHUB 11-14-25

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

# =======================================================
# Load datasets once at startup
# =======================================================
# df_label <- read.csv(
#   "C:/Users/danie/Documents/nih_postbac/ShinyAppTronoData/df_Class.csv",
#   check.names = FALSE,
#   stringsAsFactors = FALSE
# )

# Species-based clusters
df_label <- read.csv(
  "data/df_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df2 <- read.csv(
  "data/41586_2017_BFnature21683_MOESM103_ESM.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_pairs <- read.csv(
  "data/gene_cluster_pairs.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_gnomAD <- read.csv(
  "data/gnomAD_pli_oe_KZFP_genes.csv",
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

# Attempt to add gene_choices using df2
gene_choices <- sort(unique(df2$Label), decreasing = TRUE)

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  titlePanel(
    ("KZFP Conservation Viewer"),
  ),
  
  absolutePanel(
    top = 80, left = 300,
    HTML("<span style='font-size:16px; color:gray; font-style:italic;'>
            Data revisualized from Imbeault et al. (2017), DOI:
            <a href='https://doi.org/10.1038/nature21683' target='_blank' style='color:#8B008B; text-decoration:none;'>
            https://doi.org/10.1038/nature21683
            </a>
            </span>"),
    
    style = "z-index: 9999;"  # high z-index ensures it's on top
    
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
          width = 7,    # left side (e.g., plot)
          uiOutput("dynamicClusterTableUI")
        ) #,
        # column(
        #   width = 4,    # right side (e.g., summary/statistics)
        #   br(),
        #   br(),
        #   br(),
        #   br(),
        #   
        #   uiOutput("staticClusterPlotUI")
        # )
      ),
      
      br(),
      br(),
      br(),
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
        "selected_genes",
        "Select one or more Gene(s):",
        choices = gene_choices,
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
      # top = 324, left = 1046,
      img(src = "kzfp_phylogeny_leftMargin.png", height = "44px"),
      style = "top: 40%; left: 65%; z-index: 9999; background-color: rgba(255,255,255,0.9); padding: 0px; border-radius: 2px;"
      
      # style = "z-index: 9999;"  # high z-index ensures it's on top
      
    ),
    
    # Attempt to add static species plot in absolute panel
    absolutePanel(
      # top = 155, left = 315,
      # uiOutput("speciesInfoPanel"),
      uiOutput("staticClusterPlotUI"),
      style = "top: 45%; left: 65%; z-index: 9999; background-color: rgba(255,255,255,0.9); padding: 0px; border-radius: 2px;"
    ),
    
    
    absolutePanel(
      # top = 155, left = 315,
      uiOutput("speciesInfoPanel"),
      style = "top: 12%; left: 22%; z-index: 9999; background-color: rgba(255,255,255,0.9); padding: 0px; border-radius: 2px;"
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
    
    # Define the desired order of Classes
    # Class_order <- c("Coelacanth", "Amphibia", "Reptiles", "Birds", "Monotremes", "Marsupials", "Eutheria", "Primates")
    
    # Convert Class column to an ordered factor first
    # df_long$Class <- factor(df_long$Class, levels = Class_order, ordered = TRUE)
    
    
    df_sorted <- df_long %>%
      arrange(desc(timeFromHuman_MY), Species, Label)
    # Change to sort by Class first
    # arrange(Class, desc(timeFromHuman_MY), Species, Label)
    
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
      style = "width: 700px; max-width: 90%; margin: 0 auto;",  # fixed width + responsive max + centered
      tagList(
        h2(HTML(paste("KZFP Gene Conservation for <i>", species_row$Species[1], "</i> — ", species_row$CommonName[1]))),
        p(strong("Class:"), species_row$Class[1], strong("| Order:"), species_row$Order[1], strong("| Time from Human:"), species_row$timeFromHuman_MY[1], "million years")
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
      # absolutePanel(
      #   top = 160, left = 1220,
      #   img(src = img_file, height = "150px"),
      #   style = "z-index: 9999;"
      # )
      
      absolutePanel(
        img(src = img_file, height = "150px"),
        style = "top: 20%; left: 70%; z-index: 9999;"
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
    plotlyOutput("staticClusterPlot", width = "479px", height = "180px")
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
            # color = Class_colors[Aves]
          ),
          automargin = TRUE
        ),
        yaxis = list(
          title = "KRAB-ZFP",
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
    
    
    cat("\nHEAD of original df from filtered_species_data():\n")
    print(head(df))
    
    df <- df %>%
      rename(
        Cluster = Label
      )
    
    cat("\nHEAD of original df with renamed Label column to Cluster:\n")
    print(head(df))
    
    
    cat("\n===== START: Adding gene labels to df =====\n")
    
    # -----------------------------------------
    # STEP 1 — Ensure both cluster columns are the same type
    # -----------------------------------------
    
    cat("\nConverting df$Cluster and df2$`Cluster #` to character...\n")
    
    df <- df %>%
      mutate(Cluster = as.character(Cluster))
    
    df_pairs <- df_pairs %>%
      mutate(`Cluster #` = as.character(`Cluster #`))
    
    cat("\ndf Cluster type:\n"); print(str(df$Cluster))
    cat("\ndf_pairs Cluster # type:\n"); print(str(df_pairs$`Cluster #`))
    
    
    # -----------------------------------------
    # STEP 2 — Join gene labels into df
    # df$Cluster  <-->  df_pairs$`Cluster #`
    # Creates one row per gene label
    # -----------------------------------------
    
    cat("\nJoining df with df_pairs to add GeneLabel...\n")
    
    df_new <- df %>%
      left_join(
        df_pairs %>% select(GeneLabel = Label, `Cluster #`),
        by = c("Cluster" = "Cluster #"),
        relationship = "many-to-many"
      )
    
    cat("\nHEAD of df_new (expanded rows):\n")
    print(head(df_new))
    
    
    df_summary <- df_new %>%
      # ensure each gene counts at most once per species
      group_by(GeneLabel, Species) %>%
      summarise(
        present_any = any(present == TRUE),
        .groups = "drop"
      ) %>%
      # now summarize across all species
      group_by(GeneLabel) %>%
      summarise(
        PresentCount = sum(present_any),
        PercentConserved = paste0(
          round(100 * PresentCount / n(), 1), "%"
        ),
        .groups = "drop"
      ) %>%
      arrange(desc(PresentCount), GeneLabel)
    
    cat("\nHEAD of df_summary:\n")
    
    print(head(df_summary))
    
    
    df_merged <- df_summary %>%
      inner_join(df_gnomAD, by = c("GeneLabel" = "gene"))
    
    df_merged <- df_merged %>%
      mutate(
        gnomad_link = paste0(
          "https://gnomad.broadinstitute.org/gene/",
          `Gene ID`,
          "?dataset=gnomad_r4"
        )
      )
    
    cat("\nHEAD of df_merged:\n")
    
    print(head(df_merged))
    
    
    
    # --- Reorder/select columns ---
    df_display <- df_merged %>%
      dplyr::select(
        `Gene` = GeneLabel,
        `Number of Species with a KRAB-ZFP Cluster Associated with this Gene` = PresentCount,
        `Percent Conserved - All Species` = PercentConserved,
        `Gene ID` = `Gene ID`,
        `pLI` = pLI,
        `o/e` = oe,
        `GnomAD Link` = gnomad_link
        # Label,
        # # Species,
        # # CommonName,
        # # Order,
        # # Class,
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
          #   # color = Class_colors[Aves]
          # ),
          # automargin = TRUE
        ),
        yaxis = list(
          title = "KRAB-ZFP",
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
  
  
  # Attempt to clear the issue
  filtered_label_data <- reactive({
    req(input$selected_genes)
    
    cat("\n---- filtered_label_data START ----\n")
    
    selected_gene_list <- input$selected_genes
    cat("selected_gene_list:\n")
    print(selected_gene_list)
    
    # 1. Get cluster numbers for the selected genes
    clusters <- unique(df2$`Cluster #`[df2$Label %in% selected_gene_list])
    cat("\nClusters found:\n")
    print(clusters)
    
    # 2. Convert clusters to character column names
    cluster_cols <- as.character(clusters)
    
    cat("\nCluster column names to extract from df_label:\n")
    print(cluster_cols)
    
    # 3. Check which exist
    valid_cluster_cols <- intersect(cluster_cols, colnames(df_label))
    
    cat("\nValid cluster columns in df_label:\n")
    print(valid_cluster_cols)
    
    req(length(valid_cluster_cols) > 0)
    
    # 4. Subset df_label by cluster columns
    sub <- df_label %>%
      dplyr::select(
        Species, Order, Class, CommonName, timeFromHuman_MY,
        dplyr::all_of(valid_cluster_cols)
      )
    
    cat("\nSub dataframe columns:\n")
    print(colnames(sub))
    
    # 5. Pivot longer
    df_long <- sub %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(valid_cluster_cols),
        names_to = "Cluster",
        values_to = "present"
      )
    
    cat("\npivot_longer result preview:\n")
    print(head(df_long))
    
    # 6. Frequency table
    cluster_freq <- df_long %>%
      dplyr::filter(present == TRUE) %>%
      dplyr::count(Cluster, name = "Frequency_T") %>%
      dplyr::arrange(dplyr::desc(Frequency_T), Cluster)
    
    cat("\nCluster frequency table:\n")
    print(cluster_freq)
    
    # 7. Sort df
    df_sorted <- df_long %>%
      dplyr::arrange(dplyr::desc(timeFromHuman_MY), Species, Cluster)
    
    cat("\nSorted df preview:\n")
    print(head(df_sorted))
    
    # 8. Factor ordering
    df_sorted$Cluster <- factor(df_sorted$Cluster, levels = rev(cluster_freq$Cluster), ordered = TRUE)
    df_sorted$Species <- factor(df_sorted$Species, levels = unique(df_sorted$Species), ordered = TRUE)
    
    cat("\n---- filtered_label_data END ----\n")
    
    df_sorted
  })
  
  
  
  # Dynamic UI for label plot height
  output$dynamicLabelPlotUI <- renderUI({
    df <- filtered_label_data()
    n_rows <- length(unique(df$Cluster))
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
      y = ~Cluster,
      z = ~present_num,
      type = "heatmap",
      colors = c("lightgrey", "violetred4"),
      opacity = 1,
      text = ~paste(
        "Species:", Species,
        "<br>Order:", Order,
        "<br>Class:", Class,
        "<br>Common Name:", CommonName,
        "<br>Cluster:", Cluster,
        "<br>Present:", present,
        "<br>Time from Human (MY):", timeFromHuman_MY
      ),
      hoverinfo = "text",
      showscale = FALSE
    ) %>%
      layout(
        title = list(
          text = "Conservation of Selected KRAB-ZFPs Across Vertebrate Species",
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
          title = "KRAB-ZFP",
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
