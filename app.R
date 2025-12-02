# ATTEMPT FOR GITHUB 12-2-25

# TODO: add ALL genes to table, make pretty UI

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

# =======================================================
# Load datasets once at startup
# =======================================================

# Species-based clusters
df_label <- read.csv(
  "data/df_wide.csv",
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

df_table <- read.csv(
  "data/df_collapsed_stringified.csv",
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

# Attempt to add gene_choices using df_pairs
gene_choices <- sort(unique(df_pairs$Label), decreasing = TRUE)

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  titlePanel(
    ("KZFP Conservation Viewer"),
  ),
  
  absolutePanel(
    top = 58, left = 300,
    HTML("<span style='font-size:15px; color:gray; font-style:italic;'>
            Data revisualized from Imbeault et al. (2017).
            <br>DOI:
            <a href='https://doi.org/10.1038/nature21683' target='_blank' style='color:#8B008B; text-decoration:none;'>
            https://doi.org/10.1038/nature21683
            </a>
            </span>"),
    style = "z-index: 9999;"  # high z-index ensures it's on top
  ),
  
  tags$style(HTML("
    #combinedPlot .ytick text,
    #combinedPlot .xtick text,
    #combinedPlot .textpoint {
      user-select: text !important;
      -webkit-user-select: text !important;
      -moz-user-select: text !important;
      -ms-user-select: text !important;
      pointer-events: auto !important;
    }
  ")),
  
  tags$style(HTML("
    #labelPlot .ytick text,
    #labelPlot .xtick text,
    #labelPlot .textpoint {
      user-select: text !important;
      -webkit-user-select: text !important;
      -moz-user-select: text !important;
      -ms-user-select: text !important;
      pointer-events: auto !important;
    }
  ")),
  
  tags$style(HTML("
    /* Make DT horizontal scrollbar thicker and more visible */
    div.dataTables_scrollBody::-webkit-scrollbar {
      height: 16px !important;        /* scrollbar thickness */
    }
  
    div.dataTables_scrollBody::-webkit-scrollbar-track {
      background: #e0e0e0 !important; /* track color */
      border-radius: 8px;
    }
  
    div.dataTables_scrollBody::-webkit-scrollbar-thumb {
      background-color: #888 !important;  /* thumb color */
      border-radius: 8px;
      border: 3px solid #e0e0e0;          /* gap padding */
    }
  
    div.dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
      background-color: #555 !important;  /* darker on hover */
    }
  
    /* Firefox scrollbar */
    div.dataTables_scrollBody {
      scrollbar-width: thick;
      scrollbar-color: #888 #e0e0e0; 
    }
    
    /* Make scrollbar always visible */
    div.dataTables_scrollBody {
      overflow-x: scroll !important;
    }
  


  ")),
  
  
  
  
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
        selected = "Mus musculus",
        choices = setNames(species_choices$Species, species_choices$label),
        multiple = FALSE,
        options = list(
          placeholder = 'Type species or common name...',
          maxItems = 1
        ),
        width = '300px'
      ),
      br(),
      br(),
      
      # Two-column layout
      fluidRow(
        column(
          width = 12,    # left side (e.g., plot)
          uiOutput("dynamicClusterTableUI")
        ) 
      ),
      
      br(),
      br(),
      br(),
      
      fluidRow(
        column(
          width = 12,
          plotlyOutput("combinedPlot", height = "8000px")
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
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      # br(),
      br(),
      br(),
      plotlyOutput("labelPlot", height = "700px")
    )
  ),
  
  # Only show this absolutePanel when on the "View by Species" tab
  conditionalPanel(
    condition = "input.tabs == 'View by Species'",
    
    absolutePanel(
      img(src = "kzfp_phylogeny.png", height = "30px"),
      style = "top: 0%; left: 75%; z-index: 8000; background-color: rgba(255,255,255,0.9); padding: 0px; border-radius: 0px;"
    ),
    
    # Attempt to add static species plot in absolute panel
    absolutePanel(
      uiOutput("staticClusterPlotUI"),
      style = "top: 3%; left: 75%; z-index: 9990; background-color: rgba(255,255,255,0.0); padding: 0px; border-radius: 0px;"
    ),
    
    absolutePanel(
      uiOutput("speciesInfoPanel"),
      style = "top: 16%; left: 25%; z-index: 9999; background-color: rgba(255,255,255,0); padding: 0px; border-radius: 2px;"
    ),
    
    uiOutput("speciesImagePanel")
  ),
  
  # Only show this absolutePanel when on the "View by Gene" tab
  # conditionalPanel(
  #   condition = "input.tabs == 'View by Gene'",
  #   
  #   absolutePanel(
  #     top = 250, left = 165,
  #     img(src = "kzfp_phylogeny.png", height = "133px")
  #   )
  # )
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
    
    df_sorted <- df_long %>%
      arrange(desc(timeFromHuman_MY), Species, Label)
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
      style = "width: 600px; max-width: 90%; margin: 0 auto;",  # fixed width + responsive max + centered
      tagList(
        h2(HTML(paste("KZFP Orthologs for <i>", species_row$Species[1], "</i> — ", species_row$CommonName[1]))),
        p(strong("Class:"), species_row$Class[1], strong("| Order:"), species_row$Order[1], strong("| Time from Human:"), species_row$timeFromHuman_MY[1], "million years")
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
        img(src = img_file, height = "150px"),
        style = "top: 0%; left: 50%; z-index: 6000;"
      )
    }
  })
  
  # Dynamic UI for species table height
  output$dynamicClusterTableUI <- renderUI({
    df <- filtered_species_data()
    validate(
      need(!is.null(df), "No data available for selected species.")
    )
    
    n_rows <- nrow(df)
    table_height <- 750  # 20px per row, min 200px
    div(
      style = paste0("height:", table_height, "px; overflow-y:auto;"),
      DT::dataTableOutput("clusterTable", width = "100%")
    )
  })
  
  # Static UI for species plot height
  output$staticClusterPlotUI <- renderUI({
    df <- filtered_species_data()
    n_rows <- length(unique(df$Label))
    plotOutput("staticClusterPlot", width = "300px", height = "180px")
  })
  
  output$clusterTable <- DT::renderDataTable({
    df_table <- df_table %>%
      mutate(
        gnomad_link = paste0(
          "https://gnomad.broadinstitute.org/gene/",
          `gene_id`,
          "?dataset=gnomad_r4"
        )
      )
    
    df_table <- df_table %>%
      mutate(
        mgi_link = paste0(
          "https://www.informatics.jax.org/quicksearch/summary?queryType=exactPhrase&query=",
          `gene`,
          "&submit=Quick%0D%0ASearch"
        )
      )
    
    df_table <- df_table %>%
      mutate(
        impc_link = paste0(
          "https://www.mousephenotype.org/data/search?term=",
          `gene`
        )
      )
    
    df_table <- df_table %>%
      mutate(
        percent_conserved = num_species_w_cluster_associated_with_gene / 191
      )
    
    # --- Reorder/select columns ---
    df_display <- df_table %>%
      dplyr::select(
        `Gene` = gene,
        `Species with a KRAB-ZFP Cluster Associated with Gene` = num_species_w_cluster_associated_with_gene,
        `Percent Conserved - All Species` = percent_conserved,
        `Gene ID` = gene_id,
        `pLI` = pLI,
        `o/e` = oe,
        `GnomAD Link` = gnomad_link,
        `MGI Link` = mgi_link,
        `IMPC Link` = impc_link,
        `Clusters Associated with Gene` = Cluster_str
      )
    
    df_display$`GnomAD Link` <- paste0(
      "<a href='", df_display$`GnomAD Link`, 
      "' target='_blank'>View in gnomAD (", df_display$Gene, ")</a>"
    )
    
    df_display$`MGI Link` <- paste0(
      "<a href='", df_display$`MGI Link`, 
      "' target='_blank'>Search for Mouse Orthologs in MGI (", df_display$Gene, ") </a>"
    )
    
    df_display$`IMPC Link` <- paste0(
      "<a href='", df_display$`IMPC Link`, 
      "' target='_blank'>Search for Mouse Phenotypes in IMPC (", df_display$Gene, ")</a>"
    )
    
    df_display$`Percent Conserved - All Species` <-
      scales::percent(df_display$`Percent Conserved - All Species`, accuracy = 0.1)
    
    df_display$pLI <- sprintf("%.3f", df_display$pLI)
    df_display$`o/e` <- sprintf("%.3f", df_display$`o/e`)
    
    colnames(df_display) <- c(
      "<img src='gene_icon.jpg' height='20'> Gene",
      "Species with a KRAB-ZFP Cluster Associated with Gene",
      "Percent of Species with Cluster Ortholog",
      "Gene ID",
      "<img src='gnomAD.svg' height='20'> pLI",
      "<img src='gnomAD.svg' height='20'> o/e",
      "<img src='gnomAD.svg' height='20'> Link",
      "<img src='mgi_logo.png' height='20'> Link",
      "<img src='impc_logo.svg' height='20'> Link",
      "Clusters Associated with Gene"
    )
    
    # --- Display as a datatable ---
    DT::datatable(
      df_display,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size: 16px; font-weight: bold; color: #333333;',
        'Table 1: KZFP Ortholog Summary'
      ),
      
      escape = FALSE,
      rownames = FALSE,
      options = list(
        scrollY = TRUE,
        scrollX = TRUE,             # helpful if table gets wide
        pageLength = 7,
        autoWidth = TRUE,
        dom = 'tp',
        order = list(list(1, 'desc'), list(0, 'asc')),
        columnDefs = list(
          # Gene
          list(className = 'dt-center', width = '80px',  targets = 0),
          # Species with a KRAB-ZFP Cluster Associated with Gene
          list(className = 'dt-center', width = '140px', targets = 1),
          # Percent Conserved - All Species
          list(className = 'dt-center', width = '110px', targets = 2),
          # Gene ID
          list(className = 'dt-center', width = '110px', targets = 3),
          # pLI
          list(className = 'dt-center', width = '60px',  targets = 4),
          # o/e
          list(className = 'dt-center', width = '60px',  targets = 5),
          # GnomAD Link
          list(className = 'dt-center', width = '140px', targets = 6),
          # MGI Link
          list(className = 'dt-center', width = '140px', targets = 7),
          # IMPC Link
          list(className = 'dt-center', width = '140px', targets = 8),
          # Clusters Associated with Gene
          list(className = 'dt-center', width = '260px', targets = 9)
        )
      )
    )
  })
  
  output$staticClusterPlot <- renderPlot({
    df <- filtered_species_data()
    index <- which(df$Species == input$selected_species)
    
    validate(
      need(!is.null(df), paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    df$present_num <- as.numeric(df$present)
    
    ggplot(df, aes(x = Species, y = factor(Label))) +
      geom_tile(aes(fill = present), alpha = 1.0) +
      scale_fill_manual(values = c("lightgrey", "violetred4")) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks   = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0)  # remove white border
      )
  }, bg = "transparent")
  
  
  
  output$combinedPlot <- renderPlotly({
    df_pairs <- df_pairs %>%
      dplyr::rename(
        Gene = Label
      )
    
    df_pairs <- df_pairs %>%
      dplyr::rename(
        Label = `Cluster #`
      )
    
    df_pairs <- df_pairs %>%
      mutate(Label = as.character(Label))
    
    df <- filtered_species_data()
    req(df, input$selected_species)
    
    # Display message if no genes/clusters for this species
    validate(
      need(nrow(df) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    df$present_num <- as.numeric(df$present)
    
    # ---- consistent label order ----
    label_levels <- if (is.factor(df$Label)) levels(df$Label) else unique(df$Label)
    df$Label <- factor(df$Label, levels = label_levels, ordered = TRUE)
    
    # 1) Collapse all Gene values per Label into a single string
    df_pairs_collapsed <- df_pairs %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        Gene_all = paste(unique(Gene), collapse = ", "),
        .groups = "drop"
      )
    
    # 2) Join that onto your label_levels and build the text
    labels_df <- data.frame(Label = label_levels) %>%
      dplyr::left_join(
        df_pairs_collapsed,   # <- use collapsed table
        by = "Label"
      ) %>%
      dplyr::mutate(
        Label_text = dplyr::if_else(
          is.na(Gene_all),
          as.character(Label),    # fallback: show Label if no genes
          as.character(Gene_all)  # all Gene matches in one long string
        )
      )
    
    # -------- LEFT PLOT: "one-column table" of Gene names --------
    fig1 <- plot_ly(
      data = labels_df,
      x = ~0,                  # dummy x
      y = ~Label,              # keep Label for shared Y alignment
      type = "scatter",
      mode = "text",
      text = ~Label_text,      # << show Gene names here
      textposition = "middle right",
      hoverinfo = "none",
      cliponaxis = FALSE
      
    ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          title = ""
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          
          
          title = "KZFP Clusters and Associated Genes",
          tickfont = list(size = 10),
          automargin = TRUE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 150, r = 5, b = 0, t = 0)
      )
    
    # -------- RIGHT PLOT: your existing heatmap --------
    fig2 <- plot_ly(
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
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(size = 5),
          automargin = TRUE
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          title = "",
          tickfont = list(size = 10),
          automargin = TRUE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 0, r = 20, b = 0, t = 0)
      )
    
    
    fig3 <- plot_ly(
      type = "scatter",
      mode = "text",
      x = 0, y = 0,
      text = "Triple click<br><b>Gene Names</b><br>to select full list<br>per Cluster",
      textposition = "middle center",
      hoverinfo = "none"
    ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 20, b = 20, l = 20, r = 20),
        paper_bgcolor = "rgba(245,245,245,0.95)",
        plot_bgcolor  = "rgba(245,245,245,0.95)"
      )
    
    fig4 <- plot_ly() %>%
      layout(
        images = list(
          list(
            source = base64enc::dataURI(file = "www/kzfp_phylogeny.png"),
            xref = "paper", yref = "paper",
            x = 0, y = 1,
            sizex = 1, sizey = 1,
            xanchor = "left", yanchor = "top"
          )
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )
    
    # -------- COMBINE SIDE BY SIDE, SHARE Y --------
    subplot(
      # fig3,
      # fig4,
      # fig1,
      # fig2,
      # nrows  = 2,
      # shareY = TRUE,          # y-axes aligned
      # heights = c(0.023, 0.977),    # 75% height for row 1, 25% for row 2
      # widths = c(0.1, 0.9), # adjust relative widths
      # margin = 0.00
      
      fig4,
      fig3,
      fig2,
      fig1,
      nrows  = 2,
      shareY = TRUE,          # y-axes aligned
      heights = c(0.023, 0.977),    # 75% height for row 1, 25% for row 2
      widths = c(0.8, 0.2), # adjust relative widths
      margin = 0.00
      
    ) %>%
      layout(
        title = list(
          text = paste0("KZFP Gene Conservation for ", input$selected_species),
          x = 0,
          xanchor = "left",
          font = list(size = 16)
        ),
        plot_bgcolor = "#fff",
        showlegend = FALSE
        
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
    clusters <- unique(df_pairs$`Cluster #`[df_pairs$Label %in% selected_gene_list])
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
  
  
  
  
  # 
  # output$dynamicLabelPlotUI <- renderUI({
  #   df <- filtered_label_data()
  #   n_rows <- length(unique(df$Cluster))
  #   plot_height <- max(600, n_rows * 50)  # 15px per row, minimum 400px
  #   plotlyOutput("labelPlot", width = "1500px", height = paste0(plot_height, "px"))
  # })
  
  output$labelPlot <- renderPlotly({
    # attempt to adapt combinedPlot here
    # Start of previous code for labelPlot
    df <- filtered_label_data()
    req(df)
    # df$present_num <- as.numeric(df$present)
    
    df_pairs <- df_pairs %>%
      dplyr::rename(
        Gene = Label
      )
    
    df_pairs <- df_pairs %>%
      dplyr::rename(
        Label = `Cluster #`
      )
    
    df <- df %>%
      dplyr::rename(
        Label = Cluster
      )
    
    df_pairs <- df_pairs %>%
      mutate(Label = as.character(Label))
    # 
    # df <- filtered_species_data()
    # req(df, input$selected_species)
    
    # Display message if no genes/clusters for this species
    validate(
      need(nrow(df) > 0, paste("No labeled KZFP genes found for", input$selected_species))
    )
    
    df$present_num <- as.numeric(df$present)
    
    # ---- consistent label order ----
    label_levels <- if (is.factor(df$Label)) levels(df$Label) else unique(df$Label)
    df$Label <- factor(df$Label, levels = label_levels, ordered = TRUE)
    
    # 1) Collapse all Gene values per Label into a single string
    df_pairs_collapsed <- df_pairs %>%
      dplyr::group_by(Label) %>%
      dplyr::summarise(
        Gene_all = paste(unique(Gene), collapse = ", "),
        .groups = "drop"
      )
    
    # 2) Join that onto your label_levels and build the text
    labels_df <- data.frame(Label = label_levels) %>%
      dplyr::left_join(
        df_pairs_collapsed,   # <- use collapsed table
        by = "Label"
      ) %>%
      dplyr::mutate(
        Label_text = dplyr::if_else(
          is.na(Gene_all),
          as.character(Label),    # fallback: show Label if no genes
          as.character(Gene_all)  # all Gene matches in one long string
        )
      )
    
    # -------- LEFT PLOT: "one-column table" of Gene names --------
    fig1 <- plot_ly(
      data = labels_df,
      x = ~0,                  # dummy x
      y = ~Label,              # keep Label for shared Y alignment
      type = "scatter",
      mode = "text",
      text = ~Label_text,      # << show Gene names here
      textposition = "middle right",
      hoverinfo = "none",
      cliponaxis = FALSE
      
    ) %>%
      layout(
        xaxis = list(
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          title = ""
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          
          
          title = "KZFP Clusters and Associated Genes",
          tickfont = list(size = 10),
          automargin = TRUE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 150, r = 5, b = 0, t = 0)
      )
    
    # -------- RIGHT PLOT: your existing heatmap --------
    fig2 <- plot_ly(
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
        xaxis = list(
          title = "Species",
          tickangle = 60,
          tickfont = list(size = 5),
          automargin = TRUE
        ),
        yaxis = list(
          range = c(0, length(label_levels) + 0),
          title = "",
          tickfont = list(size = 10),
          automargin = TRUE,
          categoryorder = "array",
          categoryarray = label_levels
        ),
        margin = list(l = 0, r = 20, b = 0, t = 0)
      )
    
    
    fig3 <- plot_ly(
      type = "scatter",
      mode = "text",
      x = 0, y = 0,
      text = "Triple click<br><b>Gene Names</b><br>to select full list<br>per Cluster",
      textposition = "middle center",
      hoverinfo = "none"
    ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 20, b = 20, l = 20, r = 20),
        paper_bgcolor = "rgba(245,245,245,0.95)",
        plot_bgcolor  = "rgba(245,245,245,0.95)"
      )
    
    fig4 <- plot_ly() %>%
      layout(
        images = list(
          list(
            source = base64enc::dataURI(file = "www/kzfp_phylogeny.png"),
            xref = "paper", yref = "paper",
            x = 0, y = 1,
            sizex = 1, sizey = 1,
            xanchor = "left", yanchor = "top"
          )
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )
    
    # -------- COMBINE SIDE BY SIDE, SHARE Y --------
    subplot(
      # fig3,
      # fig4,
      # fig1,
      # fig2,
      # nrows  = 2,
      # shareY = TRUE,          # y-axes aligned
      # heights = c(0.023, 0.977),    # 75% height for row 1, 25% for row 2
      # widths = c(0.1, 0.9), # adjust relative widths
      # margin = 0.00
      
      fig4,
      fig3,
      fig2,
      fig1,
      nrows  = 2,
      shareY = TRUE,          # y-axes aligned
      heights = c(0.2, 0.8),    # 75% height for row 1, 25% for row 2
      widths = c(0.8, 0.2), # adjust relative widths
      margin = 0.00
      
    ) %>%
      layout(
        title = list(
          text = paste0("KZFP Gene Conservation for ", input$selected_species),
          x = 0,
          xanchor = "left",
          font = list(size = 16)
        ),
        plot_bgcolor = "#fff",
        showlegend = FALSE
        
      )
    
    
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # # Start of previous code for labelPlot
    # df <- filtered_label_data()
    # req(df)
    # df$present_num <- as.numeric(df$present)
    # 
    # plot_ly(
    #   data = df,
    #   x = ~Species,
    #   y = ~Cluster,
    #   z = ~present_num,
    #   type = "heatmap",
    #   colors = c("lightgrey", "violetred4"),
    #   opacity = 1,
    #   text = ~paste(
    #     "Species:", Species,
    #     "<br>Order:", Order,
    #     "<br>Class:", Class,
    #     "<br>Common Name:", CommonName,
    #     "<br>Cluster:", Cluster,
    #     "<br>Present:", present,
    #     "<br>Time from Human (MY):", timeFromHuman_MY
    #   ),
    #   hoverinfo = "text",
    #   showscale = FALSE
    # ) %>%
    #   layout(
    #     title = list(
    #       text = "Conservation of Selected KRAB-ZFPs Across Vertebrate Species",
    #       x = 0.05,
    #       font = list(size = 20)
    #     ),
    #     xaxis = list(
    #       title = "Species",
    #       tickangle = 60,
    #       tickfont = list(size = 5),
    #       automargin = TRUE
    #     ),
    #     yaxis = list(
    #       title = "KRAB-ZFP",
    #       tickfont = list(size = 10),
    #       automargin = TRUE
    #     ),
    #     margin = list(l = 150, r = 20, b = 0, t = 40)
    #   )
  })
}

# =======================================================
# Run App
# =======================================================
shinyApp(ui, server)
