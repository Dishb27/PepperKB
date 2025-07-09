library(shiny)
library(reshape2)
library(shinyjs)
library(plotly)
library(DT)
library(waiter)  

# Load input data
data = read.csv("fpkm_data.csv", header = TRUE, row.names = 1)
plant_data = read.csv("plantpart.csv", header = TRUE)
details_data = read.csv("details.csv", header = TRUE)

# Define UI with input controls and layout
ui = fluidPage(
  useShinyjs(),
  use_waiter(),  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # Title Panel with Styled Header
  
  
  # Input Controls
  fluidRow(
    column(12,
           div(class = "input-section",  
               # Gene ID Input
               textAreaInput("gene_ids", 
                             label = div("Enter Gene IDs:", class = "input-label"),
                             value = "", 
                             placeholder = "Enter gene ids, separated by commas.",
                             width = "100%", rows = 4),
               
               # Action Buttons and Controls
               actionButton("example_btn", "Try an Example", class = "btn-green"),
               checkboxInput("enable_plant_part", "Enable Plant Tissue Selection", value = FALSE),
               selectInput("plant_part", "Choose a Plant Tissue:", 
                           choices = c("Fruit", "Leaf", "Flower", "Root", "Stem"), 
                           selected = "Fruit", 
                           multiple = FALSE),
               selectInput("color_scale", "Choose Color Scale:", 
                           choices = c("YlGnBu", "RdBu", "Blues", "Greens", "Greys", "YlOrBr", "YlOrRd", "RdYlBu", "RdYlGn", "YlGn"), 
                           selected = "Greens"),
               div(class = "controls-section",
                   actionButton("generate", "Generate Heatmap", class = "btn-green"),
                   actionButton("reset", "Reset", class = "btn-red")
               )
           )
    )
  ),
  
  # Error Message Container
  fluidRow(
    column(12,
           div(id = "errorTextContainer", class = "error-message",
               textOutput("errorText")
           )
    )
  ),
  
  # Heatmap UI Output
  uiOutput("heatmap_ui"),
  
  # Sample Details Table
  fluidRow(
    column(12,
           div(class = "output-section",
               DTOutput("sample_details_table")
           )
    )
  )
)

server = function(input, output, session) {
  
  # Create loading screen with progress indicator
  w <- Waiter$new(
    html = tagList(
      spin_loaders(5, color = "#59a785"),
      h4("Processing data...", style = "color: #226b55; margin-top: 15px;")
    ),
    color = waiter::transparent(0.7)
  )
  
  # Reactive values to manage application state
  values <- reactiveValues(
    heatmap_data = NULL,
    filtered_data = NULL,
    error_message = NULL,
    found_genes = NULL,
    not_found_genes = NULL
  )
  
  # Display error message
  displayError <- function(message) {
    values$error_message <- message
    output$errorText <- renderText(values$error_message)
    shinyjs::show("errorTextContainer")
  }
  
  # Clear error messages
  clearError <- function() {
    values$error_message <- NULL
    output$errorText <- renderText("")
    shinyjs::hide("errorTextContainer")
  }
  
  # Load example gene IDs
  observeEvent(input$example_btn, {
    updateTextAreaInput(session, "gene_ids", value = "Pn1.1040, Pn1.1041, Pn1.1042, Pn1.1043, Pn1.1956, Pn1.1957, Pn1.1958, Pn10.2008, Pn10.2009, Pn21.402, Pn21.404, Pn21.405, Pn21.406")
  })
  
  # Toggle plant part selection
  observeEvent(input$enable_plant_part, {
    if (input$enable_plant_part) {
      shinyjs::enable("plant_part")
    } else {
      shinyjs::disable("plant_part")
      
      # Reset to full dataset if data exists
      if (!is.null(values$heatmap_data)) {
        values$filtered_data <- values$heatmap_data
        updateHeatmap()
        updateSampleTable(colnames(values$heatmap_data))
      }
    }
  })
  
  # Process and validate gene IDs
  processGeneIDs <- function(gene_ids_text) {
    # Clean and validate gene IDs
    gene_ids <- unlist(strsplit(gene_ids_text, ","))
    gene_ids <- trimws(gsub("\\.$", "", gene_ids))
    gene_ids <- gene_ids[gene_ids != ""]
    
    # Check gene ID format
    invalid_ids <- gene_ids[!grepl("^Pn\\d+\\.\\d+$", gene_ids)]
    if (length(invalid_ids) > 0) {
      displayError(paste("Invalid Gene ID format:", paste(invalid_ids, collapse=", ")))
      return(NULL)
    }
    
    # Find existing genes in dataset
    found_genes <- gene_ids[gene_ids %in% rownames(data)]
    not_found_genes <- gene_ids[!gene_ids %in% rownames(data)]
    
    values$found_genes <- found_genes
    values$not_found_genes <- not_found_genes
    
    # Comprehensive error handling for gene IDs
    if (length(found_genes) == 0) {
      displayError("No Gene IDs found in dataset.")
      return(NULL)
    }
    
    if (length(not_found_genes) > 0) {
      displayError(paste("Not found:", paste(not_found_genes, collapse=", ")))
    } else {
      clearError()
    }
    
    return(data[found_genes, , drop=FALSE])
  }
  
  # Update heatmap visualization
  updateHeatmap <- function() {
    if (is.null(values$filtered_data) || nrow(values$filtered_data) == 0) {
      return()
    }
    
    melted_data <- melt(as.matrix(values$filtered_data))
    colnames(melted_data) <- c("GeneID", "SampleID", "Expression")
    
    # Calculate dynamic height based on number of genes
    num_genes <- nrow(values$filtered_data)
    num_samples <- ncol(values$filtered_data)
    
    # Calculate minimum height needed for proper visualization
    min_height_per_gene <- 25  # pixels per gene
    min_height_per_sample <- 20  # pixels per sample
    
    dynamic_height <- max(600, num_genes * min_height_per_gene)
    dynamic_width <- max(800, num_samples * min_height_per_sample)
    
    output$heatmap_ui <- renderUI({
      div(class = "output-section",
          div(class = "heatmap-container",
              plotlyOutput("heatmapPlot", height = paste0(dynamic_height, "px"), width = "100%")
          )
      )
    })
    
    # Interactive Plotly heatmap
    output$heatmapPlot <- renderPlotly({
      plot_ly(
        data = melted_data,
        x = ~SampleID,
        y = ~GeneID,
        z = ~Expression,
        type = "heatmap",
        colors = input$color_scale,
        colorbar = list(title = "<b>Expression Level</b>", len = 0.5),
        hovertemplate = "Gene: %{y}<br>Sample: %{x}<br>Expression: %{z:.2f}",
        showscale = TRUE,
        width = dynamic_width,
        height = dynamic_height
      ) %>% 
        layout(
          title = list(
            text = paste0("<b>Expression Heatmap (", nrow(values$filtered_data), " genes × ", ncol(values$filtered_data), " samples)</b>"),
            font = list(family = "Inter", size = 18, color = "black")
          ),
          xaxis = list(
            title = list(
              text = "<b>Sample ID</b>",
              font = list(family = "Inter", size = 16, color = "black")
            ),
            tickangle = 45,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = list(
              text = "<b>Gene ID</b>",
              font = list(family = "Inter", size = 16, color = "black")
            ),
            tickfont = list(size = 12)
          ),
          margin = list(l = 100, b = 100, r = 50, t = 80)
        ) %>%
        config(
          scrollZoom = TRUE,
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c('lasso2d', 'select2d', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'),
          displaylogo = FALSE
        )
    })
    
  }
  
  # Update sample details table
  updateSampleTable <- function(sample_ids) {
    if (length(sample_ids) == 0) {
      output$sample_details_table <- renderDT(NULL)
      return()
    }
    
    matching_samples <- details_data[details_data$Sample_ID %in% sample_ids, ]
    names(matching_samples) <- gsub("\\.", " ", names(matching_samples))
    
    output$sample_details_table <- renderDT({
      datatable(
        matching_samples[, !names(matching_samples) %in% c("Plant part", "Sample")], 
        options = list(
          pageLength = 10, 
          scrollX = TRUE, 
          autoWidth = TRUE,
          dom = 'ftlip', 
          searchHighlight = TRUE,
          lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'All'))
        ), 
        class = 'custom-table display responsive nowrap',
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #226b55; font-size: 18px; font-weight: bold; margin-bottom: 15px;',
          paste0("Sample Details (", nrow(matching_samples), " samples)")
        ),
        rownames = FALSE
      )
    })
  }
  
  # Generate heatmap event handler
  observeEvent(input$generate, {
    clearError()
    w$show() # Show loading screen
    
    # Validate input
    if (trimws(input$gene_ids) == "") {
      displayError("No Gene IDs entered. Please input valid Gene IDs.")
      w$hide()
      return()
    }
    
    # Process gene IDs
    selected_data <- processGeneIDs(input$gene_ids)
    
    if (is.null(selected_data)) {
      w$hide()
      return()
    }
    
    # Store and update data
    values$heatmap_data <- selected_data
    values$filtered_data <- selected_data
    
    # Update visualizations
    updateHeatmap()
    updateSampleTable(colnames(selected_data))
    
    # Enable plant part filter if checkbox is on
    if (input$enable_plant_part) {
      filterByPlantPart()
    }
    
    w$hide()
  })
  
  # Filter samples by plant part
  filterByPlantPart <- function() {
    if (is.null(values$heatmap_data) || !input$enable_plant_part) {
      return()
    }
    
    w$show()
    
    # Get samples for selected plant part
    plant_samples <- plant_data[plant_data$PlantPart == input$plant_part, "Sample_ID"]
    
    if (length(plant_samples) == 0) {
      displayError(paste("No samples found for:", input$plant_part))
      w$hide()
      return()
    }
    
    # Find matching samples
    matching_sample_ids <- intersect(colnames(values$heatmap_data), plant_samples)
    
    if (length(matching_sample_ids) == 0) {
      displayError(paste("No matching samples for", input$plant_part))
      values$filtered_data <- values$heatmap_data[, numeric(0), drop=FALSE]
      w$hide()
      return()
    }
    
    # Update filtered data
    values$filtered_data <- values$heatmap_data[, matching_sample_ids, drop=FALSE]
    
    # Update visualizations
    updateHeatmap()
    updateSampleTable(matching_sample_ids)
    
    clearError()
    w$hide()
  }
  
  # Reset all inputs and states
  observeEvent(input$reset, {
    updateTextAreaInput(session, "gene_ids", value = "")
    updateCheckboxInput(session, "enable_plant_part", value = FALSE)
    updateSelectInput(session, "plant_part", selected = "Fruit")
    updateSelectInput(session, "color_scale", selected = "Greens")
    
    # Clear all reactive values and outputs
    values$heatmap_data <- NULL
    values$filtered_data <- NULL
    values$found_genes <- NULL
    values$not_found_genes <- NULL
    
    output$heatmap_ui <- renderUI(NULL)
    output$sample_details_table <- renderDT(NULL)
    
    clearError()
    shinyjs::disable("plant_part")
  })
}

shinyApp(ui = ui, server = server)