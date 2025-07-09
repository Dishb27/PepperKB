library(shiny)
library(heatmaply)
library(RColorBrewer)
library(DT)
library(shinyjs)
library(bslib)
library(plotly)
library(shinycssloaders)

# Load the data
data <- read.csv("fpkm_data.csv", header = TRUE, row.names = 1)
details_data <- read.csv("details.csv", header = TRUE)

# Define UI
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "lumen",
    primary = "#3e756c"
  ),
  
  # Include external CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")
  ),
  
  useShinyjs(),
  
  # Main container
  div(
    class = "app-container",
    
    # Sidebar
    div(
      class = "sidebar",
      
      # Gene IDs input
      div(
        class = "card",
        div(class = "card-header", icon("dna"), "Gene IDs"),
        div(
          class = "card-body",
          actionLink(
            "example_btn",
            span(icon("lightbulb"), "Try an example dataset"),
            class = "example-link"
          ),
          textAreaInput(
            "gene_ids",
            label = NULL,
            value = "",
            placeholder = "Enter gene IDs, separated by commas",
            rows = 4,
            resize = "vertical"
          ),
          
        )
      ),
      
      # Color scale selection
      div(
        class = "card",
        div(class = "card-header", icon("palette"), "Visualization Options"),
        div(
          class = "card-body",
          
          div(class = "mb-3",
              tags$label("Color Scale", class = "form-label"),
              selectInput(
                "color_scale",
                label = NULL,
                choices = list(
                  "Blues" = "Blues",
                  "Sequential" = c(
                    "YlGnBu" = "YlGnBu", 
                    "Greens" = "Greens", 
                    "Purples" = "Purples"
                  ),
                  "Diverging" = c(
                    "RdBu" = "RdBu",
                    "RdYlBu" = "RdYlBu", 
                    "PiYG" = "PiYG"
                  )
                ),
                selected = "Greens"
              )
          ),
          
          div(
            tags$label("Visualization Type", class = "form-label"),
            radioButtons(
              "clustering_type",
              label = NULL,
              choices = c(
                "Heatmap (Clustering)" = "both",
                "Dendrogram Only" = "row"
              ),
              selected = "both"
            )
          )
        )
      ),
      
      # Status indicator (initially hidden)
      div(
        id = "status_container",
        style = "display: none;",
        div(
          class = "status-bar",
          div(id = "status_progress", class = "status-progress")
        ),
        div(id = "status_text", class = "status-text")
      ),
      
      # Error message
      div(id = "errorTextContainer", class = "alert alert-danger", style = "display: none;"),
      
      # Action buttons
      div(
        class = "d-grid gap-2",
        actionButton(
          "generate",
          span(icon("play"), "Generate Visualization"),
          class = "btn btn-primary btn-lg"
        ),
        actionButton(
          "reset",
          span(icon("rotate"), "Reset"),
          class = "btn btn-outline-secondary"
        )
      )
    ),
    
    # Main content
    div(
      class = "main-content",
      uiOutput("results_ui")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    processed_data = NULL,
    is_processed = FALSE,
    is_loading = FALSE
  )
  
  # Helper function to update the status bar
  updateStatus <- function(percent, text) {
    shinyjs::show("status_container")
    shinyjs::runjs(sprintf("document.getElementById('status_progress').style.width = '%s%%'", percent))
    shinyjs::html("status_text", text)
  }
  
  # Reset status bar
  resetStatus <- function() {
    shinyjs::hide("status_container")
    shinyjs::runjs("document.getElementById('status_progress').style.width = '0%'")
    shinyjs::html("status_text", "")
  }
  
  # Example button
  observeEvent(input$example_btn, {
    updateTextAreaInput(
      session,
      "gene_ids",
      value = "Pn1.1,Pn1.10,Pn1.100,Pn1.1040,Pn1.1041,Pn1.1042,Pn1.1043"
    )
  })
  
  # Process data function
  processData <- function() {
    shinyjs::hide("errorTextContainer")
    updateStatus(10, "Validating input...")
    
    gene_ids <- unlist(strsplit(input$gene_ids, "\\s*,\\s*"))
    gene_ids <- trimws(gene_ids[gene_ids != ""])
    
    # Validation checks
    if (length(gene_ids) < 1) {
      shinyjs::html("errorTextContainer", "Please enter at least 1 gene ID")
      shinyjs::show("errorTextContainer")
      resetStatus()
      return(NULL)
    }
    
    updateStatus(30, "Processing data...")
    selected_data <- data[rownames(data) %in% gene_ids, , drop = FALSE]
    
    if (nrow(selected_data) == 0) {
      shinyjs::html("errorTextContainer", "No matching genes found - check your input")
      shinyjs::show("errorTextContainer")
      resetStatus()
      return(NULL)
    }
    
    updateStatus(60, "Finalizing...")
    return(selected_data)
  }
  
  # Generate button
  observeEvent(input$generate, {
    rv$is_loading <- TRUE
    
    # Process the data and update the reactive value
    selected_data <- processData()
    
    if (!is.null(selected_data)) {
      rv$processed_data <- selected_data
      rv$is_processed <- TRUE
      updateStatus(80, "Rendering visualization...")
      
      # Explicitly re-render all outputs
      output$results_ui <- renderUI({
        renderResultsUI()
      })
      
      # Schedule hiding of status bar after 1.5 seconds
      shinyjs::delay(1500, resetStatus())
    }
    
    rv$is_loading <- FALSE
  })
  
  # Reset button
  observeEvent(input$reset, {
    updateTextAreaInput(session, "gene_ids", value = "")
    updateSelectInput(session, "color_scale", selected = "Blues")
    updateRadioButtons(session, "clustering_type", selected = "both")
    rv$processed_data <- NULL
    rv$is_processed <- FALSE
    shinyjs::hide("errorTextContainer")
    resetStatus()
    
    # Force UI to reflect the reset state
    output$results_ui <- renderUI({
      div(
        class = "empty-state",
        icon("chart-simple", class = "fa-3x mb-3"),
        p("Enter gene IDs and click 'Generate Visualization' to see results here.")
      )
    })
  })
  
  # Render results UI
  renderResultsUI <- function() {
    if (!rv$is_processed || is.null(rv$processed_data)) {
      return(div(
        class = "empty-state",
        icon("chart-simple", class = "fa-3x mb-3"),
        p("Enter gene IDs and click 'Generate Visualization' to see results here.")
      ))
    }
    
   
    div(
      # Visualization section
      div(
        class = "card",
        div(
          class = "card-header",
          icon("chart-column"),
          if (input$clustering_type == "both") {
            "Gene Expression Heatmap"
          } else {
            "Gene Expression Dendrogram"
          }
        ),
        div(
          class = "card-body p-0 plot-container",
          if (input$clustering_type == "both") {
            withSpinner(
              plotlyOutput("heatmap_plot", height = "600px"),
              type = 8,
              color = "#0c8b70"
            )
          } else {
            withSpinner(
              plotOutput("dendrogram_plot", height = "600px"),
              type = 8,
              color = "#0c8b70"
            )
          }
        )
      ),
      
      # Details table
      div(
        class = "card",
        div(class = "card-header", icon("table"), "Sample Details"),
        div(
          class = "card-body p-0",
          withSpinner(
            DTOutput("sample_details_table"),
            type = 8,
            color = "#717171"
          )
        )
      )
    )
  }
  
  # Initialize the results UI with a placeholder
  output$results_ui <- renderUI({
    if (rv$is_processed && !is.null(rv$processed_data)) {
      renderResultsUI()
    } else {
      div(
        class = "empty-state",
        icon("chart-simple", class = "fa-3x mb-3"),
        p("Enter gene IDs and click 'Generate Visualization' to see results here.")
      )
    }
  })
  
  # Render heatmap
  output$heatmap_plot <- renderPlotly({
    req(rv$processed_data)
    req(input$clustering_type == "both")
    
    selected_data <- rv$processed_data
    colors_pal <- colorRampPalette(brewer.pal(9, input$color_scale))
    
    # Create the heatmap
    p <- heatmaply(
      as.matrix(selected_data),
      colors = colors_pal,
      dendrogram = "row",
      showticklabels = c(TRUE, TRUE),
      scale = "row",
      key.title = "Expression",
      fontsize_row = 10,
      fontsize_col = 10,
      margins = c(100, 100, 60, 20),
      grid_color = "white",
      grid_width = 0.00001,
      label_names = c("Gene", "Sample", "Expression"),
      main = "Gene Expression Heatmap",
      subplot_widths = c(0.9, 0.1),
      width = NULL,
      height = NULL
    ) %>%
      layout(
        autosize = TRUE,
        font = list(family = "Inter"),
        xaxis = list(tickangle = 45)
      ) %>%
      config(responsive = TRUE)
    
    p
  })
  
  # Render dendrogram
  output$dendrogram_plot <- renderPlot({
    req(rv$processed_data)
    req(input$clustering_type == "row")
    
    selected_data <- rv$processed_data
    
    par(mar = c(6, 4, 4, 2), bg = "white")
    hc <- hclust(dist(selected_data))
    plot(hc,
         main = "Gene Expression Dendrogram",
         xlab = "",
         sub = "",
         cex = 0.9,
         col = "#1e293b",
         lwd = 2)
  })
  
  # Render sample details table - MODIFIED VERSION
  output$sample_details_table <- renderDT({
    req(rv$processed_data)
    
    selected_data <- rv$processed_data
    table_data <- details_data[details_data$Sample_ID %in% colnames(selected_data), ]
    
    # Remove unwanted columns
    table_data <- table_data[, !(names(table_data) %in% c("Sample", "Plant_part"))]
    
    # Clean up column names by replacing periods with spaces
    col_names <- names(table_data)
    col_names <- gsub("\\.", " ", col_names)
    
    datatable(
      table_data,
      options = list(
        pageLength = 10,
        dom = 'lftip',  
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        ),
        # Disable column filtering but keep global search
        initComplete = JS(
          "function(settings, json) {",
          "  $('div.dt-search').show();",  # Keep global search
          "  $('div.dt-search input').attr('placeholder', 'Search all columns...');",
          "}"
        )
      ),
      colnames = col_names,  
      class = 'modern-table cell-border hover',
      rownames = FALSE,
      selection = 'single',
      filter = 'none',  
      extensions = 'Responsive'  
    )
  })
  
  # Respond to clustering type change
  observeEvent(input$clustering_type, {
    if (rv$is_processed) {
      updateStatus(50, "Updating visualization type...")
      
      # Re-render the UI to show the correct plot
      output$results_ui <- renderUI({
        renderResultsUI()
      })
      
      updateStatus(100, "Visualization updated!")
      shinyjs::delay(1000, resetStatus())
    }
  })
  
  # Respond to color scale change
  observeEvent(input$color_scale, {
    if (rv$is_processed && input$clustering_type == "both") {
      updateStatus(50, "Updating color scheme...")
      
      # Force invalidation of heatmap plot
      output$heatmap_plot <- renderPlotly({
        req(rv$processed_data)
        selected_data <- rv$processed_data
        colors_pal <- colorRampPalette(brewer.pal(9, input$color_scale))
        
        heatmaply(
          as.matrix(selected_data),
          colors = colors_pal,
          dendrogram = "row",
          showticklabels = c(TRUE, TRUE),
          scale = "row",
          key.title = "Expression",
          fontsize_row = 10,
          fontsize_col = 10,
          margins = c(100, 100, 60, 20),
          grid_color = "white",
          grid_width = 0.00001,
          label_names = c("Gene", "Sample", "Expression"),
          main = "Gene Expression Heatmap",
          subplot_widths = c(0.9, 0.1),
          width = NULL,
          height = NULL
        ) %>%
          layout(
            autosize = TRUE,
            font = list(family = "Inter"),
            xaxis = list(tickangle = 45)
          ) %>%
          config(responsive = TRUE)
      })
      
      updateStatus(100, "Color scheme updated!")
      shinyjs::delay(1000, resetStatus())
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
