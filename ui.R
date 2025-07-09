# ui.R
source("global.R")

ui = fluidPage(
  # Custom CSS for modern design
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
   
  ),
  
  useShinyjs(),
  
  # Loading spinner
  div(id = "loading-spinner",
      div(class = "spinner-card",
          div(class = "spinner"),
          div(class = "loading-text", "Processing... Please wait.")
      )
  ),
  
  # Main container
  div(class = "main-container",
      
      # Control panel
      div(class = "control-panel",
          
          # First row: Data Source and Mode Selection
          div(class = "row-controls",
              
              # Data Source Section
              div(class = "control-section",
                  div(class = "section-title",
                      div(class = "section-icon"),
                      "Data Source"
                  ),
                  selectInput("dataSource", 
                              label = NULL,
                              choices = c("Development", "Biotic", "Abiotic"), 
                              selected = "Development",
                              width = "100%")
              ),
              
              # Mode Selection Section
              div(class = "control-section",
                  div(class = "section-title",
                      div(class = "section-icon"),
                      "Analysis Mode"
                  ),
                  selectInput("mode", 
                              label = NULL,
                              choices = c("Absolute", "Relative", "Compare"), 
                              width = "100%"),
                  
                  div(class = "checkbox-wrapper",
                      checkboxInput("checkbox1", "Mask Low Expression", value = FALSE),
                      #span(class = "help-text", "Hide low expression values")
                  ),
                  
                  # Conditional Panel for Relative Mode
                  conditionalPanel(
                    condition = "input.mode == 'Relative'", 
                    div(class = "help-text", style = "margin-top: 15px;",
                        "Select a control plant part to normalize transcriptional levels in 'Relative' mode.")
                  ),
                  
                  # Control Plant Part Selection (only for Relative Mode)
                  conditionalPanel(
                    condition = "input.mode == 'Relative'", 
                    div(style = "margin-top: 15px;",
                        uiOutput("dynamicPlantPartSelector")
                    )
                  )
              )
          ),
          
          # Second row: Gene Selection and Threshold Settings
          div(class = "row-controls",
              
              # Gene Selection Section
              div(class = "control-section",
                  div(class = "section-title",
                      div(class = "section-icon"),
                      "Gene Selection"
                  ),
                  div(class = "help-text", "Enter gene IDs for analysis"),
                  textInput("id", 
                            "Primary Gene ID:", 
                            value = GENE_ID_DEFAULT1,
                            width = "100%"),
                  textInput("secondaryGeneID", 
                            "Secondary Gene ID:", 
                            value = GENE_ID_DEFAULT2,
                            width = "100%")
              ),
              
              # Threshold Settings Section
              div(class = "control-section",
                  div(class = "section-title",
                      div(class = "section-icon"),
                      "Threshold Settings"
                  ),
                  div(class = "help-text", "Set a signal threshold to filter results"),
                  
                  div(class = "checkbox-wrapper",
                      checkboxInput("use_threshold", "Use Custom Threshold", FALSE)
                  ),
                  
                  textInput("signal_threshold", 
                            "Signal Threshold:", 
                            value = "10",
                            width = "100%")
              )
          ),
          
          # Action Button
          div(style = "text-align: center; margin-top: 30px;",
              actionButton("goButton", "Analyze Expression", class = "btn-search")
          )
      ),
      
      # Visualization Area
      div(class = "visualization-area",
          h3("", 
             style = "text-align: center; color: #495057; margin-bottom: 30px; font-weight: 600;"),
          
          div(class = "image-container",
              div(class = "plant-image-wrapper",
                  img(id = "plant_image", 
                      src = "pp.svg", 
                      style = "width: 100%; height: auto; display: block;")
              ),
              
              div(id = "color_legend_div", 
                  class = "legend-wrapper",
                  style = "display: none;",
                  img(id = "color_legend", 
                      src = "www/color_legend.svg", 
                      style = "width: 100%; height: auto; display: block;")
              )
          )
      ),
      
      # Output for updated SVG
      uiOutput("updated_svg_output")
  )
)