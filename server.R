
#server.R
source("global.R")
source("helpers.R")
source("render_functions.R")

server = function(input, output, session) {
  
  # Dynamic plant part selector based on selected data source
  output$dynamicPlantPartSelector = renderUI({
    if(input$dataSource == "All") {
      plant_parts = unique(data$Plant_part)
    } else {
      plant_parts = unique(data[data$Source == input$dataSource, "Plant_part"])
    }
    
    selectInput(
      "control_plantpart", 
      label = "Select Control Plant Part", 
      choices = plant_parts, 
      selected = ifelse("leaf" %in% plant_parts, "leaf", plant_parts[1])
    )
  })
  
  observe({
    if (input$use_threshold) {
      enable("signal_threshold")  
    } else {
      disable("signal_threshold")  
    }
    
    # Process the threshold input
    threshold_data = process_input(input$signal_threshold, input$mode, input$use_threshold)
    threshold = threshold_data$threshold
    print(paste("Threshold used:", ifelse(is.null(threshold), "Default", threshold)))
  })
  
  observeEvent(input$mode, {
    if (input$mode %in% c("Absolute", "Relative")) {
      disable("secondaryGeneID") 
    } else {
      enable("secondaryGeneID")   
    }
  })
  
  observeEvent(input$goButton, {
    # Show loading spinner when button is clicked
    show("loading-spinner")
    
    selected_mode = input$mode  
    print(paste("Selected Mode:", selected_mode))  
    
    primary_id = input$id
    secondary_id = input$secondaryGeneID  
    
    threshold_data = process_input(input$signal_threshold, selected_mode, input$use_threshold)
    threshold = threshold_data$threshold
    
    if (is.null(threshold)) {
      # Hide loading spinner before showing error
      hide("loading-spinner")
      
      showNotification("Invalid threshold value. Please correct it.", type = "error")
      return()  
    }
    
    print(paste("Threshold:", threshold, "| First Call:", threshold_data$first_call))
    
    # Get gene data with filtering by source
    primary_data_raw = get_gene_data(primary_id, input$dataSource)
    
    if (is.null(primary_data_raw)) {
      # Hide loading spinner before showing error
      hide("loading-spinner")
      
      showNotification("Primary gene data not found.", type = "error")
      return()
    }
    
    # Aggregate gene data
    primary_data = aggregate_gene_data(primary_data_raw)
    
    if (selected_mode == "Compare") {
      secondary_data_raw = get_gene_data(secondary_id, input$dataSource)
      if (is.null(secondary_data_raw)) {
        # Hide loading spinner before showing error
        hide("loading-spinner")
        
        showNotification("Secondary gene data not found.", type = "error")
        return()
      }
      secondary_data = aggregate_gene_data(secondary_data_raw)
      renderComparison(primary_data, secondary_data, threshold, session)
    }
    else if (selected_mode == "Absolute") {
      renderAbsolute(primary_data, threshold, session)
    } 
    else if (selected_mode == "Relative") {
      control_plant_part = input$control_plantpart  
      renderRelative(primary_data, threshold, grey_mask = input$checkbox1, control_plant_part = control_plant_part, session)
    }
  })
}