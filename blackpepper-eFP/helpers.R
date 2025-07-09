#helpers.R
# Function to retrieve gene data with filtering by source
get_gene_data = function(gene_id, data_source = "All") {
  filtered_data = data
  
  # Filter by data source if not "All"
  if (data_source != "All") {
    filtered_data = filtered_data %>% filter(Source == data_source)
  }
  
  # Filter for the specific gene ID
  gene_data = filtered_data %>% filter(GeneID == gene_id)
  
  if (nrow(gene_data) == 0) {
    showNotification("No matching data found for Gene ID.", type = "error")
    return(NULL)
  }
  
  return(gene_data)
}

# Function to aggregate multiple transcription values for the same gene/plant part - always using average
aggregate_gene_data = function(gene_data) {
  # Always use average values - Group by GeneID and Plant_part and calculate mean of TranscriptionValue
  aggregated_data = gene_data %>%
    group_by(GeneID, Plant_part, Source) %>%
    summarize(TranscriptionValue = mean(TranscriptionValue, na.rm = TRUE),
              .groups = "drop")
  
  return(aggregated_data)
}

# Process input threshold values
process_input = function(threshold_input, mode, use_threshold) {
  # Initialize variables
  threshold = NULL  
  first_call = FALSE
  
  if (!use_threshold) {
    if (mode %in% c("Relative", "Compare")) {
      # Default threshold for Relative and Compare
      threshold = 2.0  
    } else {
      # Default threshold for Absolute mode
      threshold = 50  
    }
    first_call = TRUE
  } else {
    if (is.null(threshold_input) || threshold_input == "" || is.na(as.numeric(threshold_input))) {
      threshold = NULL  
      showNotification("Please enter a valid number for Signal Threshold.", type = "warning")
    } else {
      threshold = as.numeric(threshold_input)  
    }
    first_call = FALSE
  }
  
  return(list(threshold = threshold, use_threshold = use_threshold, first_call = first_call))
}

# Function to create the color legend SVG
create_color_legend_horizontal = function(output_file, max_signal, min_signal = 0) {
  legend_svg = xml_new_root("svg", xmlns = "http://www.w3.org/2000/svg", width = 850, height = 120)
  
  num_steps = 25
  step_width = 25
  
  # Function to generate color based on intensity
  generate_color = function(intensity, max_intensity, min_intensity) {
    normalized_intensity = (intensity - min_intensity) / (max_intensity - min_intensity)
    
    normalized_intensity = pmin(pmax(normalized_intensity, 0), 1)
    
    if (min_intensity < 0) {
      # Red to Yellow to Blue gradient
      if (normalized_intensity <= 0.5) {
        red = 255
        green = floor(255 * (normalized_intensity * 2))
        blue = 0
      } else {
        red = floor(255 * (1 - (normalized_intensity - 0.5) * 2))
        green = floor(255 * (1 - (normalized_intensity - 0.5) * 2))
        blue = floor(255 * ((normalized_intensity - 0.5) * 2))
      }
    } else {
      # Red to Yellow gradient
      red = 255
      green = floor(255 * normalized_intensity)
      blue = 0
    }
    
    return(rgb(red, green, blue, maxColorValue = 255))
  }
  
  # Add a grey rectangle 
  rect = xml_add_child(legend_svg, "rect", 
                       x = 0, 
                       y = 10, 
                       width = step_width, 
                       height = 40, 
                       fill = "grey")  
  
  label = xml_add_child(legend_svg, "text", 
                        x = step_width/2, 
                        y = 70, 
                        transform = paste("rotate(-50,", step_width / 2, ",70)"), 
                        style = "font-size: 9px; fill: black; text-anchor: middle;", 
                        "Masked")
  
  # Generate the color scale for the legend
  for (i in 0:(num_steps - 1)) {
    intensity = min_signal + (max_signal - min_signal) * (num_steps - 1 - i) / (num_steps - 1)  
    
    color = generate_color(intensity, max_signal, min_signal)
    
    rect = xml_add_child(legend_svg, "rect", 
                         x = (i+1) * step_width,  
                         y = 10, 
                         width = step_width, 
                         height = 40, 
                         fill = color)
  }
  
  for (i in 0:(num_steps - 1)) {
    signal_value = min_signal + (max_signal - min_signal) * i / (num_steps - 1)  
    
    label = xml_add_child(legend_svg, "text", 
                          x = (i+1) * step_width + step_width / 2, 
                          y = 70, 
                          transform = paste("rotate(-45,", (i+1) * step_width + step_width / 2, ",70)"),
                          style = "font-size: 12px; fill: black; text-anchor: middle;", 
                          round(signal_value, 2))
  }
  
  # Save the legend SVG to the file
  write_xml(legend_svg, output_file)
}

# Function to apply colors to SVG
apply_colors_to_svg = function(svg_file, output_file, colors_map, legend_file) {
  svg_content = read_xml(svg_file)
  
  # Iterate through the colors map and apply the colors to the corresponding parts
  for (part in names(colors_map)) {
    color = colors_map[[part]]
    node = xml_find_first(svg_content, paste0('//*[@id="', part, '"]'))
    
    if (!is.null(node)) {
      if (!is.null(xml_attr(node, "style"))) {
        xml_set_attr(node, "style", NULL)
      }
      xml_set_attr(node, "fill", color)
    }
  }
  
  write_xml(svg_content, output_file)
  message("Colors applied and saved to: ", output_file)
}

# Helper function to clamp values within a range
clamp = function(x, min_val, max_val) {
  return(pmax(pmin(x, max_val), min_val))
}
