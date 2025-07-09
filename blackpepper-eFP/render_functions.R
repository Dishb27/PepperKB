#render_functions.R
source("helpers.R")

# Define the renderAbsolute function 
renderAbsolute = function(gene_data, threshold = NULL, session) {
  max_signal = max(gene_data$TranscriptionValue, na.rm = TRUE)
  print(threshold)
  
  updateTextInput(session, "signal_threshold", value = round(max_signal,2))
  
  if (!is.null(threshold) && threshold >= minThreshold_Absolute) {
    max_signal = max(max_signal, threshold)
  }
  
  print(max_signal)
  print(threshold)
  
  print("Rendering in Absolute Mode:")
  
  plant_parts_mapping = get_plant_parts_mapping()
  colors_map = list()
  
  for (i in seq_len(nrow(gene_data))) {
    signal = gene_data$TranscriptionValue[i]
    
    intensity = floor(signal * 255.0 / max_signal)
    color = rgb(255, 255 - intensity, 0, maxColorValue = 255)  
    
    plant_part = gene_data$Plant_part[i]  
    
    if (plant_part %in% names(plant_parts_mapping)) {
      svg_ids = plant_parts_mapping[[plant_part]]
      for (svg_id in svg_ids) {
        colors_map[[svg_id]] = color
      }
    } else {
      print(paste("Unmapped Plant Part:", plant_part))
    }
    
    print(paste("Plant Part:", plant_part, "| Signal:", signal, "| Intensity:", intensity, "| Color:", color))
  }
  print(colors_map)
  
  legend_file = "www/color_legend.svg"
  create_color_legend_horizontal(legend_file, max_signal)
  
  apply_colors_to_svg("www/pp2.svg", "www/updated_absolute_colors.svg", colors_map, legend_file)
  
  timestamped_image_path = paste0("updated_absolute_colors.svg?", as.numeric(Sys.time()))
  timestamped_legend_path = paste0("color_legend.svg?", as.numeric(Sys.time()))  
  
  # Hide loading spinner
  hide("loading-spinner")
  
  runjs(paste('$("#color_legend_div").show();'))  
  
  runjs(paste('$("#plant_image").attr("src", "', timestamped_image_path, '");'))
  runjs(paste('$("#color_legend").attr("src", "', timestamped_legend_path, '");'))
}

# Function to implement the Comparison Rendering
renderComparison = function(gene1_data, gene2_data, threshold = 0.0, session) {
  all_zero_gene1 = all(gene1_data$TranscriptionValue == 0, na.rm = TRUE)
  all_zero_gene2 = all(gene2_data$TranscriptionValue == 0, na.rm = TRUE)
  
  if (all_zero_gene1) {
    # Hide loading spinner before showing error
    hide("loading-spinner")
    
    showModal(modalDialog(
      title = "Error",
      "All transcriptional values for the primary gene are zero. Comparison cannot be performed. Please enter a different Gene ID.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)  
  }
  
  if (all_zero_gene2) {
    # Hide loading spinner before showing error
    hide("loading-spinner")
    
    showModal(modalDialog(
      title = "Error",
      "All transcriptional values for the secondary gene are zero. Comparison cannot be performed. Please enter a different Gene ID.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)  
  }
  
  max_signal_gene1 = max(gene1_data$TranscriptionValue, na.rm = TRUE)
  max_signal_gene2 = max(gene2_data$TranscriptionValue, na.rm = TRUE)
  
  max_signal = max(max_signal_gene1, max_signal_gene2, threshold)
  
  if (max_signal == 0) {
    max_signal = 1e-6  
  }
  
  if (threshold >= minThreshold_Compare) {
    max_signal = threshold
  }
  
  updateTextInput(session, "signal_threshold", value = round(max_signal,2))
  
  plant_parts_mapping = get_plant_parts_mapping()
  colors_map = list()
  
  # Merge the gene data by plant part for comparison
  merged_data = merge(
    gene1_data, 
    gene2_data, 
    by = "Plant_part", 
    suffixes = c(".1", ".2"),
    all = TRUE
  )
  
  # Replace NAs with zeros
  merged_data$TranscriptionValue.1[is.na(merged_data$TranscriptionValue.1)] = 0
  merged_data$TranscriptionValue.2[is.na(merged_data$TranscriptionValue.2)] = 0
  
  for (i in seq_len(nrow(merged_data))) {
    sig1 = merged_data$TranscriptionValue.1[i]
    sig2 = merged_data$TranscriptionValue.2[i]
    
    ratio1_log = ifelse(sig1 > 0, log2(sig1 / max_signal_gene1), -10)
    ratio2_log = ifelse(sig2 > 0, log2(sig2 / max_signal_gene2), -10)
    
    intensity = floor((ratio1_log - ratio2_log) * 255.0 / max_signal)
    intensity = clamp(intensity, -255, 255)  
    
    if (intensity == 0) {
      color = rgb(169, 169, 169, maxColorValue = 255)  
    } else if (intensity > 0) {
      color = rgb(255, 255, 0, maxColorValue = 255)  
    } else {
      color = rgb(255 + intensity, 255 + intensity, -intensity, maxColorValue = 255)  
    }
    
    plant_part = merged_data$Plant_part[i]  
    
    if (plant_part %in% names(plant_parts_mapping)) {
      svg_ids = plant_parts_mapping[[plant_part]]
      for (svg_id in svg_ids) {
        colors_map[[svg_id]] = color
      }
    }
    
    print(paste("Gene1 Signal:", sig1, "| Gene2 Signal:", sig2, "| Intensity:", intensity, "| Color:", color))
  }
  
  legend_file = "www/color_legend_comparison.svg"
  create_color_legend_horizontal(legend_file, max_signal, -max_signal)
  
  apply_colors_to_svg("www/pp2.svg", "www/updated_comparison_colors.svg", colors_map, legend_file)
  
  timestamped_image_path = paste0("updated_comparison_colors.svg?", as.numeric(Sys.time()))
  timestamped_legend_path = paste0("color_legend_comparison.svg?", as.numeric(Sys.time()))  
  
  # Hide loading spinner
  hide("loading-spinner")
  
  runjs(paste('$("#color_legend_div").show();'))  
  
  runjs(paste('$("#plant_image").attr("src", "', timestamped_image_path, '");'))
  runjs(paste('$("#color_legend").attr("src", "', timestamped_legend_path, '");'))
}

renderRelative = function(gene_data, threshold = 0.0, grey_mask = FALSE, control_plant_part = "leaf", session) {
  # Find control value
  control_data = gene_data[gene_data$Plant_part == control_plant_part, ]
  
  if (nrow(control_data) == 0) {
    # Hide loading spinner before showing error
    hide("loading-spinner")
    
    showModal(modalDialog(
      title = "Error",
      paste("No data found for control plant part:", control_plant_part),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  }
  
  control_signal = control_data$TranscriptionValue[1]
  
  if (control_signal == 0) {
    # Hide loading spinner before showing warning
    hide("loading-spinner")
    
    showModal(modalDialog(
      title = "Warning",
      paste("Control plant part", control_plant_part, "has zero expression. Relative mode may not show meaningful results."),
      easyClose = TRUE,
      footer = NULL
    ))
    # Set to a small value to avoid division by zero
    control_signal = 0.0001
  }
  
  maxSignal = max(gene_data$TranscriptionValue, na.rm = TRUE)
  maxGreater = FALSE
  
  if (threshold >= minThreshold_Relative) {
    maxLog2 = threshold
    if (maxSignal > threshold) {
      maxGreater = TRUE
    }
  } else {
    maxLog2 = maxSignal
  }
  
  updateTextInput(session, "signal_threshold", value = round(maxLog2,2))
  
  intensity = 0
  log2 = log(2)
  
  n = 1
  lowAlert = 0
  plant_parts_mapping = get_plant_parts_mapping()
  colors_map = list()  
  
  # Iterate through each gene's tissue data to calculate ratios and color intensity
  for (i in seq_len(nrow(gene_data))) {
    signal = gene_data$TranscriptionValue[i]
    
    # Calculate the log2 ratio of signal/control
    if (signal == 0 || control_signal == 0) {
      ratioLog2 = 0
    } else {
      ratioLog2 = log(signal / control_signal) / log2
    }
    
    # Compute intensity based on the ratio and maximum signal
    intensity = floor(255 * (ratioLog2 / maxLog2))
    intensity = clamp(intensity, -255, 255)
    
    if (signal <= 3.5 && grey_mask) {
      color = "grey"
    } else if (intensity > 0) {
      color = rgb(255, 255 - intensity, 0, maxColorValue = 255)  
    } else {
      color = rgb(255 + intensity, 255 + intensity, -intensity, maxColorValue = 255)  
    }
    
    plant_part = gene_data$Plant_part[i]  
    if (plant_part %in% names(plant_parts_mapping)) {
      svg_ids = plant_parts_mapping[[plant_part]]
      for (svg_id in svg_ids) {
        colors_map[[svg_id]] = color
      }
    }
    
    # Handle low expression warning
    if (signal <= 1.5 && !grey_mask) {
      lowAlert = 1
    }
    
    print(paste("Plant Part:", gene_data$Plant_part[i], "| Ratio:", ratioLog2, "| Color:", color))
  }
  
  legend_file = "www/color_legend_relative.svg"
  create_color_legend_horizontal(legend_file, maxLog2, -maxLog2)  
  
  apply_colors_to_svg("www/pp2.svg", "www/updated_relative_colors.svg", colors_map, legend_file)
  
  timestamped_image_path = paste0("updated_relative_colors.svg?", as.numeric(Sys.time()))
  timestamped_legend_path = paste0("color_legend_relative.svg?", as.numeric(Sys.time()))  
  
  # Hide loading spinner
  hide("loading-spinner")
  
  runjs(paste('$("#color_legend_div").show();'))  
  
  runjs(paste('$("#plant_image").attr("src", "', timestamped_image_path, '");'))
  runjs(paste('$("#color_legend").attr("src", "', timestamped_legend_path, '");'))
  
  return(list(outImage = "www/updated_relative_colors.svg", maxSignal = maxSignal, lowAlert = lowAlert))
}
