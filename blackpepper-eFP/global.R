#global.R
# Load required libraries
library(shiny)
library(dplyr)
library(shinyjs)
library(xml2)
library(shinydashboard)

# Load CSV Data
data = read.csv("final_transformed.csv")

# Default thresholds
minThreshold_Compare = 0.6
minThreshold_Relative = 0.6
minThreshold_Absolute = 10
GENE_ID_DEFAULT1 = 'Pn1.1112'
GENE_ID_DEFAULT2 = 'Pn7.1968'

# Plant parts mapping - this maps database plant part names to SVG element IDs
get_plant_parts_mapping = function() {
  plant_parts_mapping = list(
    root = paste0("root", 1:9),   
    leaf = paste0("leaf", 1:12),  
    flower = "flower",            
    stem = "stem1",
    `2map` = "2MAP",              
    `4map` = "4MAP",              
    `6map` = "6MAP",              
    `8map` = "8MAP",
    `fruit20`= paste0("fruty", 1:5),
    `fruit40`= paste0("fruit", 1:6)
  )
  
  return(plant_parts_mapping)
}
