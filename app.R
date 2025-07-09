library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet.extras)  
library(RColorBrewer)
library(viridis)

# Read the CSV file
data <- read.csv("FAOSTAT_data_production_world.csv")

# Get country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(
    disable = TRUE
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Header background color */
        .main-header .navbar {
          background-color: #0d745c !important; 
        }
        
        /* Body background color */
        .content-wrapper, .right-side {
          background-color: #ffffff; 
        }
        
        /* Button color */
        .btn-info, .bootstrap-select .btn-info {
          background-color: #0d745c !important; 
          color: white !important;
        }
        /* Dropdown menu color */
        .bootstrap-select .dropdown-menu {
          background-color: lightgray !important;
        }
        /* Dropdown menu items color */
        .bootstrap-select .dropdown-menu a {
          color: black !important;
        }
        
        .main-header .navbar .container-fluid {
          padding: 0;
        }
        
        .main-header .logo {
          display: none;
        }
      "))
    ),
    fluidRow(
      column(12,
             div(style = "display: flex; justify-content: center; flex-wrap: wrap;",
                 div(style = "margin: 10px;",
                     pickerInput("area", "Area",
                                 choices = c("Select an area" = "", unique(data$Area)),
                                 options = list(`live-search` = TRUE, `style` = "btn-info"))),
                 
                 div(style = "margin: 10px;",
                     pickerInput("from_year", "From Year",
                                 choices = unique(data$Year), selected = min(data$Year),
                                 options = list(`style` = "btn-info"))),
                 
                 div(style = "margin: 10px;",
                     pickerInput("to_year", "To Year",
                                 choices = unique(data$Year), selected = max(data$Year),
                                 options = list(`style` = "btn-info"))),
                 
                 div(style = "margin: 10px;",
                     pickerInput("aggregation", "Aggregation",
                                 choices = c("Select an option" = "", "Average", "Sum", "Median"), 
                                 selected = "",
                                 options = list(`style` = "btn-info")))
             )
      )
    ),
    
    fluidRow(
      column(12, 
             shinycssloaders::withSpinner(leafletOutput("map", height = "800px", width = "100%"))
      )
    ),
    
    fluidRow(
      column(12,
             uiOutput("loading_indicator"))
    )
  )
)


# Define server logic for leaflet map
server <- function(input, output) {
  
  # Show progress indicator when loading data
  output$loading_indicator <- renderUI({
    if (input$area == "" || input$from_year == "" || input$to_year == "" || input$aggregation == "") {
      return(NULL)
    }
    
    withProgress(message = 'Loading map data...', {
      incProgress(1)
      return(NULL)
    })
  })
  
  output$map <- renderLeaflet({
    # Validate user inputs
    if (input$area == "" || input$from_year == "" || input$to_year == "" || input$aggregation == "") {
      return(leaflet() %>% 
               addTiles() %>% 
               addControl(html = "Please select all filters.", position = "topright"))
    }
    
    # Filter and aggregate data based on user inputs
    filtered_data <- data %>%
      filter(Area == input$area, Year >= input$from_year & Year <= input$to_year) %>%
      group_by(Area) %>%
      summarise(Value = switch(input$aggregation,
                               "Average" = mean(Value, na.rm = TRUE),
                               "Sum" = sum(Value, na.rm = TRUE),
                               "Median" = median(Value, na.rm = TRUE)), .groups = "drop")
    
    # Check if the filtered data is valid
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$Value))) {
      return(leaflet() %>% 
               addTiles() %>% 
               addControl(html = "No data available for selected filters.", position = "topright"))
    }
    
    # Recalculate breaks and palette for the filtered data
    value_range <- range(filtered_data$Value, na.rm = TRUE)
    num_breaks <- 5
    breaks <- pretty(value_range, n = num_breaks)
    palette <- colorBin("darkgreen", domain = filtered_data$Value, bins = breaks)
    
    # Merge filtered data with world boundaries
    merged_data <- world %>%
      left_join(filtered_data, by = c("name" = "Area")) %>%
      filter(!is.na(Value))  
    
    # Create leaflet map with updated data
    map <- leaflet() %>%
      addTiles() %>%  
      addPolygons(
        data = world,  
        fillColor = "lightgray",
        weight = 1, 
        opacity = 1,
        color = "white",
        dashArray = '3',
        fillOpacity = 0.5
      ) %>%
      addPolygons(
        data = merged_data,
        fillColor = ~ifelse(name == input$area, "darkgreen", palette(Value)),  
        weight = ~ifelse(name == input$area, 3, 1),  
        opacity = 1,
        color = ~ifelse(name == input$area, "darkgreen", "black"),  
        dashArray = '3',
        fillOpacity = 0.7,
        popup = ~paste("Country: ", name, "<br>Production Value: ", Value),
        label = ~ifelse(name == input$area, " ", NA)  
      ) %>%
      addLegend(pal = palette, values = filtered_data$Value, position = "topright", title = "Production Value") %>%
      addSearchFeatures(targetGroups = 'all') %>%
      addResetMapButton() %>%
      addScaleBar(position = "bottomleft")
    
    map
  })
}

# Run the application
shinyApp(ui = ui, server = server)
