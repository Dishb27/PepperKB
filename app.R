# Source all component files
source("global.R")
source("helpers.R")
source("render_functions.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui, server)