

# Load UI and server scripts
source("TeenGrowthApp/ui.R")
source("TeenGrowthApp/server.R")

shinyApp(ui, server, options = list(launch.browser = rstudioapi::viewer))
