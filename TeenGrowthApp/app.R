

# Load UI and server scripts
source("ui.R")
source("server.R")

shinyApp(ui, server, options = list(launch.browser = rstudioapi::viewer))
