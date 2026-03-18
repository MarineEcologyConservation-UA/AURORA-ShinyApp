# Run during development:
#   devtools::load_all()
#   shiny::runApp(system.file("app", package = "shinyRv02"))

library(shiny)
library(shinyRv02)
# When you run via devtools::load_all(), functions from R/ are available.
# When installed, they are available via the package namespace.

source("ui.R")
source("server.R")

shinyApp(ui, server)
