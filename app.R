library(shiny)
library(dplyr)
library(DT)
library(gmed)

# Source all R files
source("R/global.R")
source("R/helpers.R")
source("R/wrappers.R")
source("R/ui.R")
source("R/server.R")

# Run the app
shinyApp(ui = ui, server = server)
