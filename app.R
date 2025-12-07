library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(gmed)

# Ensure pipe operator is available
if (!exists("%>%")) {
  `%>%` <- dplyr::`%>%`
}

# Source all R files in correct order
source("R/global.R")      # Global configuration and data loading
source("R/helpers.R")     # Pure helper functions
source("R/wrappers.R")    # Data access wrappers
source("R/ui.R")          # UI definition

# Load data once at app startup (before server is defined, so it's in scope)
rdm_data <- load_ccc_data()

# Source server last (so rdm_data is available)
source("R/server.R")      # Server logic

# Run the app
shinyApp(ui = ui, server = server)
