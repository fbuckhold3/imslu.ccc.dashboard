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
source("R/server.R")      # Server logic

# Load data once at app startup (after all functions are defined)
rdm_data <- load_ccc_data()

# Run the app
shinyApp(ui = ui, server = server)
