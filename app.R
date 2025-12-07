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
source("R/helpers.R")     # Pure functions first
source("R/global.R")      # Data loading functions
source("R/wrappers.R")    # Data access wrappers
source("R/ui.R")          # UI definition
source("R/server.R")      # Server logic

# Load data once at app startup (after all functions are defined)
data <- load_app_data()

# Calculate current period for default selection
current_academic_year <- calculate_academic_year()
current_period <- calculate_current_period()

# Run the app
shinyApp(ui = ui, server = server)
