library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(plotly)
library(REDCapR)
library(bslib)
library(lubridate)
library(shinyjs)

# Ensure pipe operator is available
if (!exists("%>%")) {
  `%>%` <- dplyr::`%>%`
}

# Source all R files in correct order
source("R/global.R")      # Global configuration and data loading
source("R/helpers.R")     # Pure helper functions
source("R/wrappers.R")    # Data access wrappers
source("R/ui.R")          # UI definition

# Load data once at app startup
rdm_data <- load_ccc_data()

# Source server (defines create_server function)
source("R/server.R")      # Server logic

# Create server function with data
server <- create_server(rdm_data)

# Run the app
shinyApp(ui = ui, server = server)
