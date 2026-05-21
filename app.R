library(shiny)
library(dplyr)
library(DT)
library(purrr)
library(plotly)
library(REDCapR)
library(bslib)
library(lubridate)
library(shinyjs)
library(future)
library(promises)

# Ensure pipe operator is available
if (!exists("%>%")) {
  `%>%` <- dplyr::`%>%`
}

# Source all R files in correct order
source("R/global.R")             # Global configuration and data loading
source("R/helpers.R")            # Pure helper functions
source("R/wrappers.R")           # Data access wrappers
source("R/redcap_submission.R")  # REDCap write-back functions
source("R/ui.R")                 # UI definition

# Enable async background loading (Phase 2 runs in a separate R process)
future::plan(future::multisession)

# Phase 1: fast startup — residents + 3 review forms + cached medians (~4-6 sec)
# Phase 2: full data load triggered in server.R after login (~25 sec, background)
rdm_data <- load_ccc_phase1()

# Source server (defines create_server function)
source("R/server.R")      # Server logic

# Create server function with data
server <- create_server(rdm_data)

# Run the app
shinyApp(ui = ui, server = server)
