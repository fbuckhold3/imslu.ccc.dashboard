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
rdm_data <- load_ccc_phase1()

# Phase 2: full data loaded ONCE at server startup in a background worker.
# server_state is an explicit environment passed to create_server so all
# sessions can read it without relying on lexical scoping across source() calls.
server_state <- new.env(parent = emptyenv())
server_state$full_data     <- rdm_data
server_state$load_complete <- FALSE

local({
  rdm_url   <- REDCAP_CONFIG$url
  rdm_token <- REDCAP_CONFIG$rdm_token

  promises::future_promise({
    source("R/global.R")
    library(gmed); library(dplyr); library(purrr); library(REDCapR); library(lubridate)
    httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    load_ccc_data(redcap_url = rdm_url, rdm_token = rdm_token)
  }) %...>% (function(full_data) {
    server_state$full_data     <- full_data
    server_state$load_complete <- TRUE
    message("[Phase 2] Server-level full data loaded at ",
            format(Sys.time(), "%H:%M:%S"))
  }) %...!% (function(err) {
    message("[Phase 2] Background load failed: ", err$message)
  })
})

# Source server (defines create_server function)
source("R/server.R")      # Server logic

# Create server function with data
server <- create_server(rdm_data, server_state)

# Run the app
shinyApp(ui = ui, server = server)
