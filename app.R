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

# ── Disk cache configuration ───────────────────────────────────────────────────
# Cache lives in the R user cache dir (persists across process restarts on Connect).
# Falls back to tempdir() if the user dir is not writable.
CACHE_DIR <- tryCatch({
  d <- tools::R_user_dir("ccc_dashboard", "cache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (file.access(d, mode = 2) == 0) d else tempdir()
}, error = function(e) tempdir())

CACHE_PATH          <- file.path(CACHE_DIR, "phase2_cache.rds")
CACHE_MAX_AGE_HOURS <- 4   # serve from cache if < 4 hours old

message("[Cache] Path: ", CACHE_PATH)

# ── Phase 1: fast startup ──────────────────────────────────────────────────────
rdm_data <- load_ccc_phase1()

# ── server_state: shared across all sessions ───────────────────────────────────
server_state                <- new.env(parent = emptyenv())
server_state$full_data      <- rdm_data
server_state$load_complete  <- FALSE
server_state$cache_path     <- CACHE_PATH   # so server.R can clear it on Refresh

# ── Phase 2: cache check → background load ────────────────────────────────────
cache_age_hours <- if (file.exists(CACHE_PATH)) {
  as.numeric(difftime(Sys.time(), file.mtime(CACHE_PATH), units = "hours"))
} else {
  Inf
}

cache_ok <- cache_age_hours < CACHE_MAX_AGE_HOURS

if (cache_ok) {
  # Fast path: load from disk (< 1 sec)
  message(sprintf("[Phase 2] Cache hit (%.0f min old) — loading from disk...",
                  cache_age_hours * 60))
  cached <- tryCatch(readRDS(CACHE_PATH), error = function(e) {
    message("[Phase 2] Cache read failed: ", e$message); NULL
  })
  if (!is.null(cached)) {
    cached$full_load_complete   <- TRUE
    server_state$full_data      <- cached
    server_state$load_complete  <- TRUE
    message("[Phase 2] Cache loaded successfully — app is fully ready.")
  } else {
    cache_ok <- FALSE   # corrupt cache → fall through to fresh load
  }
}

if (!cache_ok) {
  # Slow path: pull from REDCap in background worker
  message("[Phase 2] No valid cache — starting background REDCap load...")
  local({
    rdm_url    <- REDCAP_CONFIG$url
    rdm_token  <- REDCAP_CONFIG$rdm_token
    cache_path <- CACHE_PATH

    promises::future_promise({
      source("R/global.R")
      library(gmed); library(dplyr); library(purrr); library(REDCapR); library(lubridate)
      httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      load_ccc_data(redcap_url = rdm_url, rdm_token = rdm_token)
    }) %...>% (function(full_data) {
      full_data$full_load_complete <- TRUE
      # Save to cache for next startup
      tryCatch({
        dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
        saveRDS(full_data, cache_path)
        message("[Phase 2] Data cached to disk: ", cache_path)
      }, error = function(e) {
        message("[Phase 2] Cache save failed: ", e$message)
      })
      server_state$full_data     <- full_data
      server_state$load_complete <- TRUE
      message("[Phase 2] Server-level full data loaded at ",
              format(Sys.time(), "%H:%M:%S"))
    }) %...!% (function(err) {
      message("[Phase 2] Background load failed: ", err$message)
    })
  })
}

# Source server (defines create_server function)
source("R/server.R")      # Server logic

# Create server function with data
server <- create_server(rdm_data, server_state)

# Run the app
shinyApp(ui = ui, server = server)
