# global.R - Data loading and configuration
# Loads data via gmed package - minimal processing, just structure

#' Load all required data from REDCap via gmed
#'
#' @return List containing all data frames from load_rdm_complete()
load_app_data <- function() {
  # Load data using gmed package
  # This function expects environment variables:
  # - ACCESS_CODE
  # - RDM_TOKEN
  # - EVAL_TOKEN (optional)
  # - FAC_TOKEN (optional)

  tryCatch({
    data <- gmed::load_rdm_complete()

    # Validate required data frames exist
    required_tables <- c(
      "demographics",
      "milestone_entry",
      "milestone_self",
      "milestone_acgme",
      "ccc_review"
    )

    missing_tables <- setdiff(required_tables, names(data))
    if (length(missing_tables) > 0) {
      warning("Missing expected tables: ", paste(missing_tables, collapse = ", "))
    }

    # Return data as-is - all filtering/processing happens in wrappers
    return(data)

  }, error = function(e) {
    stop("Failed to load data from REDCap: ", e$message)
  })
}

# Load data once at app startup
data <- load_app_data()

# Calculate current period for default selection
current_academic_year <- calculate_academic_year()
current_period <- calculate_current_period()
