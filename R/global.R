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

  # Check for required environment variables
  required_vars <- c("ACCESS_CODE", "RDM_TOKEN")
  missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]

  if (length(missing_vars) > 0) {
    stop(
      "Missing required environment variables: ",
      paste(missing_vars, collapse = ", "),
      "\n\nPlease create a .Renviron file with these variables.",
      "\nSee .Renviron.example for template."
    )
  }

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
    stop("Failed to load data from REDCap: ", e$message, call. = FALSE)
  })
}

# Note: Data loading and period calculations are done in app.R after all files are sourced
