# helpers.R - Pure functions for period calculations and data mapping
# No data access - all functions are pure transformations

#' Calculate academic year from date
#'
#' @param date A date object
#' @return Character string in format "YYYY-YYYY" (e.g., "2024-2025")
calculate_academic_year <- function(date = Sys.Date()) {
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))

  # Academic year starts July 1
  if (month >= 7) {
    paste0(year, "-", year + 1)
  } else {
    paste0(year - 1, "-", year)
  }
}

#' Calculate current evaluation period
#'
#' @param date A date object
#' @return Integer 1-6 representing the evaluation period
calculate_current_period <- function(date = Sys.Date()) {
  year <- as.numeric(format(date, "%Y"))
  month <- as.numeric(format(date, "%m"))
  day <- as.numeric(format(date, "%d"))

  # Period boundaries (month, day)
  # Period 1: July 1 - Aug 31
  # Period 2: Sep 1 - Oct 31
  # Period 3: Nov 1 - Dec 31
  # Period 4: Jan 1 - Feb 28/29
  # Period 5: Mar 1 - Apr 30
  # Period 6: May 1 - Jun 30

  if (month >= 7) {
    # July-December
    if (month <= 8) {
      return(1)
    } else if (month <= 10) {
      return(2)
    } else {
      return(3)
    }
  } else {
    # January-June
    if (month <= 2) {
      return(4)
    } else if (month <= 4) {
      return(5)
    } else {
      return(6)
    }
  }
}

#' Get period name from period number
#'
#' @param period Integer 1-6
#' @return Character string name of period
get_period_name <- function(period) {
  period_names <- c(
    "1" = "Period 1 (Jul-Aug)",
    "2" = "Period 2 (Sep-Oct)",
    "3" = "Period 3 (Nov-Dec)",
    "4" = "Period 4 (Jan-Feb)",
    "5" = "Period 5 (Mar-Apr)",
    "6" = "Period 6 (May-Jun)"
  )

  period_names[as.character(period)]
}

#' Map REDCap field names to display names
#'
#' @param field_name Character string of REDCap field name
#' @return Character string of display name
map_field_to_display <- function(field_name) {
  # Add mapping logic as needed
  # Example: "pc1_rating" -> "Patient Care 1"
  field_name
}
