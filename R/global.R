# ==============================================================================
# IMSLU CCC DASHBOARD - GLOBAL CONFIGURATION
# R/global.R
# ==============================================================================
#
# This file sets up:
# - REDCap configuration
# - Period definitions
# - Data loading function with period calculation
# - Helper functions for form data access
#
# ==============================================================================

# ==============================================================================
# API CONFIGURATION
# ==============================================================================

# REDCap API configuration
REDCAP_CONFIG <- list(
  url = "https://redcapsurvey.slu.edu/api/",
  rdm_token = Sys.getenv("RDM_TOKEN"),
  access_code = Sys.getenv("ACCESS_CODE"),
  timeout = 300  # 5 minutes for large data pulls
)

# Validate required tokens
if (!nzchar(REDCAP_CONFIG$rdm_token)) {
  stop("RDM_TOKEN not found. Please set in .Renviron file")
}

if (!nzchar(REDCAP_CONFIG$access_code)) {
  stop("ACCESS_CODE not found. Please set in .Renviron file")
}

# ==============================================================================
# PERIOD DEFINITIONS
# ==============================================================================
# Based on RDM 2.0 structure - using gmed's period system

# Period names (in chronological order)
PERIOD_NAMES <- c(
  "Entering Residency",  # Period 0 (July orientation)
  "Mid Intern",          # Period 1 (December)
  "End Intern",          # Period 2 (June)
  "Mid PGY2",            # Period 3 (December)
  "End PGY2",            # Period 4 (June)
  "Mid PGY3",            # Period 5 (December)
  "Graduating"           # Period 6 (April/May)
)

# Period number mapping (for REDCap instances)
PERIOD_TO_NUMBER <- setNames(0:6, PERIOD_NAMES)

# Reverse mapping
NUMBER_TO_PERIOD <- setNames(PERIOD_NAMES, 0:6)

#' Get Current CCC Review Period
#'
#' Determines which CCC review period we're currently in based on the date
#' CCC reviews happen twice per year: December (mid-year) and June (end of year)
#'
#' @param current_date Date to check (defaults to today)
#' @return Character string "Mid Year" or "End Year"
#' @export
get_current_ccc_period <- function(current_date = Sys.Date()) {
  month <- as.numeric(format(current_date, "%m"))

  # Mid-year reviews: September - January (periods 1, 3, 5)
  # End-year reviews: February - August (periods 2, 4, 6)

  if (month >= 9 || month <= 1) {
    return("Mid Year")
  } else {
    return("End Year")
  }
}

#' Get Period Number from Name
#'
#' @param period_name Character string of period name
#' @return Integer period number (0-6) or NA if invalid
#' @export
get_period_number <- function(period_name) {
  if (is.na(period_name) || is.null(period_name)) return(NA_integer_)
  PERIOD_TO_NUMBER[period_name]
}

#' Get Period Name from Number
#'
#' @param period_number Integer period number (0-6)
#' @return Character string of period name or NA if invalid
#' @export
get_period_name <- function(period_number) {
  if (is.na(period_number) || is.null(period_number)) return(NA_character_)
  NUMBER_TO_PERIOD[as.character(period_number)]
}

# ==============================================================================
# DATA LOADING FUNCTION
# ==============================================================================

#' Load RDM Data for CCC Dashboard
#'
#' Loads all necessary data from REDCap using gmed::load_rdm_complete()
#' Calculates expected periods for all residents using gmed logic
#'
#' @param redcap_url REDCap API URL
#' @param rdm_token RDM REDCap token
#' @param include_archived Logical, whether to include archived residents
#' @return List with all_forms, residents, milestone_medians, data_dict
#' @export
load_ccc_data <- function(
  redcap_url = REDCAP_CONFIG$url,
  rdm_token = REDCAP_CONFIG$rdm_token,
  include_archived = FALSE
) {

  message(sprintf(
    "[%s] Loading RDM data for CCC dashboard...",
    format(Sys.time(), "%H:%M:%S")
  ))

  # Use gmed's data loading function with raw format
  rdm_data <- gmed::load_rdm_complete(
    redcap_url = redcap_url,
    rdm_token = rdm_token,
    raw_or_label = "raw"  # Required for numeric fields and checkboxes
  )

  message("  -> Processing resident data...")

  # Filter archived residents if requested
  if (!include_archived && "residents" %in% names(rdm_data)) {
    if ("res_archive" %in% names(rdm_data$residents)) {
      archived_records <- rdm_data$residents %>%
        filter(!is.na(res_archive) & res_archive == "1") %>%
        pull(record_id)

      if (length(archived_records) > 0) {
        message("  Filtering out ", length(archived_records), " archived residents")

        # Filter from residents
        rdm_data$residents <- rdm_data$residents %>%
          filter(!(record_id %in% archived_records))

        # Filter from all_forms
        for (form_name in names(rdm_data$all_forms)) {
          if ("record_id" %in% names(rdm_data$all_forms[[form_name]])) {
            rdm_data$all_forms[[form_name]] <- rdm_data$all_forms[[form_name]] %>%
              filter(!(record_id %in% archived_records))
          }
        }
      }
    }
  }

  # Translate type and grad_yr from codes to labels
  if (!is.null(rdm_data$data_dict) && "residents" %in% names(rdm_data)) {
    message("  -> Translating type and grad_yr codes...")

    # Translate type field
    type_choices <- rdm_data$data_dict %>%
      filter(field_name == "type") %>%
      pull(select_choices_or_calculations)

    if (length(type_choices) > 0 && !is.na(type_choices[1])) {
      choice_pairs <- strsplit(type_choices[1], "\\|")[[1]]
      type_map <- list()
      for (pair in choice_pairs) {
        parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))
          type_map[[code]] <- label
        }
      }
      type_map <- unlist(type_map)

      rdm_data$residents <- rdm_data$residents %>%
        mutate(
          type_code = type,  # Keep original code
          type = if_else(!is.na(type) & type %in% names(type_map),
                        type_map[type], type)
        )
    }

    # Translate grad_yr field
    grad_yr_choices <- rdm_data$data_dict %>%
      filter(field_name == "grad_yr") %>%
      pull(select_choices_or_calculations)

    if (length(grad_yr_choices) > 0 && !is.na(grad_yr_choices[1])) {
      choice_pairs <- strsplit(grad_yr_choices[1], "\\|")[[1]]
      grad_yr_map <- list()
      for (pair in choice_pairs) {
        parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          year <- trimws(paste(parts[-1], collapse = ","))
          grad_yr_map[[code]] <- year
        }
      }
      grad_yr_map <- unlist(grad_yr_map)

      rdm_data$residents <- rdm_data$residents %>%
        mutate(
          grad_yr = if_else(!is.na(grad_yr) & grad_yr %in% names(grad_yr_map),
                           grad_yr_map[grad_yr], grad_yr)
        )
    }
  }

  # Translate period fields from codes to labels
  if (!is.null(rdm_data$data_dict)) {
    message("  -> Translating period field codes to labels...")

    # Period field candidates (all forms that have period fields)
    period_field_candidates <- c(
      "s_e_period",           # S Eval
      "coach_period",         # Coach Review
      "second_period",        # Second Review
      "prog_mile_period",     # Milestone Entry
      "prog_mile_period_self",# Milestone Self-Evaluation
      "acgme_mile_period",    # ACGME Miles
      "ccc_session"           # CCC Review
    )

    for (field in period_field_candidates) {
      # Get period choices from data dictionary
      period_choices <- rdm_data$data_dict %>%
        filter(field_name == !!field) %>%
        pull(select_choices_or_calculations)

      if (length(period_choices) > 0 && !is.na(period_choices[1])) {
        # Build translation map from codes to labels
        choice_pairs <- strsplit(period_choices[1], "\\|")[[1]]
        period_map <- list()
        for (pair in choice_pairs) {
          parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
          if (length(parts) >= 2) {
            code <- trimws(parts[1])
            label <- trimws(paste(parts[-1], collapse = ","))
            period_map[[code]] <- label
          }
        }
        period_map <- unlist(period_map)

        # Translate in all forms that have this field
        for (form_name in names(rdm_data$all_forms)) {
          if (field %in% names(rdm_data$all_forms[[form_name]])) {
            rdm_data$all_forms[[form_name]] <- rdm_data$all_forms[[form_name]] %>%
              mutate(
                !!field := if_else(
                  !is.na(.data[[field]]) & .data[[field]] %in% names(period_map),
                  period_map[.data[[field]]],
                  .data[[field]]
                )
              )
            message(sprintf("    Translated %s in form %s", field, form_name))
          }
        }
      }
    }
    message("  Period field translation complete")
  }

  # Calculate current period for each resident using gmed logic
  message("  -> Calculating expected periods for all residents...")

  current_date <- Sys.Date()

  if ("residents" %in% names(rdm_data) &&
      "type_code" %in% names(rdm_data$residents) &&
      "grad_yr" %in% names(rdm_data$residents)) {

    rdm_data$residents <- rdm_data$residents %>%
      rowwise() %>%
      mutate(
        current_period = {
          tryCatch({
            # Convert grad_yr to numeric
            grad_year_num <- suppressWarnings(as.numeric(grad_yr))
            type_num <- suppressWarnings(as.numeric(type_code))

            if (!is.na(grad_year_num) && !is.na(type_num) && type_num %in% c(1, 2)) {
              # Use gmed function to calculate expected period
              period_calc <- gmed::calculate_pgy_and_period(
                grad_yr = grad_year_num,
                type = type_num,
                current_date = current_date
              )

              if (!is.null(period_calc$period_name) &&
                  !is.na(period_calc$period_name) &&
                  period_calc$is_valid) {
                period_calc$period_name
              } else {
                NA_character_
              }
            } else {
              NA_character_
            }
          }, error = function(e) {
            NA_character_
          })
        },
        current_period_num = get_period_number(current_period),
        full_name = if_else(
          !is.na(first_name) & !is.na(last_name),
          paste(first_name, last_name),
          name
        )
      ) %>%
      ungroup()
  }

  message(sprintf(
    "[%s] Data loading complete! %d residents found.",
    format(Sys.time(), "%H:%M:%S"),
    if ("residents" %in% names(rdm_data)) nrow(rdm_data$residents) else 0
  ))

  return(rdm_data)
}

# ==============================================================================
# FORM DATA ACCESS HELPER
# ==============================================================================

#' Get Form Data for Resident and Period
#'
#' Handles different period field names for each form type
#'
#' @param all_forms List of all form data
#' @param form_name Name of the form
#' @param record_id Resident record ID
#' @param period_name Period name (e.g., "Mid PGY3")
#' @return Filtered data frame
#' @export
get_form_data_for_period <- function(all_forms, form_name, record_id, period_name) {

  # Return empty if form doesn't exist
  if (is.null(all_forms[[form_name]])) {
    return(data.frame())
  }

  # Get base data for this resident
  form_data <- all_forms[[form_name]] %>%
    filter(record_id == !!record_id)

  if (nrow(form_data) == 0) {
    return(data.frame())
  }

  # Form-specific period filtering
  filtered_data <- switch(
    form_name,

    # S Eval uses s_e_period
    "s_eval" = {
      if ("s_e_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "s_eval") %>%
          filter(!is.na(s_e_period), s_e_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "s_eval")
      }
    },

    # Milestone Entry uses prog_mile_period
    "milestone_entry" = {
      if ("prog_mile_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_entry") %>%
          filter(!is.na(prog_mile_period), prog_mile_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_entry")
      }
    },

    # Milestone Self-Evaluation uses prog_mile_period_self
    "milestone_selfevaluation_c33c" = {
      if ("prog_mile_period_self" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_selfevaluation_c33c") %>%
          filter(!is.na(prog_mile_period_self), prog_mile_period_self == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_selfevaluation_c33c")
      }
    },

    # ACGME Miles uses acgme_mile_period
    "acgme_miles" = {
      if ("acgme_mile_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "acgme_miles") %>%
          filter(!is.na(acgme_mile_period), acgme_mile_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "acgme_miles")
      }
    },

    # Coach Review uses coach_period
    "coach_rev" = {
      if ("coach_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "coach_rev") %>%
          filter(!is.na(coach_period), coach_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "coach_rev")
      }
    },

    # Second Review uses second_period
    "second_review" = {
      if ("second_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "second_review") %>%
          filter(!is.na(second_period), second_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "second_review")
      }
    },

    # CCC Review uses ccc_session
    "ccc_review" = {
      if ("ccc_session" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "ccc_review") %>%
          filter(!is.na(ccc_session), ccc_session == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "ccc_review")
      }
    },

    # Default: no period filtering
    {
      form_data
    }
  )

  return(filtered_data)
}

# ==============================================================================
# STARTUP MESSAGE
# ==============================================================================
message("=====================================================")
message("IMSLU CCC Dashboard")
message("=====================================================")
message("REDCap URL: ", REDCAP_CONFIG$url)
message("Token configured: ", nzchar(REDCAP_CONFIG$rdm_token))
message("Current CCC Period: ", get_current_ccc_period())
message("=====================================================")
