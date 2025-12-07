# wrappers.R - Data access wrapper functions
# All data filtering and joining logic goes here
# Server should ONLY call these functions, never access data directly

#' Get list of all residents
#'
#' @param rdm_data List containing all data from load_ccc_data()
#' @param filter_level Optional: filter by PGY level ("Intern", "PGY2", "PGY3")
#' @return Data frame of residents
get_resident_list <- function(rdm_data, filter_level = NULL) {
  if (!"residents" %in% names(rdm_data)) {
    return(data.frame())
  }

  residents <- rdm_data$residents

  if (!is.null(filter_level)) {
    # Calculate current Level based on type and grad_yr
    # This should already be calculated in load_ccc_data, but adding as backup
    residents <- residents %>%
      filter(!is.na(current_period))  # Only include residents with valid periods
  }

  residents %>%
    select(record_id, full_name, type, grad_yr, current_period, current_period_num) %>%
    arrange(full_name)
}

#' Get CCC review table with completion status
#'
#' @param rdm_data List containing all data
#' @param review_period Which review period to show ("Mid Year" or "End Year")
#' @return Data frame with resident info and completion status
get_ccc_review_table <- function(rdm_data, review_period = get_current_ccc_period()) {

  if (!"residents" %in% names(rdm_data)) {
    return(data.frame(
      record_id = character(0),
      resident = character(0),
      level = character(0),
      expected_period = character(0),
      ccc_complete = logical(0)
    ))
  }

  # Filter residents based on review period
  # Mid Year = periods 1, 3, 5 (Mid Intern, Mid PGY2, Mid PGY3)
  # End Year = periods 2, 4, 6 (End Intern, End PGY2, Graduating)

  mid_year_periods <- c("Mid Intern", "Mid PGY2", "Mid PGY3")
  end_year_periods <- c("End Intern", "End PGY2", "Graduating")

  target_periods <- if (review_period == "Mid Year") {
    mid_year_periods
  } else {
    end_year_periods
  }

  # Get residents in target periods
  residents_for_review <- rdm_data$residents %>%
    filter(!is.na(current_period), current_period %in% target_periods)

  if (nrow(residents_for_review) == 0) {
    return(data.frame(
      record_id = character(0),
      resident = character(0),
      level = character(0),
      expected_period = character(0),
      ccc_complete = logical(0)
    ))
  }

  # Check CCC completion for each resident
  purrr::map_dfr(1:nrow(residents_for_review), function(i) {
    res <- residents_for_review[i, ]

    # Check if CCC review form is complete for this resident's current period
    ccc_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "ccc_review",
      res$record_id,
      res$current_period
    )

    ccc_complete <- nrow(ccc_data) > 0 &&
                    "ccc_review_complete" %in% names(ccc_data) &&
                    !is.na(ccc_data$ccc_review_complete[1]) &&
                    ccc_data$ccc_review_complete[1] == "2"

    data.frame(
      record_id = res$record_id,
      resident = res$full_name,
      level = res$current_period,
      expected_period = res$current_period,
      ccc_complete = ccc_complete,
      stringsAsFactors = FALSE
    )
  })
}

#' Get resident milestone data for display
#'
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name (e.g., "Mid PGY3")
#' @return List with program, self, and acgme milestone data
get_resident_milestones <- function(rdm_data, record_id, period_name) {

  # Get previous period for ACGME milestones
  period_num <- get_period_number(period_name)
  previous_period_num <- if (!is.na(period_num) && period_num > 0) period_num - 1 else NA
  previous_period_name <- if (!is.na(previous_period_num)) {
    get_period_name(previous_period_num)
  } else {
    NA_character_
  }

  list(
    program = get_form_data_for_period(
      rdm_data$all_forms,
      "milestone_entry",
      record_id,
      period_name
    ),
    self = get_form_data_for_period(
      rdm_data$all_forms,
      "milestone_selfevaluation_c33c",
      record_id,
      period_name
    ),
    acgme = if (!is.na(previous_period_name)) {
      get_form_data_for_period(
        rdm_data$all_forms,
        "acgme_miles",
        record_id,
        previous_period_name
      )
    } else {
      data.frame()
    }
  )
}

#' Get resident CCC review data
#'
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with CCC review data or empty df
get_resident_ccc_review <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "ccc_review",
    record_id,
    period_name
  )
}
