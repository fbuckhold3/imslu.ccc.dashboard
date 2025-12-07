# wrappers.R - Data access wrapper functions
# All data filtering and joining logic goes here
# Server should ONLY call these functions, never access data directly

#' Get list of all residents
#'
#' @param data List containing all data frames from load_rdm_complete()
#' @return Character vector of resident names
get_resident_list <- function(data) {
  if (!"demographics" %in% names(data)) {
    return(character(0))
  }

  data$demographics %>%
    filter(!is.na(name)) %>%
    pull(name) %>%
    unique() %>%
    sort()
}

#' Get resident's record_id from name
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @return Integer record_id or NA if not found
get_resident_record_id <- function(data, resident_name) {
  if (!"demographics" %in% names(data)) {
    return(NA_integer_)
  }

  record <- data$demographics %>%
    filter(name == resident_name) %>%
    slice(1)

  if (nrow(record) == 0) {
    return(NA_integer_)
  }

  record$record_id
}

#' Get resident's demographics
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @return Data frame with one row of demographics or empty data frame
get_resident_demographics <- function(data, resident_name) {
  if (!"demographics" %in% names(data)) {
    return(data.frame())
  }

  data$demographics %>%
    filter(name == resident_name) %>%
    slice(1)
}

#' Get resident's program milestones for a specific period
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @param period Integer 1-6 representing evaluation period
#' @return Data frame of milestone ratings
get_resident_program_milestones <- function(data, resident_name, period) {
  if (!"milestone_entry" %in% names(data)) {
    return(data.frame())
  }

  record_id <- get_resident_record_id(data, resident_name)
  if (is.na(record_id)) {
    return(data.frame())
  }

  data$milestone_entry %>%
    filter(
      record_id == !!record_id,
      evaluation_period == period
    )
}

#' Get resident's self-evaluation milestones for a specific period
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @param period Integer 1-6 representing evaluation period
#' @return Data frame of self-evaluation milestone ratings
get_resident_self_milestones <- function(data, resident_name, period) {
  if (!"milestone_self" %in% names(data)) {
    return(data.frame())
  }

  record_id <- get_resident_record_id(data, resident_name)
  if (is.na(record_id)) {
    return(data.frame())
  }

  data$milestone_self %>%
    filter(
      record_id == !!record_id,
      evaluation_period == period
    )
}

#' Get resident's ACGME milestones from previous period
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @param period Integer 1-6 representing current period (will get previous)
#' @return Data frame of ACGME milestone ratings from previous period
get_resident_acgme_milestones <- function(data, resident_name, period) {
  if (!"milestone_acgme" %in% names(data)) {
    return(data.frame())
  }

  record_id <- get_resident_record_id(data, resident_name)
  if (is.na(record_id)) {
    return(data.frame())
  }

  # Calculate previous period (wrap around from 1 to 6)
  previous_period <- if (period == 1) 6 else period - 1

  data$milestone_acgme %>%
    filter(
      record_id == !!record_id,
      evaluation_period == previous_period
    )
}

#' Check if CCC review is complete for a resident/period
#'
#' @param data List containing all data frames
#' @param resident_name Character string of resident name
#' @param period Integer 1-6 representing evaluation period
#' @return Logical TRUE if complete, FALSE otherwise
check_ccc_completion <- function(data, resident_name, period) {
  if (!"ccc_review" %in% names(data)) {
    return(FALSE)
  }

  record_id <- get_resident_record_id(data, resident_name)
  if (is.na(record_id)) {
    return(FALSE)
  }

  review <- data$ccc_review %>%
    filter(
      record_id == !!record_id,
      evaluation_period == period
    )

  if (nrow(review) == 0) {
    return(FALSE)
  }

  # Check if review has completion indicator (adjust field name as needed)
  # This is a placeholder - adjust based on actual REDCap field
  !is.na(review$review_complete) && review$review_complete == 1
}

#' Get all residents for CCC review table
#'
#' @param data List containing all data frames
#' @param academic_year Character string "YYYY-YYYY"
#' @param period Integer 1-6
#' @return Data frame with resident info and completion status
get_ccc_review_table <- function(data, academic_year, period) {
  residents <- get_resident_list(data)

  if (length(residents) == 0) {
    return(data.frame(
      resident = character(0),
      pgy = integer(0),
      completed = logical(0)
    ))
  }

  # Build table with completion status
  purrr::map_dfr(residents, function(res_name) {
    demo <- get_resident_demographics(data, res_name)
    completed <- check_ccc_completion(data, res_name, period)

    data.frame(
      resident = res_name,
      pgy = if (nrow(demo) > 0) demo$pgy else NA_integer_,
      completed = completed
    )
  })
}
