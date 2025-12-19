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

  # Check completion status for each resident
  purrr::map_dfr(1:nrow(residents_for_review), function(i) {
    res <- residents_for_review[i, ]

    # Check coach review completion
    coach_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "coach_rev",
      res$record_id,
      res$current_period
    )

    coach_complete <- nrow(coach_data) > 0
    coach_name <- if (coach_complete && "coach" %in% names(coach_data)) {
      as.character(coach_data$coach[1])
    } else {
      NA_character_
    }

    # Check second review completion
    second_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "second_review",
      res$record_id,
      res$current_period
    )

    second_complete <- nrow(second_data) > 0
    second_rev_name <- if (second_complete && "second_rev" %in% names(second_data)) {
      as.character(second_data$second_rev[1])
    } else {
      NA_character_
    }

    # Check CCC review completion
    ccc_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "ccc_review",
      res$record_id,
      res$current_period
    )

    ccc_complete <- nrow(ccc_data) > 0

    # Determine PGY level from current period
    pgy_level <- if (!is.na(res$current_period)) {
      if (grepl("Intern", res$current_period)) {
        "Intern"
      } else if (grepl("PGY2", res$current_period)) {
        "PGY2"
      } else if (grepl("PGY3|Graduating", res$current_period)) {
        "PGY3"
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }

    data.frame(
      record_id = res$record_id,
      resident = res$full_name,
      level = res$current_period,
      pgy_level = pgy_level,
      expected_period = res$current_period,
      coach_name = coach_name,
      second_rev_name = second_rev_name,
      coach_complete = coach_complete,
      second_complete = second_complete,
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

#' Get Coach Review for Resident and Period
#'
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with coach review data or empty df
get_resident_coach_review <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "coach_rev",
    record_id,
    period_name
  )
}

#' Get Second Review for Resident and Period
#'
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with second review data or empty df
get_resident_second_review <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "second_review",
    record_id,
    period_name
  )
}

#' Get Milestone Descriptions for Resident and Period
#'
#' Extracts populated description fields from milestone_entry and self-evaluation
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with competency, source, score, and description
get_milestone_descriptions <- function(rdm_data, record_id, period_name) {

  # Get program milestone data
  program_data <- get_form_data_for_period(
    rdm_data$all_forms,
    "milestone_entry",
    record_id,
    period_name
  )

  # Get self-evaluation data
  self_data <- get_form_data_for_period(
    rdm_data$all_forms,
    "milestone_selfevaluation_c33c",
    record_id,
    period_name
  )

  descriptions <- data.frame(
    competency = character(),
    competency_full = character(),
    source = character(),
    score = numeric(),
    description = character(),
    stringsAsFactors = FALSE
  )

  # Extract program milestone descriptions
  if (nrow(program_data) > 0) {
    desc_cols <- grep("_desc$", names(program_data), value = TRUE)
    desc_cols <- desc_cols[!grepl("_self_desc$", desc_cols)]  # Exclude self descriptions

    for (col in desc_cols) {
      if (!is.na(program_data[[col]][1]) && nchar(trimws(program_data[[col]][1])) > 0) {
        # Extract competency name (e.g., "rep_pc1_desc" -> "PC1")
        competency <- toupper(gsub("rep_|_desc", "", col))
        # Get full competency name
        competency_full <- get_competency_full_name(competency)
        # Get the corresponding value field (e.g., "rep_pc1")
        value_field <- gsub("_desc", "", col)
        score <- if (value_field %in% names(program_data)) {
          as.numeric(program_data[[value_field]][1])
        } else {
          NA
        }

        descriptions <- rbind(descriptions, data.frame(
          competency = competency,
          competency_full = competency_full,
          source = "Program",
          score = score,
          description = program_data[[col]][1],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Extract self-evaluation descriptions
  if (nrow(self_data) > 0) {
    desc_cols <- grep("_self_desc$", names(self_data), value = TRUE)

    for (col in desc_cols) {
      if (!is.na(self_data[[col]][1]) && nchar(trimws(self_data[[col]][1])) > 0) {
        # Extract competency name (e.g., "rep_pc1_self_desc" -> "PC1")
        competency <- toupper(gsub("rep_|_self_desc", "", col))
        # Get full competency name
        competency_full <- get_competency_full_name(competency)
        # Get the corresponding value field (e.g., "rep_pc1_self")
        value_field <- gsub("_desc", "", col)
        score <- if (value_field %in% names(self_data)) {
          as.numeric(self_data[[value_field]][1])
        } else {
          NA
        }

        descriptions <- rbind(descriptions, data.frame(
          competency = competency,
          competency_full = competency_full,
          source = "Self",
          score = score,
          description = self_data[[col]][1],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(descriptions)
}

#' Get Milestone Entry Values for Editing
#'
#' Gets current milestone values for a resident and period for editing
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with competency names and values
get_milestone_values_for_edit <- function(rdm_data, record_id, period_name) {

  # Get program milestone data
  program_data <- get_form_data_for_period(
    rdm_data$all_forms,
    "milestone_entry",
    record_id,
    period_name
  )

  if (nrow(program_data) == 0) {
    return(data.frame(
      competency = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Get all milestone value columns (not descriptions, not metadata)
  value_cols <- grep("^rep_(pc|mk|sbp|pbli|prof|ics)\\d+$", names(program_data), value = TRUE)

  milestone_values <- data.frame(
    competency = character(),
    field_name = character(),
    value = numeric(),
    stringsAsFactors = FALSE
  )

  for (col in value_cols) {
    # Extract competency name (e.g., "rep_pc1" -> "PC1")
    competency <- toupper(gsub("rep_", "", col))
    value <- program_data[[col]][1]

    # Only include if value exists
    if (!is.na(value)) {
      milestone_values <- rbind(milestone_values, data.frame(
        competency = competency,
        field_name = col,
        value = as.numeric(value),
        stringsAsFactors = FALSE
      ))
    }
  }

  return(milestone_values)
}

#' Get Coach Review Data
#'
#' Gets coach review data for a resident and period
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with coach review data (single row or empty)
get_coach_review_data <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "coach_rev",
    record_id,
    period_name
  )
}

#' Get Second Review Data
#'
#' Gets second review data for a resident and period
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with second review data (single row or empty)
get_second_review_data <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "second_review",
    record_id,
    period_name
  )
}

#' Get CCC Review Data
#'
#' Gets CCC review data for a resident and period
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with CCC review data (single row or empty)
get_ccc_review_data <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "ccc_review",
    record_id,
    period_name
  )
}

#' Get Action Data Table
#'
#' Gets all CCC action data (concerns and issues) for a resident
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @return Data frame with all action items
get_action_data_table <- function(rdm_data, record_id) {

  if (is.null(rdm_data$all_forms$ccc_review)) {
    return(data.frame(
      Date = character(),
      Session = character(),
      Type = character(),
      Issues = character(),
      Comments = character(),
      Competency = character(),
      Action = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Get all CCC reviews for this resident
  ccc_data <- rdm_data$all_forms$ccc_review %>%
    filter(record_id == !!record_id, redcap_repeat_instrument == "ccc_review")

  if (nrow(ccc_data) == 0) {
    return(data.frame(
      Date = character(),
      Session = character(),
      Type = character(),
      Issues = character(),
      Comments = character(),
      Competency = character(),
      Action = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Filter for rows that have either concern or issues follow up
  # Check if required fields exist
  has_concern <- "ccc_concern" %in% names(ccc_data)
  has_issues <- "ccc_issues_follow_up" %in% names(ccc_data)

  if (!has_concern && !has_issues) {
    # No relevant fields exist, return empty
    return(data.frame(
      Date = character(),
      Session = character(),
      Type = character(),
      Issues = character(),
      Comments = character(),
      Competency = character(),
      Action = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Filter for rows with concerns or issues
  if (has_concern && has_issues) {
    action_data <- ccc_data %>%
      filter(
        (!is.na(ccc_concern) & ccc_concern == "1") |
        (!is.na(ccc_issues_follow_up) & nchar(trimws(ccc_issues_follow_up)) > 0)
      )
  } else if (has_concern) {
    action_data <- ccc_data %>%
      filter(!is.na(ccc_concern) & ccc_concern == "1")
  } else {
    action_data <- ccc_data %>%
      filter(!is.na(ccc_issues_follow_up) & nchar(trimws(ccc_issues_follow_up)) > 0)
  }

  if (nrow(action_data) == 0) {
    return(data.frame(
      Date = character(),
      Session = character(),
      Type = character(),
      Issues = character(),
      Comments = character(),
      Competency = character(),
      Action = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Build output data frame with safe column access
  # For checkbox fields, we need to translate the checked values to labels
  result_list <- list()

  for (i in 1:nrow(action_data)) {
    row <- action_data[i, ]

    # Helper function to get checkbox labels for a field
    get_checkbox_labels <- function(field_base_name) {
      # First, check for individual checkbox columns (field___1, field___2, etc.)
      checkbox_cols <- grep(paste0("^", field_base_name, "___"), names(row), value = TRUE)

      if (length(checkbox_cols) > 0) {
        # New format: individual checkbox columns
        checked <- c()
        for (col in checkbox_cols) {
          col_value <- row[[col]]
          if (!is.na(col_value) && as.character(col_value) == "1") {
            checked <- c(checked, col)
          }
        }
        return(translate_checkbox_values(rdm_data$data_dict, field_base_name, checked))
      } else if (field_base_name %in% names(row)) {
        # Old format: single field with comma-separated codes
        codes_str <- as.character(row[[field_base_name]])
        if (!is.na(codes_str) && nchar(trimws(codes_str)) > 0) {
          # Split comma-separated codes
          codes <- trimws(strsplit(codes_str, ",")[[1]])
          if (length(codes) > 0) {
            # Get choices from data dictionary
            choices <- get_field_choices(rdm_data$data_dict, field_base_name)
            if (length(choices) > 0) {
              labels <- choices[codes]
              labels <- labels[!is.na(labels)]
              if (length(labels) > 0) {
                return(paste(labels, collapse = ", "))
              }
            }
          }
        }
      }
      return("")
    }

    # Get labels for each checkbox field
    competency_labels <- get_checkbox_labels("ccc_competency")
    action_labels <- get_checkbox_labels("ccc_action")
    status_labels <- get_checkbox_labels("ccc_action_status")

    result_list[[i]] <- data.frame(
      Date = if ("ccc_date" %in% names(row)) as.character(row$ccc_date) else "",
      Session = if ("ccc_session" %in% names(row)) as.character(row$ccc_session) else "",
      Type = if ("ccc_rev_type" %in% names(row)) as.character(row$ccc_rev_type) else "",
      Issues = if ("ccc_issues_follow_up" %in% names(row)) as.character(row$ccc_issues_follow_up) else "",
      Comments = if ("ccc_comments" %in% names(row)) as.character(row$ccc_comments) else "",
      Competency = competency_labels,
      Action = action_labels,
      Status = status_labels,
      stringsAsFactors = FALSE
    )
  }

  result <- do.call(rbind, result_list)

  # Replace NA with empty strings
  result[is.na(result)] <- ""

  # Sort by date if available
  if ("ccc_date" %in% names(action_data) && any(nchar(result$Date) > 0)) {
    result <- result %>% arrange(desc(Date))
  }

  return(result)
}
