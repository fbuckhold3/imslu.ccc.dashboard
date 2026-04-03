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

    # Get coach and second reviewer names from residents data
    coach_name <- if ("coach" %in% names(res) && !is.na(res$coach)) {
      as.character(res$coach)
    } else {
      NA_character_
    }

    second_rev_name <- if ("second_rev" %in% names(res) && !is.na(res$second_rev)) {
      as.character(res$second_rev)
    } else {
      NA_character_
    }

    # Translate coach name if it's a code
    if (!is.na(coach_name) && !is.null(rdm_data$data_dict)) {
      if ("coach" %in% rdm_data$data_dict$field_name) {
        choices <- get_field_choices(rdm_data$data_dict, "coach")
        if (length(choices) > 0 && coach_name %in% names(choices)) {
          coach_name <- as.character(choices[coach_name])
        }
      }
    }

    # Translate second reviewer name if it's a code
    if (!is.na(second_rev_name) && !is.null(rdm_data$data_dict)) {
      if ("second_rev" %in% rdm_data$data_dict$field_name) {
        choices <- get_field_choices(rdm_data$data_dict, "second_rev")
        if (length(choices) > 0 && second_rev_name %in% names(choices)) {
          second_rev_name <- as.character(choices[second_rev_name])
        }
      }
    }

    # Check coach review completion
    coach_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "coach_rev",
      res$record_id,
      res$current_period
    )

    coach_complete <- nrow(coach_data) > 0

    # Check second review completion
    second_data <- get_form_data_for_period(
      rdm_data$all_forms,
      "second_review",
      res$record_id,
      res$current_period
    )

    second_complete <- nrow(second_data) > 0

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
  value_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", names(program_data), value = TRUE)

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

# ==============================================================================
# FOLLOW-UP TRACKER HELPERS
# ==============================================================================

#' Build Follow-up Tracker Summary
#'
#' Returns one row per resident who has ANY qualifying ccc_review record.
#' A record qualifies if: ccc_rev_type=="2" OR ccc_concern=="1" OR any
#' ccc_action___N=="1".
#' Aggregates across all qualifying records for each resident.
#'
#' @param rdm_data List containing all data from load_ccc_data()
#' @return Data frame with one row per qualifying resident, columns:
#'   record_id, Resident, Level, Coach, Type, Last Review,
#'   Current Status, All Actions, Person Responsible, Follow-up Notes,
#'   is_interim, is_initiation, is_ongoing, is_resolved, is_recurring
get_tracker_summary <- function(rdm_data) {

  if (is.null(rdm_data$all_forms$ccc_review)) return(data.frame())

  ccc_all <- rdm_data$all_forms$ccc_review %>%
    filter(redcap_repeat_instrument == "ccc_review")

  if (nrow(ccc_all) == 0) return(data.frame())

  # Name / level / coach lookups
  residents <- rdm_data$residents
  id_to_name  <- setNames(as.character(residents$full_name),    as.character(residents$record_id))
  id_to_level <- setNames(as.character(residents$current_period), as.character(residents$record_id))

  id_to_coach <- setNames(rep("", nrow(residents)), as.character(residents$record_id))
  if ("coach" %in% names(residents)) {
    coach_choices <- get_field_choices(rdm_data$data_dict, "coach")
    for (i in seq_len(nrow(residents))) {
      cv <- as.character(residents$coach[i])
      if (!is.na(cv) && nchar(cv) > 0) {
        id_to_coach[as.character(residents$record_id[i])] <-
          if (length(coach_choices) > 0 && cv %in% names(coach_choices))
            as.character(coach_choices[cv])
          else cv
      }
    }
  }

  # Identify qualifying rows
  action_cols_present  <- intersect(paste0("ccc_action___", 1:8),  names(ccc_all))
  status_cols_present  <- intersect(paste0("ccc_action_status___", 1:4), names(ccc_all))

  has_action <- if (length(action_cols_present) > 0) {
    apply(ccc_all[, action_cols_present, drop = FALSE], 1, function(r) any(!is.na(r) & r == "1"))
  } else rep(FALSE, nrow(ccc_all))

  has_interim  <- !is.na(ccc_all$ccc_rev_type) & ccc_all$ccc_rev_type == "2"
  has_concern  <- if ("ccc_concern" %in% names(ccc_all))
    !is.na(ccc_all$ccc_concern) & ccc_all$ccc_concern == "1"
  else rep(FALSE, nrow(ccc_all))

  qualifying <- ccc_all[has_interim | has_concern | has_action, ]
  if (nrow(qualifying) == 0) return(data.frame())

  qualifying_rids <- unique(qualifying$record_id)

  result_list <- lapply(qualifying_rids, function(rid) {
    recs <- qualifying[qualifying$record_id == rid, ]

    # Sort descending by date
    if ("ccc_date" %in% names(recs)) {
      recs <- recs[order(recs$ccc_date, decreasing = TRUE, na.last = TRUE), ]
    }
    most_recent <- recs[1, ]

    # Type
    type_display <- if (any(!is.na(recs$ccc_rev_type) & recs$ccc_rev_type == "2")) "Interim" else "Scheduled"

    # Last review date
    last_date <- if ("ccc_date" %in% names(most_recent)) {
      as.character(most_recent$ccc_date[1])
    } else ""

    # Current status from most recent record
    status_labels <- c()
    for (sc in status_cols_present) {
      n <- gsub("ccc_action_status___", "", sc)
      val <- most_recent[[sc]][1]
      if (!is.na(val) && val == "1") {
        status_labels <- c(status_labels, CCC_STATUS_LABELS[n])
      }
    }
    current_status <- if (length(status_labels) > 0) paste(status_labels, collapse = ", ") else "—"

    # All actions across all qualifying records (deduplicated)
    action_labels <- c()
    for (ac in action_cols_present) {
      n <- gsub("ccc_action___", "", ac)
      if (any(!is.na(recs[[ac]]) & recs[[ac]] == "1")) {
        action_labels <- c(action_labels, CCC_ACTION_LABELS[n])
      }
    }
    all_actions <- if (length(action_labels) > 0) paste(unique(action_labels), collapse = ", ") else "—"

    # Person responsible: most recent non-empty
    person_resp <- ""
    if ("ccc_fu_resp" %in% names(recs)) {
      for (i in seq_len(nrow(recs))) {
        v <- as.character(recs$ccc_fu_resp[i])
        if (!is.na(v) && nchar(trimws(v)) > 0) { person_resp <- v; break }
      }
    }

    # Follow-up notes: most recent non-empty
    follow_up <- ""
    if ("ccc_issues_follow_up" %in% names(recs)) {
      for (i in seq_len(nrow(recs))) {
        v <- as.character(recs$ccc_issues_follow_up[i])
        if (!is.na(v) && nchar(trimws(v)) > 0) { follow_up <- v; break }
      }
    }

    # Flags for value-box counts and filter
    is_interim    <- any(!is.na(recs$ccc_rev_type)     & recs$ccc_rev_type == "2")
    is_concern    <- "ccc_concern" %in% names(recs) &&
                     any(!is.na(recs$ccc_concern) & recs$ccc_concern == "1")

    make_status_flag <- function(code) {
      sc <- paste0("ccc_action_status___", code)
      sc %in% names(recs) && any(!is.na(recs[[sc]]) & recs[[sc]] == "1")
    }
    is_initiation <- make_status_flag("1")
    is_ongoing    <- make_status_flag("2")
    is_resolved   <- make_status_flag("3")
    is_recurring  <- make_status_flag("4")

    rid_str <- as.character(rid)
    data.frame(
      record_id            = rid_str,
      Resident             = if (!is.na(id_to_name[rid_str]))  id_to_name[rid_str]  else rid_str,
      Level                = if (!is.na(id_to_level[rid_str])) id_to_level[rid_str] else "",
      Coach                = if (!is.na(id_to_coach[rid_str])) id_to_coach[rid_str] else "",
      Type                 = type_display,
      Concern              = if (is_concern) "Yes" else "—",
      `Last Review`        = last_date,
      `Current Status`     = current_status,
      `All Actions`        = all_actions,
      `Person Responsible` = person_resp,
      `Follow-up Notes`    = follow_up,
      is_interim           = is_interim,
      is_concern           = is_concern,
      is_initiation        = is_initiation,
      is_ongoing           = is_ongoing,
      is_resolved          = is_resolved,
      is_recurring         = is_recurring,
      stringsAsFactors     = FALSE,
      check.names          = FALSE
    )
  })

  do.call(rbind, result_list)
}

#' Get All CCC Review Records (for CCC Review tab)
#'
#' Returns a display-ready data frame of ALL ccc_review records, newest first.
#' Resident names are joined from residents data.
#'
#' @param rdm_data List containing all data from load_ccc_data()
#' @return Data frame with columns:
#'   record_id, Resident, Session, Date, Type, Concern, Actions, Status, Follow-up Notes
get_ccc_review_all <- function(rdm_data) {

  if (is.null(rdm_data$all_forms$ccc_review)) return(data.frame())

  ccc_all <- rdm_data$all_forms$ccc_review %>%
    filter(redcap_repeat_instrument == "ccc_review")

  if (nrow(ccc_all) == 0) return(data.frame())

  residents   <- rdm_data$residents
  id_to_name  <- setNames(as.character(residents$full_name), as.character(residents$record_id))

  action_cols_present <- intersect(paste0("ccc_action___", 1:8), names(ccc_all))
  status_cols_present <- intersect(paste0("ccc_action_status___", 1:4), names(ccc_all))

  rows <- lapply(seq_len(nrow(ccc_all)), function(i) {
    row <- ccc_all[i, ]
    rid_str <- as.character(row$record_id[1])

    type_label <- if (!is.na(row$ccc_rev_type[1]) && row$ccc_rev_type[1] == "2") "Interim" else "Scheduled"

    concern_label <- if ("ccc_concern" %in% names(row) &&
                         !is.na(row$ccc_concern[1]) && row$ccc_concern[1] == "1") "Yes" else "No"

    action_labels <- c()
    for (ac in action_cols_present) {
      n <- gsub("ccc_action___", "", ac)
      if (!is.na(row[[ac]][1]) && row[[ac]][1] == "1")
        action_labels <- c(action_labels, CCC_ACTION_LABELS[n])
    }
    actions_str <- if (length(action_labels) > 0) paste(action_labels, collapse = "; ") else ""

    status_labels <- c()
    for (sc in status_cols_present) {
      n <- gsub("ccc_action_status___", "", sc)
      if (!is.na(row[[sc]][1]) && row[[sc]][1] == "1")
        status_labels <- c(status_labels, CCC_STATUS_LABELS[n])
    }
    status_str <- if (length(status_labels) > 0) paste(status_labels, collapse = ", ") else ""

    data.frame(
      record_id          = rid_str,
      Resident           = if (!is.na(id_to_name[rid_str])) id_to_name[rid_str] else rid_str,
      Session            = if ("ccc_session"          %in% names(row)) as.character(row$ccc_session[1])          else "",
      Date               = if ("ccc_date"             %in% names(row)) as.character(row$ccc_date[1])             else "",
      Type               = type_label,
      Concern            = concern_label,
      Actions            = actions_str,
      Status             = status_str,
      `Follow-up Notes`  = if ("ccc_issues_follow_up" %in% names(row)) as.character(row$ccc_issues_follow_up[1]) else "",
      stringsAsFactors   = FALSE,
      check.names        = FALSE
    )
  })

  result <- do.call(rbind, rows)

  # Sort newest first
  if ("Date" %in% names(result) && any(nchar(result$Date) > 0)) {
    result <- result[order(result$Date, decreasing = TRUE, na.last = TRUE), ]
  }

  result
}

# ==============================================================================
# EXISTING: Get Action Data Table
# ==============================================================================

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

# ==============================================================================
# AD HOC REVIEW CONTEXT HELPER
# ==============================================================================

#' Get context data for the Ad Hoc Review panel
#'
#' Returns a named list with the most relevant coaching and CCC review data
#' for a resident, regardless of which period it's from. Used to populate
#' the "Previous Reviews" two-column summary table on the Ad Hoc page.
#'
#' @param rdm_data List containing all data from load_ccc_data()
#' @param record_id Resident record ID
#' @return Named list with fields:
#'   coach_period, coach_summary, coach_ilp,
#'   ccc_session, ccc_comments, ccc_ilp, ccc_issues, ccc_concern,
#'   last_concern_date, last_concern_notes, last_concern_type
get_adhoc_review_context <- function(rdm_data, record_id) {

  ctx <- list(
    coach_period       = "",  coach_summary      = "",  coach_ilp          = "",
    ccc_session        = "",  ccc_comments       = "",  ccc_ilp            = "",
    ccc_issues         = "",  ccc_concern        = "No",
    last_concern_date  = "",  last_concern_notes = "",  last_concern_type  = ""
  )

  # ---- Coach review (most recent overall) ----
  coach_df <- rdm_data$all_forms$coach_rev
  if (!is.null(coach_df) && nrow(coach_df) > 0) {
    coach_res <- coach_df %>%
      filter(record_id == !!record_id, redcap_repeat_instrument == "coach_rev") %>%
      arrange(desc(redcap_repeat_instance))

    if (nrow(coach_res) > 0) {
      recent <- coach_res[1, ]
      if ("coach_period" %in% names(recent))
        ctx$coach_period <- as.character(recent$coach_period[1])

      # Summary / notes: try known names first, then any field matching
      # "summary|note|comment|narrative|interim" (excluding metadata columns)
      meta_cols <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
                     "redcap_survey_identifier", "coach_period", "coach_date")
      summary_candidates <- c(
        "coach_summary", "coach_comments", "coach_notes", "coach_narrative",
        grep("(summary|comment|note|narrative)", names(recent),
             value = TRUE, ignore.case = TRUE)
      )
      summary_candidates <- unique(setdiff(summary_candidates, meta_cols))
      for (fld in summary_candidates) {
        if (fld %in% names(recent)) {
          v <- as.character(recent[[fld]][1])
          if (!is.na(v) && nchar(trimws(v)) > 0) { ctx$coach_summary <- v; break }
        }
      }

      # ILP / goals: try explicit names then grep for ilp|goal|mile
      ilp_candidates <- c(
        "coach_ilp_final", "coach_ilp", "coach_mile_goal",
        grep("(ilp|goal|mile)", names(recent),
             value = TRUE, ignore.case = TRUE)
      )
      ilp_candidates <- unique(setdiff(ilp_candidates, meta_cols))
      for (fld in ilp_candidates) {
        if (fld %in% names(recent)) {
          v <- as.character(recent[[fld]][1])
          if (!is.na(v) && nchar(trimws(v)) > 0) { ctx$coach_ilp <- v; break }
        }
      }

      # Wellness / career as bonus fields
      for (fld in grep("(wellness|career)", names(recent), value = TRUE, ignore.case = TRUE)) {
        v <- as.character(recent[[fld]][1])
        if (!is.na(v) && nchar(trimws(v)) > 0) {
          ctx$coach_summary <- paste0(
            if (nchar(ctx$coach_summary) > 0) paste0(ctx$coach_summary, "\n") else "",
            tools::toTitleCase(gsub("coach_|_", " ", fld)), ": ", v
          )
          break
        }
      }
    }
  }

  # ---- CCC review ----
  ccc_df <- rdm_data$all_forms$ccc_review
  if (!is.null(ccc_df) && nrow(ccc_df) > 0) {
    ccc_res <- ccc_df %>%
      filter(record_id == !!record_id, redcap_repeat_instrument == "ccc_review") %>%
      arrange(desc(ccc_date), desc(redcap_repeat_instance))

    if (nrow(ccc_res) > 0) {

      # Most recent semi-annual (type == "1")
      ccc_semi <- ccc_res %>%
        filter(!is.na(ccc_rev_type) & ccc_rev_type == "1") %>%
        slice(1)

      if (nrow(ccc_semi) > 0) {
        if ("ccc_session"          %in% names(ccc_semi)) ctx$ccc_session  <- as.character(ccc_semi$ccc_session[1])
        if ("ccc_comments"         %in% names(ccc_semi)) ctx$ccc_comments <- as.character(ccc_semi$ccc_comments[1])
        if ("ccc_ilp"              %in% names(ccc_semi)) ctx$ccc_ilp      <- as.character(ccc_semi$ccc_ilp[1])
        if ("ccc_issues_follow_up" %in% names(ccc_semi)) ctx$ccc_issues   <- as.character(ccc_semi$ccc_issues_follow_up[1])
        if ("ccc_concern"          %in% names(ccc_semi) &&
            !is.na(ccc_semi$ccc_concern[1]) && ccc_semi$ccc_concern[1] == "1")
          ctx$ccc_concern <- "Yes"
      }

      # Most recent record with any concern or follow-up (any type, any period)
      has_fu_col <- "ccc_issues_follow_up" %in% names(ccc_res)
      ccc_concern_rows <- if (has_fu_col) {
        ccc_res %>%
          filter((!is.na(ccc_concern) & ccc_concern == "1") |
                 (!is.na(ccc_issues_follow_up) & nchar(trimws(ccc_issues_follow_up)) > 0))
      } else {
        ccc_res %>% filter(!is.na(ccc_concern) & ccc_concern == "1")
      }

      if (nrow(ccc_concern_rows) > 0) {
        cr <- ccc_concern_rows[1, ]
        if ("ccc_date"             %in% names(cr)) ctx$last_concern_date  <- as.character(cr$ccc_date[1])
        if ("ccc_issues_follow_up" %in% names(cr)) ctx$last_concern_notes <- as.character(cr$ccc_issues_follow_up[1])
        ctx$last_concern_type <- if (!is.na(cr$ccc_rev_type[1]) && cr$ccc_rev_type[1] == "2") "Interim" else "Scheduled"
      }
    }
  }

  # Normalise NA → ""
  ctx <- lapply(ctx, function(v) {
    v <- as.character(v)
    if (is.na(v) || trimws(v) == "NA") "" else v
  })

  ctx
}

# ==============================================================================
# MILESTONE ANALYSIS HELPER
# ==============================================================================

#' Build Longitudinal Milestone Dataset
#'
#' Converts all milestone_entry records into a long-format data frame suitable
#' for trajectory plots.  Pass rdm_data loaded with include_archived = TRUE to
#' include historical residents.
#'
#' @param rdm_data List from load_ccc_data()
#' @return Data frame with columns:
#'   record_id, full_name, grad_yr, prog_mile_period (ordered factor),
#'   period_num, milestone (e.g. "PC1"), category (e.g. "PC"), score (numeric)
get_milestone_longitudinal_data <- function(rdm_data) {

  mile_data <- rdm_data$all_forms[["milestone_entry"]]
  if (is.null(mile_data) || nrow(mile_data) == 0) return(data.frame())

  mile_data <- mile_data %>%
    filter(
      redcap_repeat_instrument == "milestone_entry",
      !is.na(prog_mile_period),
      nchar(trimws(as.character(prog_mile_period))) > 0
    )

  if (nrow(mile_data) == 0) return(data.frame())

  # Milestone numeric value columns
  value_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$",
                     names(mile_data), value = TRUE)
  if (length(value_cols) == 0) return(data.frame())

  # Resident lookups
  residents <- rdm_data$residents %>%
    mutate(record_id = as.character(record_id))
  id_to_name <- setNames(as.character(residents$full_name), residents$record_id)
  id_to_grad <- setNames(as.character(residents$grad_yr),   residents$record_id)

  period_order <- c(
    "Mid Intern", "End Intern",
    "Mid PGY2",   "End PGY2",
    "Mid PGY3",   "Graduating"
  )

  # Manual pivot to long (avoids hard tidyr dependency)
  long_rows <- lapply(seq_len(nrow(mile_data)), function(i) {
    row    <- mile_data[i, ]
    rid    <- as.character(row$record_id)
    period <- as.character(row$prog_mile_period)

    rows_for_row <- lapply(value_cols, function(col) {
      val <- suppressWarnings(as.numeric(row[[col]]))
      if (is.na(val)) return(NULL)
      ms  <- toupper(gsub("rep_", "", col))
      cat <- gsub("[0-9]+$", "", ms)
      data.frame(
        record_id        = rid,
        full_name        = if (!is.na(id_to_name[rid])) id_to_name[rid] else rid,
        grad_yr          = if (!is.na(id_to_grad[rid]))  id_to_grad[rid]  else NA_character_,
        prog_mile_period = period,
        period_num       = match(period, period_order),
        milestone        = ms,
        category         = cat,
        score            = val,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, Filter(Negate(is.null), rows_for_row))
  })

  long_data <- do.call(rbind, Filter(Negate(is.null), long_rows))
  if (is.null(long_data) || nrow(long_data) == 0) return(data.frame())

  long_data$prog_mile_period <- factor(
    long_data$prog_mile_period,
    levels = period_order
  )

  long_data
}
