# redcap_submission.R - REDCap write-back functions for CCC Dashboard
# These functions handle all REDCap write operations beyond the inline server logic.

# ==============================================================================
# ACTION CODES (hardcoded as fallback; data dict is authoritative when present)
# ==============================================================================

CCC_ACTION_LABELS <- c(
  "1" = "Remediation plan",
  "2" = "Probation",
  "3" = "Referral for professionalism",
  "4" = "Coach follow up",
  "5" = "Meet with PD/CCC Chair",
  "6" = "Meet with Chiefs",
  "7" = "Other (see notes)",
  "8" = "Referral to behavioral health"
)

CCC_STATUS_LABELS <- c(
  "1" = "Initiation",
  "2" = "Ongoing",
  "3" = "Resolved",
  "4" = "Recurring"
)

# ==============================================================================
# FUNCTION 1: UPDATE CCC TRACKER STATUS
# ==============================================================================

#' Update CCC Tracker Status
#'
#' Sets ccc_action_status___1 through ___4 explicitly (0 or 1) for the given
#' ccc_review instance and optionally updates ccc_issues_follow_up.
#' Uses overwrite_with_blanks = TRUE so that unchecked statuses are cleared.
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param instance_number Repeating instance number to update
#' @param new_status_values Character vector of checked status codes (e.g., c("1","3"))
#' @param updated_notes Follow-up notes text (NULL = do not update)
#' @return List with success (logical) and message (character)
#' @export
update_ccc_tracker_status <- function(
  redcap_url,
  redcap_token,
  record_id,
  instance_number,
  new_status_values,
  updated_notes = NULL
) {
  update_df <- data.frame(
    record_id                = as.character(record_id),
    redcap_repeat_instrument = "ccc_review",
    redcap_repeat_instance   = as.character(instance_number),
    ccc_action_status___1    = if ("1" %in% as.character(new_status_values)) "1" else "0",
    ccc_action_status___2    = if ("2" %in% as.character(new_status_values)) "1" else "0",
    ccc_action_status___3    = if ("3" %in% as.character(new_status_values)) "1" else "0",
    ccc_action_status___4    = if ("4" %in% as.character(new_status_values)) "1" else "0",
    stringsAsFactors = FALSE
  )

  if (!is.null(updated_notes) && nchar(trimws(as.character(updated_notes))) > 0) {
    update_df$ccc_issues_follow_up <- as.character(updated_notes)
  }

  tryCatch({
    result <- REDCapR::redcap_write(
      ds_to_write        = update_df,
      redcap_uri         = redcap_url,
      token              = redcap_token,
      overwrite_with_blanks = TRUE
    )
    list(success = result$success, message = result$outcome_message)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
}

# ==============================================================================
# FUNCTION 2: SUBMIT AD HOC MEETING NOTES
# ==============================================================================

#' Submit Ad Hoc Meeting Notes
#'
#' Creates a new coach_rev repeating instrument record for an ad hoc/interim
#' meeting, using coach_period = "8" and an instance number >= 9 to avoid
#' colliding with scheduled period instances (1-8).
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param meeting_date Date of meeting (Date or character yyyy-mm-dd)
#' @param meeting_notes Required summary/notes text
#' @param wellness_notes Optional wellness notes
#' @param career_notes Optional career notes
#' @param milestone_notes Optional milestone/goals notes
#' @return List with success (logical) and message (character)
#' @export
submit_adhoc_meeting_notes <- function(
  redcap_url,
  redcap_token,
  record_id,
  meeting_date,
  meeting_notes,
  wellness_notes  = NULL,
  career_notes    = NULL,
  milestone_notes = NULL
) {
  # Find next available instance >= 9
  next_instance <- tryCatch({
    existing <- REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_url,
      token      = redcap_token,
      records    = as.character(record_id),
      forms      = "coach_rev"
    )$data

    if (!is.null(existing) && nrow(existing) > 0 &&
        "redcap_repeat_instance" %in% names(existing)) {
      existing_inst <- suppressWarnings(as.numeric(existing$redcap_repeat_instance))
      existing_inst <- existing_inst[!is.na(existing_inst)]
      max(c(8L, existing_inst), na.rm = TRUE) + 1L
    } else {
      9L
    }
  }, error = function(e) {
    9L
  })

  if (next_instance < 9L) next_instance <- 9L

  new_record <- data.frame(
    record_id                = as.character(record_id),
    redcap_repeat_instrument = "coach_rev",
    redcap_repeat_instance   = as.character(next_instance),
    coach_period             = "8",
    coach_date               = as.character(meeting_date),
    coach_summary            = as.character(meeting_notes),
    stringsAsFactors = FALSE
  )

  if (!is.null(wellness_notes) && nchar(trimws(as.character(wellness_notes))) > 0)
    new_record$coach_wellness  <- as.character(wellness_notes)
  if (!is.null(career_notes) && nchar(trimws(as.character(career_notes))) > 0)
    new_record$coach_career    <- as.character(career_notes)
  if (!is.null(milestone_notes) && nchar(trimws(as.character(milestone_notes))) > 0)
    new_record$coach_mile_goal <- as.character(milestone_notes)

  tryCatch({
    result <- REDCapR::redcap_write(
      ds_to_write = new_record,
      redcap_uri  = redcap_url,
      token       = redcap_token
    )
    list(success = result$success, message = result$outcome_message)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
}

# ==============================================================================
# FUNCTION 3: SUBMIT NEW TRACKER ISSUE
# ==============================================================================

#' Submit New Tracker Issue
#'
#' Creates a new ccc_review repeating instrument record for an ad hoc / interim
#' issue. Always sets ccc_rev_type = "2" (Interim) and ccc_concern = "1".
#' Uses instance number >= 9 for interim entries.
#' Sets all action (1-8), status (1-4), and competency (1-7) checkboxes.
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param description Issue description (stored in ccc_interim)
#' @param actions Character vector of selected action codes (e.g., c("1","4"))
#' @param status Character vector of selected status codes (e.g., c("1"))
#' @param competency Character vector of selected competency codes (e.g., c("1","3"))
#' @param person_resp Person responsible (stored in ccc_fu_resp)
#' @param notes Follow-up notes (stored in ccc_issues_follow_up)
#' @return List with success (logical) and message (character)
#' @export
submit_new_tracker_issue <- function(
  redcap_url,
  redcap_token,
  record_id,
  description,
  actions   = character(0),
  status    = character(0),
  competency = character(0),
  person_resp = NULL,
  notes       = NULL
) {
  # Find next available instance >= 9
  next_instance <- tryCatch({
    existing <- REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_url,
      token      = redcap_token,
      records    = as.character(record_id),
      forms      = "ccc_review"
    )$data

    if (!is.null(existing) && nrow(existing) > 0 &&
        "redcap_repeat_instance" %in% names(existing)) {
      existing_inst <- suppressWarnings(as.numeric(existing$redcap_repeat_instance))
      existing_inst <- existing_inst[!is.na(existing_inst)]
      max(c(8L, existing_inst), na.rm = TRUE) + 1L
    } else {
      9L
    }
  }, error = function(e) {
    9L
  })

  if (next_instance < 9L) next_instance <- 9L

  new_record <- data.frame(
    record_id                = as.character(record_id),
    redcap_repeat_instrument = "ccc_review",
    redcap_repeat_instance   = as.character(next_instance),
    ccc_rev_type             = "2",
    ccc_concern              = "1",
    ccc_date                 = as.character(Sys.Date()),
    ccc_interim              = as.character(description),
    stringsAsFactors = FALSE
  )
  # ccc_session is intentionally omitted — "8" is not a valid category in REDCap;
  # interim tracker entries are not tied to a scheduled review session.

  if (!is.null(person_resp) && nchar(trimws(as.character(person_resp))) > 0)
    new_record$ccc_fu_resp <- as.character(person_resp)
  if (!is.null(notes) && nchar(trimws(as.character(notes))) > 0)
    new_record$ccc_issues_follow_up <- as.character(notes)

  # Action checkboxes 1-8
  for (n in 1:8) {
    new_record[[paste0("ccc_action___", n)]] <-
      if (as.character(n) %in% as.character(actions)) "1" else "0"
  }

  # Status checkboxes 1-4
  for (n in 1:4) {
    new_record[[paste0("ccc_action_status___", n)]] <-
      if (as.character(n) %in% as.character(status)) "1" else "0"
  }

  # Competency checkboxes 1-7
  for (n in 1:7) {
    new_record[[paste0("ccc_competency___", n)]] <-
      if (as.character(n) %in% as.character(competency)) "1" else "0"
  }

  tryCatch({
    result <- REDCapR::redcap_write(
      ds_to_write = new_record,
      redcap_uri  = redcap_url,
      token       = redcap_token
    )
    list(success = result$success, message = result$outcome_message)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
}
