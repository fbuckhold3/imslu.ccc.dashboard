# helpers.R - Pure functions for data mapping and utilities
# No data access - all functions are pure transformations
# Period calculation is handled in global.R using gmed logic

#' Map REDCap field names to display names
#'
#' @param field_name Character string of REDCap field name
#' @return Character string of display name
map_field_to_display <- function(field_name) {
  # Mapping for milestone field names to readable labels
  field_map <- c(
    # Patient Care
    "rep_pc1" = "PC1",
    "rep_pc2" = "PC2",
    "rep_pc3" = "PC3",
    "rep_pc4" = "PC4",
    "rep_pc5" = "PC5",
    "rep_pc6" = "PC6",

    # Medical Knowledge
    "rep_mk1" = "MK1",
    "rep_mk2" = "MK2",
    "rep_mk3" = "MK3",

    # Systems-Based Practice
    "rep_sbp1" = "SBP1",
    "rep_sbp2" = "SBP2",
    "rep_sbp3" = "SBP3",

    # Practice-Based Learning
    "rep_pbli1" = "PBLI1",
    "rep_pbli2" = "PBLI2",
    "rep_pbli3" = "PBLI3",

    # Professionalism
    "rep_prof1" = "PROF1",
    "rep_prof2" = "PROF2",
    "rep_prof3" = "PROF3",
    "rep_prof4" = "PROF4",

    # Interpersonal Communication
    "rep_ics1" = "ICS1",
    "rep_ics2" = "ICS2",
    "rep_ics3" = "ICS3"
  )

  # Return mapped name if exists, otherwise return original
  if (field_name %in% names(field_map)) {
    return(field_map[field_name])
  } else {
    return(field_name)
  }
}

#' Create Completion Indicator Icon
#'
#' Returns HTML for completion status indicator
#'
#' @param is_complete Logical
#' @return HTML tag for icon
#' @export
completion_icon <- function(is_complete) {
  if (is_complete) {
    tags$span(
      style = "color: #28a745; font-size: 1.2em;",
      title = "Completed",
      "\u2713"  # Check mark
    )
  } else {
    tags$span(
      style = "color: #dc3545; font-size: 1.2em;",
      title = "Not completed",
      "\u25CF"  # Bullet point
    )
  }
}

#' Get Full Subcompetency Name
#'
#' Maps competency codes to full descriptive names
#'
#' @param competency Character string of competency code (e.g., "PC1")
#' @return Character string of full name
get_competency_full_name <- function(competency) {
  competency_names <- c(
    # Patient Care
    "PC1" = "PC1: History",
    "PC2" = "PC2: Physical Examination",
    "PC3" = "PC3: Clinical Reasoning",
    "PC4" = "PC4: Patient Management - Inpatient",
    "PC5" = "PC5: Patient Management - Outpatient",
    "PC6" = "PC6: Digital Health",

    # Medical Knowledge
    "MK1" = "MK1: Applied Foundational Sciences",
    "MK2" = "MK2: Therapeutic Knowledge",
    "MK3" = "MK3: Knowledge of Diagnostic Testing",

    # Systems-Based Practice
    "SBP1" = "SBP1: Patient Safety and Quality Improvement",
    "SBP2" = "SBP2: System Navigation for Patient-Centered Care",
    "SBP3" = "SBP3: Physician Role in Health Care Systems",

    # Practice-Based Learning and Improvement
    "PBLI1" = "PBLI1: Evidence-Based and Informed Practice",
    "PBLI2" = "PBLI2: Reflective Practice and Commitment to Personal Growth",

    # Professionalism
    "PROF1" = "PROF1: Professional Behavior",
    "PROF2" = "PROF2: Ethical Principles",
    "PROF3" = "PROF3: Accountability/Conscientiousness",
    "PROF4" = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",

    # Interpersonal and Communication Skills
    "ICS1" = "ICS1: Patient- and Family-Centered Communication",
    "ICS2" = "ICS2: Interprofessional and Team Communication",
    "ICS3" = "ICS3: Communication within Health Care Systems"
  )

  if (competency %in% names(competency_names)) {
    return(competency_names[competency])
  } else {
    return(competency)
  }
}

#' Parse Data Dictionary Choices
#'
#' Parses the select_choices_or_calculations field from REDCap data dictionary
#' @param choices_string String from data dictionary (format: "1, Label 1 | 2, Label 2")
#' @return Named vector where names are codes and values are labels
parse_data_dict_choices <- function(choices_string) {
  if (is.na(choices_string) || nchar(trimws(choices_string)) == 0) {
    return(character(0))
  }

  # Debug: Show raw choices string
  message("parse_data_dict_choices - Raw string: ", substr(choices_string, 1, 200))

  # Split by pipe delimiter
  choice_pairs <- strsplit(choices_string, "\\|")[[1]]

  choices <- character(0)
  for (pair in choice_pairs) {
    # Split by first comma
    parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      code <- trimws(parts[1])
      label <- trimws(paste(parts[-1], collapse = ","))
      choices[code] <- label
      message("  Parsed: code='", code, "' -> label='", label, "'")
    }
  }

  return(choices)
}

#' Get Field Choices from Data Dictionary
#'
#' Gets choices for a specific field from the data dictionary
#' @param data_dict Data dictionary data frame
#' @param field_name Name of the field
#' @param for_ui Logical, if TRUE returns labels as names (for Shiny UI), if FALSE returns codes as names (for lookup/translation)
#' @return Named vector of choices
get_field_choices <- function(data_dict, field_name, for_ui = FALSE) {
  if (is.null(data_dict) || !field_name %in% data_dict$field_name) {
    return(character(0))
  }

  field_info <- data_dict %>%
    filter(field_name == !!field_name) %>%
    slice(1)

  if (nrow(field_info) == 0 || is.na(field_info$select_choices_or_calculations)) {
    return(character(0))
  }

  choices <- parse_data_dict_choices(field_info$select_choices_or_calculations)

  # For Shiny UI components, we need labels as names and codes as values
  # For translation/lookup, we need codes as names and labels as values
  if (for_ui && length(choices) > 0) {
    # Reverse: make labels the names and codes the values
    reversed <- names(choices)
    names(reversed) <- unname(choices)
    return(reversed)
  }

  return(choices)
}

#' Translate Checkbox Values to Labels
#'
#' Translates REDCap checkbox field values (multiple checked) to readable labels
#' @param data_dict Data dictionary data frame
#' @param field_name Base field name (without ___N suffix)
#' @param checked_cols Vector of column names that are checked (value = "1")
#' @return Comma-separated string of labels
translate_checkbox_values <- function(data_dict, field_name, checked_cols) {
  # Get choices from data dictionary
  choices <- get_field_choices(data_dict, field_name)

  # Debug output
  message("translate_checkbox_values called:")
  message("  field_name: ", field_name)
  message("  checked_cols: ", paste(checked_cols, collapse = ", "))
  message("  choices available: ", length(choices))
  if (length(choices) > 0) {
    message("  choice codes: ", paste(names(choices), collapse = ", "))
  }

  if (length(choices) == 0 || length(checked_cols) == 0) {
    message("  returning empty (no choices or no checked)")
    return("")
  }

  # Extract codes from column names (e.g., "ccc_competency___1" -> "1")
  codes <- gsub(paste0(field_name, "___"), "", checked_cols)
  message("  extracted codes: ", paste(codes, collapse = ", "))

  # Get labels for checked codes
  labels <- choices[codes]
  message("  labels found: ", paste(labels[!is.na(labels)], collapse = ", "))
  labels <- labels[!is.na(labels)]

  if (length(labels) == 0) {
    message("  returning empty (no labels matched)")
    return("")
  }

  result <- paste(labels, collapse = ", ")
  message("  returning: ", result)
  return(result)
}
