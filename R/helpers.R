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
    "PC2" = "PC2: Physical Exam",
    "PC3" = "PC3: Differential Diagnosis",
    "PC4" = "PC4: Management",
    "PC5" = "PC5: Procedures",
    "PC6" = "PC6: Requesting Consultation",

    # Medical Knowledge
    "MK1" = "MK1: Core Knowledge",
    "MK2" = "MK2: Clinical Reasoning",
    "MK3" = "MK3: Scholarly Activities",

    # Systems-Based Practice
    "SBP1" = "SBP1: Systems Awareness",
    "SBP2" = "SBP2: Care Coordination",
    "SBP3" = "SBP3: Quality Improvement",

    # Practice-Based Learning and Improvement
    "PBLI1" = "PBLI1: Self-Assessment",
    "PBLI2" = "PBLI2: Evidence-Based Medicine",
    "PBLI3" = "PBLI3: Teaching",

    # Professionalism
    "PROF1" = "PROF1: Professional Behavior",
    "PROF2" = "PROF2: Ethical Principles",
    "PROF3" = "PROF3: Accountability",
    "PROF4" = "PROF4: Well-Being",

    # Interpersonal and Communication Skills
    "ICS1" = "ICS1: Patient Communication",
    "ICS2" = "ICS2: Team Communication",
    "ICS3" = "ICS3: Transitions of Care"
  )

  if (competency %in% names(competency_names)) {
    return(competency_names[competency])
  } else {
    return(competency)
  }
}
