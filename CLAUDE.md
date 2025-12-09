# CLAUDE.md - AI Assistant Guide for imslu.ccc.dashboard

This document provides comprehensive guidance for AI assistants working with the SLU Internal Medicine CCC Dashboard codebase.

## Project Overview

**Repository**: imslu.ccc.dashboard
**Type**: R Shiny Web Application
**Purpose**: Clinical Competency Committee (CCC) review dashboard for Saint Louis University Internal Medicine residency program
**Technology Stack**: R, Shiny, REDCap integration via gmed package

### What This Application Does

The CCC Dashboard enables faculty to review resident competency evaluations during semi-annual review periods. It displays:
- Program milestone evaluations (entered by program)
- Self-evaluation milestones (entered by residents)
- ACGME milestone data (from previous period)
- Coach and second review completion status
- CCC review decisions

## Repository Structure

```
imslu.ccc.dashboard/
├── .gitignore              # Git ignore patterns
├── .Renviron.example       # Template for environment variables (NOT in repo)
├── README.md               # User-facing documentation
├── CLAUDE.md               # This file - AI assistant guide
├── app.R                   # Application entry point
├── manifest.json           # Posit Connect deployment configuration
├── R/                      # All R source code (ONLY .R files)
│   ├── global.R           # API config, period definitions, data loading
│   ├── helpers.R          # Pure functions (no data dependencies)
│   ├── wrappers.R         # Data access layer (ALL data filtering/joining)
│   ├── ui.R               # Shiny UI definition
│   └── server.R           # Shiny server logic (reactive only)
└── www/                    # Static web assets
    ├── custom.css         # SLU-branded styling
    └── milestones/        # PNG images of milestone descriptions
        ├── pc1.png through pc6.png
        ├── mk1.png through mk3.png
        ├── sbp1.png through sbp3.png
        ├── pbli1.png, pbli2.png
        ├── prof1.png through prof3.png
        └── ics1.png through ics3.png
```

## Architecture Principles

### 1. Layered Architecture (CRITICAL - MUST FOLLOW)

The codebase uses a strict layered architecture. **Violations of this pattern will break the application.**

```
┌─────────────────────────────────────────┐
│  UI Layer (ui.R)                        │
│  - Shiny UI components only             │
│  - No business logic                    │
└─────────────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────┐
│  Server Layer (server.R)                │
│  - Reactive logic ONLY                  │
│  - NEVER accesses data directly         │
│  - Calls wrapper functions              │
└─────────────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────┐
│  Data Access Layer (wrappers.R)         │
│  - ALL data filtering/joining           │
│  - Returns processed data frames        │
│  - Uses helpers for transformations     │
└─────────────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────┐
│  Pure Functions (helpers.R)             │
│  - No data dependencies                 │
│  - Mapping, formatting, calculations    │
└─────────────────────────────────────────┘
                   ↓
┌─────────────────────────────────────────┐
│  Data Loading (global.R)                │
│  - REDCap API configuration             │
│  - load_ccc_data() function             │
│  - Period definitions and mappings      │
└─────────────────────────────────────────┘
```

**RULES YOU MUST FOLLOW:**

1. **server.R**:
   - NEVER access `app_data()$residents` or `app_data()$all_forms` directly
   - ONLY call wrapper functions from wrappers.R
   - Contains reactive logic only

2. **wrappers.R**:
   - ALL data filtering, joining, and transformation logic
   - Returns clean data frames ready for display
   - When adding new data access, ADD A NEW WRAPPER FUNCTION HERE

3. **helpers.R**:
   - Pure functions with NO data dependencies
   - Mappings, formatting, calculations
   - Must be testable in isolation

4. **global.R**:
   - Configuration and data loading only
   - Period definitions and mappings
   - Environment variable handling

5. **ui.R**:
   - UI components only
   - No business logic whatsoever

### 2. Data Loading and Structure

**Data is loaded ONCE at startup** in app.R using `load_ccc_data()`:

```r
rdm_data <- load_ccc_data()
```

This returns a list with:
- `residents`: Data frame of all residents with calculated `current_period`
- `all_forms`: Named list of all REDCap forms
- `data_dict`: REDCap data dictionary
- `milestone_workflow`: Milestone competency workflow from gmed

**Important**: Data is passed to the server via closure in `create_server(rdm_data)`.

### 3. Period System

The application uses a 7-period system (0-6) aligned with residency training:

| Period # | Name                | Timing          | Review Type |
|----------|---------------------|-----------------|-------------|
| 0        | Entering Residency  | July            | -           |
| 1        | Mid Intern          | December        | Mid Year    |
| 2        | End Intern          | June            | End Year    |
| 3        | Mid PGY2            | December        | Mid Year    |
| 4        | End PGY2            | June            | End Year    |
| 5        | Mid PGY3            | December        | Mid Year    |
| 6        | Graduating          | April/May       | End Year    |

**Review Periods**:
- **Mid Year Reviews**: September - January (periods 1, 3, 5)
- **End Year Reviews**: February - August (periods 2, 4, 6)

**Period Calculation**:
- Done automatically by `gmed::calculate_pgy_and_period()` in global.R
- Stored in `residents$current_period` (name) and `residents$current_period_num` (number)
- Use `get_period_number()` and `get_period_name()` for conversions

## Key Functions and Their Purpose

### Data Access Wrappers (wrappers.R)

**ALWAYS use these functions in server.R. NEVER access data directly.**

```r
get_resident_list(rdm_data, filter_level = NULL)
# Returns: Data frame with record_id, full_name, type, grad_yr, current_period

get_ccc_review_table(rdm_data, review_period = "Mid Year")
# Returns: Data frame with resident info and completion status for reviews

get_resident_milestones(rdm_data, record_id, period_name)
# Returns: List with $program, $self, $acgme milestone data frames

get_resident_ccc_review(rdm_data, record_id, period_name)
# Returns: Data frame with CCC review form data

get_resident_coach_review(rdm_data, record_id, period_name)
# Returns: Data frame with coach review form data

get_resident_second_review(rdm_data, record_id, period_name)
# Returns: Data frame with second review form data

get_milestone_descriptions(rdm_data, record_id, period_name)
# Returns: Data frame with competency, source, score, description

get_milestone_values_for_edit(rdm_data, record_id, period_name)
# Returns: Data frame with competency names and current values for editing
```

### Pure Helper Functions (helpers.R)

```r
map_field_to_display(field_name)
# Maps REDCap field names (e.g., "rep_pc1") to display names (e.g., "PC1")

completion_icon(is_complete)
# Returns HTML tag for completion status indicator (✓ or ●)

get_competency_full_name(competency)
# Maps competency code (e.g., "PC1") to full name (e.g., "PC1: History")
```

### Global Functions (global.R)

```r
load_ccc_data(redcap_url, rdm_token, include_archived = FALSE)
# Loads all REDCap data via gmed, calculates periods, returns rdm_data list

get_current_ccc_period(current_date = Sys.Date())
# Returns "Mid Year" or "End Year" based on current date

get_period_number(period_name)
# Converts period name to number (0-6)

get_period_name(period_number)
# Converts period number to name

get_form_data_for_period(all_forms, form_name, record_id, period_name)
# Helper for filtering form data by period (handles form-specific period fields)
```

## Competency Structure

The application tracks 19 ACGME Internal Medicine subcompetencies across 6 domains:

### Patient Care (PC)
- PC1: History
- PC2: Physical Examination
- PC3: Clinical Reasoning
- PC4: Patient Management - Inpatient
- PC5: Patient Management - Outpatient
- PC6: Digital Health

### Medical Knowledge (MK)
- MK1: Applied Foundational Sciences
- MK2: Therapeutic Knowledge
- MK3: Knowledge of Diagnostic Testing

### Systems-Based Practice (SBP)
- SBP1: Patient Safety and Quality Improvement
- SBP2: System Navigation for Patient-Centered Care
- SBP3: Physician Role in Health Care Systems

### Practice-Based Learning and Improvement (PBLI)
- PBLI1: Evidence-Based and Informed Practice
- PBLI2: Reflective Practice and Commitment to Personal Growth

### Professionalism (PROF)
- PROF1: Professional Behavior
- PROF2: Ethical Principles
- PROF3: Accountability/Conscientiousness
- PROF4: Knowledge of Systemic and Individual Factors of Well-Being

### Interpersonal and Communication Skills (ICS)
- ICS1: Patient- and Family-Centered Communication
- ICS2: Interprofessional and Team Communication
- ICS3: Communication within Health Care Systems

**Field Naming Convention in REDCap**:
- Program milestones: `rep_pc1`, `rep_pc2`, etc.
- Self-evaluation: `rep_pc1_self`, `rep_pc2_self`, etc.
- Descriptions: `rep_pc1_desc`, `rep_pc1_self_desc`, etc.

**Milestone Values**: Range from 1-9 (resident progress levels)

## REDCap Integration

### Environment Variables (REQUIRED)

The application requires these environment variables in `.Renviron`:

```
ACCESS_CODE=your_access_code_here
RDM_TOKEN=your_rdm_token_here
EVAL_TOKEN=your_eval_token_here  # May be used by gmed
FAC_TOKEN=your_fac_token_here    # May be used by gmed
```

**NEVER commit .Renviron to git** (it's in .gitignore).

### REDCap Configuration

```r
REDCAP_CONFIG <- list(
  url = "https://redcapsurvey.slu.edu/api/",
  rdm_token = Sys.getenv("RDM_TOKEN"),
  access_code = Sys.getenv("ACCESS_CODE"),
  timeout = 300  # 5 minutes for large data pulls
)
```

### Form-Specific Period Fields

Different REDCap forms use different field names for periods:

| Form Name                       | Period Field Name        |
|---------------------------------|--------------------------|
| s_eval                          | s_e_period               |
| coach_rev                       | coach_period             |
| second_review                   | second_period            |
| milestone_entry                 | prog_mile_period         |
| milestone_selfevaluation_c33c   | prog_mile_period_self    |
| acgme_miles                     | acgme_mile_period        |
| ccc_review                      | ccc_session              |

**These are handled automatically by `get_form_data_for_period()`**. Don't write custom filtering logic.

## Code Style and Conventions

### Naming Conventions

1. **Functions**: `snake_case`
   - `get_resident_list()`, `calculate_period()`, `load_ccc_data()`

2. **Variables**: `snake_case`
   - `resident_data`, `current_period`, `review_table`

3. **Constants**: `SCREAMING_SNAKE_CASE`
   - `REDCAP_CONFIG`, `PERIOD_NAMES`, `PERIOD_TO_NUMBER`

4. **Reactive values**: `snake_case` with clear names
   - `app_data`, `selected_resident_id`, `show_list_view`

### R Style Guidelines

1. **Use explicit argument names** for clarity:
   ```r
   # Good
   get_ccc_review_table(rdm_data = app_data(), review_period = "Mid Year")

   # Acceptable for very common patterns
   get_resident_list(app_data())
   ```

2. **Use the pipe operator** for data transformations:
   ```r
   residents %>%
     filter(!is.na(current_period)) %>%
     select(record_id, full_name, current_period) %>%
     arrange(full_name)
   ```

3. **Explicit data frame creation**:
   ```r
   # Always include stringsAsFactors = FALSE
   data.frame(
     record_id = character(0),
     resident = character(0),
     stringsAsFactors = FALSE
   )
   ```

4. **Function documentation**: Use roxygen-style comments
   ```r
   #' Get list of all residents
   #'
   #' @param rdm_data List containing all data from load_ccc_data()
   #' @param filter_level Optional: filter by PGY level
   #' @return Data frame of residents
   #' @export
   get_resident_list <- function(rdm_data, filter_level = NULL) {
     # ...
   }
   ```

5. **Error handling**: Use `tryCatch()` with informative messages
   ```r
   tryCatch({
     new_data <- load_ccc_data()
     app_data(new_data)
     showNotification("Data refreshed successfully", type = "message")
   }, error = function(e) {
     showNotification(
       paste("Error refreshing data:", e$message),
       type = "error"
     )
   })
   ```

## UI and Styling

### SLU Branding

**Primary Color**: SLU Blue `#003087`
**Secondary Color**: Darker blue `#002366` (hover states)

### Custom CSS Classes

Defined in `www/custom.css`:
- `.well` - Sidebar panels with light gray background
- `.stats-box` - Statistics display boxes
- `.plot-container` - Border around plots
- `.image-modal` - Full-screen image viewer
- `.btn-primary` - SLU blue buttons

### UI Patterns

1. **Conditional Panels**: Toggle between list and detail views
   ```r
   conditionalPanel(
     condition = "output.show_resident_list == true",
     # List view content
   )
   ```

2. **DataTables**: Interactive tables with sorting/filtering
   ```r
   DT::datatable(
     data,
     selection = "single",
     rownames = FALSE,
     options = list(pageLength = 25, dom = 'ft')
   )
   ```

3. **Modal Image Viewer**: Click milestone images to view full-size
   - JavaScript functions in ui.R header: `showImage()`, `closeModal()`
   - Images stored in `www/milestones/`

## Development Workflow

### Local Development

1. **Setup environment variables**:
   ```bash
   cp .Renviron.example .Renviron
   # Edit .Renviron with your tokens
   ```

2. **Install dependencies**:
   ```r
   install.packages(c("shiny", "dplyr", "DT", "purrr", "plotly", "REDCapR"))
   remotes::install_github("fbuckhold3/gmed")
   ```

3. **Run locally**:
   ```r
   # In RStudio: Open app.R and click "Run App"
   # Or from console:
   shiny::runApp()
   ```

### File Source Order

**CRITICAL**: Files must be sourced in this order in app.R:

```r
source("R/global.R")      # 1. Configuration and data loading
source("R/helpers.R")     # 2. Pure functions
source("R/wrappers.R")    # 3. Data access (depends on helpers)
source("R/ui.R")          # 4. UI definition
source("R/server.R")      # 5. Server logic (depends on wrappers)
```

### Adding New Features

Follow this pattern for ANY new feature:

1. **New data access needed?**
   → Add wrapper function to `R/wrappers.R`
   → Use `get_form_data_for_period()` for period filtering
   → Return clean data frame

2. **New calculation/transformation?**
   → Add pure function to `R/helpers.R`
   → No data dependencies
   → Testable in isolation

3. **New UI element?**
   → Update `R/ui.R`
   → Follow SLU branding
   → Use conditional panels for view switching

4. **New server logic?**
   → Update `R/server.R`
   → Call wrappers ONLY (never direct data access)
   → Use reactive values for state

### Code Review Checklist

Before committing changes, verify:

- [ ] No direct data access in server.R (only wrapper calls)
- [ ] New data logic added to wrappers.R
- [ ] Pure functions in helpers.R have no data dependencies
- [ ] Function documentation follows roxygen style
- [ ] Used `stringsAsFactors = FALSE` in data frames
- [ ] Followed snake_case naming conventions
- [ ] No .Renviron or credentials committed
- [ ] UI follows SLU blue branding
- [ ] Error handling with informative messages

## Deployment

### Posit Connect

The app deploys to Posit Connect using `manifest.json`:

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "manifest.json", "R/", "www/"),
  appTitle = "SLU CCC Dashboard"
)
```

**Environment variables must be configured in Posit Connect UI** (not in code).

### Dependencies

Specified in `manifest.json`:
- **CRAN**: shiny, dplyr, DT, purrr, plotly, REDCapR
- **GitHub**: gmed (fbuckhold3/gmed)

## Common Patterns and Examples

### Example 1: Adding a New Data Wrapper

**Scenario**: You need to get evaluation data for a resident.

**Step 1**: Add wrapper function to `R/wrappers.R`:
```r
#' Get Resident Evaluations
#'
#' @param rdm_data List containing all data
#' @param record_id Resident record ID
#' @param period_name Period name
#' @return Data frame with evaluation data
get_resident_evaluations <- function(rdm_data, record_id, period_name) {
  get_form_data_for_period(
    rdm_data$all_forms,
    "s_eval",  # Form name
    record_id,
    period_name
  )
}
```

**Step 2**: Call from server.R:
```r
eval_data <- get_resident_evaluations(
  app_data(),
  selected_resident_id(),
  current_period
)
```

**NEVER DO THIS**:
```r
# WRONG - Direct data access in server.R
eval_data <- app_data()$all_forms$s_eval %>%
  filter(record_id == selected_resident_id())
```

### Example 2: Adding a Pure Helper Function

**Scenario**: You need to format a date for display.

**Step 1**: Add to `R/helpers.R`:
```r
#' Format date for display
#'
#' @param date Date object
#' @return Character string formatted as "Month DD, YYYY"
format_display_date <- function(date) {
  if (is.na(date)) return("")
  format(date, "%B %d, %Y")
}
```

**Step 2**: Use in wrappers or server:
```r
formatted_date <- format_display_date(eval_date)
```

### Example 3: Adding a New UI Tab

**Step 1**: Add tab panel to `R/ui.R`:
```r
tabPanel(
  title = "My New Feature",
  value = "my_feature_tab",
  fluidRow(
    column(
      width = 12,
      h3("My Feature Title"),
      DTOutput("my_feature_table")
    )
  )
)
```

**Step 2**: Add server logic to `R/server.R`:
```r
output$my_feature_table <- renderDT({
  # Call wrapper function (not direct data access!)
  data <- get_my_feature_data(app_data())

  datatable(
    data,
    options = list(pageLength = 25)
  )
})
```

## Troubleshooting

### Common Issues

1. **"RDM_TOKEN not found" error**
   - Check `.Renviron` file exists and has valid token
   - Restart R session after changing `.Renviron`

2. **Empty data tables**
   - Verify period calculation: Check `residents$current_period`
   - Check period filtering: Is the period name exactly correct?
   - Verify REDCap data: Use `View(app_data()$all_forms$form_name)`

3. **"Object not found" errors**
   - Verify source order in app.R
   - Check function dependencies (wrappers need helpers)

4. **Milestone images not displaying**
   - Verify files exist in `www/milestones/`
   - Check filename matches competency code (lowercase, e.g., `pc1.png`)
   - Verify Shiny can serve static files from www/

### Debugging Tips

1. **Check data structure**:
   ```r
   # In console during development
   str(rdm_data)
   names(rdm_data$all_forms)
   View(rdm_data$residents)
   ```

2. **Test wrapper functions**:
   ```r
   # Load data manually
   rdm_data <- load_ccc_data()

   # Test wrapper
   result <- get_resident_list(rdm_data)
   View(result)
   ```

3. **Verify period calculations**:
   ```r
   # Check current CCC period
   get_current_ccc_period()

   # Check resident periods
   rdm_data$residents %>%
     select(full_name, current_period, current_period_num) %>%
     View()
   ```

## gmed Package Integration

This application depends heavily on the `gmed` package (GitHub: fbuckhold3/gmed) for:

1. **Data loading**: `gmed::load_rdm_complete()`
   - Loads all REDCap forms
   - Handles raw vs. label format
   - Returns standardized structure

2. **Period calculation**: `gmed::calculate_pgy_and_period()`
   - Calculates current PGY level and period
   - Based on grad_yr and resident type
   - Validates periods

3. **Milestone workflow**: `gmed::create_milestone_workflow_from_dict()`
   - Creates competency structure from data dictionary
   - Maps field names to competencies

**When updating gmed dependency**:
1. Test data loading: `load_ccc_data()` should complete without errors
2. Verify period calculations: Check `residents$current_period`
3. Check milestone structure: Verify competency mappings

## Security and Privacy

### Sensitive Data Handling

1. **Never commit credentials**:
   - .Renviron is in .gitignore
   - Tokens stored as environment variables
   - No hardcoded passwords/tokens

2. **REDCap data is PHI** (Protected Health Information):
   - Only load data when necessary
   - Don't log sensitive data
   - Follow SLU data handling policies

3. **Environment variables required**:
   - Development: `.Renviron` file
   - Production: Posit Connect environment configuration

### Access Control

Access control is handled at the Posit Connect level, not within the app. The app assumes:
- User has appropriate SLU permissions
- REDCap tokens have correct access rights
- User understands PHI handling requirements

## Best Practices for AI Assistants

### When Asked to Modify Code

1. **Always read relevant files first**:
   - Understand current implementation
   - Check for existing patterns
   - Verify file structure

2. **Follow the architecture**:
   - Data access → wrappers.R
   - Pure functions → helpers.R
   - Reactive logic → server.R
   - UI components → ui.R

3. **Maintain consistency**:
   - Follow existing naming conventions
   - Use same code style
   - Match documentation patterns

4. **Test your changes**:
   - Verify layer separation
   - Check for direct data access
   - Ensure functions are pure where required

### When Asked to Add Features

1. **Understand the request fully**:
   - What data is needed?
   - Which period(s) are relevant?
   - Where should it appear in the UI?

2. **Plan the implementation**:
   - What wrappers are needed?
   - What helpers might be useful?
   - How will the UI be updated?

3. **Implement in order**:
   - helpers.R (if needed)
   - wrappers.R (data access)
   - ui.R (UI components)
   - server.R (reactive logic)

4. **Verify architecture compliance**:
   - No data access in server.R
   - Pure functions in helpers.R
   - Wrappers return clean data

### When Asked to Debug

1. **Understand the error**:
   - Read error message carefully
   - Identify which layer has the issue
   - Check function dependencies

2. **Verify data flow**:
   - Is data loading correctly?
   - Are periods calculated?
   - Are wrappers returning expected data?

3. **Check common issues**:
   - Missing environment variables
   - Incorrect period names
   - Wrong form names
   - Source order problems

4. **Provide clear explanations**:
   - What was wrong
   - Why it occurred
   - How the fix works

## Quick Reference

### File Locations
- Configuration: `R/global.R`
- Pure functions: `R/helpers.R`
- Data access: `R/wrappers.R`
- UI: `R/ui.R`
- Server logic: `R/server.R`
- Styling: `www/custom.css`
- Images: `www/milestones/*.png`

### Key Functions
- Load data: `load_ccc_data()`
- Get period: `get_current_ccc_period()`
- Get residents: `get_resident_list(rdm_data)`
- Get reviews: `get_ccc_review_table(rdm_data, period)`
- Get milestones: `get_resident_milestones(rdm_data, record_id, period)`

### Important Constants
- `PERIOD_NAMES`: Array of period names
- `PERIOD_TO_NUMBER`: Map names to numbers
- `NUMBER_TO_PERIOD`: Map numbers to names
- `REDCAP_CONFIG`: API configuration

### Common Patterns
- Period filtering: Use `get_form_data_for_period()`
- Data access: Call wrappers from server.R
- Reactive state: Use `reactiveVal()` or `reactive()`
- Tables: Use `DT::datatable()` with formatStyle

---

# REDCap Reference Guide for R Development

This section provides detailed reference for working with REDCap in R, including data dictionary handling, repeating instruments, and API integration.

## Table of Contents
1. [Data Dictionary Column Names](#data-dictionary-column-names)
2. [Repeating Instruments](#repeating-instruments)
3. [Field Types and Values](#field-types-and-values)
4. [Common Patterns](#common-patterns-1)
5. [API Endpoints](#api-endpoints)
6. [Best Practices](#best-practices-1)

---

## Data Dictionary Column Names

REDCap data dictionaries have **different column names** depending on how they're retrieved.

### API Export (metadata endpoint)
When fetching via REDCap API using metadata export:

| Standard Name | API Column Name |
|--------------|-----------------|
| Field Name | `field_name` |
| Form Name | `form_name` |
| Section Header | `section_header` |
| Field Type | `field_type` |
| Field Label | `field_label` |
| Choices/Calculations | `select_choices_or_calculations` |
| Field Note | `field_note` |
| Text Validation | `text_validation_type_or_show_slider_number` |
| Min Value | `text_validation_min` |
| Max Value | `text_validation_max` |
| Identifier | `identifier` |
| Branching Logic | `branching_logic` |
| Required Field | `required_field` |
| Custom Alignment | `custom_alignment` |
| Question Number | `question_number` |
| Matrix Group | `matrix_group_name` |
| Matrix Ranking | `matrix_ranking` |
| Field Annotation | `field_annotation` |

### CSV Export
When downloading the data dictionary as CSV from REDCap UI:

| Standard Name | CSV Column Name |
|--------------|-----------------|
| Field Name | `Variable...Field.Name` or `Variable / Field Name` |
| Form Name | `Form.Name` or `Form Name` |
| Field Label | `Field.Label` or `Field Label` |
| Choices/Calculations | `Choices..Calculations..OR.Slider.Labels` or `Choices, Calculations, OR Slider Labels` |

**Note:** CSV exports use dots (`.`) or spaces where API uses underscores (`_`)

### Standardization Function

Use this function to handle both formats:

```r
standardize_dict_names <- function(dict) {
  if (is.null(dict)) return(NULL)

  name_mapping <- list(
    field_name = c("field_name", "Variable / Field Name", "Variable...Field.Name"),
    form_name = c("form_name", "Form Name", "Form.Name"),
    field_label = c("field_label", "Field Label", "Field.Label"),
    field_type = c("field_type", "Field Type", "Field.Type"),
    choices = c("select_choices_or_calculations",
                "Choices, Calculations, OR Slider Labels",
                "Choices..Calculations..OR.Slider.Labels"),
    branching_logic = c("branching_logic", "Branching Logic", "Branching.Logic")
  )

  for (standard_name in names(name_mapping)) {
    possible_names <- name_mapping[[standard_name]]
    for (poss_name in possible_names) {
      if (poss_name %in% names(dict)) {
        names(dict)[names(dict) == poss_name] <- standard_name
        break
      }
    }
  }

  return(dict)
}
```

---

## Repeating Instruments

### Key Fields in Data Export

When exporting data with repeating instruments, REDCap adds these fields:

- `redcap_repeat_instrument` - Name of the repeating form (empty for non-repeating)
- `redcap_repeat_instance` - Instance number (1, 2, 3, etc.)

### Two Patterns for Repeating Data

#### Pattern 1: OVERWRITE (Fixed Instances)
Used for forms tied to specific periods (e.g., evaluations by semester)

**Characteristics:**
- Instance number = period or time point
- Each instance represents a specific evaluation period
- Updates overwrite existing data for that instance
- Examples: coaching reviews, CCC reviews, self-evaluations

**Code Pattern:**
```r
# Get instance number based on period/level
instance <- get_redcap_instance(
  level = resident_level,
  period = evaluation_period,
  review_type = "scheduled"
)

# Submit (overwrites if exists)
submit_overwrite_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = data,
  instrument_name = "coach_rev",
  period = period,
  level = level
)
```

#### Pattern 2: ADDITIVE (Continuous Instances)
Used for forms that continuously add new entries

**Characteristics:**
- Each submission creates a new instance
- Instance numbers increment automatically
- Never overwrites existing data
- Examples: scholarship, faculty evaluations, presentations

**Code Pattern:**
```r
# Get next available instance
next_instance <- get_next_additive_instance(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  instrument_name = "scholarship"
)

# Submit new instance
submit_additive_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = data,
  instrument_name = "scholarship"
)
```

### Getting Next Instance Number

```r
get_next_additive_instance <- function(redcap_url, redcap_token, record_id, instrument_name) {
  # Fetch existing instances
  result <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = instrument_name
    ),
    encode = "form"
  )

  data <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"))

  if (nrow(data) == 0) return(1)

  # Filter to this instrument only
  data <- data[data$redcap_repeat_instrument == instrument_name, ]

  if (nrow(data) == 0) return(1)

  # Get max instance and add 1
  max_instance <- max(as.numeric(data$redcap_repeat_instance), na.rm = TRUE)
  return(max_instance + 1)
}
```

---

## Field Types and Values

### Dropdown/Radio Fields

**Format in data dictionary:**
```
"1, Label One | 2, Label Two | 3, Label Three"
```

**Parsing function:**
```r
parse_choices <- function(choices_raw) {
  if (is.na(choices_raw) || choices_raw == "") return(NULL)

  choices <- strsplit(choices_raw, "\\|")[[1]]
  lookup <- list()

  for (choice in choices) {
    parts <- trimws(strsplit(choice, ",")[[1]])
    if (length(parts) >= 2) {
      value <- parts[1]
      label <- paste(parts[-1], collapse = ",")
      lookup[[value]] <- label
    }
  }

  return(lookup)
}
```

### Yes/No Fields

**Standard encoding:**
- `"1"` = Yes
- `"0"` = No

**Usage:**
```r
# In data
schol_ps = "1"  # Patient safety = Yes

# In UI
radioButtons("field", "Question?", choices = c("Yes" = "1", "No" = "0"))
```

### Checkbox Fields

**Encoding:**
- Checked = `"1"`
- Unchecked = `"0"` or `NA`

**In REDCap data:**
```r
# Each checkbox option becomes a separate column
field___1  # Option 1
field___2  # Option 2
field___3  # Option 3
```

### Date Fields

**Format:** `YYYY-MM-DD` (ISO 8601)

**Usage:**
```r
# Store in REDCap
date_field = format(Sys.Date(), "%Y-%m-%d")

# Date ranges (for data dictionary)
date:field:start
date:field:end
date:field:is_datetime  # 0 or 1
```

### Text/Notes Fields

**Types:**
- `text` - Single line
- `notes` - Multi-line (textarea)

**Validation types:**
- `email`
- `number`
- `integer`
- `phone`
- `date_ymd`
- `datetime_ymd`

---

## Common Patterns (REDCap-Specific)

### 1. Conditional Logic Based on Type

Many forms have different fields based on a "type" selector:

```r
# Example: Scholarship entry
if (type == "1") {
  # Show QI-specific fields
} else if (type == "3") {
  # Show research-specific fields
}
```

**Implementation with conditionalPanel:**
```r
conditionalPanel(
  condition = paste0("input['", ns("type_field"), "'] == '1'"),
  # Fields for type 1
)
```

### 2. Multi-Step Workflows

For complex data entry:

```r
state <- reactiveValues(
  step = "select_type",
  data = list()
)

# Step progression
observeEvent(input$continue, {
  if (state$step == "step_1") {
    # Submit step 1 data
    # Move to step 2
    state$step <- "step_2"
  }
})
```

### 3. Dynamic Field Display

Pull field labels from data dictionary:

```r
get_label <- function(field_name, data_dict) {
  dict_row <- data_dict[data_dict$field_name == field_name, ]
  if (nrow(dict_row) == 0) return(field_name)
  return(dict_row$field_label[1])
}
```

### 4. Vectorized Choice Labels

Convert coded values to labels for display:

```r
get_choice_label <- function(field_name, values, data_dict) {
  dict_row <- data_dict[data_dict$field_name == field_name, ]
  if (nrow(dict_row) == 0) return(values)

  choices_raw <- dict_row$choices[1]
  if (is.na(choices_raw)) return(values)

  # Parse choices
  lookup <- parse_choices(choices_raw)

  # Vectorized lookup
  sapply(values, function(val) {
    if (is.na(val)) return(NA_character_)
    label <- lookup[[as.character(val)]]
    if (is.null(label)) return(as.character(val))
    return(label)
  })
}
```

---

## API Endpoints

### Export Records
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "record",
    action = "export",
    format = "json",
    type = "flat",
    records = record_id,        # Optional: specific records
    forms = "form_name",        # Optional: specific forms
    fields = "field1,field2",   # Optional: specific fields
    rawOrLabel = "raw",         # "raw" for codes, "label" for labels
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "false",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Import Records
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "record",
    action = "import",
    format = "json",
    type = "flat",
    overwriteBehavior = "normal",  # or "overwrite"
    data = jsonlite::toJSON(data_df, auto_unbox = TRUE),
    returnContent = "count",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Export Metadata (Data Dictionary)
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "metadata",
    format = "json",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Export Project Information
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "project",
    format = "json",
    returnFormat = "json"
  ),
  encode = "form"
)
```

---

## Best Practices (REDCap-Specific)

### 1. Always Use Proper Field Names
```r
# DON'T hardcode labels
data$`Field Label` <- value

# DO use field names
data$field_name <- value
```

### 2. Handle NA Values
```r
# REDCap uses empty strings, not NA
scholarship_data <- list(
  field1 = ifelse(is.null(input$field1) || input$field1 == "", NA, input$field1)
)
```

### 3. Validate Before Submission
```r
# Check required fields
if (is.na(data$required_field) || data$required_field == "") {
  # Show error
  return()
}
```

### 4. Use Transactions for Related Data
```r
# Submit base data first
result1 <- submit_data(base_data)

if (result1$success) {
  # Then submit related data
  result2 <- submit_data(related_data)
}
```

### 5. Provide User Feedback
```r
# After submission
if (result$success) {
  # Refresh display immediately
  refresh_callback()

  # Show success message
  showNotification("Data saved successfully!", type = "message")
}
```

### 6. Error Handling
```r
tryCatch({
  result <- httr::POST(url, body = body, encode = "form")

  if (httr::status_code(result) == 200) {
    # Success
  } else {
    # Handle error
    error_text <- httr::content(result, "text", encoding = "UTF-8")
    warning("REDCap error: ", error_text)
  }
}, error = function(e) {
  warning("API call failed: ", e$message)
})
```

### 7. Data Dictionary Caching
```r
# Cache data dictionary at app startup
app_data <- list(
  data_dict = fetch_metadata(token, url),
  # ... other data
)

# Reuse throughout the app
get_field_label(field_name, app_data$data_dict)
```

### 8. Separate Display and Submission Logic
```r
# Display module (gmed package)
display_scholarship <- function(data, data_dict) {
  # Transform data for display
}

# Submission module (app-specific)
scholarship_entry_server <- function(id, ...) {
  # Handle form submission
}
```

### 9. Use Namespacing in Modules
```r
# In module UI
ns <- NS(id)
textInput(ns("field_name"), "Label")

# In module server
moduleServer(id, function(input, output, session) {
  # input$field_name is automatically namespaced
})
```

### 10. Test with Empty Data
```r
# Always handle empty data frames
if (is.null(data) || nrow(data) == 0) {
  return(empty_result)
}
```

---

## Troubleshooting Common REDCap Issues

### Issue: "attempt to use zero-length variable name"
**Cause:** Using `sprintf()` with `ns()` in conditionalPanel
**Fix:** Use `paste0()` instead
```r
# BAD
condition = sprintf("input['%s'] == '1'", ns("field"))

# GOOD
condition = paste0("input['", ns("field"), "'] == '1'")
```

### Issue: Numeric values instead of labels in display
**Cause:** Not applying choice labels from data dictionary
**Fix:** Use vectorized `get_choice_label()` function

### Issue: Modal buttons not working
**Cause:** Button IDs not properly namespaced
**Fix:** Namespace modal button IDs
```r
actionButton(ns("modal_btn"), "Click")
```

### Issue: Data not refreshing after submission
**Cause:** Not calling refresh callback
**Fix:** Always call refresh after successful submission
```r
if (result$success) {
  if (!is.null(refresh_callback)) refresh_callback()
}
```

### Issue: Repeating instances overwriting each other
**Cause:** Using fixed instance numbers for additive data
**Fix:** Use `get_next_additive_instance()` for new entries

---

## REDCap Resources

- **REDCap API Documentation:** Check your institution's REDCap API page at `/api/help/`
- **gmed Package:** Internal package with REDCap helper functions for SLU GME
- **REDCapR Package:** Alternative R package for REDCap integration (CRAN)
- **REDCap Community:** https://community.projectredcap.org/

---

**Last Updated**: 2025-12-09
**Maintained By**: SLU GME Development Team
**Questions**: Contact repository maintainer or SLU GME team
