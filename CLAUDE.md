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

**Last Updated**: 2025-12-09
**Maintained By**: SLU GME Development Team
**Questions**: Contact repository maintainer or SLU GME team
