# SLU Internal Medicine CCC Dashboard

A Shiny application for Saint Louis University Internal Medicine Clinical Competency Committee (CCC) reviews, integrated with the gmed package for REDCap data access.

## Features

- View all residents for CCC review by period
- Track review completion status
- Display three types of milestone evaluations per resident:
  - ACGME milestones (previous period)
  - Program milestones (current period)
  - Self-evaluation milestones (current period)
- CCC review form for committee decisions

## Project Structure

```
imslu.ccc.dashboard/
├── .Renviron.example        # Template for environment variables
├── .gitignore
├── README.md
├── app.R                    # Single-file app entry point
├── manifest.json            # Posit Connect deployment config
├── R/
│   ├── global.R            # Data loading via gmed
│   ├── helpers.R           # Pure functions (period calc, mapping)
│   ├── wrappers.R          # Data access wrappers
│   ├── ui.R                # UI definition
│   └── server.R            # Server logic
└── www/
    └── custom.css          # SLU-themed styling
```

## Architecture Principles

### 1. Clean Data Flow

```
gmed::load_rdm_complete()
  → global.R (minimal processing)
  → wrappers.R (all filtering/joining)
  → server.R (reactivity only)
```

### 2. Data Access via Wrappers

All data access goes through wrapper functions in `R/wrappers.R`. The server NEVER accesses data directly.

Key wrapper functions:
- `get_resident_list()` - Get all resident names
- `get_resident_record_id()` - Convert name to record_id
- `get_resident_program_milestones()` - Program milestone data
- `get_resident_self_milestones()` - Self-evaluation data
- `get_resident_acgme_milestones()` - ACGME data (previous period)
- `check_ccc_completion()` - Check if CCC review is complete
- `get_ccc_review_table()` - Build review table with all residents

### 3. Pure Helper Functions

`R/helpers.R` contains pure functions with no data dependencies:
- `calculate_academic_year()` - Get current academic year
- `calculate_current_period()` - Get current evaluation period (1-6)
- `get_period_name()` - Map period number to display name

## Setup

### 1. Install Dependencies

```r
install.packages(c("shiny", "dplyr", "DT"))

# Install gmed from GitHub
remotes::install_github("fbuckhold3/gmed")
```

### 2. Configure Environment Variables

Copy `.Renviron.example` to `.Renviron` and add your tokens:

```
ACCESS_CODE=your_access_code_here
RDM_TOKEN=your_rdm_token_here
EVAL_TOKEN=your_eval_token_here
FAC_TOKEN=your_fac_token_here
```

### 3. Run Locally

```r
# Open app.R in RStudio and click "Run App"
# Or from console:
shiny::runApp()
```

## Deployment

### Posit Connect

The app includes a `manifest.json` for deployment to Posit Connect:

```r
# Deploy using rsconnect
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "manifest.json", "R/", "www/"),
  appTitle = "SLU CCC Dashboard"
)
```

## Evaluation Periods

The app uses 6 evaluation periods per academic year:

- **Period 1**: July - August
- **Period 2**: September - October
- **Period 3**: November - December
- **Period 4**: January - February
- **Period 5**: March - April
- **Period 6**: May - June

Academic years run from July 1 to June 30.

## Development

### Adding New Features

1. **New data access** → Add wrapper function to `R/wrappers.R`
2. **New calculations** → Add pure function to `R/helpers.R`
3. **New UI elements** → Update `R/ui.R`
4. **New server logic** → Update `R/server.R` (use wrappers only!)

### Code Style

- Never access `data$table_name` directly in server.R
- All data filtering/joining happens in wrappers.R
- Keep helpers.R pure (no data dependencies)
- Use meaningful function names
- Document functions with roxygen-style comments

## License

Internal use only - Saint Louis University Internal Medicine

## Contact

For issues or questions, contact the GME development team.
