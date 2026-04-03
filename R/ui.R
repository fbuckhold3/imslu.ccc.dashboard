# ui.R - UI definition for CCC Dashboard

ui <- gmed::gmed_page(
  title = "CCC Dashboard",
  theme_variant = "slucare",
  base_font = "Inter",
  heading_font = "Inter",

  # Enable shinyjs
  shinyjs::useShinyjs(),

  # Custom CSS and JavaScript
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(HTML("
      function showImage(filename) {
        var modal = document.getElementById('imageModal');
        var modalImg = document.getElementById('modalImage');
        modal.style.display = 'block';
        modalImg.src = filename;
      }

      function closeModal() {
        document.getElementById('imageModal').style.display = 'none';
      }

      // Close modal when clicking outside the image
      window.onclick = function(event) {
        var modal = document.getElementById('imageModal');
        if (event.target == modal) {
          modal.style.display = 'none';
        }
      }
    "))
  ),

  # Image Modal
  tags$div(id = "imageModal", class = "image-modal",
    tags$span(class = "image-modal-close", onclick = "closeModal()", HTML("&times;")),
    tags$img(id = "modalImage", class = "image-modal-content")
  ),

  # Access Code Page
  conditionalPanel(
    condition = "output.authenticated == false",
    tags$div(
      style = "max-width: 500px; margin: 100px auto; padding: 40px;",
      gmed::gmed_card(
        title = "Access Required",
        tags$div(
          style = "text-align: center; padding: 20px;",
          tags$p(
            "Please enter your access code to view the CCC Dashboard",
            style = "margin-bottom: 30px; color: #6c757d;"
          ),
          passwordInput(
            inputId = "access_code",
            label = "Access Code:",
            placeholder = "Enter your access code",
            width = "100%"
          ),
          tags$br(),
          actionButton(
            inputId = "submit_access_code",
            label = "Submit",
            icon = icon("sign-in-alt"),
            class = "btn-primary w-100",
            style = "margin-top: 10px;"
          ),
          tags$br(),
          uiOutput("access_error_message")
        )
      )
    )
  ),

  # Main Dashboard (shown after authentication)
  conditionalPanel(
    condition = "output.authenticated == true",

    # Application Header
    gmed::gmed_app_header(
      title = "Clinical Competency Committee Dashboard",
      subtitle = "SLU Internal Medicine Residency Program"
    ),

  # Main tab panel for three modes
  tabsetPanel(
    id = "main_mode",
    type = "tabs",

    # ===========================================================================
    # MODE 1: CCC SEMI-ANNUAL REVIEW
    # ===========================================================================
    tabPanel(
      title = "CCC Semi-Annual Review",
      value = "ccc_semiannual",

      br(),

      # List View
      conditionalPanel(
        condition = "output.show_resident_list == true",
        fluidRow(
          column(
            width = 3,
            gmed::gmed_card(
              title = "Current Review Period",
              textOutput("current_period_display"),
              tags$hr(style = "margin: 15px 0;"),
              actionButton(
                inputId = "refresh_data",
                label = "Refresh Data",
                icon = icon("sync"),
                class = "btn-primary w-100"
              )
            ),
            tags$br(),
            gmed::gmed_card(
              title = "Review Progress",
              uiOutput("review_stats")
            )
          ),

          column(
            width = 9,
            gmed::gmed_card(
              title = "Filters",
              tags$div(
                tags$strong("Review Status:"),
                tags$br(),
                tags$div(
                  style = "margin: 10px 0;",
                  actionButton("filter_all", "All", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_all_done", "All Done", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_coach_done", "Coach Done", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_coach_second_done", "Coach & Second Done", class = "btn-sm")
                )
              ),
              tags$hr(style = "margin: 15px 0;"),
              tags$div(
                tags$strong("PGY Level:"),
                tags$br(),
                tags$div(
                  style = "margin: 10px 0;",
                  actionButton("filter_pgy_all", "All", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_pgy_intern", "Intern", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_pgy_pgy2", "PGY2", class = "btn-sm", style = "margin-right: 5px;"),
                  actionButton("filter_pgy_pgy3", "PGY3", class = "btn-sm")
                )
              ),
              tags$hr(style = "margin: 15px 0;"),
              fluidRow(
                column(
                  width = 6,
                  tags$strong("Coach:"),
                  tags$br(),
                  uiOutput("filter_coach_buttons")
                ),
                column(
                  width = 6,
                  tags$strong("Second Reviewer:"),
                  tags$br(),
                  uiOutput("filter_second_buttons")
                )
              )
            ),
            tags$br(),
            gmed::gmed_card(
              title = "Residents for Review",
              DT::DTOutput("resident_review_table")
            )
          )
        )
      ),

      # Detail View
      conditionalPanel(
        condition = "output.show_resident_list == false",
        fluidRow(
          column(
            width = 12,
            actionButton(
              inputId = "back_to_list",
              label = "← Back to List",
              icon = icon("arrow-left"),
              class = "btn-secondary"
            ),
            hr(),
            uiOutput("resident_detail_panel")
          )
        )
      )
    ),

    # ===========================================================================
    # MODE 2: AD HOC DISCUSSION
    # ===========================================================================
    tabPanel(
      title = "Ad Hoc Discussion",
      value = "ad_hoc",

      br(),

      fluidRow(
        column(
          width = 3,
          gmed::gmed_card(
            title = "Select Resident",
            selectizeInput(
              inputId = "adhoc_resident",
              label = "Resident:",
              choices = NULL,  # Populated in server
              selected = NULL,
              options = list(
                placeholder = "Type to search...",
                maxOptions = 100
              )
            )
          )
        ),

        column(
          width = 9,
          uiOutput("adhoc_review_panel")
        )
      )
    ),

    # ===========================================================================
    # MODE 3: MILESTONE ANALYSIS
    # ===========================================================================
    tabPanel(
      title = "Milestone Analysis",
      value = "milestone_analysis",

      br(),

      tabsetPanel(
        id   = "ms_subtabs",
        type = "tabs",

        # -------------------------------------------------------------------
        # MILESTONE SUB-TAB 1: PROGRAM TRENDS
        # -------------------------------------------------------------------
        tabPanel(
          title = "Program Trends",
          value = "ms_program",
          br(),

          fluidRow(
            # Left — filters
            column(width = 3,
              gmed::gmed_card(
                title = "Filters",
                selectInput("ms_category", "Competency Category:",
                  choices  = c("All", "PC", "MK", "SBP", "PBL", "PROF", "ICS"),
                  selected = "All", width = "100%"),
                selectInput("ms_specific", "Specific Subcompetency:",
                  choices  = c("All (Average)" = "__all__"),
                  selected = "__all__", width = "100%"),
                tags$hr(),
                checkboxInput("ms_show_cohorts",
                  "Color boxes by graduation year", value = FALSE),
                checkboxInput("ms_show_points",
                  "Show individual data points",     value = FALSE),
                tags$hr(),
                tags$p(class = "text-muted",
                  style = "font-size:0.88rem; line-height:1.6;",
                  icon("circle-info"),
                  " Includes active and historical (archived) residents. ",
                  "Box = 25th–75th percentile; line = median. ",
                  "Dashed red line = Level 4 target."
                )
              )
            ),

            # Right — box plot
            column(width = 9,
              gmed::gmed_card(
                title = "Milestone Score Distribution by Review Period",
                uiOutput("ms_loading_indicator"),
                plotly::plotlyOutput("ms_program_plot", height = "500px")
              )
            )
          )
        ),

        # -------------------------------------------------------------------
        # MILESTONE SUB-TAB 2: INDIVIDUAL RESIDENT
        # -------------------------------------------------------------------
        tabPanel(
          title = "Individual Resident",
          value = "ms_individual",
          br(),

          fluidRow(
            # Left — controls
            column(width = 3,
              gmed::gmed_card(
                title = "Select Resident",
                selectizeInput("ms_resident", "Resident:",
                  choices = NULL,
                  options = list(
                    placeholder = "Type to search...",
                    maxOptions  = 300
                  )
                ),
                selectInput("ms_ind_category", "Competency Category:",
                  choices  = c("All", "PC", "MK", "SBP", "PBL", "PROF", "ICS"),
                  selected = "All", width = "100%"),
                tags$hr(),
                tags$p(class = "text-muted",
                  style = "font-size:0.88rem; line-height:1.6;",
                  icon("circle-info"),
                  " Each colored line = one subcompetency (e.g. PC1, PC2…). ",
                  "Grey ribbon + dotted line = program 25th–75th %ile and median ",
                  "for the selected category. Dashed red = Level 4 target."
                )
              )
            ),

            # Right — trajectory plot
            column(width = 9,
              gmed::gmed_card(
                title = "Individual Subcompetency Trajectories vs. Program",
                uiOutput("ms_ind_loading_indicator"),
                plotly::plotlyOutput("ms_individual_plot", height = "520px")
              )
            )
          )
        )
      )
    ),

    # ===========================================================================
    # MODE 4: FOLLOW-UP TRACKER
    # ===========================================================================
    tabPanel(
      title = "Follow-up Tracker",
      value = "followup_tracker",

      br(),

      tabsetPanel(
        id = "tracker_subtabs",
        type = "tabs",

        # -----------------------------------------------------------------------
        # TRACKER SUB-TAB 1: ACTION ITEMS
        # -----------------------------------------------------------------------
        tabPanel(
          title = "Action Items",
          value = "tracker_action_items",
          br(),

          # --- Value boxes (one per status type) ---
          fluidRow(
            column(width = 2,
              div(class = "card text-white mb-3",
                  style = "background-color: #6f42c1;",
                div(class = "card-body p-3",
                  div(class = "small text-white-50", "Interim Reviews"),
                  div(class = "fs-4 fw-bold", textOutput("tracker_vb_interim", inline = TRUE))
                )
              )
            ),
            column(width = 2,
              div(class = "card text-white mb-3",
                  style = "background-color: #fd7e14;",
                div(class = "card-body p-3",
                  div(class = "small text-white-50", "Initiation"),
                  div(class = "fs-4 fw-bold", textOutput("tracker_vb_initiation", inline = TRUE))
                )
              )
            ),
            column(width = 2,
              div(class = "card text-white mb-3",
                  style = "background-color: #0dcaf0;",
                div(class = "card-body p-3",
                  div(class = "small", style = "color: rgba(0,0,0,.5);", "Ongoing"),
                  div(class = "fs-4 fw-bold text-dark", textOutput("tracker_vb_ongoing", inline = TRUE))
                )
              )
            ),
            column(width = 2,
              div(class = "card text-white mb-3",
                  style = "background-color: #198754;",
                div(class = "card-body p-3",
                  div(class = "small text-white-50", "Resolved"),
                  div(class = "fs-4 fw-bold", textOutput("tracker_vb_resolved", inline = TRUE))
                )
              )
            ),
            column(width = 2,
              div(class = "card text-white mb-3",
                  style = "background-color: #dc3545;",
                div(class = "card-body p-3",
                  div(class = "small text-white-50", "Recurring"),
                  div(class = "fs-4 fw-bold", textOutput("tracker_vb_recurring", inline = TRUE))
                )
              )
            )
          ),

          # --- Filter buttons ---
          gmed::gmed_card(
            title = "Filters",
            tags$div(
              style = "margin: 5px 0;",
              actionButton("tracker_filter_all",        "All",          class = "btn-sm btn-outline-secondary", style = "margin-right:5px;"),
              actionButton("tracker_filter_interim",    "Interim",      class = "btn-sm btn-outline-secondary", style = "margin-right:5px;"),
              actionButton("tracker_filter_concern",    "Has Concern",  class = "btn-sm btn-outline-secondary", style = "margin-right:5px;"),
              actionButton("tracker_filter_initiation", "Initiation",   class = "btn-sm btn-outline-warning",   style = "margin-right:5px;"),
              actionButton("tracker_filter_ongoing",    "Ongoing",      class = "btn-sm btn-outline-info",      style = "margin-right:5px;"),
              actionButton("tracker_filter_resolved",   "Resolved",     class = "btn-sm btn-outline-success",   style = "margin-right:5px;"),
              actionButton("tracker_filter_recurring",  "Recurring",    class = "btn-sm btn-outline-danger")
            )
          ),

          br(),

          # --- Main action items table ---
          gmed::gmed_card(
            title = "Action Items — click a row to update or add an issue",
            tags$p(class = "text-muted", style = "font-size:0.95em;",
                   "One row per resident. Click a row to open the update/add-issue dialog."),
            DT::DTOutput("tracker_action_table")
          )
        ),

        # -----------------------------------------------------------------------
        # TRACKER SUB-TAB 2: CCC REVIEW
        # -----------------------------------------------------------------------
        tabPanel(
          title = "CCC Review",
          value = "tracker_ccc_review",
          br(),

          gmed::gmed_card(
            title = "Filters",
            fluidRow(
              column(width = 3,
                selectInput("tracker_rev_type_filter", "Type:",
                            choices = c("All", "Scheduled", "Interim"),
                            selected = "All", width = "100%")
              ),
              column(width = 3,
                selectInput("tracker_session_filter", "Session:",
                            choices = c("All"),   # populated server-side
                            selected = "All", width = "100%")
              )
            )
          ),

          br(),

          gmed::gmed_card(
            title = "All CCC Review Records",
            tags$p(class = "text-muted", style = "font-size:0.95em;",
                   "Purple rows = Interim; yellow rows = Concern flagged."),
            DT::DTOutput("tracker_ccc_review_table")
          )
        )
      )
    )

  )
  )  # Close conditionalPanel for authenticated content
)
