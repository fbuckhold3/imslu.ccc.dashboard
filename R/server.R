# server.R - Server logic for CCC Dashboard
# Uses ONLY wrapper functions - never accesses data directly
#
# Note: This function expects rdm_data to be passed as a parameter

# ── Section UI helpers (called from main_view renderUI) ────────────────────────

semiannual_section_ui <- function() {
  tagList(
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
            actionButton("refresh_data", "Refresh Data",
                         icon = icon("sync"), class = "btn-primary w-100")
          ),
          tags$br(),
          gmed::gmed_card(title = "Review Progress", uiOutput("review_stats"))
        ),
        column(
          width = 9,
          gmed::gmed_card(
            title = "Filters",
            tags$div(
              tags$strong("Review Status:"),
              tags$br(),
              uiOutput("filter_status_buttons")
            ),
            tags$hr(style = "margin: 15px 0;"),
            tags$div(
              tags$strong("PGY Level:"),
              tags$br(),
              uiOutput("filter_pgy_buttons")
            ),
            tags$hr(style = "margin: 15px 0;"),
            fluidRow(
              column(width = 6, tags$strong("Coach:"),           tags$br(), uiOutput("filter_coach_buttons")),
              column(width = 6, tags$strong("Second Reviewer:"), tags$br(), uiOutput("filter_second_buttons"))
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
          actionButton("back_to_list", "\u2190 Back to List",
                       icon = icon("arrow-left"), class = "btn-secondary"),
          hr(),
          uiOutput("resident_detail_panel")
        )
      )
    )
  )
}

interim_section_ui <- function() {
  tagList(
    tags$style(HTML("
      /* ── Table wrapper ── */
      #tracker_ccc_review_table .dataTables_wrapper {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
      }

      /* ── Base table ── */
      #tracker_ccc_review_table table.dataTable {
        font-size: 1.0rem !important;
        border-collapse: collapse !important;
        width: 100% !important;
      }

      /* ── Header ── */
      #tracker_ccc_review_table table.dataTable thead th {
        background: linear-gradient(135deg, #003d5c 0%, #0066a1 100%) !important;
        color: #ffffff !important;
        font-size: 0.85rem !important;
        font-weight: 700 !important;
        letter-spacing: 0.06em !important;
        text-transform: uppercase !important;
        padding: 14px 18px !important;
        border: none !important;
        white-space: nowrap;
      }
      #tracker_ccc_review_table table.dataTable thead th:first-child { border-radius: 0; }

      /* ── Body cells ── */
      #tracker_ccc_review_table table.dataTable tbody td {
        padding: 13px 18px !important;
        vertical-align: middle !important;
        border-bottom: 1px solid #e8eef5 !important;
        border-right: none !important;
        font-size: 0.95rem !important;
        line-height: 1.5;
        color: #2d3748;
      }

      /* ── Zebra rows ── */
      #tracker_ccc_review_table table.dataTable tbody tr:nth-child(odd)  td { background-color: #ffffff !important; }
      #tracker_ccc_review_table table.dataTable tbody tr:nth-child(even) td { background-color: #f4f8fd !important; }

      /* ── Hover ── */
      #tracker_ccc_review_table table.dataTable tbody tr:hover td {
        background-color: #dbeafe !important;
        cursor: pointer;
        transition: background-color 0.15s ease;
      }

      /* ── Selected ── */
      #tracker_ccc_review_table table.dataTable tbody tr.selected td {
        background-color: #bfdbfe !important;
      }

      /* ── Search / length controls ── */
      #tracker_ccc_review_table .dataTables_filter input,
      #tracker_ccc_review_table .dataTables_length select {
        border: 1.5px solid #cbd5e0;
        border-radius: 6px;
        padding: 6px 10px;
        font-size: 0.9rem;
      }
      #tracker_ccc_review_table .dataTables_filter label,
      #tracker_ccc_review_table .dataTables_length label,
      #tracker_ccc_review_table .dataTables_info {
        font-size: 0.88rem;
        color: #4a5568;
      }
    ")),

    gmed::gmed_card(
      title = "CCC Review Records",
      tags$p(
        style = "font-size:0.9rem; color:#6c757d; margin-bottom:14px;",
        tags$i(class = "bi bi-cursor-fill me-1", style = "color:#0066a1;"),
        "Click a resident to log a new interim review."
      ),
      DT::DTOutput("tracker_ccc_review_table")
    )
  )
}

milestones_section_ui <- function() {
  tabsetPanel(
    id   = "ms_subtabs",
    type = "tabs",

    tabPanel(
      title = "Program Trends",
      value = "ms_program",
      br(),
      fluidRow(
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
            checkboxInput("ms_show_cohorts", "Color boxes by graduation year", value = FALSE),
            checkboxInput("ms_show_points",  "Show individual data points",     value = FALSE),
            tags$hr(),
            tags$p(class = "text-muted", style = "font-size:0.88rem; line-height:1.6;",
              icon("circle-info"),
              " Includes active and historical (archived) residents. ",
              "Box = 25th\u201375th percentile; line = median. ",
              "Dashed red line = Level 4 target.")
          )
        ),
        column(width = 9,
          gmed::gmed_card(
            title = "Milestone Score Distribution by Review Period",
            uiOutput("ms_loading_indicator"),
            plotly::plotlyOutput("ms_program_plot", height = "500px")
          )
        )
      )
    ),

    tabPanel(
      title = "Individual Resident",
      value = "ms_individual",
      br(),
      fluidRow(
        column(width = 3,
          gmed::gmed_card(
            title = "Select Resident",
            selectizeInput("ms_resident", "Resident:", choices = NULL,
              options = list(placeholder = "Type to search...", maxOptions = 300)),
            selectInput("ms_ind_category", "Competency Category:",
              choices  = c("All", "PC", "MK", "SBP", "PBL", "PROF", "ICS"),
              selected = "All", width = "100%"),
            tags$hr(),
            tags$p(class = "text-muted", style = "font-size:0.88rem; line-height:1.6;",
              icon("circle-info"),
              " Each colored line = one subcompetency (e.g. PC1, PC2\u2026). ",
              "Grey ribbon + dotted line = program 25th\u201375th %ile and median ",
              "for the selected category. Dashed red = Level 4 target.")
          )
        ),
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
}

# ──────────────────────────────────────────────────────────────────────────────

create_server <- function(initial_data, server_state = NULL) {
  function(input, output, session) {

    # Log session lifecycle for disconnect diagnosis
    session$onSessionEnded(function() {
      message("SESSION_ENDED for session ", session$token)
    })

    # Reactive values
    app_data <- reactiveVal(initial_data)
    selected_resident_id <- reactiveVal(NULL)
    show_list_view <- reactiveVal(TRUE)
    nav_state  <- reactiveValues(current = "login")
    auth_error <- reactiveVal(NULL)
    filtered_review_table <- reactiveVal(NULL)

    # Ad hoc milestone modal data
    adhoc_prog_miles  <- reactiveVal(NULL)
    adhoc_self_miles  <- reactiveVal(NULL)
    adhoc_acgme_miles <- reactiveVal(NULL)
    adhoc_radar_type  <- reactiveVal(NULL)

    # Filter reactive values
    filter_completion <- reactiveVal("all")
    filter_pgy <- reactiveVal("all")
    filter_coach <- reactiveVal("all")
    filter_second <- reactiveVal("all")

    # Single active toggle for exclusive tab-like behavior in detail panel
    # Values: "eval" | "pd" | "board" | "mile" | NULL (all closed)
    active_toggle <- reactiveVal(NULL)

    # ── Existing CCC review data for the selected resident + current period ──
    # Used to pre-populate form fields when a resident already has a draft.
    selected_ccc_data <- reactive({
      rid <- selected_resident_id()
      if (is.null(rid)) return(data.frame())
      resident_info <- app_data()$residents %>%
        filter(record_id == rid) %>%
        slice(1)
      if (nrow(resident_info) == 0) return(data.frame())
      tryCatch(
        get_ccc_review_data(app_data(), rid, resident_info$current_period),
        error = function(e) data.frame()
      )
    })

    # ── Phase 2 polling ─────────────────────────────────────────────────────
    # Phase 2 runs ONCE at server startup (app.R), shared across all sessions.
    # server_state is an explicit env passed from app.R — no lexical scoping
    # issues across source() boundaries on Connect.
    if (!is.null(server_state)) {
      observe({
        invalidateLater(3000)
        if (isTRUE(server_state$load_complete) && !isTRUE(app_data()$full_load_complete)) {
          app_data(server_state$full_data)
          message("[Phase 2] Session ", session$token, " received full data")
        }
      })
    }

  # ===========================================================================
  # GMED MODULE SETUP (wired once; data reactives update on resident change)
  # assessment form is available after Phase 2 load; gracefully returns NULL
  # until then so toggles show a "loading" message.
  # ===========================================================================

  # Capture data_dict at startup (static — doesn't change)
  .local_data_dict <- initial_data$data_dict

  # Assessment data (all rows) — modules filter by record_id internally
  .ccc_assessment_data <- reactive({
    d <- app_data()$all_forms$assessment
    if (is.null(d) || nrow(d) == 0) return(data.frame())
    d
  })

  gmed::mod_eval_feedback_server(
    "ccc_ef_mod",
    assessment_data = .ccc_assessment_data,
    record_id       = selected_resident_id,
    data_dict       = reactive({ app_data()$data_dict })
  )

  gmed::mod_plus_delta_table_server(
    "ccc_pd_mod",
    rdm_data  = .ccc_assessment_data,
    record_id = selected_resident_id
  )

  gmed::mod_seval_boards_display_server(
    "ccc_boards_mod",
    current_s_eval = reactive({
      rid <- selected_resident_id()
      if (is.null(rid)) return(NULL)
      res_info <- app_data()$residents %>% filter(record_id == rid) %>% slice(1)
      if (nrow(res_info) == 0) return(NULL)
      get_form_data_for_period(
        app_data()$all_forms, "s_eval", rid, res_info$current_period
      ) %>% slice(1)
    }),
    resident_info = reactive({
      rid <- selected_resident_id()
      if (is.null(rid)) return(NULL)
      app_data()$residents %>% filter(record_id == rid) %>% slice(1)
    }),
    test_data = reactive({
      rid <- selected_resident_id()
      if (is.null(rid)) return(NULL)
      td <- app_data()$all_forms$test_data
      if (is.null(td) || nrow(td) == 0) return(NULL)
      td %>% filter(record_id == rid)
    }),
    pgy = reactive({
      rid <- selected_resident_id()
      if (is.null(rid)) return(NA_integer_)
      res_info <- app_data()$residents %>% filter(record_id == rid) %>% slice(1)
      period <- if (nrow(res_info) > 0) res_info$current_period else NA_character_
      if (is.na(period)) return(NA_integer_)
      if (grepl("Intern",              period)) 1L
      else if (grepl("PGY2",           period)) 2L
      else if (grepl("PGY3|Graduating",period)) 3L
      else NA_integer_
    }),
    data_dict = reactive({ app_data()$data_dict })
  )

  # ===========================================================================
  # AUTHENTICATION
  # ===========================================================================

  observeEvent(input$submit_access_code, {
    expected_code <- Sys.getenv("CCC_ACCESS_CODE")

    if (expected_code == "") {
      auth_error("Access code not configured. Please set CCC_ACCESS_CODE.")
      return()
    }

    if (!is.null(input$access_code) && input$access_code == expected_code) {
      nav_state$current <- "home"
      auth_error(NULL)
    } else {
      auth_error("Invalid access code. Please try again.")
    }
  })

  # ── Navigation ──────────────────────────────────────────────────────────────

  observeEvent(input$nav_block, {
    req(nav_state$current == "home")
    nav_state$current <- input$nav_block
  })

  observeEvent(input$nav_back, {
    nav_state$current <- "home"
  })

  # ── Main view ───────────────────────────────────────────────────────────────

  output$main_view <- renderUI({
    state <- nav_state$current

    # ── LOGIN ──────────────────────────────────────────────────────────────────
    if (state == "login") {
      return(tagList(

        div(
          style = paste(
            "background: #003d5c; color: white;",
            "padding: 18px 32px;",
            "display: flex; align-items: center; gap: 16px;",
            "margin: -24px -24px 40px -24px;"
          ),
          div(
            style = "background: rgba(255,255,255,0.15); font-size:0.68rem;
                     font-weight:700; letter-spacing:0.12em; padding: 4px 10px;
                     border-radius: 3px; white-space: nowrap;",
            "SSM HEALTH \u00b7 SLUCARE"
          ),
          tags$h1(
            "CCC Dashboard",
            style = "margin:0; font-size:1.2rem; font-weight:700; letter-spacing:0.01em;"
          ),
          div(
            style = "margin-left:auto; font-size:0.8rem; opacity:0.65;",
            "Internal Medicine \u00b7 Saint Louis University"
          )
        ),

        div(
          class = "row justify-content-center",
          div(
            class = "col-lg-8 col-md-10 col-12",
            div(
              class = "card shadow-lg",
              div(
                class = "card-body p-5",

                div(
                  class = "text-center mb-4",
                  tags$h2(
                    class = "mb-2",
                    style = "color: #003d5c; font-weight: 700;",
                    tags$i(class = "bi bi-shield-lock me-2", style = "color: #0066a1;"),
                    "CCC Dashboard Access"
                  ),
                  tags$p(
                    class = "lead text-muted",
                    "Clinical Competency Committee \u00b7 IMSLU Internal Medicine Residency"
                  ),
                  tags$hr()
                ),

                div(
                  style = paste(
                    "background: #f8fafc;",
                    "border: 1px solid #dde5ed;",
                    "border-left: 4px solid #0066a1;",
                    "border-radius: 4px;",
                    "padding: 16px 18px;",
                    "font-size: 0.82rem;",
                    "color: #4a5568;",
                    "line-height: 1.7;",
                    "margin-bottom: 28px;"
                  ),
                  paste(
                    "This dashboard is restricted to authorized Clinical Competency Committee",
                    "members and program leadership. Resident data displayed here is",
                    "confidential. By entering your access code you acknowledge that",
                    "unauthorized access or distribution of this information is prohibited.",
                    "All activity may be monitored and logged."
                  )
                ),

                div(
                  tags$label(
                    "Access Code",
                    style = "font-size:0.82rem; font-weight:600; color:#2d3748; margin-bottom:6px; display:block;"
                  ),
                  passwordInput(
                    inputId     = "access_code",
                    label       = NULL,
                    placeholder = "Enter access code",
                    width       = "100%"
                  ),
                  actionButton(
                    inputId = "submit_access_code",
                    label   = "Sign In",
                    icon    = icon("sign-in-alt"),
                    class   = "btn-primary w-100",
                    style   = "margin-top: 10px;"
                  ),
                  if (!is.null(auth_error())) {
                    div(
                      style = "color: #dc3545; margin-top: 12px; font-size: 0.88rem;",
                      tags$i(class = "bi bi-exclamation-circle me-1"),
                      auth_error()
                    )
                  }
                )
              )
            )
          )
        )
      ))
    }

    # ── HOME ───────────────────────────────────────────────────────────────────
    if (state == "home") {
      return(tagList(
        div(class = "gmed-page-header",
          tags$h2("CCC Dashboard"),
          tags$p("Clinical Competency Committee \u00b7 SLU Internal Medicine")
        ),
        div(class = "gmed-nav-grid",
          lapply(ccc_nav_blocks, function(b) {
            div(
              class   = "gmed-nav-block",
              onclick = sprintf(
                "Shiny.setInputValue('nav_block', '%s', {priority: 'event'})", b$id
              ),
              div(class = "gmed-nav-block-icon",
                  tags$i(class = paste0("bi bi-", b$icon))),
              div(
                div(class = "gmed-nav-block-label", b$label),
                div(class = "gmed-nav-block-desc",  b$desc)
              )
            )
          })
        )
      ))
    }

    # ── SECTION ────────────────────────────────────────────────────────────────
    block_info    <- Filter(function(b) b$id == state, ccc_nav_blocks)
    section_label <- if (length(block_info)) block_info[[1]]$label else state
    section_icon  <- if (length(block_info)) block_info[[1]]$icon  else "grid"

    tagList(
      div(
        class = "d-flex align-items-center mb-4 pb-3",
        style = "border-bottom: 1px solid var(--ssm-border, #dde5ed);",
        tags$button(
          class   = "btn btn-sm btn-outline-secondary me-3",
          onclick = "Shiny.setInputValue('nav_back', Math.random(), {priority: 'event'})",
          tags$i(class = "bi bi-arrow-left me-1"), "Home"
        ),
        tags$i(
          class = paste0("bi bi-", section_icon, " me-2"),
          style = "color: #0066a1; font-size: 1.15rem;"
        ),
        tags$span(
          section_label,
          style = "font-weight: 700; font-size: 1.1rem; color: #003d5c;"
        )
      ),

      switch(state,
        semiannual = semiannual_section_ui(),
        interim    = interim_section_ui(),
        milestones = milestones_section_ui(),
        div("Unknown section")
      )
    )
  })

  # ===========================================================================
  # INITIALIZATION
  # ===========================================================================

  # Populate resident selectors
  observe({
    residents <- get_resident_list(app_data())

    if (nrow(residents) > 0) {
      # Build named vector: displayed label = full_name, value = record_id
      choices <- setNames(
        as.character(residents$record_id),
        residents$full_name
      )
      # Drop any rows with missing names
      choices <- choices[!is.na(names(choices)) & nchar(trimws(names(choices))) > 0]
      choices <- choices[order(names(choices))]   # alphabetical

      # server = FALSE: all choices sent to browser at once — search works
      # instantly and reliably for rosters up to ~500 residents
      updateSelectizeInput(
        session,
        "adhoc_resident",
        choices = choices,
        server  = FALSE
      )
    }
  })

  # Handle data refresh
  observeEvent(input$refresh_data, {
    # 1. Clear the disk cache so next startup hits REDCap fresh
    cache_path <- server_state$cache_path
    tryCatch(unlink(cache_path), error = function(e) NULL)

    # 2. Mark data as loading so the loading banner re-appears
    tmp <- app_data()
    tmp$full_load_complete <- FALSE
    app_data(tmp)
    server_state$load_complete <- FALSE

    # 3. Show persistent loading notification
    showNotification(
      tagList(
        tags$span(class = "spinner-border spinner-border-sm me-2", role = "status"),
        "Refreshing from REDCap — takes about 30–45 seconds…"
      ),
      type = "message", duration = NULL, id = "refresh_notif"
    )

    # 4. Fire fresh background load (same as Phase 2 startup)
    rdm_url    <- initial_data$redcap_url
    rdm_token  <- initial_data$rdm_token

    promises::future_promise({
      source("R/global.R")
      library(gmed); library(dplyr); library(purrr); library(REDCapR); library(lubridate)
      httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      load_ccc_data(redcap_url = rdm_url, rdm_token = rdm_token)
    }) %...>% (function(new_data) {
      new_data$full_load_complete <- TRUE
      # Write new cache
      tryCatch({
        dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
        saveRDS(new_data, cache_path)
        message("[Refresh] New data cached to disk")
      }, error = function(e) message("[Refresh] Cache save failed: ", e$message))
      server_state$full_data     <- new_data
      server_state$load_complete <- TRUE
      app_data(new_data)
      removeNotification("refresh_notif")
      showNotification("✓ Data refreshed successfully!", type = "message", duration = 5)
    }) %...!% (function(err) {
      removeNotification("refresh_notif")
      showNotification(paste("Refresh failed:", err$message), type = "error", duration = 15)
      # Restore previous full_load_complete state so banner goes away
      tmp2 <- app_data(); tmp2$full_load_complete <- TRUE; app_data(tmp2)
    })
  })

  # ===========================================================================
  # LAZY MILESTONE WORKFLOW
  # Computed once the first time a resident is selected (not at startup).
  # Stored back into app_data() so spider plots pick it up automatically.
  # ===========================================================================

  observe({
    rid <- selected_resident_id()
    req(rid)

    # Wait for Phase 2 — milestone_workflow needs the full all_forms dataset
    req(isTRUE(app_data()$full_load_complete))

    # Already computed — nothing to do
    if (!is.null(app_data()$milestone_workflow)) return()

    mw <- tryCatch({
      gmed::create_milestone_workflow_from_dict(
        all_forms     = app_data()$all_forms,
        data_dict     = app_data()$data_dict,
        resident_data = app_data()$residents
      )
    }, error = function(e) NULL)

    if (!is.null(mw)) {
      d <- app_data()
      d$milestone_workflow <- mw
      app_data(d)
    }
  }, priority = -10)   # low priority — runs after UI renders

  # ===========================================================================
  # MODE 1: CCC SEMI-ANNUAL REVIEW
  # ===========================================================================

  # Control which view to show
  output$show_resident_list <- reactive({
    show_list_view()
  })
  outputOptions(output, "show_resident_list", suspendWhenHidden = FALSE)

  # Back to list button
  observeEvent(input$back_to_list, {
    show_list_view(TRUE)
    selected_resident_id(NULL)
  })

  # Display current review period
  output$current_period_display <- renderText({
    paste(get_current_ccc_period(), "Reviews")
  })

  # ===========================================================================
  # FILTER BUTTON OBSERVERS
  # ===========================================================================

  # Review status filters
  observeEvent(input$filter_all, { filter_completion("all") })
  observeEvent(input$filter_all_done, { filter_completion("all_done") })
  observeEvent(input$filter_coach_done, { filter_completion("coach_done") })
  observeEvent(input$filter_coach_second_done, { filter_completion("coach_second_done") })

  # PGY level filters
  observeEvent(input$filter_pgy_all, { filter_pgy("all") })
  observeEvent(input$filter_pgy_intern, { filter_pgy("Intern") })
  observeEvent(input$filter_pgy_pgy2, { filter_pgy("PGY2") })
  observeEvent(input$filter_pgy_pgy3, { filter_pgy("PGY3") })

  # ── Filter button renderUIs with active-state coloring ─────────────────────
  .mk_filter_btn <- function(id, label, current_val, btn_val) {
    active <- identical(current_val, btn_val)
    actionButton(id, label,
      class = if (active) "btn btn-success btn-sm" else "btn btn-outline-secondary btn-sm",
      style = "margin: 3px 5px 3px 0;"
    )
  }

  output$filter_status_buttons <- renderUI({
    cur <- filter_completion()
    div(style = "margin: 8px 0;",
      .mk_filter_btn("filter_all",               "All",                 cur, "all"),
      .mk_filter_btn("filter_all_done",           "All Done",            cur, "all_done"),
      .mk_filter_btn("filter_coach_done",         "Coach Done",          cur, "coach_done"),
      .mk_filter_btn("filter_coach_second_done",  "Coach & Second Done", cur, "coach_second_done")
    )
  })

  output$filter_pgy_buttons <- renderUI({
    cur <- filter_pgy()
    div(style = "margin: 8px 0;",
      .mk_filter_btn("filter_pgy_all",    "All",    cur, "all"),
      .mk_filter_btn("filter_pgy_intern", "Intern", cur, "Intern"),
      .mk_filter_btn("filter_pgy_pgy2",   "PGY2",   cur, "PGY2"),
      .mk_filter_btn("filter_pgy_pgy3",   "PGY3",   cur, "PGY3")
    )
  })

  # Dynamic coach filter buttons
  output$filter_coach_buttons <- renderUI({
    tryCatch({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) == 0) {
      return(tags$div(style = "margin: 8px 0;",
        .mk_filter_btn("filter_coach_all", "All", filter_coach(), "all")))
    }

    cur <- filter_coach()
    coaches <- sort(unique(review_table$coach_name[!is.na(review_table$coach_name) & review_table$coach_name != ""]))
    buttons <- list(.mk_filter_btn("filter_coach_all", "All", cur, "all"))
    for (coach in coaches) {
      btn_id <- paste0("filter_coach_", gsub("[^A-Za-z0-9]", "_", coach))
      buttons <- c(buttons, list(.mk_filter_btn(btn_id, coach, cur, coach)))
    }
    tags$div(style = "margin: 8px 0;", buttons)
    }, error = function(e) {
      warning("filter_coach_buttons error: ", e$message)
      tags$div(style = "margin: 8px 0;", actionButton("filter_coach_all", "All", class = "btn-outline-secondary btn-sm"))
    })
  })

  # Dynamic second reviewer filter buttons
  output$filter_second_buttons <- renderUI({
    tryCatch({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) == 0) {
      return(tags$div(style = "margin: 8px 0;",
        .mk_filter_btn("filter_second_all", "All", filter_second(), "all")))
    }

    cur <- filter_second()
    second_revs <- sort(unique(review_table$second_rev_name[!is.na(review_table$second_rev_name) & review_table$second_rev_name != ""]))
    buttons <- list(.mk_filter_btn("filter_second_all", "All", cur, "all"))
    for (second_rev in second_revs) {
      btn_id <- paste0("filter_second_", gsub("[^A-Za-z0-9]", "_", second_rev))
      buttons <- c(buttons, list(.mk_filter_btn(btn_id, second_rev, cur, second_rev)))
    }
    tags$div(style = "margin: 8px 0;", buttons)
    }, error = function(e) {
      warning("filter_second_buttons error: ", e$message)
      tags$div(style = "margin: 8px 0;", actionButton("filter_second_all", "All", class = "btn-outline-secondary btn-sm"))
    })
  })

  # Observe coach button clicks
  observe({
    tryCatch({
      current_period <- get_current_ccc_period()
      review_table <- get_ccc_review_table(app_data(), current_period)

      if (nrow(review_table) > 0) {
        coaches <- unique(review_table$coach_name)
        coaches <- coaches[!is.na(coaches) & coaches != ""]

        observeEvent(input$filter_coach_all, {
          filter_coach("all")
        }, ignoreInit = TRUE)

        for (coach in coaches) {
          local({
            coach_name <- coach
            btn_id <- paste0("filter_coach_", gsub("[^A-Za-z0-9]", "_", coach_name))
            observeEvent(input[[btn_id]], {
              filter_coach(coach_name)
            }, ignoreInit = TRUE)
          })
        }
      }
    }, error = function(e) warning("coach filter observe error: ", e$message))
  })

  # Observe second reviewer button clicks
  observe({
    tryCatch({
      current_period <- get_current_ccc_period()
      review_table <- get_ccc_review_table(app_data(), current_period)

      if (nrow(review_table) > 0) {
        second_revs <- unique(review_table$second_rev_name)
        second_revs <- second_revs[!is.na(second_revs) & second_revs != ""]

        observeEvent(input$filter_second_all, {
          filter_second("all")
        }, ignoreInit = TRUE)

        for (second_rev in second_revs) {
          local({
            second_rev_name <- second_rev
            btn_id <- paste0("filter_second_", gsub("[^A-Za-z0-9]", "_", second_rev_name))
            observeEvent(input[[btn_id]], {
              filter_second(second_rev_name)
            }, ignoreInit = TRUE)
          })
        }
      }
    }, error = function(e) warning("second filter observe error: ", e$message))
  })

  # Resident review table
  output$resident_review_table <- DT::renderDT({
    tryCatch({
    # Use automatic period detection
    current_period <- get_current_ccc_period()

    review_table <- get_ccc_review_table(
      app_data(),
      current_period
    )

    # Trigger reactivity on filter changes
    completion_filter <- filter_completion()
    pgy_filter <- filter_pgy()
    coach_filter <- filter_coach()
    second_filter <- filter_second()

    if (nrow(review_table) == 0) {
      return(data.frame(
        Resident = character(0),
        Level = character(0),
        `Coach Name` = character(0),
        `Coach Status` = character(0),
        `Second Reviewer` = character(0),
        `Second Status` = character(0),
        CCC = character(0),
        check.names = FALSE
      ))
    }

    # Apply filters
    filtered_table <- review_table

    # Filter by completion status
    if (completion_filter != "all") {
      if (completion_filter == "all_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete & second_complete & ccc_complete)
      } else if (completion_filter == "coach_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete)
      } else if (completion_filter == "coach_second_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete & second_complete)
      }
    }

    # Filter by PGY level
    if (pgy_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(pgy_level == pgy_filter)
    }

    # Filter by coach
    if (coach_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(!is.na(coach_name) & coach_name == coach_filter)
    }

    # Filter by second reviewer
    if (second_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(!is.na(second_rev_name) & second_rev_name == second_filter)
    }

    if (nrow(filtered_table) == 0) {
      return(data.frame(
        Resident = character(0),
        Level = character(0),
        `Coach Name` = character(0),
        `Coach Status` = character(0),
        `Second Reviewer` = character(0),
        `Second Status` = character(0),
        CCC = character(0),
        check.names = FALSE
      ))
    }

    # Store the filtered table for row selection
    filtered_review_table(filtered_table)

    # Format for display
    display_table <- filtered_table %>%
      select(
        Resident = resident,
        Level = level,
        `Coach Name` = coach_name,
        `Coach Status` = coach_complete,
        `Second Reviewer` = second_rev_name,
        `Second Status` = second_complete,
        CCC = ccc_complete
      ) %>%
      mutate(
        `Coach Name` = ifelse(is.na(`Coach Name`), "", `Coach Name`),
        `Second Reviewer` = ifelse(is.na(`Second Reviewer`), "", `Second Reviewer`),
        `Coach Status` = ifelse(`Coach Status`, "✓", "✗"),
        `Second Status` = ifelse(`Second Status`, "✓", "✗"),
        CCC = ifelse(CCC, "✓", "✗")
      )

    DT::datatable(
      display_table,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = 'ft'
      )
    ) %>%
      DT::formatStyle(
        'Coach Status',
        color = DT::styleEqual(c("✓", "✗"), c('#28a745', '#dc3545'))
      ) %>%
      DT::formatStyle(
        'Second Status',
        color = DT::styleEqual(c("✓", "✗"), c('#28a745', '#dc3545'))
      ) %>%
      DT::formatStyle(
        'CCC',
        color = DT::styleEqual(c("✓", "✗"), c('#28a745', '#dc3545'))
      )
    }, error = function(e) {
      message("[ResidentTable] render error: ", e$message)
      DT::datatable(data.frame(Error = paste("Could not load review table:", e$message)))
    })
  })

  # Review statistics
  output$review_stats <- renderUI({
    tryCatch({
    current_period <- get_current_ccc_period()

    review_table <- get_ccc_review_table(
      app_data(),
      current_period
    )

    # Trigger reactivity on filter changes
    completion_filter <- filter_completion()
    pgy_filter <- filter_pgy()
    coach_filter <- filter_coach()
    second_filter <- filter_second()

    # Apply same filters as the table
    filtered_table <- review_table

    # Filter by completion status
    if (completion_filter != "all") {
      if (completion_filter == "all_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete & second_complete & ccc_complete)
      } else if (completion_filter == "coach_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete)
      } else if (completion_filter == "coach_second_done") {
        filtered_table <- filtered_table %>%
          filter(coach_complete & second_complete)
      }
    }

    # Filter by PGY level
    if (pgy_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(pgy_level == pgy_filter)
    }

    # Filter by coach
    if (coach_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(!is.na(coach_name) & coach_name == coach_filter)
    }

    # Filter by second reviewer
    if (second_filter != "all") {
      filtered_table <- filtered_table %>%
        filter(!is.na(second_rev_name) & second_rev_name == second_filter)
    }

    total <- nrow(filtered_table)
    coach_completed <- sum(filtered_table$coach_complete, na.rm = TRUE)
    second_completed <- sum(filtered_table$second_complete, na.rm = TRUE)
    ccc_completed <- sum(filtered_table$ccc_complete, na.rm = TRUE)

    tagList(
      p(
        strong("Total Residents: "), total, br(),
        strong("Coach Reviews: "), coach_completed, " / ", total, br(),
        strong("Second Reviews: "), second_completed, " / ", total, br(),
        strong("CCC Reviews: "), ccc_completed, " / ", total
      )
    )
    }, error = function(e) {
      warning("review_stats render error: ", e$message)
      div()
    })
  })

  # Handle table row selection - show resident details
  observeEvent(input$resident_review_table_rows_selected, {
    req(input$resident_review_table_rows_selected)

    # Use the filtered review table
    review_table <- filtered_review_table()

    if (is.null(review_table) || nrow(review_table) == 0) {
      return()
    }

    selected_rid <- review_table$record_id[input$resident_review_table_rows_selected]
    selected_resident_id(selected_rid)
    show_list_view(FALSE)  # Switch to detail view
  })

  # ── Reset active toggle when switching residents ───────────────────────────
  observeEvent(selected_resident_id(), {
    active_toggle(NULL)
  })

  # ── Toggle observers (exclusive — clicking a new button auto-closes current) ──
  observeEvent(input$toggle_eval_data,     active_toggle(if (identical(active_toggle(), "eval"))  NULL else "eval"))
  observeEvent(input$toggle_plus_delta,    active_toggle(if (identical(active_toggle(), "pd"))    NULL else "pd"))
  observeEvent(input$toggle_board_data,    active_toggle(if (identical(active_toggle(), "board")) NULL else "board"))
  observeEvent(input$toggle_mile_progress, active_toggle(if (identical(active_toggle(), "mile"))  NULL else "mile"))

  # ── Toggle button CSS update (shinyjs — no re-render, avoids feedback loop) ──
  observe({
    at <- active_toggle()
    shinyjs::toggleClass("toggle_eval_data",     "btn-success",           condition = identical(at, "eval"))
    shinyjs::toggleClass("toggle_eval_data",     "btn-outline-secondary", condition = !identical(at, "eval"))
    shinyjs::toggleClass("toggle_plus_delta",    "btn-success",           condition = identical(at, "pd"))
    shinyjs::toggleClass("toggle_plus_delta",    "btn-outline-secondary", condition = !identical(at, "pd"))
    shinyjs::toggleClass("toggle_board_data",    "btn-success",           condition = identical(at, "board"))
    shinyjs::toggleClass("toggle_board_data",    "btn-outline-secondary", condition = !identical(at, "board"))
    shinyjs::toggleClass("toggle_mile_progress", "btn-success",           condition = identical(at, "mile"))
    shinyjs::toggleClass("toggle_mile_progress", "btn-outline-secondary", condition = !identical(at, "mile"))
  })

  # ── Phase 2 loading banner ───────────────────────────────────────────────────
  output$phase2_loading_banner <- renderUI({
    if (isTRUE(app_data()$full_load_complete)) return(NULL)
    div(
      class = "alert alert-info d-flex align-items-center gap-2",
      style = "font-size:0.88rem; margin-bottom:12px;",
      tags$span(class = "spinner-border spinner-border-sm", role = "status",
                style = "flex-shrink:0;"),
      tags$span(
        tags$strong("Loading additional data in background."),
        " Evaluations, Board Predictor, and Plus/Delta will appear when ready",
        " (usually 15–30 seconds after opening a resident). This page will update automatically."
      )
    )
  })

  # ── Conditional section UIs ─────────────────────────────────────────────────

  output$ccc_eval_section <- renderUI({
    req(identical(active_toggle(), "eval"))
    req(selected_resident_id())
    if (!isTRUE(app_data()$full_load_complete)) {
      return(div(
        class = "alert alert-info mt-3 d-flex align-items-center gap-2",
        style = "font-size:0.9em;",
        tags$span(class = "spinner-border spinner-border-sm"),
        " Evaluation data loading in background — will appear when ready."
      ))
    }
    if (is.null(app_data()$all_forms$assessment) ||
        nrow(app_data()$all_forms$assessment) == 0) {
      return(p(class = "text-muted mt-2", icon("info-circle"),
               " No evaluation data found for this resident."))
    }
    tagList(tags$hr(style = "margin: 12px 0 8px;"), gmed::mod_eval_feedback_ui("ccc_ef_mod"))
  })

  output$ccc_pd_section <- renderUI({
    req(identical(active_toggle(), "pd"))
    req(selected_resident_id())
    if (!isTRUE(app_data()$full_load_complete)) {
      return(div(
        class = "alert alert-info mt-3 d-flex align-items-center gap-2",
        style = "font-size:0.9em;",
        tags$span(class = "spinner-border spinner-border-sm"),
        " Plus/Delta data loading in background — will appear when ready."
      ))
    }
    if (is.null(app_data()$all_forms$assessment) ||
        nrow(app_data()$all_forms$assessment) == 0) {
      return(p(class = "text-muted mt-2", icon("info-circle"),
               " No assessment data available for Plus/Delta."))
    }
    tagList(tags$hr(style = "margin: 12px 0 8px;"), gmed::mod_plus_delta_table_ui("ccc_pd_mod"))
  })

  output$ccc_boards_section <- renderUI({
    req(identical(active_toggle(), "board"))
    req(selected_resident_id())
    if (!isTRUE(app_data()$full_load_complete)) {
      return(div(
        class = "alert alert-info mt-3 d-flex align-items-center gap-2",
        style = "font-size:0.9em;",
        tags$span(class = "spinner-border spinner-border-sm"),
        " Board/ITE data loading in background — will appear when ready."
      ))
    }
    tagList(
      tags$hr(style = "margin: 12px 0 8px;"),
      gmed::mod_seval_boards_display_ui("ccc_boards_mod")
    )
  })

  output$ccc_mile_section <- renderUI({
    req(identical(active_toggle(), "mile"))
    rid <- selected_resident_id()
    req(rid)
    tagList(
      tags$hr(style = "margin: 12px 0 8px;"),
      div(
        class = "d-flex align-items-center gap-3 mb-2 flex-wrap",
        tags$span("Select milestone:", style = "font-size:0.9rem; font-weight:600; color:#003d5c; white-space:nowrap;"),
        uiOutput("ccc_mile_selector")
      ),
      plotly::plotlyOutput("ccc_milestone_plot", height = "360px")
    )
  })

  # ── Milestone selector for individual tracking ───────────────────────────────
  output$ccc_mile_selector <- renderUI({
    long_data <- tryCatch(
      get_milestone_longitudinal_data(app_data()),
      error = function(e) NULL
    )
    rid <- selected_resident_id()
    req(rid)

    if (is.null(long_data) || nrow(long_data) == 0) return(NULL)
    res_data <- long_data %>% filter(record_id == as.character(rid))
    if (nrow(res_data) == 0) return(NULL)

    milestones <- sort(unique(res_data$milestone))
    # Build named choices: "PC1" -> "PC 1 (Patient Care)"
    comp_labels <- c(
      PC   = "Patient Care",
      MK   = "Medical Knowledge",
      SBP  = "Systems-Based Practice",
      PBL  = "Practice-Based Learning",
      PROF = "Professionalism",
      ICS  = "Interpersonal & Communication"
    )
    make_label <- function(ms) {
      prefix <- gsub("[0-9]+$", "", ms)
      num    <- gsub("^[A-Z]+", "", ms)
      comp   <- comp_labels[prefix]
      if (!is.na(comp)) paste0(ms, " — ", comp, " ", num) else ms
    }
    choices <- setNames(milestones, vapply(milestones, make_label, character(1)))
    selectInput("ccc_sel_mile", NULL, choices = choices, selected = milestones[1], width = "280px")
  })

  # ── Individual milestone progression plot ────────────────────────────────────
  output$ccc_milestone_plot <- plotly::renderPlotly({
    rid <- selected_resident_id()
    req(rid)

    empty_plot <- function(msg) {
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          annotations = list(list(
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            text = msg, showarrow = FALSE, font = list(size = 14, color = "#6c757d")
          ))
        )
    }

    long_data <- tryCatch(
      get_milestone_longitudinal_data(app_data()),
      error = function(e) NULL
    )
    if (is.null(long_data) || nrow(long_data) == 0) {
      return(empty_plot("No milestone data available yet."))
    }

    res_data <- long_data %>% filter(record_id == as.character(rid))
    if (nrow(res_data) == 0) {
      return(empty_plot("No milestone entries recorded for this resident."))
    }

    sel_mile <- input$ccc_sel_mile
    if (is.null(sel_mile) || !sel_mile %in% res_data$milestone) {
      sel_mile <- res_data$milestone[1]
    }

    plot_data <- res_data %>%
      filter(milestone == sel_mile) %>%
      arrange(period_num)

    if (nrow(plot_data) == 0) {
      return(empty_plot(paste("No data found for milestone", sel_mile)))
    }

    period_order <- c("Mid Intern","End Intern","Mid PGY2","End PGY2","Mid PGY3","Graduating")
    res_name <- tryCatch(
      app_data()$residents %>% filter(record_id == rid) %>% slice(1) %>% pull(full_name),
      error = function(e) ""
    )

    plotly::plot_ly() %>%
      plotly::add_trace(
        data   = plot_data,
        x      = ~prog_mile_period,
        y      = ~score,
        type   = "scatter",
        mode   = "lines+markers",
        name   = sel_mile,
        line   = list(color = "#0066a1", width = 2.5),
        marker = list(color = "#0066a1", size = 9)
      ) %>%
      plotly::add_segments(
        x = period_order[1], xend = tail(period_order, 1),
        y = 7, yend = 7,
        line = list(dash = "dot", color = "#e67e22", width = 2),
        name = "Graduation Target (Level 7)",
        showlegend = TRUE
      ) %>%
      plotly::layout(
        title  = list(
          text = sprintf("%s — %s", sel_mile, res_name),
          font = list(size = 13, color = "#2d3748")
        ),
        xaxis  = list(
          title         = "",
          categoryorder = "array",
          categoryarray = period_order,
          tickangle     = -20,
          gridcolor     = "#e9ecef"
        ),
        yaxis  = list(
          title     = "Score (1–9)",
          range     = c(0, 9),
          dtick     = 1,
          gridcolor = "#e9ecef"
        ),
        legend        = list(orientation = "h", x = 0, y = -0.25, font = list(size = 11)),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#f8f9fa",
        margin        = list(l = 50, r = 20, t = 45, b = 80),
        font          = list(family = "Inter, sans-serif", size = 12)
      )
  })

  # ── Previous CCC ILP display ─────────────────────────────────────────────────
  output$prev_ccc_ilp_display <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    if (nrow(resident_info) == 0) return(NULL)

    period_num      <- get_period_number(resident_info$current_period)
    prev_period_num <- if (!is.na(period_num) && period_num > 0) period_num - 1L else NA_integer_

    if (is.na(prev_period_num)) {
      return(p(
        class = "text-muted",
        style = "font-size:0.88rem; font-style:italic; margin-bottom:12px;",
        icon("info-circle"), " No previous period — this is the resident's first review."
      ))
    }

    prev_period_name <- get_period_name(prev_period_num)
    prev_ccc <- tryCatch(
      get_form_data_for_period(app_data()$all_forms, "ccc_review", rid, prev_period_name),
      error = function(e) data.frame()
    )

    if (nrow(prev_ccc) == 0 || !("ccc_ilp" %in% names(prev_ccc)) ||
        is.na(prev_ccc$ccc_ilp[1]) || nchar(trimws(as.character(prev_ccc$ccc_ilp[1]))) == 0) {
      return(p(
        class = "text-muted",
        style = "font-size:0.88rem; font-style:italic; margin-bottom:12px;",
        icon("info-circle"),
        sprintf(" No CCC ILP was recorded for the previous period (%s).", prev_period_name)
      ))
    }

    div(
      class = "alert alert-secondary",
      style = "border-left: 4px solid #6c757d; padding: 10px 14px; margin-bottom: 14px;",
      tags$small(
        class = "text-muted d-block",
        style = "font-size:0.78rem; text-transform:uppercase; letter-spacing:0.05em; margin-bottom:4px;",
        sprintf("Previous CCC ILP — %s", prev_period_name)
      ),
      p(
        style = "margin: 0; font-size: 0.94em; line-height: 1.55; white-space: pre-wrap;",
        as.character(prev_ccc$ccc_ilp[1])
      )
    )
  })

  # Resident detail panel
  output$resident_detail_panel <- renderUI({
    rid <- selected_resident_id()

    if (is.null(rid)) {
      return(div(class = "alert alert-info",
        "Select a resident from the table above to view details and conduct CCC review"))
    }

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    # ── Pre-populate helpers ─────────────────────────────────────────────────
    # Fetch any existing CCC review row for this resident + period
    existing_ccc <- selected_ccc_data()

    # Safely extract a single text field; returns `default` if absent/NA/blank
    fld <- function(col, default = "") {
      if (nrow(existing_ccc) > 0 &&
          col %in% names(existing_ccc) &&
          !is.na(existing_ccc[[col]][1]) &&
          nchar(trimws(as.character(existing_ccc[[col]][1]))) > 0) {
        as.character(existing_ccc[[col]][1])
      } else {
        default
      }
    }

    tagList(
      # ── 1. Resident header + Ad Hoc button ──────────────────────────────────
      fluidRow(
        column(width = 10,
          gmed::gmed_resident_panel(
            resident_name = resident_info$full_name,
            level         = resident_info$current_period,
            coach         = if ("coach_name" %in% names(resident_info)) resident_info$coach_name else NULL
          )
        ),
        column(width = 2,
          div(style = "padding-top: 15px; text-align: right;",
            actionButton("log_adhoc_meeting", "Log Ad Hoc Meeting",
                         icon = icon("calendar-plus"), class = "btn-warning")
          )
        )
      ),
      uiOutput("phase2_loading_banner"),
      tags$hr(),

      # ── 2. Current Period Review Materials ───────────────────────────────────
      h4("Review Materials", style = "margin-top: 20px; margin-bottom: 12px;"),
      fluidRow(
        # Left half: past CCC reviews
        column(width = 6,
          gmed::gmed_card(
            title = "Past CCC Reviews",
            DT::DTOutput("past_ccc_reviews_table")
          )
        ),
        # Right half: action items
        column(width = 6,
          gmed::gmed_card(
            title = "Previous Action Items",
            DT::DTOutput("action_data_table")
          )
        )
      ),
      tags$hr(),

      # ── 3. ILP Section ───────────────────────────────────────────────────────
      h4("CCC Review Form", style = "margin-top: 20px;"),
      p(class = "text-muted", style = "margin-bottom: 12px;",
        "Complete the following fields based on the committee's discussion and review of the resident's performance."),
      fluidRow(
        column(width = 12,
          gmed::gmed_card(
            title = "Individual Learning Plan",
            uiOutput("prev_ccc_ilp_display"),
            uiOutput("coach_ilp_display"),
            tags$br(),
            uiOutput("second_comments_display"),
            tags$br(),
            textAreaInput("ccc_ilp", "CCC ILP:",
              value = fld("ccc_ilp"), rows = 4, width = "100%",
              placeholder = "Enter CCC Individual Learning Plan...")
          )
        )
      ),
      tags$br(),

      # ── 4. Additional Resident Data (toggle buttons, no re-render) ──────────
      fluidRow(
        column(width = 12,
          gmed::gmed_card(
            title = "Additional Resident Data",
            p(class = "text-muted", style = "font-size:0.88rem; margin-bottom: 10px;",
              "Toggle sections to view evaluation data, plus/delta feedback,",
              " board predictor, and milestone progression."),
            div(
              style = "display:flex; flex-wrap:wrap; gap:8px; margin-bottom:8px;",
              actionButton("toggle_eval_data",     tagList(icon("star"),           " Evaluations"),
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton("toggle_plus_delta",    tagList(icon("comments"),       " Plus/Delta"),
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton("toggle_board_data",    tagList(icon("graduation-cap"), " Board Predictor/ITE"),
                           class = "btn btn-outline-secondary btn-sm"),
              actionButton("toggle_mile_progress", tagList(icon("chart-line"),     " Milestone History"),
                           class = "btn btn-outline-secondary btn-sm")
            ),
            uiOutput("ccc_eval_section"),
            uiOutput("ccc_pd_section"),
            uiOutput("ccc_boards_section"),
            uiOutput("ccc_mile_section")
          )
        )
      ),
      tags$br(),

      # ── 5. Milestone Grid (2×2) ──────────────────────────────────────────────
      h4("Milestones", style = "margin-top: 10px; margin-bottom: 12px;"),
      fluidRow(
        column(width = 6,
          tags$p(class = "text-muted mb-1", style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:.06em;",
                 "ACGME Milestones (Previous Period)"),
          plotly::plotlyOutput("plot_acgme_spider", height = "380px")
        ),
        column(width = 6,
          tags$p(class = "text-muted mb-1", style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:.06em;",
                 "Program Milestones (Current Period)"),
          plotly::plotlyOutput("plot_program_spider", height = "380px")
        )
      ),
      tags$br(),
      fluidRow(
        column(width = 6,
          tags$p(class = "text-muted mb-1", style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:.06em;",
                 "Self-Evaluation (Current Period)"),
          plotly::plotlyOutput("plot_self_spider", height = "380px")
        ),
        column(width = 6,
          gmed::gmed_card(
            title = "Milestone Descriptions",
            p(class = "text-muted", style = "font-size: 0.9em;",
              "Descriptions provided for specific competencies during this period."),
            DT::DTOutput("milestone_descriptions_table")
          )
        )
      ),
      tags$br(),

      # ── 6. Milestone Discussion (conditional edit table inside) ──────────────
      fluidRow(
        column(width = 12,
          gmed::gmed_card(
            title = "Milestone Discussion",
            radioButtons("ccc_mile", "Any changes to milestones?",
              choices  = c("No" = "0", "Yes" = "1"),
              selected = fld("ccc_mile", "0"), inline = TRUE),
            uiOutput("milestone_discussion_content")
          )
        )
      ),
      tags$br(),

      # ── 7. Follow-up | Concerns side by side ─────────────────────────────────
      fluidRow(
        column(width = 6,
          gmed::gmed_card(
            title = "Follow-up Issues",
            radioButtons("ccc_issues_yn", "Any Follow-up Issues?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = if (nchar(fld("ccc_issues_follow_up")) > 0) "1" else "0",
              inline = TRUE),
            conditionalPanel(
              condition = "input.ccc_issues_yn == '1'",
              textAreaInput("ccc_issues_follow_up", "Describe Follow-up Issues:",
                value = fld("ccc_issues_follow_up"), rows = 3, width = "100%",
                placeholder = "Describe any issues requiring follow-up...")
            )
          )
        ),
        column(width = 6,
          gmed::gmed_card(
            title = "Concerns",
            radioButtons("ccc_concern", "Any Concerns?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = fld("ccc_concern", "0"), inline = TRUE),
            uiOutput("concern_content")
          )
        )
      ),
      br(),

      # ── 8. Submit ────────────────────────────────────────────────────────────
      fluidRow(
        column(width = 12,
          actionButton("submit_ccc_review", "Submit CCC Review",
                       class = "btn-primary btn-lg", icon = icon("check")),
          span(class = "text-muted", style = "margin-left: 15px;",
               "This will save the review to REDCap")
        )
      )
    )
  })

  # Past CCC Reviews Table — Date, Type, Notes (ccc_ilp + ccc_issues_follow_up combined)
  output$past_ccc_reviews_table <- DT::renderDT({
    rid <- selected_resident_id()
    req(rid)

    ad       <- app_data()
    ccc_all  <- ad$all_forms$ccc_review

    empty_tbl <- data.frame(Date=character(), Type=character(),
                            Notes=character(), stringsAsFactors=FALSE)

    if (is.null(ccc_all) || nrow(ccc_all) == 0) {
      return(DT::datatable(empty_tbl, options=list(dom='t', ordering=FALSE,
                           searching=FALSE), rownames=FALSE))
    }

    past <- ccc_all %>%
      filter(as.character(record_id) == as.character(rid),
             redcap_repeat_instrument == "ccc_review")

    if (nrow(past) == 0) {
      return(DT::datatable(empty_tbl, options=list(dom='t', ordering=FALSE,
                           searching=FALSE), rownames=FALSE))
    }

    # Translate ccc_rev_type code → label
    type_label <- function(x) {
      dplyr::case_when(
        is.na(x)    ~ "Scheduled",
        x == "2"    ~ "Interim",
        x == "1"    ~ "Scheduled",
        TRUE        ~ as.character(x)
      )
    }

    # Combine ccc_ilp and ccc_issues_follow_up into a single Notes column
    get_col <- function(df, col) {
      if (col %in% names(df)) trimws(as.character(df[[col]])) else rep("", nrow(df))
    }

    ilp     <- get_col(past, "ccc_ilp")
    issues  <- get_col(past, "ccc_issues_follow_up")
    interim <- get_col(past, "ccc_interim")

    notes <- mapply(function(i, f, it) {
      parts <- c(
        if (nzchar(i))  paste0("ILP: ", i)          else NULL,
        if (nzchar(f))  paste0("Follow-up: ", f)    else NULL,
        if (nzchar(it)) paste0("Interim: ", it)     else NULL
      )
      if (length(parts) == 0) "" else paste(parts, collapse=" | ")
    }, ilp, issues, interim, SIMPLIFY=TRUE, USE.NAMES=FALSE)

    result <- data.frame(
      Date  = get_col(past, "ccc_date"),
      Type  = type_label(get_col(past, "ccc_rev_type")),
      Notes = notes,
      stringsAsFactors = FALSE
    )

    # Sort newest first
    result <- result[order(result$Date, decreasing=TRUE, na.last=TRUE), ]

    DT::datatable(
      result,
      options = list(
        pageLength = 5,
        dom        = 'tp',
        ordering   = FALSE,
        searching  = FALSE,
        scrollX    = TRUE,
        columnDefs = list(
          list(width="85px",  targets=0),   # Date
          list(width="80px",  targets=1),   # Type
          list(className="dt-wrap", targets=2)  # Notes — wraps
        )
      ),
      rownames  = FALSE,
      selection = 'none'
    )
  })

  # Action Data Table
  output$action_data_table <- DT::renderDT({
    rid <- selected_resident_id()
    req(rid)

    action_data <- get_action_data_table(app_data(), rid)

    DT::datatable(
      action_data,
      options = list(
        pageLength  = 5,
        dom         = 'tp',         # table + pagination only
        ordering    = FALSE,
        searching   = FALSE,
        scrollX     = TRUE,         # horizontal scroll within card
        autoWidth   = FALSE,
        columnDefs  = list(
          list(width = "70px",  targets = 0),   # Date
          list(width = "80px",  targets = 1),   # Session
          list(width = "65px",  targets = 2),   # Type
          list(width = "120px", targets = 3),   # Issues
          list(width = "120px", targets = 4),   # Comments
          list(width = "90px",  targets = 5),   # Competency
          list(width = "80px",  targets = 6),   # Action
          list(width = "70px",  targets = 7)    # Status
        )
      ),
      rownames  = FALSE,
      selection = 'none'
    )
  })

  # Coach ILP Display
  output$coach_ilp_display <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    coach_data <- get_coach_review_data(
      app_data(),
      rid,
      resident_info$current_period
    )

    if (nrow(coach_data) == 0 || is.na(coach_data$coach_ilp_final[1]) || nchar(trimws(coach_data$coach_ilp_final[1])) == 0) {
      return(p(
        class = "text-muted",
        icon("info-circle"),
        " No coach ILP available for this period"
      ))
    }

    div(
      p(strong("Coach ILP:")),
      p(coach_data$coach_ilp_final[1])
    )
  })

  # Second Comments Display
  output$second_comments_display <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    second_data <- get_second_review_data(
      app_data(),
      rid,
      resident_info$current_period
    )

    if (nrow(second_data) == 0 || is.na(second_data$second_comments[1]) || nchar(trimws(second_data$second_comments[1])) == 0) {
      return(p(
        class = "text-muted",
        icon("info-circle"),
        " No second review comments available for this period"
      ))
    }

    div(
      p(strong("Second Review Comments:")),
      p(second_data$second_comments[1])
    )
  })

  # Milestone Discussion Content
  output$milestone_discussion_content <- renderUI({
    req(input$ccc_mile)
    if (input$ccc_mile == "0") return(NULL)

    rid <- selected_resident_id()
    req(rid)

    # Pre-populate notes from any existing CCC review row
    existing_notes <- {
      ex <- selected_ccc_data()
      if (nrow(ex) > 0 && "ccc_mile_notes" %in% names(ex) &&
          !is.na(ex$ccc_mile_notes[1]) &&
          nchar(trimws(as.character(ex$ccc_mile_notes[1]))) > 0)
        as.character(ex$ccc_mile_notes[1])
      else ""
    }

    tagList(
      textAreaInput(
        "ccc_mile_notes",
        "CCC Milestone Notes:",
        value = existing_notes,
        rows = 3,
        width = "100%",
        placeholder = "Enter notes about milestone discussion..."
      ),
      br(),
      gmed::gmed_card(
        title = "Update Program Milestones",
        p(class = "text-muted", style = "font-size: 0.9em;",
          "Select the updated rating (1–9) for each subcompetency. ",
          tags$em("Leave '—' for any unchanged. Milestone values are saved together with the CCC review.")),
        uiOutput("milestone_entry_inputs")
      )
    )
  })

  # Concern Content
  output$concern_content <- renderUI({
    req(input$ccc_concern)

    if (input$ccc_concern == "0") {
      return(NULL)
    }

    # Get checkbox choices from data dictionary (for_ui = TRUE to get labels as names)
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency", for_ui = TRUE)
    action_choices     <- get_field_choices(app_data()$data_dict, "ccc_action",     for_ui = TRUE)
    status_choices     <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    # Pre-populate from existing CCC review row
    ex <- selected_ccc_data()

    existing_comments <- {
      if (nrow(ex) > 0 && "ccc_comments" %in% names(ex) &&
          !is.na(ex$ccc_comments[1]) &&
          nchar(trimws(as.character(ex$ccc_comments[1]))) > 0)
        as.character(ex$ccc_comments[1])
      else ""
    }

    # Decode checked checkbox codes from ___code columns
    # choices is from get_field_choices(..., for_ui=TRUE): names=labels, values=codes
    decode_checks <- function(base, choices) {
      if (nrow(ex) == 0) return(NULL)
      codes <- unname(choices)   # the VALUES are the REDCap codes (e.g. "1","2","3")
      checked <- codes[vapply(codes, function(code) {
        col <- paste0(base, "___", code)
        isTRUE(col %in% names(ex) && !is.na(ex[[col]][1]) && ex[[col]][1] == "1")
      }, logical(1))]
      if (length(checked) == 0) NULL else checked
    }

    tagList(
      textAreaInput(
        "ccc_comments",
        "CCC Comments:",
        value = existing_comments,
        rows = 3,
        width = "100%",
        placeholder = "Describe the concerns..."
      ),
      checkboxGroupInput(
        "ccc_competency",
        "Competency Areas:",
        choices  = competency_choices,
        selected = decode_checks("ccc_competency", competency_choices)
      ),
      checkboxGroupInput(
        "ccc_action",
        "Action Required:",
        choices  = action_choices,
        selected = decode_checks("ccc_action", action_choices)
      ),
      checkboxGroupInput(
        "ccc_action_status",
        "Action Status:",
        choices  = status_choices,
        selected = decode_checks("ccc_action_status", status_choices)
      )
    )
  })

  # Coach Review Summary
  output$coach_review_summary <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    coach_data <- get_coach_review_data(
      app_data(),
      rid,
      resident_info$current_period
    )

    if (nrow(coach_data) == 0) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.85rem;",
               icon("info-circle"), " Not yet completed"))
    }

    # Compact summary: ILP + any notes/summary
    ilp_text     <- if ("coach_ilp_final" %in% names(coach_data) && !is.na(coach_data$coach_ilp_final[1]) && nchar(trimws(coach_data$coach_ilp_final[1])) > 0) coach_data$coach_ilp_final[1] else NULL
    summary_text <- if ("coach_summary"   %in% names(coach_data) && !is.na(coach_data$coach_summary[1])   && nchar(trimws(coach_data$coach_summary[1]))   > 0) coach_data$coach_summary[1]   else NULL
    notes_text   <- if ("coach_notes"     %in% names(coach_data) && !is.na(coach_data$coach_notes[1])     && nchar(trimws(coach_data$coach_notes[1]))     > 0) coach_data$coach_notes[1]     else NULL

    mk_row <- function(label, val) {
      if (is.null(val)) return(NULL)
      div(style = "margin-bottom:8px;",
        tags$span(class = "fw-semibold", style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:.04em; color:#546e7a;",
                  label),
        p(style = "font-size:0.88rem; margin:2px 0 0 0; line-height:1.4;", val)
      )
    }

    tagList(
      if (!is.null(ilp_text))     mk_row("ILP",     ilp_text),
      if (!is.null(summary_text)) mk_row("Summary", summary_text),
      if (!is.null(notes_text))   mk_row("Notes",   notes_text),
      if (is.null(ilp_text) && is.null(summary_text) && is.null(notes_text))
        p(class = "text-success", style = "font-size:0.88rem;", icon("check"), " Completed")
    )
  })

  # Second Review Summary
  output$second_review_summary <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    second_data <- get_second_review_data(
      app_data(),
      rid,
      resident_info$current_period
    )

    if (nrow(second_data) == 0) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.85rem;",
               icon("info-circle"), " Not yet completed"))
    }

    mk_row <- function(label, val) {
      if (is.null(val) || is.na(val) || nchar(trimws(val)) == 0) return(NULL)
      div(style = "margin-bottom:8px;",
        tags$span(class = "fw-semibold", style = "font-size:0.8rem; text-transform:uppercase; letter-spacing:.04em; color:#546e7a;",
                  label),
        p(style = "font-size:0.88rem; margin:2px 0 0 0; line-height:1.4;", val)
      )
    }

    approved_val <- if ("second_approve" %in% names(second_data) && !is.na(second_data$second_approve[1]))
      if (second_data$second_approve[1] == "1") "Yes — milestones approved" else "No" else NULL

    tagList(
      mk_row("Comments",          if ("second_comments"      %in% names(second_data)) second_data$second_comments[1]      else NULL),
      mk_row("Milestones",        approved_val),
      mk_row("Milestone Notes",   if ("second_miles_comment" %in% names(second_data)) second_data$second_miles_comment[1] else NULL),
      if (is.null(approved_val) &&
          (!"second_comments" %in% names(second_data) || is.na(second_data$second_comments[1])))
        p(class = "text-success", style = "font-size:0.88rem;", icon("check"), " Completed")
    )
  })

  # ── Previous period coach note (for Review Materials section) ───────────────
  output$prev_period_coach_display <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)
    if (nrow(resident_info) == 0) return(NULL)

    period_num      <- get_period_number(resident_info$current_period)
    prev_period_num <- if (!is.na(period_num) && period_num > 0) period_num - 1L else NA_integer_

    if (is.na(prev_period_num)) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.82rem; margin:0;",
               icon("info-circle"), " First review period — no prior coach data."))
    }

    prev_period_name <- get_period_name(prev_period_num)
    prev_coach <- tryCatch(
      get_form_data_for_period(app_data()$all_forms, "coach_rev", rid, prev_period_name),
      error = function(e) data.frame()
    )

    if (nrow(prev_coach) == 0) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.82rem; margin:0;",
               icon("info-circle"), sprintf(" No coach review recorded for %s.", prev_period_name)))
    }

    # Pull the most useful text fields
    summary_text <- if ("coach_summary" %in% names(prev_coach) &&
                        !is.na(prev_coach$coach_summary[1]) &&
                        nzchar(trimws(prev_coach$coach_summary[1])))
      prev_coach$coach_summary[1] else NULL
    ilp_text <- if ("coach_ilp_final" %in% names(prev_coach) &&
                    !is.na(prev_coach$coach_ilp_final[1]) &&
                    nzchar(trimws(prev_coach$coach_ilp_final[1])))
      prev_coach$coach_ilp_final[1] else NULL

    if (is.null(summary_text) && is.null(ilp_text)) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.82rem; margin:0;",
               icon("check"), sprintf(" Coach review completed (%s) — no text fields.", prev_period_name)))
    }

    div(
      class = "alert alert-secondary",
      style = "border-left:4px solid #6c757d; padding:8px 12px; margin-top:8px; margin-bottom:0;",
      tags$small(class = "text-muted d-block",
                 style = "font-size:0.76rem; text-transform:uppercase; letter-spacing:0.05em;",
                 sprintf("Previous Coach Review — %s", prev_period_name)),
      if (!is.null(summary_text))
        p(style = "margin:4px 0 0; font-size:0.88em; line-height:1.5; white-space:pre-wrap;", summary_text),
      if (!is.null(ilp_text) && is.null(summary_text))
        p(style = "margin:4px 0 0; font-size:0.88em; line-height:1.5; white-space:pre-wrap;", ilp_text)
    )
  })

  # ── Previous period CCC note (for Review Materials section) ─────────────────
  output$prev_period_ccc_display <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)
    if (nrow(resident_info) == 0) return(NULL)

    period_num      <- get_period_number(resident_info$current_period)
    prev_period_num <- if (!is.na(period_num) && period_num > 0) period_num - 1L else NA_integer_

    if (is.na(prev_period_num)) return(NULL)

    prev_period_name <- get_period_name(prev_period_num)
    prev_ccc <- tryCatch(
      get_form_data_for_period(app_data()$all_forms, "ccc_review", rid, prev_period_name),
      error = function(e) data.frame()
    )

    if (nrow(prev_ccc) == 0 || !("ccc_ilp" %in% names(prev_ccc)) ||
        is.na(prev_ccc$ccc_ilp[1]) || !nzchar(trimws(as.character(prev_ccc$ccc_ilp[1])))) {
      return(p(class = "text-muted fst-italic", style = "font-size:0.82rem; margin:0; margin-top:8px;",
               icon("info-circle"), sprintf(" No CCC ILP recorded for %s.", prev_period_name)))
    }

    div(
      class = "alert alert-secondary",
      style = "border-left:4px solid #6c757d; padding:8px 12px; margin-top:8px; margin-bottom:0;",
      tags$small(class = "text-muted d-block",
                 style = "font-size:0.76rem; text-transform:uppercase; letter-spacing:0.05em;",
                 sprintf("Previous CCC ILP — %s", prev_period_name)),
      p(style = "margin:4px 0 0; font-size:0.88em; line-height:1.5; white-space:pre-wrap;",
        as.character(prev_ccc$ccc_ilp[1]))
    )
  })

  # Milestone Descriptions Table
  output$milestone_descriptions_table <- DT::renderDT({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    descriptions <- get_milestone_descriptions(
      app_data(),
      rid,
      resident_info$current_period
    )

    if (nrow(descriptions) == 0) {
      descriptions <- data.frame(
        Competency = "No descriptions available",
        "Full Name" = "",
        Source = "",
        Score = NA,
        Description = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      names(descriptions) <- c("Competency", "Full Name", "Source", "Score", "Description")
    }

    DT::datatable(
      descriptions,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = TRUE,
        searching = FALSE
      ),
      rownames = FALSE,
      selection = 'none'
    )
  })

  # Milestone Edit Table (Editable)
  # Milestone entry inputs — grouped selects pre-populated from existing data
  output$milestone_entry_inputs <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    existing_mile <- tryCatch(
      get_milestone_values_for_edit(app_data(), rid, resident_info$current_period),
      error = function(e) {
        message("[MilestoneInputs] error: ", e$message)
        data.frame(competency = character(), field_name = character(),
                   value = numeric(), stringsAsFactors = FALSE)
      }
    )

    get_val <- function(field) {
      if (nrow(existing_mile) > 0 && "field_name" %in% names(existing_mile)) {
        idx <- which(existing_mile$field_name == field)
        if (length(idx) > 0 && !is.na(existing_mile$value[idx[1]]))
          return(as.character(as.integer(existing_mile$value[idx[1]])))
      }
      ""
    }

    cf      <- mile_comp_fields()
    domains <- unique(cf$domain)

    domain_blocks <- lapply(domains, function(dom) {
      rows <- cf[cf$domain == dom, ]
      comp_rows <- lapply(seq_len(nrow(rows)), function(i) {
        code      <- rows$code[i]
        field     <- rows$field[i]
        full_name <- as.character(get_competency_full_name(code))
        cur_val   <- get_val(field)
        img_file  <- paste0("milestones/", tolower(code), ".png")
        tags$tr(
          tags$td(
            style = "font-size:0.84rem; padding:2px 8px; vertical-align:middle; color:#333;",
            full_name
          ),
          tags$td(
            style = "width:85px; padding:1px 4px;",
            selectInput(
              inputId  = paste0("mile_", field),
              label    = NULL,
              choices  = c("—" = "", as.character(1:9)),
              selected = cur_val,
              width    = "78px"
            )
          ),
          tags$td(
            style = "width:36px; padding:2px 4px; text-align:center; vertical-align:middle;",
            tags$a(
              href    = "#",
              onclick = sprintf("showImage('%s'); return false;", img_file),
              title   = paste("View", code, "milestone"),
              style   = "color:#546e7a; font-size:0.95rem;",
              HTML("&#128247;")   # 📷 camera emoji fallback; or use icon("image")
            )
          )
        )
      })

      tagList(
        tags$tr(
          tags$td(
            colspan = "2",
            style   = "padding:6px 8px 2px; background:#f8f9fa; border-top:1px solid #dee2e6;",
            tags$span(
              style = "font-size:0.78rem; font-weight:700; text-transform:uppercase;
                       letter-spacing:0.05em; color:#003d5c;",
              dom
            )
          )
        ),
        comp_rows
      )
    })

    tags$table(
      style = "width:100%; border-collapse:collapse;",
      tags$tbody(domain_blocks)
    )
  })

  # ACGME Spider Plot (Previous Period)
  output$plot_acgme_spider <- plotly::renderPlotly({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    # Get previous period
    period_num <- get_period_number(resident_info$current_period)
    previous_period_num <- if (!is.na(period_num) && period_num > 0) period_num - 1 else NA
    previous_period_name <- if (!is.na(previous_period_num)) {
      get_period_name(previous_period_num)
    } else {
      NA_character_
    }

    if (is.na(previous_period_name)) {
      return(plotly::plot_ly() %>%
        plotly::add_annotations(
          text = "No previous period",
          x = 0.5, y = 0.5,
          showarrow = FALSE
        ))
    }

    tryCatch({
      # Check if milestone workflow exists
      if (is.null(app_data()$milestone_workflow)) {
        return(plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "Milestone workflow not available",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          ))
      }

      # Create ACGME spider plot using milestone workflow
      dashboard <- gmed::create_milestone_overview_dashboard(
        milestone_results = app_data()$milestone_workflow,
        resident_id = rid,
        period_text = previous_period_name,
        milestone_type = "program",
        milestone_system = "acgme",
        resident_data = app_data()$residents
      )

      # Extract spider plot from dashboard object
      if (!is.null(dashboard) && !is.null(dashboard$spider_plot)) {
        dashboard$spider_plot
      } else {
        plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "No data available for this period",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    }, error = function(e) {
      plotly::plot_ly() %>%
        plotly::add_annotations(
          text = paste("Error:", e$message),
          x = 0.5, y = 0.5,
          showarrow = FALSE
        )
    })
  })

  # Program Spider Plot (Current Period)
  output$plot_program_spider <- plotly::renderPlotly({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    tryCatch({
      # Check if milestone workflow exists
      if (is.null(app_data()$milestone_workflow)) {
        return(plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "Milestone workflow not available",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          ))
      }

      # Create program milestone spider plot using milestone workflow (REP system)
      dashboard <- gmed::create_milestone_overview_dashboard(
        milestone_results = app_data()$milestone_workflow,
        resident_id = rid,
        period_text = resident_info$current_period,
        milestone_type = "program",
        milestone_system = "rep",
        resident_data = app_data()$residents
      )

      # Extract spider plot from dashboard object
      if (!is.null(dashboard) && !is.null(dashboard$spider_plot)) {
        dashboard$spider_plot
      } else {
        plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "No data available for this period",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    }, error = function(e) {
      plotly::plot_ly() %>%
        plotly::add_annotations(
          text = paste("Error:", e$message),
          x = 0.5, y = 0.5,
          showarrow = FALSE
        )
    })
  })

  # Self-Evaluation Spider Plot (Current Period)
  output$plot_self_spider <- plotly::renderPlotly({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    tryCatch({
      # Check if milestone workflow exists
      if (is.null(app_data()$milestone_workflow)) {
        return(plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "Milestone workflow not available",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          ))
      }

      # Create self-evaluation spider plot using milestone workflow (REP system)
      dashboard <- gmed::create_milestone_overview_dashboard(
        milestone_results = app_data()$milestone_workflow,
        resident_id = rid,
        period_text = resident_info$current_period,
        milestone_type = "self",
        milestone_system = "rep",
        resident_data = app_data()$residents
      )

      # Extract spider plot from dashboard object
      if (!is.null(dashboard) && !is.null(dashboard$spider_plot)) {
        dashboard$spider_plot
      } else {
        plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "No data available for this period",
            x = 0.5, y = 0.5,
            showarrow = FALSE
          )
      }
    }, error = function(e) {
      plotly::plot_ly() %>%
        plotly::add_annotations(
          text = paste("Error:", e$message),
          x = 0.5, y = 0.5,
          showarrow = FALSE
        )
    })
  })

  # ── Milestone spider for confirmation modal ───────────────────────────────────
  output$modal_mile_spider <- plotly::renderPlotly({
    rid <- selected_resident_id()
    if (is.null(rid)) return(plotly::plot_ly())

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)
    if (nrow(resident_info) == 0) return(plotly::plot_ly())

    cf <- mile_comp_fields()

    # Existing milestone values for current period
    existing <- tryCatch(
      get_milestone_values_for_edit(app_data(), rid, resident_info$current_period),
      error = function(e) data.frame()
    )
    get_existing <- function(field) {
      if (nrow(existing) > 0 && "field_name" %in% names(existing)) {
        idx <- which(existing$field_name == field)
        if (length(idx) > 0 && !is.na(existing$value[idx[1]])) return(existing$value[idx[1]])
      }
      NA_real_
    }

    existing_vals <- vapply(cf$field, get_existing, numeric(1))

    # New (form input) values — available when the milestone section is open
    new_vals <- vapply(cf$field, function(f) {
      v <- input[[paste0("mile_", f)]]
      if (!is.null(v) && nzchar(v)) as.numeric(v) else NA_real_
    }, numeric(1))

    has_new     <- any(!is.na(new_vals))
    has_existing <- any(!is.na(existing_vals))

    if (!has_new && !has_existing) {
      return(plotly::plot_ly(type = "scatterpolar") %>%
        plotly::layout(
          polar = list(radialaxis = list(visible = FALSE)),
          annotations = list(list(text = "No milestone data", x = 0.5, y = 0.5,
                                  showarrow = FALSE, xref = "paper", yref = "paper"))
        ))
    }

    # Close polygon for radar
    theta <- c(cf$code, cf$code[1])

    traces <- list()
    if (has_existing) {
      r_ex <- c(existing_vals, existing_vals[1])
      r_ex[is.na(r_ex)] <- 0
      traces[[length(traces) + 1]] <- list(
        type = "scatterpolar", mode = "lines+markers",
        r = r_ex, theta = theta, fill = "toself", name = "Current",
        fillcolor = "rgba(108,117,125,0.15)",
        line = list(color = "#6c757d", width = 1.5),
        marker = list(color = "#6c757d", size = 4)
      )
    }
    if (has_new) {
      # Merge: new value if entered, else existing
      merged <- ifelse(!is.na(new_vals), new_vals, existing_vals)
      r_new  <- c(merged, merged[1])
      r_new[is.na(r_new)] <- 0
      traces[[length(traces) + 1]] <- list(
        type = "scatterpolar", mode = "lines+markers",
        r = r_new, theta = theta, fill = "toself", name = "Updated",
        fillcolor = "rgba(0,61,92,0.2)",
        line = list(color = "#003d5c", width = 2),
        marker = list(color = "#003d5c", size = 5)
      )
    }

    p <- plotly::plot_ly()
    for (tr in traces) {
      p <- p %>% plotly::add_trace(
        type = tr$type, mode = tr$mode,
        r = tr$r, theta = tr$theta,
        fill = tr$fill, name = tr$name,
        fillcolor = tr$fillcolor,
        line = tr$line, marker = tr$marker
      )
    }

    p %>% plotly::layout(
      polar = list(
        radialaxis = list(
          visible = TRUE, range = c(0, 9),
          tickvals = c(1, 3, 5, 7, 9), tickfont = list(size = 9)
        ),
        angularaxis = list(tickfont = list(size = 9))
      ),
      legend = list(orientation = "h", x = 0, y = -0.1, font = list(size = 10)),
      margin = list(l = 30, r = 30, t = 20, b = 20)
    )
  })

  # ── Submit CCC Review — Step 1: show confirmation modal ──────────────────────
  observeEvent(input$submit_ccc_review, {
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    if (nrow(resident_info) == 0) {
      showNotification("Resident not found. Please refresh.", type = "error", duration = 8)
      return()
    }

    if (is.null(input$ccc_ilp) || nchar(trimws(input$ccc_ilp)) == 0) {
      showNotification("Please enter a CCC ILP before submitting.", type = "warning", duration = 5)
      return()
    }

    # Helper: safe field read from input
    inp <- function(nm, default = "") {
      v <- input[[nm]]
      if (is.null(v) || (length(v) == 1 && !nzchar(trimws(as.character(v))))) default
      else paste(as.character(v), collapse = ", ")
    }

    # Build confirmation modal content
    mk_row <- function(label, val, empty_label = "—") {
      v <- if (!is.null(val) && nzchar(trimws(paste(val, collapse = "")))) val else empty_label
      tags$tr(
        tags$th(style = "width:180px; font-weight:600; color:#003d5c; padding:6px 10px; vertical-align:top;", label),
        tags$td(style = "padding:6px 10px;", v)
      )
    }

    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency", for_ui = TRUE)
    action_choices     <- get_field_choices(app_data()$data_dict, "ccc_action",     for_ui = TRUE)
    status_choices     <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    sel_labels <- function(choices, selected_codes) {
      if (length(selected_codes) == 0 || is.null(selected_codes)) return("None")
      lbls <- names(choices)[unname(choices) %in% selected_codes]
      if (length(lbls) == 0) "None" else paste(lbls, collapse = "; ")
    }

    showModal(modalDialog(
      title = tags$span(icon("clipboard-check"), " Confirm CCC Review Submission"),
      size  = "l",
      easyClose = FALSE,

      tags$p(class = "text-muted mb-3",
        "Please review the entries below before submitting. ",
        tags$strong("This will save (or update) the CCC review record for this period.")),

      tags$table(class = "table table-sm table-bordered mb-0",
        tags$tbody(
          mk_row("Resident",    resident_info$full_name),
          mk_row("Period",      resident_info$current_period),
          mk_row("Date",        as.character(Sys.Date())),
          tags$tr(tags$td(colspan = "2",
            tags$hr(style = "margin: 4px 0;"))),
          mk_row("ILP",         inp("ccc_ilp")),
          mk_row("Milestone Δ", if (inp("ccc_mile", "0") == "1") "Yes" else "No"),
          mk_row("Mile. Notes", inp("ccc_mile_notes")),
          if (inp("ccc_mile", "0") == "1") {
            cf      <- mile_comp_fields()
            changed <- Filter(function(x) !is.null(x) && nzchar(x),
              setNames(lapply(cf$field, function(f) input[[paste0("mile_", f)]]), cf$code))
            mk_row(
              "Milestones",
              if (length(changed) > 0)
                paste(names(changed), unlist(changed), sep = "=", collapse = "  |  ")
              else
                tags$em("(no values entered)")
            )
          } else NULL,
          tags$tr(tags$td(colspan = "2",
            tags$hr(style = "margin: 4px 0;"))),
          mk_row("Follow-up Issues", if (inp("ccc_issues_yn", "0") == "1") inp("ccc_issues_follow_up") else "No"),
          mk_row("Concerns",    if (inp("ccc_concern", "0") == "1") "Yes" else "No"),
          mk_row("Comments",    inp("ccc_comments")),
          mk_row("Competencies", sel_labels(competency_choices, input$ccc_competency)),
          mk_row("Actions",      sel_labels(action_choices,     input$ccc_action)),
          mk_row("Status",       sel_labels(status_choices,     input$ccc_action_status))
        )
      ),

      tags$hr(style = "margin:12px 0 8px;"),
      tags$p(class = "text-muted mb-1",
             style = "font-size:0.82rem; font-weight:600; text-transform:uppercase; letter-spacing:.04em;",
             icon("chart-area"), " Milestone Overview"),
      plotly::plotlyOutput("modal_mile_spider", height = "280px"),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_submit_ccc", "Submit to REDCap",
                     class = "btn-primary", icon = icon("check"))
      )
    ))
  })

  # ── Submit CCC Review — Step 2: confirmed, do the write ───────────────────────
  observeEvent(input$confirm_submit_ccc, {
    removeModal()

    rid <- selected_resident_id()
    req(rid)

    tryCatch({
      resident_info <- app_data()$residents %>%
        filter(record_id == rid) %>%
        slice(1)

      period_code <- switch(
        resident_info$current_period,
        "Mid Intern"         = 1, "End Intern"         = 2,
        "Mid PGY2"           = 3, "End PGY2"           = 4,
        "Mid PGY3"           = 5, "Graduating"         = 6,
        "Entering Residency" = 7, 1
      )

      review_type <- if (resident_info$current_period %in%
                         c("Mid Intern", "End Intern", "Mid PGY2",
                           "End PGY2", "Mid PGY3", "Graduating")) "1" else "2"

      # Instance = period_code (1-7), matching coach dash pattern.
      # If this period's instance already exists, REDCap will update it.
      next_instance <- period_code

      ccc_data <- data.frame(
        record_id                = as.character(rid),
        redcap_repeat_instrument = "ccc_review",
        redcap_repeat_instance   = as.character(next_instance),
        ccc_date                 = as.character(Sys.Date()),
        ccc_rev_type             = as.character(review_type),
        ccc_session              = as.character(period_code),
        ccc_ilp              = if (!is.null(input$ccc_ilp))              as.character(input$ccc_ilp)              else "",
        ccc_mile             = if (!is.null(input$ccc_mile))             as.character(input$ccc_mile)             else "0",
        ccc_mile_notes       = if (!is.null(input$ccc_mile_notes))       as.character(input$ccc_mile_notes)       else "",
        ccc_issues_follow_up = if (!is.null(input$ccc_issues_follow_up)) as.character(input$ccc_issues_follow_up) else "",
        ccc_concern          = if (!is.null(input$ccc_concern))          as.character(input$ccc_concern)          else "0",
        ccc_comments         = if (!is.null(input$ccc_comments))         as.character(input$ccc_comments)         else "",
        ccc_review_complete  = "2",
        stringsAsFactors = FALSE
      )

      for (code in names(get_field_choices(app_data()$data_dict, "ccc_competency"))) {
        ccc_data[[paste0("ccc_competency___", code)]] <-
          if (!is.null(input$ccc_competency) && code %in% input$ccc_competency) "1" else "0"
      }
      for (code in names(get_field_choices(app_data()$data_dict, "ccc_action"))) {
        ccc_data[[paste0("ccc_action___", code)]] <-
          if (!is.null(input$ccc_action) && code %in% input$ccc_action) "1" else "0"
      }
      for (code in names(get_field_choices(app_data()$data_dict, "ccc_action_status"))) {
        ccc_data[[paste0("ccc_action_status___", code)]] <-
          if (!is.null(input$ccc_action_status) && code %in% input$ccc_action_status) "1" else "0"
      }

      message("[Submit] Writing CCC review — record_id=", rid,
              " instance=", next_instance, " period=", resident_info$current_period)

      # ── Write to REDCap (synchronous — matches coach dash pattern) ────────────
      result <- REDCapR::redcap_write_oneshot(
        ds         = ccc_data,
        redcap_uri = REDCAP_CONFIG$url,
        token      = REDCAP_CONFIG$rdm_token
      )

      message("[Submit] result$success=", result$success,
              " msg=", result$outcome_message)

      if (isTRUE(result$success)) {
        showNotification(
          paste("✓ CCC Review saved for", resident_info$full_name),
          type = "message", duration = 6
        )

        # ── Optimistic local update so the table reflects CCC complete immediately ──
        # Add the submitted record (with translated ccc_session) to app_data()
        # before the async refresh, so the list view renders correctly.
        tryCatch({
          submitted_labeled <- ccc_data
          submitted_labeled$ccc_session <- resident_info$current_period  # label not code

          current_review <- app_data()$all_forms$ccc_review
          if (!is.null(current_review) && nrow(current_review) > 0) {
            # Remove any existing row for this record+period (handles update case)
            current_review <- current_review %>%
              filter(!(as.character(record_id) == as.character(rid) &
                       redcap_repeat_instrument == "ccc_review" &
                       as.character(redcap_repeat_instance) == as.character(period_code)))
            new_review <- dplyr::bind_rows(current_review, submitted_labeled)
          } else {
            new_review <- submitted_labeled
          }
          opt <- app_data()
          opt$all_forms$ccc_review <- new_review
          app_data(opt)
        }, error = function(e) {
          message("[Submit] Optimistic update failed (non-fatal): ", e$message)
        })

        # ── Write milestone_entry if ccc_mile == "1" and any values entered ───
        if (!is.null(input$ccc_mile) && input$ccc_mile == "1") {
          cf <- mile_comp_fields()
          mile_vals <- setNames(
            lapply(cf$field, function(f) {
              v <- input[[paste0("mile_", f)]]
              if (!is.null(v) && nzchar(v)) as.numeric(v) else NA_real_
            }),
            cf$field
          )

          if (any(!is.na(unlist(mile_vals)))) {
            mile_data <- data.frame(
              record_id                = as.character(rid),
              redcap_repeat_instrument = "milestone_entry",
              redcap_repeat_instance   = as.character(period_code),
              prog_mile_date           = as.character(Sys.Date()),
              prog_mile_period         = as.character(period_code),
              stringsAsFactors = FALSE
            )
            for (f in cf$field) mile_data[[f]] <- mile_vals[[f]]

            message("[Submit] Writing milestone_entry — record_id=", rid,
                    " instance=", period_code)

            mile_result <- tryCatch(
              REDCapR::redcap_write_oneshot(
                ds         = mile_data,
                redcap_uri = REDCAP_CONFIG$url,
                token      = REDCAP_CONFIG$rdm_token
              ),
              error = function(e)
                list(success = FALSE, outcome_message = e$message)
            )

            message("[Submit] milestone result$success=", mile_result$success)

            if (!isTRUE(mile_result$success)) {
              showNotification(
                tagList(tags$strong("Milestone write failed:"), tags$br(),
                        mile_result$outcome_message),
                type = "error", duration = 30
              )
            }
          }
        }

        # Quick async refresh of ccc_review + milestone_entry
        rdm_url   <- REDCAP_CONFIG$url
        rdm_token <- REDCAP_CONFIG$rdm_token
        dd        <- app_data()$data_dict

        promises::future_promise({
          library(REDCapR); library(dplyr)
          httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
          list(
            ccc_review = REDCapR::redcap_read_oneshot(
              redcap_uri   = rdm_url, token = rdm_token,
              forms        = "ccc_review",
              raw_or_label = "raw", verbose = FALSE
            )$data,
            milestone_entry = REDCapR::redcap_read_oneshot(
              redcap_uri   = rdm_url, token = rdm_token,
              forms        = "milestone_entry",
              raw_or_label = "raw", verbose = FALSE
            )$data
          )
        }) %...>% (function(fresh) {
          updated <- app_data()

          # Translate ccc_session codes → labels
          new_ccc <- fresh$ccc_review
          if (!is.null(new_ccc) && nrow(new_ccc) > 0 &&
              "ccc_session" %in% names(new_ccc) && !is.null(dd)) {
            pf_str <- dd %>%
              filter(field_name == "ccc_session") %>%
              pull(select_choices_or_calculations)
            if (length(pf_str) > 0 && !is.na(pf_str[1])) {
              pm <- .parse_choices(pf_str[1])
              new_ccc[["ccc_session"]] <- .translate_codes(new_ccc[["ccc_session"]], pm)
            }
            updated$all_forms$ccc_review <- new_ccc
          }

          # Translate prog_mile_period codes → labels
          new_mile <- fresh$milestone_entry
          if (!is.null(new_mile) && nrow(new_mile) > 0 &&
              "prog_mile_period" %in% names(new_mile) && !is.null(dd)) {
            pf_str <- dd %>%
              filter(field_name == "prog_mile_period") %>%
              pull(select_choices_or_calculations)
            if (length(pf_str) > 0 && !is.na(pf_str[1])) {
              pm <- .parse_choices(pf_str[1])
              new_mile[["prog_mile_period"]] <-
                .translate_codes(new_mile[["prog_mile_period"]], pm)
            }
            if (!"redcap_repeat_instrument" %in% names(new_mile) ||
                all(is.na(new_mile$redcap_repeat_instrument)))
              new_mile$redcap_repeat_instrument <- "milestone_entry"
            updated$all_forms$milestone_entry <- new_mile
          }

          app_data(updated)
        }) %...!% (function(err) {
          message("[Submit] post-submit refresh failed: ", err$message)
        })

        show_list_view(TRUE)
        selected_resident_id(NULL)

      } else {
        showNotification(
          tagList(
            tags$strong("REDCap write failed:"), tags$br(),
            result$outcome_message
          ),
          type = "error", duration = 30
        )
      }

    }, error = function(e) {
      showNotification(
        paste("Error during submission:", e$message),
        type = "error", duration = 30
      )
      message("[Submit] Error: ", e$message)
    })
  })

  # Save Milestone Edits — REMOVED: milestones now written in confirm_submit_ccc
  # observeEvent(input$save_milestone_edits, {  # kept as comment for reference

  if (FALSE) {
    rid <- selected_resident_id()

    # Get resident info for current period
    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    # Get the current milestone data with all fields including instance info
    milestone_entry_data <- get_form_data_for_period(
      app_data()$all_forms,
      "milestone_entry",
      rid,
      resident_info$current_period
    )

    # Determine if we're creating a new entry or updating existing
    creating_new_entry <- (nrow(milestone_entry_data) == 0)

    if (creating_new_entry) {
      # Calculate next instance number for new entry
      all_milestone_entries <- app_data()$all_forms$milestone_entry %>%
        filter(record_id == rid, redcap_repeat_instrument == "milestone_entry")

      if (nrow(all_milestone_entries) > 0) {
        next_instance <- max(as.numeric(all_milestone_entries$redcap_repeat_instance), na.rm = TRUE) + 1
      } else {
        next_instance <- 1
      }

      # Create template with all milestone fields (21 total)
      milestone_cols <- c(
        paste0("rep_pc", 1:6),
        paste0("rep_mk", 1:3),
        paste0("rep_sbp", 1:3),
        paste0("rep_pbl", 1:2),
        paste0("rep_prof", 1:4),
        paste0("rep_ics", 1:3)
      )

      # Initialize milestone_entry_data as a template for new entry
      milestone_entry_data <- data.frame(
        record_id = as.character(rid),
        redcap_repeat_instrument = "milestone_entry",
        redcap_repeat_instance = next_instance,
        prog_mile_date = as.character(Sys.Date()),  # Set current date
        stringsAsFactors = FALSE
      )

      # Add empty milestone fields
      for (col in milestone_cols) {
        milestone_entry_data[[col]] <- NA
      }
    }

    # Get the milestone values with field names (for existing data)
    # For new entries, this will be empty but we'll map from the template
    milestone_values <- get_milestone_values_for_edit(
      app_data(),
      rid,
      resident_info$current_period
    )

    # If creating new entry, build milestone_values from template
    if (creating_new_entry) {
      template_competencies <- c(
        paste0("PC", 1:6),
        paste0("MK", 1:3),
        paste0("SBP", 1:3),
        paste0("PBL", 1:2),
        paste0("PROF", 1:4),
        paste0("ICS", 1:3)
      )

      template_fields <- c(
        paste0("rep_pc", 1:6),
        paste0("rep_mk", 1:3),
        paste0("rep_sbp", 1:3),
        paste0("rep_pbl", 1:2),
        paste0("rep_prof", 1:4),
        paste0("rep_ics", 1:3)
      )

      milestone_values <- data.frame(
        competency = template_competencies,
        field_name = template_fields,
        value = rep(NA, length(template_competencies)),
        stringsAsFactors = FALSE
      )
    }

    # Check if there were any cell edits
    if (!is.null(input$milestone_edit_table_cell_edit)) {
      # Process cell edits
      edit_info <- input$milestone_edit_table_cell_edit

      # Get the row and column that was edited
      row_idx <- edit_info$row + 1  # DT uses 0-based indexing
      col_idx <- edit_info$col
      new_value <- edit_info$value

      # Map back to competency and field name
      # Row index corresponds to milestone_values row
      if (row_idx <= nrow(milestone_values)) {
        field_name <- milestone_values$field_name[row_idx]
        numeric_value <- as.numeric(new_value)

        # Validate value is between 1 and 9
        if (is.na(numeric_value) || numeric_value < 1 || numeric_value > 9) {
          showNotification(
            paste("Invalid milestone value:", new_value, ". Must be between 1 and 9."),
            type = "error",
            duration = 5
          )
          return()
        }

        # Build minimal data frame for REDCap submission with only required fields
        # Get all milestone value fields from the original data
        milestone_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$",
                              names(milestone_entry_data), value = TRUE)

        update_data <- data.frame(
          record_id = as.character(milestone_entry_data$record_id[1]),
          redcap_repeat_instrument = "milestone_entry",
          redcap_repeat_instance = as.numeric(milestone_entry_data$redcap_repeat_instance[1]),
          stringsAsFactors = FALSE
        )

        # Add all milestone fields - ensure numeric values
        for (col in milestone_cols) {
          if (col == field_name) {
            update_data[[col]] <- as.numeric(numeric_value)
          } else if (col %in% names(milestone_entry_data)) {
            # Ensure all milestone values are numeric
            update_data[[col]] <- as.numeric(milestone_entry_data[[col]][1])
          }
        }

        # Add the period field - MUST be raw numeric code for REDCap
        # Convert period name back to numeric code
        period_code <- switch(
          resident_info$current_period,
          "Mid Intern" = 1,
          "End Intern" = 2,
          "Mid PGY2" = 3,
          "End PGY2" = 4,
          "Mid PGY3" = 5,
          "Graduating" = 6,
          "Entering Residency" = 7,
          1  # fallback
        )

        update_data$prog_mile_period <- as.character(period_code)

        # Try to save to REDCap (synchronous — matches coach dash pattern)
        tryCatch({
          message("[Milestone] Writing milestone_entry — record_id=", rid,
                  " instance=", milestone_entry_data$redcap_repeat_instance[1],
                  " field=", field_name, " value=", numeric_value)

          result <- REDCapR::redcap_write_oneshot(
            ds         = update_data,
            redcap_uri = REDCAP_CONFIG$url,
            token      = REDCAP_CONFIG$rdm_token
          )

          message("[Milestone] result$success=", result$success,
                  " msg=", result$outcome_message)

          if (isTRUE(result$success)) {
            message_text <- if (creating_new_entry) {
              paste("New milestone entry created for", resident_info$full_name)
            } else {
              paste("Milestone values updated for", resident_info$full_name)
            }

            showNotification(message_text, type = "message", duration = 5)

            # Async mini-refresh of just milestone_entry (no source global.R)
            rdm_url   <- REDCAP_CONFIG$url
            rdm_token <- REDCAP_CONFIG$rdm_token
            dd        <- app_data()$data_dict

            promises::future_promise({
              library(REDCapR); library(dplyr)
              httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
              REDCapR::redcap_read_oneshot(
                redcap_uri   = rdm_url, token = rdm_token,
                forms        = "milestone_entry",
                raw_or_label = "raw", verbose = FALSE
              )$data
            }) %...>% (function(new_mile) {
              if (!is.null(new_mile) && nrow(new_mile) > 0) {
                # Translate prog_mile_period codes → labels (same as load_ccc_data)
                if ("prog_mile_period" %in% names(new_mile) && !is.null(dd)) {
                  pf_str <- dd %>%
                    filter(field_name == "prog_mile_period") %>%
                    pull(select_choices_or_calculations)
                  if (length(pf_str) > 0 && !is.na(pf_str[1])) {
                    pm <- .parse_choices(pf_str[1])
                    new_mile[["prog_mile_period"]] <-
                      .translate_codes(new_mile[["prog_mile_period"]], pm)
                  }
                }
                # Ensure redcap_repeat_instrument is populated
                if ("redcap_repeat_instrument" %in% names(new_mile)) {
                  new_mile$redcap_repeat_instrument[
                    is.na(new_mile$redcap_repeat_instrument) |
                    new_mile$redcap_repeat_instrument == ""
                  ] <- "milestone_entry"
                } else {
                  new_mile$redcap_repeat_instrument <- "milestone_entry"
                }
                updated <- app_data()
                updated$all_forms$milestone_entry <- new_mile
                app_data(updated)
              }
            }) %...!% (function(err) {
              message("[Milestone] milestone_entry refresh failed: ", err$message)
            })

          } else {
            showNotification(
              tagList(tags$strong("REDCap write failed:"), tags$br(),
                      result$outcome_message),
              type = "error", duration = 30
            )
          }
        }, error = function(e) {
          showNotification(
            paste("Error saving milestone values:", e$message),
            type = "error",
            duration = 30
          )
          message("[Milestone] Error: ", e$message)
        })
      }
    } else {
      showNotification(
        "No edits detected. Make changes to the table before saving.",
        type = "info",
        duration = 5
      )
    }
  }  # end if(FALSE) — old save_milestone_edits block

  # View Plus/Delta Comments
  observeEvent(input$view_plus_delta, {
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    # Get plus/delta comments from assessment data for current period
    assessment_data <- if ("assessment" %in% names(app_data()$all_forms)) {
      all_assessments <- app_data()$all_forms$assessment %>%
        filter(record_id == rid)

      # Try to filter by period if the field exists
      if ("ass_period" %in% names(all_assessments)) {
        all_assessments %>%
          filter(!is.na(ass_period), ass_period == resident_info$current_period)
      } else if ("assessment_period" %in% names(all_assessments)) {
        all_assessments %>%
          filter(!is.na(assessment_period), assessment_period == resident_info$current_period)
      } else {
        # If no period field, return all assessments for this resident
        all_assessments
      }
    } else {
      data.frame()
    }

    plus_delta_content <- if (nrow(assessment_data) > 0) {
      # Check if gmed has plus_delta_table function
      tryCatch({
        if (exists("create_plus_delta_table", where = "package:gmed", mode = "function")) {
          gmed::create_plus_delta_table(
            assessment_data = assessment_data,
            resident_id = rid,
            period = resident_info$current_period
          )
        } else {
          # Fallback: display raw plus/delta fields
          plus_cols <- grep("^(plus|delta)_", names(assessment_data), value = TRUE)
          if (length(plus_cols) > 0) {
            content_list <- list()
            for (col in plus_cols) {
              # Collect all non-empty values for this column across all assessments
              values <- assessment_data[[col]][!is.na(assessment_data[[col]]) & nchar(trimws(assessment_data[[col]])) > 0]
              if (length(values) > 0) {
                content_list <- c(content_list, list(
                  tags$div(
                    tags$strong(gsub("_", " ", tools::toTitleCase(col)), ":"),
                    tags$ul(
                      lapply(values, function(v) tags$li(v))
                    )
                  ),
                  tags$hr()
                ))
              }
            }
            if (length(content_list) > 0) {
              do.call(tagList, content_list)
            } else {
              tags$p("No plus/delta comments available for this period.")
            }
          } else {
            tags$p("No plus/delta comments available for this period.")
          }
        }
      }, error = function(e) {
        tags$p("Error loading plus/delta comments: ", e$message)
      })
    } else {
      tags$p("No assessment data available for this period.")
    }

    showModal(modalDialog(
      title = paste("Plus/Delta Comments -", resident_info$full_name, "-", resident_info$current_period),
      plus_delta_content,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # View Current ILP Goals
  observeEvent(input$view_ilp_goals, {
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    # Get all coach reviews for this resident (all periods)
    all_coach_reviews <- app_data()$all_forms$coach_rev %>%
      filter(record_id == rid, redcap_repeat_instrument == "coach_rev") %>%
      arrange(desc(coach_period))

    ilp_content <- if (nrow(all_coach_reviews) > 0) {
      # Look for ILP goals in recent reviews
      ilp_list <- list()
      for (i in 1:min(3, nrow(all_coach_reviews))) {
        review <- all_coach_reviews[i, ]
        period <- if ("coach_period" %in% names(review)) review$coach_period else "Unknown"

        # Look for ILP-related fields
        ilp_fields <- grep("ilp|goal", names(review), value = TRUE, ignore.case = TRUE)

        if (length(ilp_fields) > 0) {
          for (field in ilp_fields) {
            if (!is.na(review[[field]]) && nchar(trimws(as.character(review[[field]]))) > 0) {
              ilp_list <- c(ilp_list, list(
                tags$div(
                  tags$h5(paste("Period:", period)),
                  tags$strong(gsub("_", " ", tools::toTitleCase(field)), ":"),
                  tags$p(as.character(review[[field]])),
                  tags$hr()
                )
              ))
            }
          }
        }
      }

      if (length(ilp_list) > 0) {
        do.call(tagList, ilp_list)
      } else {
        tags$p("No ILP goals found in recent coach reviews.")
      }
    } else {
      tags$p("No coach review data available.")
    }

    showModal(modalDialog(
      title = paste("Current ILP Goals -", resident_info$full_name),
      ilp_content,
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # ===========================================================================
  # MODE 2: AD HOC DISCUSSION
  # ===========================================================================

  output$adhoc_review_panel <- renderUI({
    req(input$adhoc_resident)

    # Get resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == input$adhoc_resident) %>%
      slice(1)

    if (nrow(resident_info) == 0) return(p("Resident not found"))

    current_period <- resident_info$current_period

    # ------------------------------------------------------------------
    # Most-recent milestone data across ALL periods (not period-specific)
    # ------------------------------------------------------------------
    safe_recent <- function(form_name, period_field) {
      d <- app_data()$all_forms[[form_name]]
      if (is.null(d)) return(list(df = data.frame(), period = "—"))
      d2 <- d %>%
        filter(record_id == input$adhoc_resident,
               redcap_repeat_instrument == form_name) %>%
        arrange(desc(redcap_repeat_instance))
      period_label <- if (nrow(d2) > 0 && period_field %in% names(d2))
        as.character(d2[[period_field]][1]) else "—"
      list(df = if (nrow(d2) > 0) d2[1, ] else data.frame(), period = period_label)
    }

    prog  <- safe_recent("milestone_entry",                "prog_mile_period")
    self  <- safe_recent("milestone_selfevaluation_c33c",  "prog_mile_period_self")
    acgme <- safe_recent("acgme_miles",                    "acgme_mile_period")

    # Cache for radar modal
    adhoc_prog_miles(prog)
    adhoc_self_miles(self)
    adhoc_acgme_miles(acgme)

    # Small button card content — click to open radar modal
    milestone_btn <- function(btn_id, res) {
      if (nrow(res$df) == 0) {
        tags$span(class = "text-muted", style = "font-size:0.85rem;",
                  icon("circle-xmark"), " No data")
      } else {
        tagList(
          if (res$period != "—")
            tags$small(class = "text-muted d-block mb-2",
                       icon("calendar"), " ", res$period)
          else NULL,
          actionButton(
            btn_id, "View Radar",
            class = "btn-sm btn-outline-primary",
            icon  = icon("chart-area")
          )
        )
      }
    }

    # ------------------------------------------------------------------
    # Previous Reviews context (two-column summary)
    # ------------------------------------------------------------------
    ctx <- get_adhoc_review_context(app_data(), input$adhoc_resident)

    info_row <- function(label, value) {
      if (is.null(value) || nchar(trimws(value)) == 0) return(NULL)
      tags$tr(
        tags$td(tags$strong(label),
                style = "width:28%; color:#6c757d; vertical-align:top;
                         font-size:0.95rem; padding:8px 14px; white-space:nowrap;"),
        tags$td(value,
                style = "vertical-align:top; font-size:0.875rem; padding:7px 12px;
                         line-height:1.5;")
      )
    }

    concern_display <- if (ctx$ccc_concern == "Yes")
      tags$span(class = "badge bg-danger", "Yes — Concern flagged")
    else
      tags$span(class = "text-muted", "No")

    ccc_label   <- paste0("Last CCC Session",
      if (nchar(ctx$ccc_session) > 0)  paste0(" (", ctx$ccc_session,  ")") else "")
    concern_label <- paste0("CCC Concern",
      if (nchar(ctx$ccc_session) > 0) paste0(" (", ctx$ccc_session, ")") else "")
    last_concern_label <- if (nchar(ctx$last_concern_date) > 0)
      paste0("Follow-up Notes (", ctx$last_concern_date, " — ", ctx$last_concern_type, ")")
    else "Follow-up Notes"

    context_table <- tags$table(
      class = "table table-sm table-borderless",
      style = "margin:0;",
      tags$tbody(
        info_row("Coach ILP / Goals", ctx$coach_ilp),
        info_row("CCC ILP",           ctx$ccc_ilp),
        if (ctx$ccc_concern == "Yes" || nchar(ctx$ccc_issues) > 0)
          tags$tr(
            tags$td(tags$strong(concern_label),
                    style = "width:28%; color:#6c757d; vertical-align:top;
                             font-size:0.95rem; padding:8px 14px;"),
            tags$td(concern_display,
                    style = "vertical-align:top; padding:7px 12px;")
          ),
        info_row("CCC Follow-up Issues", ctx$ccc_issues),
        if (nchar(ctx$last_concern_notes) > 0 && ctx$last_concern_notes != ctx$ccc_issues)
          info_row(last_concern_label, ctx$last_concern_notes)
        else NULL
      )
    )

    has_any_context <- any(nchar(c(ctx$coach_ilp, ctx$ccc_ilp,
                                   ctx$ccc_issues, ctx$last_concern_notes)) > 0) ||
                       ctx$ccc_concern == "Yes"

    tagList(
      # Resident info header
      gmed::gmed_resident_panel(
        resident_name = resident_info$full_name,
        level         = current_period,
        coach         = if ("type" %in% names(resident_info)) paste0("Type: ", resident_info$type) else NULL
      ),
      tags$hr(),

      h4("Most Recent Milestone Data", style = "margin-top: 20px;"),
      tags$p(class = "text-muted", style = "font-size:0.85rem; margin-top:-8px;",
             "Showing the latest available record regardless of review period."),
      fluidRow(
        column(width = 4,
          gmed::gmed_card(title = "Program Milestones",  milestone_btn("adhoc_prog_btn",  prog))
        ),
        column(width = 4,
          gmed::gmed_card(title = "Self-Evaluation",     milestone_btn("adhoc_self_btn",  self))
        ),
        column(width = 4,
          gmed::gmed_card(title = "ACGME Milestones",    milestone_btn("adhoc_acgme_btn", acgme))
        )
      ),

      br(),
      fluidRow(
        # Left — Coach Summary (always shown)
        column(width = 6,
          gmed::gmed_card(
            title = paste0("Coach Summary",
                           if (nchar(ctx$coach_period) > 0)
                             paste0(" (", ctx$coach_period, ")") else ""),
            if (nchar(trimws(ctx$coach_summary)) > 0)
              tags$p(style = "font-size:0.95rem; line-height:1.6; white-space:pre-wrap;",
                     ctx$coach_summary)
            else
              tags$p(class = "text-muted", style = "font-style:italic;",
                     icon("info-circle"), " No coach summary on file.")
          )
        ),
        # Right — Prior CCC context
        column(width = 6,
          gmed::gmed_card(
            title = "Prior CCC Notes",
            if (has_any_context)
              context_table
            else
              tags$p(class = "text-muted", style = "font-style:italic;",
                     icon("info-circle"), " No prior CCC review data found.")
          )
        )
      ),

      tags$hr(),
      h4("Ad Hoc Review Form", style = "margin-top: 30px;"),

      # Previous action items
      gmed::gmed_card(
        title = "Previous Action Items",
        DT::DTOutput("adhoc_action_data_table")
      ),
      tags$br(),

      # Notes (left) + Concerns (right) in one row
      fluidRow(
        # Left col — two text boxes stacked
        column(width = 8,
          gmed::gmed_card(
            title = "Review Notes",
            textAreaInput(
              "adhoc_ccc_interim",
              "Interim Notes:",
              value       = "",
              rows        = 4,
              width       = "100%",
              placeholder = "Interim update notes..."
            ),
            tags$br(),
            textAreaInput(
              "adhoc_ccc_issues_follow_up",
              "Follow-up Issues:",
              value       = "",
              rows        = 4,
              width       = "100%",
              placeholder = "Issues requiring follow-up..."
            )
          )
        ),

        # Right col — concern checkboxes, always visible
        column(width = 4,
          gmed::gmed_card(
            title = "Concerns",
            tags$p(
              class = "text-muted",
              style = "font-size:0.88rem; margin-bottom:10px;",
              icon("circle-info"),
              " Check any that apply. Submitting with selections will automatically flag a concern."
            ),
            uiOutput("adhoc_concern_content")
          )
        )
      ),

      br(),

      actionButton(
        "submit_adhoc_review",
        "Submit Ad Hoc Review",
        class = "btn-primary btn-lg",
        icon  = icon("check")
      )
    )
  })

  # ===========================================================================
  # AD HOC — Milestone radar buttons
  # ===========================================================================
  ms_fields_ordered <- c(
    "rep_pc1","rep_pc2","rep_pc3","rep_pc4","rep_pc5","rep_pc6",
    "rep_mk1","rep_mk2","rep_mk3",
    "rep_sbp1","rep_sbp2","rep_sbp3",
    "rep_pbl1","rep_pbl2",
    "rep_prof1","rep_prof2","rep_prof3","rep_prof4",
    "rep_ics1","rep_ics2","rep_ics3"
  )

  build_radar_modal <- function(res, title_str) {
    showModal(modalDialog(
      title = title_str,
      plotly::plotlyOutput("adhoc_radar_plot", height = "420px"),
      easyClose = TRUE,
      footer     = modalButton("Close"),
      size       = "m"
    ))
  }

  observeEvent(input$adhoc_prog_btn, {
    adhoc_radar_type("prog")
    build_radar_modal(adhoc_prog_miles(), "Program Milestones — Radar")
  })
  observeEvent(input$adhoc_self_btn, {
    adhoc_radar_type("self")
    build_radar_modal(adhoc_self_miles(), "Self-Evaluation — Radar")
  })
  observeEvent(input$adhoc_acgme_btn, {
    adhoc_radar_type("acgme")
    build_radar_modal(adhoc_acgme_miles(), "ACGME Milestones — Radar")
  })

  output$adhoc_radar_plot <- plotly::renderPlotly({
    mtype <- adhoc_radar_type()
    if (is.null(mtype)) return(plotly::plotly_empty())

    res <- switch(mtype,
      "prog"  = adhoc_prog_miles(),
      "self"  = adhoc_self_miles(),
      "acgme" = adhoc_acgme_miles()
    )
    if (is.null(res) || nrow(res$df) == 0) return(plotly::plotly_empty())

    row     <- res$df
    present <- ms_fields_ordered[ms_fields_ordered %in% names(row)]
    if (length(present) == 0) return(plotly::plotly_empty())

    labels <- sapply(present, map_field_to_display)
    scores <- suppressWarnings(as.numeric(sapply(present, function(f) row[[f]][1])))
    scores[is.na(scores)] <- 0

    # Close the polygon — all three vectors must be the same length
    text_vals <- paste0(labels, ": ", scores)
    r_vals    <- c(scores,     scores[1])
    th_vals   <- c(labels,     labels[1])
    text_vals <- c(text_vals,  text_vals[1])

    period_note <- if (!is.null(res$period) && res$period != "—")
      paste0("Period: ", res$period) else ""

    plotly::plot_ly(
      type      = "scatterpolar",
      r         = r_vals,
      theta     = th_vals,
      fill      = "toself",
      fillcolor = "rgba(0,48,135,0.28)",
      line      = list(color = "#003087", width = 2.5),
      mode      = "lines+markers",
      marker    = list(color = "#003087", size = 7,
                       line = list(color = "#ffffff", width = 1.5)),
      name      = "Score",
      text      = text_vals,
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        annotations = list(list(
          text      = period_note,
          x         = 0.5, y = -0.08,
          xref      = "paper", yref = "paper",
          showarrow = FALSE,
          font      = list(size = 13, color = "#6c757d")
        )),
        polar = list(
          bgcolor    = "#ffffff",
          radialaxis = list(
            visible  = TRUE,
            range    = c(0, 9),
            dtick    = 1,
            tickfont = list(size = 12, color = "#2d3748"),
            gridcolor = "#c7d2da"
          ),
          angularaxis = list(
            tickfont = list(size = 13, color = "#1a202c")
          )
        ),
        paper_bgcolor = "#ffffff",
        showlegend    = FALSE,
        font          = list(family = "Inter, sans-serif"),
        margin        = list(l = 40, r = 40, t = 30, b = 50)
      )
  })
  # Note: suspendWhenHidden removed — pre-rendering caused session crashes on startup
  # (plotly widget doesn't tolerate shiny.silent.error from req() at init time)

  # Ad Hoc Action Data Table
  output$adhoc_action_data_table <- DT::renderDT({
    req(input$adhoc_resident)

    action_data <- get_action_data_table(app_data(), input$adhoc_resident, concerns_only = FALSE)

    DT::datatable(
      action_data,
      options = list(
        pageLength   = nrow(action_data),
        dom          = 't',
        ordering     = TRUE,
        searching    = FALSE,
        scrollY      = "260px",
        scrollCollapse = TRUE,
        paging       = FALSE
      ),
      rownames = FALSE,
      selection = 'none'
    )
  })

  # Ad Hoc Concern Content — always visible; ccc_concern derived on submit
  output$adhoc_concern_content <- renderUI({
    req(input$adhoc_resident)

    # Get checkbox choices from data dictionary (for_ui = TRUE to get labels as names)
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency", for_ui = TRUE)
    action_choices     <- get_field_choices(app_data()$data_dict, "ccc_action",     for_ui = TRUE)
    status_choices     <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    tagList(
      checkboxGroupInput(
        "adhoc_ccc_competency",
        "Competency Areas:",
        choices = competency_choices,
        selected = NULL
      ),
      checkboxGroupInput(
        "adhoc_ccc_action",
        "Action Required:",
        choices = action_choices,
        selected = NULL
      ),
      checkboxGroupInput(
        "adhoc_ccc_action_status",
        "Action Status:",
        choices = status_choices,
        selected = NULL
      )
    )
  })

  # Submit Ad Hoc Review
  observeEvent(input$submit_adhoc_review, {
    req(input$adhoc_resident)

    # Get resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == input$adhoc_resident) %>%
      slice(1)

    # Convert period name to code for REDCap
    period_code <- switch(
      resident_info$current_period,
      "Mid Intern" = 1,
      "End Intern" = 2,
      "Mid PGY2" = 3,
      "End PGY2" = 4,
      "Mid PGY3" = 5,
      "Graduating" = 6,
      "Entering Residency" = 7,
      1  # fallback
    )

    # Ad hoc review type
    review_type <- "2"  # Ad hoc

    # Calculate next instance number for this resident
    all_ccc <- app_data()$all_forms$ccc_review %>%
      filter(record_id == input$adhoc_resident, redcap_repeat_instrument == "ccc_review")
    if (nrow(all_ccc) > 0 && "redcap_repeat_instance" %in% names(all_ccc)) {
      next_instance <- max(as.numeric(all_ccc$redcap_repeat_instance), na.rm = TRUE) + 1
    } else {
      next_instance <- 1
    }

    # Build base data frame
    ccc_data <- data.frame(
      record_id = as.character(input$adhoc_resident),
      redcap_repeat_instrument = "ccc_review",
      redcap_repeat_instance = as.character(next_instance),
      ccc_date = as.character(Sys.Date()),
      ccc_rev_type = as.character(review_type),
      ccc_session = as.character(period_code),
      ccc_interim = if (!is.null(input$adhoc_ccc_interim) && nchar(trimws(input$adhoc_ccc_interim)) > 0) as.character(input$adhoc_ccc_interim) else "",
      ccc_mile = "0",
      ccc_mile_notes = "",
      ccc_issues_follow_up = if (!is.null(input$adhoc_ccc_issues_follow_up) && nchar(trimws(input$adhoc_ccc_issues_follow_up)) > 0) as.character(input$adhoc_ccc_issues_follow_up) else "",
      # Derive ccc_concern: "1" if any competency, action, or status box is checked
      ccc_concern = if (length(input$adhoc_ccc_competency) > 0 ||
                        length(input$adhoc_ccc_action) > 0 ||
                        length(input$adhoc_ccc_action_status) > 0) "1" else "0",
      ccc_comments = "",
      ccc_review_complete = "2",  # Complete status
      stringsAsFactors = FALSE
    )

    # Handle checkbox fields - ccc_competency
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency")
    if (length(competency_choices) > 0) {
      for (code in names(competency_choices)) {
        col_name <- paste0("ccc_competency___", code)
        ccc_data[[col_name]] <- if (!is.null(input$adhoc_ccc_competency) && code %in% input$adhoc_ccc_competency) "1" else "0"
      }
    }

    # Handle checkbox fields - ccc_action
    action_choices <- get_field_choices(app_data()$data_dict, "ccc_action")
    if (length(action_choices) > 0) {
      for (code in names(action_choices)) {
        col_name <- paste0("ccc_action___", code)
        ccc_data[[col_name]] <- if (!is.null(input$adhoc_ccc_action) && code %in% input$adhoc_ccc_action) "1" else "0"
      }
    }

    # Handle checkbox fields - ccc_action_status
    status_choices <- get_field_choices(app_data()$data_dict, "ccc_action_status")
    if (length(status_choices) > 0) {
      for (code in names(status_choices)) {
        col_name <- paste0("ccc_action_status___", code)
        ccc_data[[col_name]] <- if (!is.null(input$adhoc_ccc_action_status) && code %in% input$adhoc_ccc_action_status) "1" else "0"
      }
    }

    # Validate required fields
    if (nchar(trimws(input$adhoc_ccc_interim)) == 0 && nchar(trimws(input$adhoc_ccc_ilp)) == 0) {
      showNotification(
        "Please enter either interim notes or CCC ILP before submitting.",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Try to save to REDCap
    tryCatch({
      # Use REDCapR to write data
      result <- REDCapR::redcap_write(
        ds_to_write = ccc_data,
        redcap_uri = REDCAP_CONFIG$url,
        token = REDCAP_CONFIG$rdm_token
      )

      if (result$success) {
        showNotification(
          paste("Ad Hoc Review saved successfully for", resident_info$full_name),
          type = "message",
          duration = 5
        )

        # Refresh data
        tryCatch({
          new_data <- load_ccc_data()
          app_data(new_data)
        }, error = function(e) {
          showNotification(
            "Review saved, but could not refresh data. Please use Refresh Data button.",
            type = "warning",
            duration = 5
          )
        })

        # Clear form inputs
        updateTextAreaInput(session, "adhoc_ccc_interim", value = "")
        updateTextAreaInput(session, "adhoc_ccc_issues_follow_up", value = "")
        updateCheckboxGroupInput(session, "adhoc_ccc_competency",    selected = character(0))
        updateCheckboxGroupInput(session, "adhoc_ccc_action",        selected = character(0))
        updateCheckboxGroupInput(session, "adhoc_ccc_action_status", selected = character(0))

        showNotification(
          "Form cleared. You can now review another resident.",
          type = "message",
          duration = 3
        )

      } else {
        showNotification(
          paste("Error saving review:", result$outcome_message),
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Error saving ad hoc review:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # ===========================================================================
  # MODE 3: ADMIN PAGE
  # ===========================================================================

  # Admin resident table
  output$admin_resident_table <- DT::renderDT({
    req(input$admin_view)

    # Get appropriate resident list based on view
    if (input$admin_view == "all") {
      residents <- get_resident_list(app_data())
    } else if (input$admin_view == "mid") {
      review_table <- get_ccc_review_table(app_data(), "Mid Year")
      # Join with full resident data to get type and grad_yr
      residents <- review_table %>%
        select(record_id) %>%
        left_join(app_data()$residents %>%
                   select(record_id, full_name, type, grad_yr, current_period),
                 by = "record_id")
    } else {
      review_table <- get_ccc_review_table(app_data(), "End Year")
      # Join with full resident data to get type and grad_yr
      residents <- review_table %>%
        select(record_id) %>%
        left_join(app_data()$residents %>%
                   select(record_id, full_name, type, grad_yr, current_period),
                 by = "record_id")
    }

    if (nrow(residents) == 0) {
      return(data.frame(
        Resident = character(0),
        Type = character(0),
        `Grad Year` = character(0),
        Period = character(0),
        check.names = FALSE
      ))
    }

    display_table <- residents %>%
      select(
        Resident = full_name,
        Type = type,
        `Grad Year` = grad_yr,
        Period = current_period
      )

    DT::datatable(
      display_table,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 50,
        dom = 'ftp'
      )
    )
  })

  # Admin data entry panel
  output$admin_data_entry_panel <- renderUI({
    p(em("Admin data entry interface will be added here"))
  })

  # ===========================================================================
  # FEATURE 2: LOG AD HOC MEETING
  # ===========================================================================

  observeEvent(input$log_adhoc_meeting, {
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    showModal(modalDialog(
      title = paste("Log Ad Hoc Meeting —", resident_info$full_name),
      size  = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_adhoc_meeting", "Submit", class = "btn-primary",
                     icon = icon("save"))
      ),

      dateInput("adhoc_meet_date", "Meeting Date:", value = Sys.Date(), width = "100%"),

      textAreaInput("adhoc_meet_summary", "Meeting Summary (required):",
                    rows = 4, width = "100%",
                    placeholder = "Summarize the meeting discussion..."),

      textAreaInput("adhoc_meet_wellness", "Wellness Notes (optional):",
                    rows = 3, width = "100%", placeholder = ""),

      textAreaInput("adhoc_meet_career", "Career Notes (optional):",
                    rows = 3, width = "100%", placeholder = ""),

      textAreaInput("adhoc_meet_milestone", "Milestone / Goals Notes (optional):",
                    rows = 3, width = "100%", placeholder = "")
    ))
  })

  observeEvent(input$submit_adhoc_meeting, {
    rid <- selected_resident_id()
    req(rid)

    # Validate required summary field
    if (is.null(input$adhoc_meet_summary) ||
        nchar(trimws(input$adhoc_meet_summary)) == 0) {
      showNotification("Meeting summary is required.", type = "warning", duration = 5)
      return()
    }

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    result <- submit_adhoc_meeting_notes(
      redcap_url      = REDCAP_CONFIG$url,
      redcap_token    = REDCAP_CONFIG$rdm_token,
      record_id       = rid,
      meeting_date    = input$adhoc_meet_date,
      meeting_notes   = input$adhoc_meet_summary,
      wellness_notes  = input$adhoc_meet_wellness,
      career_notes    = input$adhoc_meet_career,
      milestone_notes = input$adhoc_meet_milestone
    )

    if (result$success) {
      removeModal()
      showNotification(
        paste("Ad hoc meeting logged for", resident_info$full_name),
        type = "message", duration = 4
      )
      tryCatch({
        app_data(load_ccc_data())
      }, error = function(e) {
        showNotification("Meeting saved; could not refresh data automatically.",
                         type = "warning", duration = 5)
      })
    } else {
      showNotification(paste("Error saving meeting:", result$message),
                       type = "error", duration = 10)
    }
  })

  # ===========================================================================
  # FEATURE 1: FOLLOW-UP TRACKER — reactive state
  # ===========================================================================

  tracker_filter        <- reactiveVal("all")   # "all","interim","concern","initiation","ongoing","resolved","recurring"
  tracker_display_table <- reactiveVal(NULL)     # current (filtered) summary data frame
  tracker_selected_rid  <- reactiveVal(NULL)     # record_id of row clicked in action table

  # Filter button observers
  observeEvent(input$tracker_filter_all,        { tracker_filter("all") })
  observeEvent(input$tracker_filter_interim,    { tracker_filter("interim") })
  observeEvent(input$tracker_filter_concern,    { tracker_filter("concern") })
  observeEvent(input$tracker_filter_initiation, { tracker_filter("initiation") })
  observeEvent(input$tracker_filter_ongoing,    { tracker_filter("ongoing") })
  observeEvent(input$tracker_filter_resolved,   { tracker_filter("resolved") })
  observeEvent(input$tracker_filter_recurring,  { tracker_filter("recurring") })

  # ===========================================================================
  # FOLLOW-UP TRACKER — value boxes
  # ===========================================================================

  tracker_summary_data <- reactive({
    get_tracker_summary(app_data())
  })

  output$tracker_vb_interim    <- renderText({
    s <- tracker_summary_data()
    if (is.null(s) || nrow(s) == 0) return("0")
    as.character(sum(s$is_interim, na.rm = TRUE))
  })
  output$tracker_vb_initiation <- renderText({
    s <- tracker_summary_data()
    if (is.null(s) || nrow(s) == 0) return("0")
    as.character(sum(s$is_initiation, na.rm = TRUE))
  })
  output$tracker_vb_ongoing    <- renderText({
    s <- tracker_summary_data()
    if (is.null(s) || nrow(s) == 0) return("0")
    as.character(sum(s$is_ongoing, na.rm = TRUE))
  })
  output$tracker_vb_resolved   <- renderText({
    s <- tracker_summary_data()
    if (is.null(s) || nrow(s) == 0) return("0")
    as.character(sum(s$is_resolved, na.rm = TRUE))
  })
  output$tracker_vb_recurring  <- renderText({
    s <- tracker_summary_data()
    if (is.null(s) || nrow(s) == 0) return("0")
    as.character(sum(s$is_recurring, na.rm = TRUE))
  })

  # ===========================================================================
  # FOLLOW-UP TRACKER — Action Items table
  # ===========================================================================

  output$tracker_action_table <- DT::renderDT({
    summary_df <- tracker_summary_data()
    active_filter <- tracker_filter()

    if (is.null(summary_df) || nrow(summary_df) == 0) {
      empty_df <- data.frame(
        Resident = character(0), Level = character(0), Coach = character(0),
        Type = character(0), Concern = character(0), `Last Review` = character(0),
        `Current Status` = character(0), `All Actions` = character(0),
        `Person Responsible` = character(0), `Follow-up Notes` = character(0),
        check.names = FALSE
      )
      tracker_display_table(empty_df)
      return(DT::datatable(empty_df, rownames = FALSE,
                            options = list(dom = "t", pageLength = 25)))
    }

    # Apply filter
    filtered <- switch(active_filter,
      "interim"    = summary_df[summary_df$is_interim,    ],
      "concern"    = summary_df[summary_df$is_concern,    ],
      "initiation" = summary_df[summary_df$is_initiation, ],
      "ongoing"    = summary_df[summary_df$is_ongoing,    ],
      "resolved"   = summary_df[summary_df$is_resolved,   ],
      "recurring"  = summary_df[summary_df$is_recurring,  ],
      summary_df   # "all"
    )

    if (is.null(filtered) || nrow(filtered) == 0) {
      filtered <- summary_df[FALSE, ]
    }

    tracker_display_table(filtered)

    display_cols <- c("Resident", "Level", "Coach", "Type", "Concern",
                      "Last Review", "Current Status", "All Actions",
                      "Person Responsible", "Follow-up Notes")

    DT::datatable(
      filtered[, display_cols, drop = FALSE],
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 25,
        dom        = "ft",
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "150px", targets = 0),           # Resident
          list(width = "110px", targets = c(1, 2)),     # Level, Coach
          list(width = "85px",  targets = c(3, 4, 5)),  # Type, Concern, Last Review
          list(width = "130px", targets = c(6, 7, 8)),  # Status, Actions, Person
          list(width = "200px", targets = 9)            # Notes
        )
      )
    ) %>%
      DT::formatStyle("Type",
        backgroundColor = DT::styleEqual(c("Interim", "Scheduled"),
                                         c("#ede7f6", "transparent")),
        color           = DT::styleEqual(c("Interim", "Scheduled"),
                                         c("#4a1d8e", "inherit"))
      ) %>%
      DT::formatStyle("Concern",
        color      = DT::styleEqual(c("Yes", "—"), c("#dc3545", "#adb5bd")),
        fontWeight = DT::styleEqual(c("Yes", "—"), c("bold", "normal"))
      )
  })

  # Row click → open update/add-issue modal
  observeEvent(input$tracker_action_table_rows_selected, {
    idx <- input$tracker_action_table_rows_selected
    tbl <- tracker_display_table()
    if (is.null(tbl) || is.null(idx) || nrow(tbl) < idx) return()

    rid <- tbl$record_id[idx]
    tracker_selected_rid(rid)

    resident_name <- tbl$Resident[idx]

    # Get all qualifying instances for this resident
    ccc_all <- app_data()$all_forms$ccc_review
    if (is.null(ccc_all)) {
      showNotification("No CCC review data available.", type = "warning"); return()
    }

    res_reviews <- ccc_all %>%
      filter(record_id == rid, redcap_repeat_instrument == "ccc_review")

    action_cols_p <- intersect(paste0("ccc_action___", 1:8), names(res_reviews))
    has_action <- if (length(action_cols_p) > 0) {
      apply(res_reviews[, action_cols_p, drop = FALSE], 1,
            function(r) any(!is.na(r) & r == "1"))
    } else rep(FALSE, nrow(res_reviews))
    has_int <- !is.na(res_reviews$ccc_rev_type) & res_reviews$ccc_rev_type == "2"
    has_con <- "ccc_concern" %in% names(res_reviews) &
               !is.na(res_reviews$ccc_concern) & res_reviews$ccc_concern == "1"

    qualifying_reviews <- res_reviews[has_int | has_con | has_action, ]

    if (nrow(qualifying_reviews) == 0) {
      showNotification("No qualifying review records found for this resident.",
                       type = "info"); return()
    }

    # Build instance choice labels
    instance_choices <- setNames(
      as.character(qualifying_reviews$redcap_repeat_instance),
      paste0(
        "Instance ", qualifying_reviews$redcap_repeat_instance,
        " — ",
        ifelse(!is.na(qualifying_reviews$ccc_date) & qualifying_reviews$ccc_date != "",
               as.character(qualifying_reviews$ccc_date), "no date"),
        " (",
        ifelse(!is.na(qualifying_reviews$ccc_rev_type) & qualifying_reviews$ccc_rev_type == "2",
               "Interim", "Scheduled"),
        ")"
      )
    )

    # Pre-populate status from the first (most recent) instance
    default_inst_row <- qualifying_reviews[
      order(qualifying_reviews$ccc_date, decreasing = TRUE, na.last = TRUE), ][1, ]
    default_instance <- as.character(default_inst_row$redcap_repeat_instance[1])

    default_status <- c()
    for (n in 1:4) {
      sc <- paste0("ccc_action_status___", n)
      if (sc %in% names(default_inst_row) &&
          !is.na(default_inst_row[[sc]][1]) && default_inst_row[[sc]][1] == "1") {
        default_status <- c(default_status, as.character(n))
      }
    }

    default_notes <- if ("ccc_issues_follow_up" %in% names(default_inst_row))
      as.character(default_inst_row$ccc_issues_follow_up[1]) else ""
    if (is.na(default_notes)) default_notes <- ""

    # Choices for "Add New Issue"
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency", for_ui = TRUE)
    action_choices_ui  <- get_field_choices(app_data()$data_dict, "ccc_action",     for_ui = TRUE)
    status_choices_ui  <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    if (length(competency_choices) == 0)
      competency_choices <- c("PC1"="1","PC2"="2","PC3"="3","PC4"="4",
                               "PC5"="5","PC6"="6","MK1"="7")
    if (length(action_choices_ui) == 0)
      action_choices_ui <- setNames(as.character(1:8), CCC_ACTION_LABELS)
    if (length(status_choices_ui) == 0)
      status_choices_ui <- setNames(as.character(1:4), CCC_STATUS_LABELS)

    # ---- Build inline history table ----------------------------------------
    hist_sorted <- qualifying_reviews[
      order(qualifying_reviews$ccc_date, decreasing = TRUE, na.last = TRUE), ]

    status_cols_p <- intersect(paste0("ccc_action_status___", 1:4), names(hist_sorted))

    hist_rows <- lapply(seq_len(nrow(hist_sorted)), function(i) {
      row <- hist_sorted[i, ]

      date_val <- if (!is.na(row$ccc_date) && row$ccc_date != "")
        as.character(row$ccc_date) else "—"

      type_val <- if (!is.na(row$ccc_rev_type) && row$ccc_rev_type == "2")
        tags$span(class = "badge", style = "background:#6f42c1; color:#fff;", "Interim")
      else
        tags$span(class = "badge", style = "background:#0d6efd; color:#fff;", "Scheduled")

      status_labels <- c()
      for (sc in status_cols_p) {
        n <- gsub("ccc_action_status___", "", sc)
        if (!is.na(row[[sc]]) && row[[sc]] == "1")
          status_labels <- c(status_labels, CCC_STATUS_LABELS[n])
      }
      status_val <- if (length(status_labels) > 0)
        paste(status_labels, collapse = ", ") else "—"

      notes_val <- if ("ccc_issues_follow_up" %in% names(row) &&
                       !is.na(row$ccc_issues_follow_up) &&
                       nchar(trimws(row$ccc_issues_follow_up)) > 0)
        as.character(row$ccc_issues_follow_up) else "—"

      tags$tr(
        tags$td(date_val,   style = "white-space:nowrap; width:100px;"),
        tags$td(type_val,   style = "white-space:nowrap; width:90px;"),
        tags$td(status_val, style = "width:140px;"),
        tags$td(notes_val,  style = "font-size:0.9rem; color:#374151;")
      )
    })

    history_table <- tags$div(
      style = "max-height:220px; overflow-y:auto; margin-bottom:4px;",
      tags$table(
        class = "table table-sm table-striped table-hover mb-0",
        style = "font-size:0.95rem;",
        tags$thead(
          class = "table-light",
          tags$tr(
            tags$th("Date"), tags$th("Type"), tags$th("Status"), tags$th("Follow-up Notes")
          )
        ),
        do.call(tags$tbody, hist_rows)
      )
    )
    # -----------------------------------------------------------------------

    showModal(modalDialog(
      title     = paste("Action Items —", resident_name),
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Close"),

      # History at the top
      h5("Review History", style = "font-weight:700; margin-bottom:8px;"),
      history_table,
      tags$hr(style = "margin:16px 0 14px;"),

      tabsetPanel(
        id = "tracker_modal_tabs",

        # Tab 1: Add Status Update (creates a new interim instance)
        tabPanel("Add Status Update",
          br(),
          tags$p(class = "text-muted", style = "font-size:0.875rem;",
            icon("info-circle"), " This will create a new interim record capturing the updated status.",
            if (nchar(default_notes) > 0)
              tags$span(paste0(" Last note: \u201c", substr(default_notes, 1, 80),
                               if (nchar(default_notes) > 80) "\u2026" else "", "\u201d"))
          ),
          checkboxGroupInput("tracker_update_status",
                             "New Status (check all that apply):",
                             choices  = c("Initiation"="1","Ongoing"="2",
                                          "Resolved"="3","Recurring"="4"),
                             selected = default_status, inline = TRUE),
          textAreaInput("tracker_update_notes", "Follow-up Notes:",
                        value = "", rows = 4, width = "100%",
                        placeholder = "Describe the current status and any follow-up actions..."),
          textInput("tracker_update_person_resp", "Person Responsible:", width = "100%"),
          br(),
          actionButton("tracker_save_update", "Add Status Update",
                       class = "btn-primary", icon = icon("plus-circle"))
        ),

        # Tab 2: Add New Issue
        tabPanel("Add New Issue",
          br(),
          textAreaInput("tracker_new_description", "Issue Description:",
                        rows = 3, width = "100%",
                        placeholder = "Describe the new issue..."),
          checkboxGroupInput("tracker_new_competency", "Competency Area(s):",
                             choices = competency_choices),
          checkboxGroupInput("tracker_new_actions", "Action(s) Required:",
                             choices = action_choices_ui),
          checkboxGroupInput("tracker_new_status", "Initial Status:",
                             choices = status_choices_ui, inline = TRUE),
          textInput("tracker_new_person_resp", "Person Responsible:", width = "100%"),
          textAreaInput("tracker_new_notes", "Follow-up Notes:",
                        rows = 3, width = "100%"),
          br(),
          actionButton("tracker_add_issue", "Add Issue",
                       class = "btn-success", icon = icon("plus-circle"))
        )
      )
    ))
  })

  # Save status update — creates a NEW ccc_review interim instance
  observeEvent(input$tracker_save_update, {
    rid <- tracker_selected_rid()
    req(rid)

    notes_text <- if (!is.null(input$tracker_update_notes))
      trimws(input$tracker_update_notes) else ""

    description <- if (nchar(notes_text) > 0) notes_text else "Status update"

    result <- submit_new_tracker_issue(
      redcap_url   = REDCAP_CONFIG$url,
      redcap_token = REDCAP_CONFIG$rdm_token,
      record_id    = rid,
      description  = description,
      status       = input$tracker_update_status,
      person_resp  = input$tracker_update_person_resp,
      notes        = notes_text
    )

    if (result$success) {
      removeModal()
      showNotification("Status update recorded as new entry.", type = "message", duration = 4)
      tryCatch({
        app_data(load_ccc_data())
      }, error = function(e) {
        showNotification("Saved; could not auto-refresh data.",
                         type = "warning", duration = 5)
      })
    } else {
      showNotification(paste("Error saving status update:", result$message),
                       type = "error", duration = 10)
    }
  })

  # Add new issue
  observeEvent(input$tracker_add_issue, {
    rid <- tracker_selected_rid()
    req(rid)

    if (is.null(input$tracker_new_description) ||
        nchar(trimws(input$tracker_new_description)) == 0) {
      showNotification("Issue description is required.", type = "warning", duration = 5)
      return()
    }

    result <- submit_new_tracker_issue(
      redcap_url  = REDCAP_CONFIG$url,
      redcap_token = REDCAP_CONFIG$rdm_token,
      record_id   = rid,
      description = input$tracker_new_description,
      actions     = input$tracker_new_actions,
      status      = input$tracker_new_status,
      competency  = input$tracker_new_competency,
      person_resp = input$tracker_new_person_resp,
      notes       = input$tracker_new_notes
    )

    if (result$success) {
      removeModal()
      showNotification("New issue added successfully.", type = "message", duration = 4)
      tryCatch({
        app_data(load_ccc_data())
      }, error = function(e) {
        showNotification("Issue saved; could not auto-refresh data.",
                         type = "warning", duration = 5)
      })
    } else {
      showNotification(paste("Error adding issue:", result$message),
                       type = "error", duration = 10)
    }
  })

  # ===========================================================================
  # INTERIM REVIEWS — CCC Review table (one row per resident, most recent review)
  # ===========================================================================

  # Store the exact tbl used to render the DT so the row-click observer can
  # do a reliable record_id lookup (avoids index drift from independent sorts)
  ccc_review_tbl_data <- reactiveVal(NULL)

  output$tracker_ccc_review_table <- DT::renderDT({
    tryCatch({
    all_reviews <- tryCatch(
      get_ccc_review_all(app_data()),
      error = function(e) { warning("get_ccc_review_all failed: ", e$message); data.frame() }
    )

    trunc80 <- function(x) {
      s <- trimws(as.character(x))
      if (is.na(s) || s == "NA" || nchar(s) == 0) return("")
      if (nchar(s) > 80) paste0(substr(s, 1, 80), "\u2026") else s
    }

    # ── All residents as the base (ensures interns etc. always appear) ────────
    res_base <- app_data()$residents %>%
      filter(!is.na(full_name) & nchar(trimws(full_name)) > 0) %>%
      mutate(
        record_id = trimws(as.character(record_id)),
        PGY = dplyr::case_when(
          grepl("Intern",          current_period, ignore.case = TRUE) ~ "PGY1",
          grepl("PGY2",            current_period, ignore.case = TRUE) ~ "PGY2",
          grepl("PGY3|Graduating", current_period, ignore.case = TRUE) ~ "PGY3",
          TRUE ~ ""
        )
      ) %>%
      select(record_id, Resident = full_name, PGY)

    # Most recent CCC review per resident (NULL-safe)
    recent_rev <- if (!is.null(all_reviews) && nrow(all_reviews) > 0) {
      all_reviews %>%
        mutate(record_id = trimws(as.character(record_id))) %>%
        arrange(dplyr::desc(dplyr::coalesce(Date, ""))) %>%
        group_by(record_id) %>%
        slice(1) %>%
        ungroup() %>%
        select(record_id, Date, Type, Session, Status,
               `Progress Notes`, ILP, `Follow-up Notes`, `Person Resp`, Competency)
    } else {
      data.frame(
        record_id = character(0), Date = character(0),
        Type = character(0), Session = character(0), Status = character(0),
        `Progress Notes` = character(0), ILP = character(0),
        `Follow-up Notes` = character(0), `Person Resp` = character(0),
        Competency = character(0),
        check.names = FALSE
      )
    }

    tbl <- res_base %>%
      left_join(recent_rev, by = "record_id") %>%
      mutate(
        `Last Review Date` = if_else(is.na(Date), "", as.character(Date)),
        `Last Review` = dplyr::case_when(
          is.na(Type) | nchar(trimws(Type)) == 0 ~ "",
          !is.na(Session) & nchar(trimws(Session)) > 0 ~
            paste0(Type, " \u00b7 ", Session),
          TRUE ~ Type
        ),
        # Notes: ccc_interim if last was Interim, ccc_ilp if Scheduled
        Notes = mapply(function(type, interim, ilp) {
          raw <- if (!is.na(type) && grepl("Interim", type, ignore.case = TRUE)) interim else ilp
          trunc80(raw)
        }, Type, `Progress Notes`, ILP, SIMPLIFY = TRUE),
        # Follow-up is always ccc_issues_follow_up
        `Follow-up` = vapply(`Follow-up Notes`, trunc80, character(1)),
        Person = vapply(`Person Resp`, function(x) {
          s <- trimws(as.character(x))
          if (is.na(s) || s == "NA") "" else s
        }, character(1))
      ) %>%
      mutate(
        Competency = vapply(Competency, function(x) {
          s <- trimws(as.character(x))
          if (is.na(s) || s == "NA" || nchar(s) == 0) return("")
          if (nchar(s) > 60) paste0(substr(s, 1, 60), "\u2026") else s
        }, character(1))
      ) %>%
      select(record_id, Resident, PGY, `Last Review Date`, `Last Review`,
             Status, Competency, Notes, `Follow-up`, Person) %>%
      arrange(Resident)

    # Cache the exact data used for rendering so the row-click observer can
    # do a direct record_id lookup by row index (independent of client sort)
    ccc_review_tbl_data(tbl)

    # No HTML in cells — use formatStyle for all visual treatment
    # (escape=TRUE is safe, no JS serialization issues on row click)
    DT::datatable(
      tbl,
      selection = "single",
      rownames  = FALSE,
      escape    = TRUE,
      class     = "cell-border",
      options   = list(
        paging  = FALSE,
        dom     = "ft",
        scrollX = TRUE,
        order   = list(list(1, "asc")),
        columnDefs = list(
          list(visible = FALSE, targets = 0),   # record_id
          list(width = "170px", targets = 1),   # Resident
          list(width = "55px",  targets = 2),   # PGY
          list(width = "105px", targets = 3),   # Last Review Date
          list(width = "165px", targets = 4),   # Last Review
          list(width = "110px", targets = 5),   # Status
          list(width = "160px", targets = 6),   # Competency
          list(width = "220px", targets = 7),   # Notes
          list(width = "220px", targets = 8),   # Follow-up
          list(width = "115px", targets = 9),   # Person
          list(className = "dt-body-left", targets = "_all")
        )
      )
    ) %>%
      DT::formatStyle("Resident",
        fontWeight = "700", fontSize = "1.0rem") %>%
      DT::formatStyle("Last Review Date",
        fontWeight = "500", fontSize = "0.97rem", color = "#2d3748") %>%
      DT::formatStyle("Last Review",
        fontWeight = "600", color = "#2d3748") %>%
      DT::formatStyle("Status",
        backgroundColor = DT::styleEqual(
          c("Initiation", "Ongoing",  "Resolved", "Recurring"),
          c("#fff3cd",    "#cff4fc",  "#d1e7dd",  "#f8d7da"),
          default = ""
        ),
        color = DT::styleEqual(
          c("Initiation", "Ongoing",  "Resolved", "Recurring"),
          c("#7a5c00",    "#0a4a5e",  "#0f5132",  "#842029"),
          default = "#495057"
        ),
        fontWeight = "600",
        borderRadius = "20px",
        padding = "3px 8px") %>%
      DT::formatStyle("Notes",
        fontSize = "0.95rem", color = "#4a5568") %>%
      DT::formatStyle("Follow-up",
        fontSize = "0.95rem", color = "#4a5568") %>%
      DT::formatStyle("Person",
        fontSize = "0.95rem", color = "#4a5568") %>%
      DT::formatStyle("Competency",
        fontSize = "0.88rem", color = "#6f42c1", fontStyle = "italic") %>%
      DT::formatStyle("PGY",
        fontWeight = "600", fontSize = "0.95rem", color = "#0066a1")
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Table error:", e$message)))
    })
  })

  # ===========================================================================
  # INTERIM REVIEWS — row click → new interim entry modal
  # ===========================================================================

  interim_edit_rid <- reactiveVal(NULL)

  observeEvent(input$tracker_ccc_review_table_rows_selected, {
    tryCatch({
    idx <- input$tracker_ccc_review_table_rows_selected
    req(length(idx) > 0)
    message("MODAL[1] row clicked idx=", idx)

    # Use the cached render data for a reliable record_id lookup
    tbl_snap <- isolate(ccc_review_tbl_data())
    req(!is.null(tbl_snap))
    if (idx > nrow(tbl_snap)) { message("MODAL[1b] idx out of range, nrow=", nrow(tbl_snap)); return() }
    rid <- as.character(tbl_snap$record_id[idx])
    message("MODAL[2] rid=", rid)
    interim_edit_rid(rid)

    # Snapshot app_data once for everything below
    dat <- isolate(app_data())

    # ── Resident display name ─────────────────────────────────────────────────
    nm_val   <- dat$residents %>% filter(record_id == rid) %>% pull(full_name)
    res_name <- if (length(nm_val) > 0 && !is.na(nm_val[1])) nm_val[1] else rid
    message("MODAL[3] res_name=", res_name)

    # ── History table: ALL records for this resident ──────────────────────────
    raw_all <- dat$all_forms$ccc_review

    hist_fields <- list(
      list("ccc_interim",          "Discussion Summary"),
      list("ccc_fu_resp",          "Person Responsible"),
      list("ccc_issues_follow_up", "Follow-up / Action Items"),
      list("__competency__",       "Competency")   # special: derived from checkboxes
    )

    past <- if (!is.null(raw_all)) {
      raw_all %>%
        filter(record_id == rid, redcap_repeat_instrument == "ccc_review") %>%
        arrange(desc(ccc_date))
    } else data.frame()

    message("MODAL[4] past nrow=", nrow(past))
    history_table_ui <- if (nrow(past) == 0) {
      div(
        style = "color:#6c757d; font-size:0.85rem; padding:8px 0;",
        tags$i(class = "bi bi-clock-history me-1"),
        "No prior CCC records for this resident."
      )
    } else {
      th_style <- paste0(
        "font-size:0.75rem; font-weight:700; color:#6c757d; text-transform:uppercase;",
        "letter-spacing:0.05em; padding:6px 10px; border-bottom:2px solid #dee2e6;",
        "white-space:nowrap; background:#f8f9fa; position:sticky; top:0;"
      )
      td_style <- "font-size:0.85rem; color:#2d3748; padding:6px 10px; vertical-align:top; border-bottom:1px solid #f0f4f8;"
      td_meta  <- paste0(td_style, " white-space:nowrap; color:#6c757d;")

      header_row <- tags$tr(
        tags$th(style = th_style, "Date"),
        tags$th(style = th_style, "Type / Session"),
        lapply(hist_fields, function(fl) tags$th(style = th_style, fl[[2]]))
      )

      # Pre-build competency label map for history rows (code -> label)
      hist_comp_map <- tryCatch({
        cs <- dat$data_dict %>%
          filter(field_name == "ccc_competency") %>%
          pull(select_choices_or_calculations)
        if (length(cs) > 0 && !is.na(cs[1]) && nchar(cs[1]) > 0) {
          pairs <- strsplit(cs[1], "\\|")[[1]]
          m <- list()
          for (p in pairs) {
            parts <- strsplit(trimws(p), ",", fixed = TRUE)[[1]]
            if (length(parts) >= 2)
              m[[trimws(parts[1])]] <- trimws(paste(parts[-1], collapse = ","))
          }
          if (length(m) > 0) m else NULL
        } else NULL
      }, error = function(e) NULL)

      body_rows <- lapply(seq_len(nrow(past)), function(i) {
        hr_row   <- past[i, ]
        h_date   <- clean_str(if ("ccc_date"    %in% names(hr_row)) hr_row$ccc_date[1]    else "")
        h_sess   <- clean_str(if ("ccc_session" %in% names(hr_row)) hr_row$ccc_session[1] else "")
        h_type   <- if ("ccc_rev_type" %in% names(hr_row) && !is.na(hr_row$ccc_rev_type[1]) &&
                        hr_row$ccc_rev_type[1] == "2") "Interim" else "Scheduled"
        meta_str <- paste0(h_type, if (nchar(h_sess) > 0) paste0(" \u00b7 ", h_sess) else "")

        tags$tr(
          tags$td(style = td_meta, h_date),
          tags$td(style = td_meta, meta_str),
          lapply(hist_fields, function(fl) {
            val <- if (fl[[1]] == "__competency__") {
              lbls <- c()
              for (n in 1:7) {
                col <- paste0("ccc_competency___", n)
                if (col %in% names(hr_row) && !is.na(hr_row[[col]][1]) && hr_row[[col]][1] == "1") {
                  lbl <- if (!is.null(hist_comp_map) && !is.null(hist_comp_map[[as.character(n)]]))
                    hist_comp_map[[as.character(n)]] else as.character(n)
                  lbls <- c(lbls, lbl)
                }
              }
              if (length(lbls) > 0) paste(lbls, collapse = ", ") else ""
            } else {
              # Sanitize and truncate — removes control chars that break JSON/WS
              raw <- if (fl[[1]] %in% names(hr_row)) hr_row[[fl[[1]]]][1] else ""
              s   <- clean_str(raw)
              s   <- gsub("[[:cntrl:]]", " ", s)  # strip control characters (incl. null bytes)
              if (nchar(s) > 300) paste0(substr(s, 1, 300), "\u2026") else s
            }
            tags$td(style = paste0(td_style, " white-space:pre-wrap;"), val)
          })
        )
      })

      div(
        style = "max-height:260px; overflow-y:auto; border:1px solid #dee2e6; border-radius:4px;",
        tags$table(
          style = "width:100%; border-collapse:collapse;",
          tags$thead(header_row),
          tags$tbody(body_rows)
        )
      )
    }

    message("MODAL[5] history_table_ui built")
    # ── Access code + dashboard link ─────────────────────────────────────────
    ac_val <- tryCatch({
      res_row <- dat$residents %>% filter(record_id == rid)
      if ("access_code" %in% names(res_row) && nrow(res_row) > 0) {
        v <- res_row$access_code[1]
        if (!is.na(v) && nchar(trimws(v)) > 0) trimws(v) else ""
      } else ""
    }, error = function(e) "")
    dash_url <- paste0(
      "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud",
      if (nchar(ac_val) > 0) paste0("?code=", ac_val) else ""
    )

    # ── ABIM risk (nomogram: McDonald et al. 2020) ───────────────────────────
    .ccc_pass_prob <- function(pct, pgy) {
      params <- list(
        `1` = list(b0 = -7.0641,  b1 = 0.1811),
        `2` = list(b0 = -8.1445,  b1 = 0.1733),
        `3` = list(b0 = -11.9491, b1 = 0.2173)
      )
      p <- params[[as.character(pgy)]]
      if (is.null(p)) return(NA_real_)
      round(100 / (1 + exp(-(p$b0 + p$b1 * pct))), 1)
    }

    # ── Shared quick-links (always shown) ────────────────────────────────────
    quick_links_ui <- div(
      style = "display:flex; flex-direction:column; gap:8px; min-width:190px;",
      # MKSAP button
      tags$a(
        href   = "https://mksap.acponline.org/login?forward=%2Ftracker#/",
        target = "_blank",
        style  = paste0(
          "display:flex; align-items:center; justify-content:center; gap:7px;",
          "padding:9px 16px; border-radius:7px; font-size:0.88rem; font-weight:600;",
          "color:#0066a1; background:#ebf8ff; border:1.5px solid #90cdf4;",
          "text-decoration:none; white-space:nowrap; transition:background 0.15s;"
        ),
        tags$i(class = "bi bi-journal-medical", style = "font-size:1rem;"),
        "MKSAP Tracker \u2197"
      ),
      # Resident dashboard button — prominent
      tags$a(
        href   = dash_url,
        target = "_blank",
        style  = paste0(
          "display:flex; align-items:center; justify-content:center; gap:7px;",
          "padding:9px 16px; border-radius:7px; font-size:0.88rem; font-weight:700;",
          "color:#ffffff; background:linear-gradient(135deg,#003d5c 0%,#0066a1 100%);",
          "border:none; text-decoration:none; white-space:nowrap;",
          "box-shadow:0 2px 8px rgba(0,102,161,0.35); transition:opacity 0.15s;"
        ),
        tags$i(class = "bi bi-person-video3", style = "font-size:1rem;"),
        paste0("Open ", res_name, "\u2019s Dashboard \u2197")
      )
    )

    message("MODAL[6] dash_url built, calling abim_ui")
    abim_ui <- tryCatch({
      td <- dat$all_forms$test_data
      td_row <- if (!is.null(td)) td %>% filter(record_id == rid) else NULL
      if (!is.null(td_row) && nrow(td_row) > 0) {
        pcts <- c(
          "1" = suppressWarnings(as.numeric(td_row$pgy1_tot_correct[1])),
          "2" = suppressWarnings(as.numeric(td_row$pgy2_tot_correct[1])),
          "3" = suppressWarnings(as.numeric(td_row$pgy3_tot_correct[1]))
        )
        valid <- pcts[!is.na(pcts) & pcts > 0]
        if (length(valid) > 0) {
          last_pgy  <- names(valid)[length(valid)]
          last_pct  <- valid[[last_pgy]]
          probs     <- sapply(names(valid), function(g) .ccc_pass_prob(valid[[g]], g))
          best_prob <- max(probs, na.rm = TRUE)
          risk_col  <- if (best_prob < 50) "#d32f2f" else if (best_prob < 75) "#e65100" else "#2e7d32"
          risk_bg   <- if (best_prob < 50) "#fff5f5" else if (best_prob < 75) "#fffaf0" else "#f0fff4"
          risk_lbl  <- if (best_prob < 50) "High Risk" else if (best_prob < 75) "Moderate Risk" else "Low Risk"

          div(
            style = "display:flex; align-items:stretch; gap:12px; flex-wrap:wrap;",
            # ITE score card
            div(
              style = paste0(
                "background:#f8faff; border:1.5px solid #c3d9f0; border-radius:8px;",
                "padding:10px 18px; min-width:140px; flex:1;"
              ),
              tags$div(
                style = "font-size:0.72rem; font-weight:700; color:#4a6785;
                         text-transform:uppercase; letter-spacing:0.07em; margin-bottom:3px;",
                paste0("ITE Score \u00b7 PGY-", last_pgy)
              ),
              tags$div(
                style = "font-size:1.5rem; font-weight:800; color:#003d5c; line-height:1.1;",
                paste0(round(last_pct, 1), "%")
              ),
              tags$div(
                style = "font-size:0.75rem; color:#6c757d; margin-top:2px;",
                "% correct on in-training exam"
              )
            ),
            # Pass probability card
            div(
              style = paste0(
                "background:", risk_bg, "; border:1.5px solid ", risk_col, "60;",
                "border-radius:8px; padding:10px 18px; min-width:160px; flex:1;"
              ),
              tags$div(
                style = "font-size:0.72rem; font-weight:700; color:#4a6785;
                         text-transform:uppercase; letter-spacing:0.07em; margin-bottom:3px;",
                "P(Pass ABIM)"
              ),
              tags$div(
                tags$span(
                  style = paste0("font-size:1.5rem; font-weight:800; color:", risk_col, "; line-height:1.1;"),
                  paste0(round(best_prob, 1), "%")
                )
              ),
              div(
                style = paste0(
                  "display:inline-block; margin-top:4px; padding:2px 8px; border-radius:20px;",
                  "background:", risk_col, "; color:#fff;",
                  "font-size:0.72rem; font-weight:700; letter-spacing:0.04em;"
                ),
                risk_lbl
              )
            ),
            # Quick links column
            quick_links_ui
          )
        } else {
          div(style = "display:flex; align-items:center; gap:12px;",
              div(style = "font-size:0.88rem; color:#6c757d; font-style:italic;",
                  "No ITE data on file."),
              quick_links_ui)
        }
      } else {
        div(style = "display:flex; align-items:center; gap:12px;",
            div(style = "font-size:0.88rem; color:#6c757d; font-style:italic;",
                "No ITE data on file."),
            quick_links_ui)
      }
    }, error = function(e) {
      div(style = "display:flex; gap:12px;", quick_links_ui)
    })

    message("MODAL[7] abim_ui built, getting choices")
    # ── Choices from data dict ───────────────────────────────────────────────
    dd                 <- dat$data_dict
    action_choices     <- get_field_choices(dd, "ccc_action",        for_ui = TRUE)
    competency_choices <- get_field_choices(dd, "ccc_competency",    for_ui = TRUE)
    status_choices     <- get_field_choices(dd, "ccc_action_status", for_ui = TRUE)

    if (length(action_choices) == 0)
      action_choices <- setNames(as.character(1:8), unname(CCC_ACTION_LABELS))
    if (length(status_choices) == 0)
      status_choices <- c("Initiation" = "1", "Ongoing" = "2", "Resolved" = "3", "Recurring" = "4")

    # Estimate modal HTML size — large messages can trip Posit Connect WS limits
    tryCatch({
      modal_html_size <- nchar(paste(capture.output(print(
        modalDialog(title=res_name, history_table_ui, abim_ui)
      )), collapse=""))
      message(sprintf("MODAL[8] calling showModal for %s | est. modal HTML chars: %d",
                      res_name, modal_html_size))
    }, error = function(e) message("MODAL[8] calling showModal for ", res_name))
    showModal(modalDialog(
      title = div(
        style = "display:flex; align-items:center; gap:12px;",
        tags$i(class = "bi bi-person-lines-fill",
               style = "color:rgba(255,255,255,0.9); font-size:1.4rem;"),
        div(
          tags$span(res_name,
                    style = "font-weight:700; font-size:1.2rem; color:#ffffff; display:block;"),
          tags$span("New Interim Review",
                    style = "font-size:0.8rem; color:rgba(255,255,255,0.7); font-weight:400;")
        )
      ),
      size      = "xl",
      easyClose = TRUE,
      footer    = tagList(
        actionButton("interim_save_btn", "Save",
                     icon = icon("floppy-disk"), class = "btn-primary"),
        modalButton("Cancel")
      ),

      # ── History table ─────────────────────────────────────────────────────
      tags$p(
        style = "font-size:0.78rem; font-weight:700; color:#6c757d;
                 text-transform:uppercase; letter-spacing:0.06em; margin-bottom:8px;",
        tags$i(class = "bi bi-clock-history me-1"), "Prior CCC Notes"
      ),
      history_table_ui,

      # ── ABIM risk + MKSAP ─────────────────────────────────────────────────
      div(style = "margin-top:12px;", abim_ui),

      tags$hr(style = "margin: 16px 0 14px;"),

      # ── New entry form ────────────────────────────────────────────────────
      tags$p(
        style = "font-size:0.78rem; font-weight:700; color:#6c757d;
                 text-transform:uppercase; letter-spacing:0.06em; margin-bottom:12px;",
        "New Interim Entry"
      ),

      textAreaInput("interim_progress_notes", "Discussion Summary:",
                    value = "", rows = 4, width = "100%",
                    placeholder = "Summary of the CCC discussion\u2026"),

      tags$hr(style = "margin: 14px 0 12px;"),

      # ── Action needed ─────────────────────────────────────────────────────
      checkboxInput("interim_action_needed",
                    tags$span(tags$strong("Action Needed")),
                    value = FALSE),

      conditionalPanel(
        condition = "input.interim_action_needed == true",
        div(
          style = "background:#f8f9fa; border:1px solid #dee2e6; border-radius:6px;
                   padding:14px 16px; margin-top:8px; margin-bottom:12px;",

          checkboxGroupInput("interim_status", "Action Status:",
                             choices = status_choices, selected = NULL,
                             inline = TRUE),

          div(style = "margin-top:10px;",
            checkboxGroupInput("interim_action", "Action Required:",
                               choices = action_choices, selected = NULL,
                               inline = TRUE)
          ),

          div(style = "margin-top:10px;",
            textAreaInput("interim_followup_notes", "Follow-up Action Item:",
                          value = "", rows = 2, width = "100%",
                          placeholder = "Specific follow-up item\u2026")
          ),

          div(style = "margin-top:6px;",
            fluidRow(column(width = 7,
              textInput("interim_person_resp", "Person Responsible:",
                        value = "", width = "100%",
                        placeholder = "Name or role\u2026")
            ))
          )
        )
      ),

      # ── CCC Concern ───────────────────────────────────────────────────────
      checkboxInput("interim_concern",
                    tags$span(tags$strong("CCC Concern")),
                    value = FALSE),

      conditionalPanel(
        condition = "input.interim_concern == true",
        div(
          style = "background:#fff5f5; border:1px solid #fed7d7; border-radius:6px;
                   padding:14px 16px; margin-top:8px;",

          if (length(competency_choices) > 0)
            checkboxGroupInput("interim_competency", "Competency Area(s):",
                               choices = competency_choices, selected = NULL,
                               inline = TRUE)
        )
      )
    ))

    message("MODAL[9] showModal completed for ", res_name)
    }, error = function(e) {
      message("MODAL[ERR] ", conditionMessage(e))
      showNotification(
        paste("Could not open review form:", conditionMessage(e)),
        type = "error", duration = 8
      )
    }) # end tryCatch
  })

  # ── JS error logger — pipes browser errors into Posit Connect logs ───────────
  observeEvent(input$js_error_log, {
    err <- input$js_error_log
    message(sprintf("JS_ERROR: %s  [%s:%s:%s]",
                    err$message, err$source, err$line, err$col))
    if (nchar(err$stack) > 0) message("JS_STACK: ", err$stack)
  }, ignoreInit = TRUE)

  # ── Save handler — always creates a new interim instance ─────────────────────
  # IMPORTANT: actionButton initialises to 0 when the modal is first rendered.
  # req(... > 0) prevents this initialisation from triggering an accidental save.
  observeEvent(input$interim_save_btn, {
    req(isTRUE(input$interim_save_btn > 0))   # only fire on actual user click

    tryCatch({
    rid <- interim_edit_rid()
    req(!is.null(rid))
    message("SAVE[1] saving for rid=", rid)

    action_needed <- isTRUE(input$interim_action_needed)

    result <- submit_new_tracker_issue(
      redcap_url   = REDCAP_CONFIG$url,
      redcap_token = REDCAP_CONFIG$rdm_token,
      record_id    = rid,
      description  = input$interim_progress_notes,
      actions      = if (action_needed) input$interim_action     else character(0),
      status       = if (action_needed) input$interim_status     else character(0),
      competency   = if (isTRUE(input$interim_concern)) input$interim_competency else character(0),
      person_resp  = if (action_needed) input$interim_person_resp  else NULL,
      notes        = if (action_needed) input$interim_followup_notes else NULL
    )
    message("SAVE[2] result$success=", isTRUE(result$success))

    if (isTRUE(result$success)) {
      removeModal()
      # Show a persistent "refreshing" notification, replace it once done
      refresh_id <- showNotification(
        tagList(icon("rotate"), " Refreshing\u2026"),
        type = "message", duration = NULL, closeButton = FALSE
      )
      tryCatch({
        # Fast path: re-fetch only the ccc_review instrument (~1s vs ~10s full reload)
        app_data(refresh_ccc_review(app_data()))
        removeNotification(refresh_id)
        showNotification(
          tagList(icon("circle-check"), " Interim review saved."),
          type = "message", duration = 3
        )
      }, error = function(e) {
        removeNotification(refresh_id)
        # Fall back to full reload if targeted refresh fails
        tryCatch({
          app_data(load_ccc_data())
          showNotification(
            tagList(icon("circle-check"), " Saved (full refresh)."),
            type = "message", duration = 3
          )
        }, error = function(e2) {
          showNotification("Saved, but data refresh failed — please click Refresh Data.",
                           type = "warning", duration = 6)
        })
      })
    } else {
      showNotification(paste("Save failed:", result$message), type = "error", duration = 10)
    }
    }, error = function(e) {
      message("SAVE[ERR] ", conditionMessage(e))
      showNotification(paste("Save error:", conditionMessage(e)), type = "error", duration = 10)
    })
  })

  # ===========================================================================
  # MODE 3: MILESTONE ANALYSIS
  # ===========================================================================

  # -- Internal helper: return an empty plotly with a centred message ---------
  ms_empty_plot <- function(msg = "No data available") {
    plotly::plot_ly() %>%
      plotly::layout(
        xaxis      = list(visible = FALSE),
        yaxis      = list(visible = FALSE),
        annotations = list(list(
          text      = msg,
          xref      = "paper", yref = "paper",
          x = 0.5,  y = 0.5,
          showarrow = FALSE,
          font      = list(size = 15, color = "#6b7280")
        )),
        paper_bgcolor = "#ffffff",
        plot_bgcolor  = "#fafafa"
      )
  }

  # Reactive vals ---------------------------------------------------------------
  milestone_full_data <- reactiveVal(NULL)
  milestone_loading   <- reactiveVal(FALSE)

  # Load full data (archived + active) the first time the milestones section is visited
  observeEvent(nav_state$current, {
    req(nav_state$current == "milestones")
    if (!is.null(milestone_full_data())) return()   # already loaded

    milestone_loading(TRUE)
    tryCatch({
      full <- load_ccc_data(include_archived = TRUE)
      milestone_full_data(full)
    }, error = function(e) {
      showNotification(
        paste("Error loading milestone data:", e$message),
        type = "error", duration = 10
      )
    })
    milestone_loading(FALSE)
  }, ignoreInit = TRUE)

  # Long-format milestone data --------------------------------------------------
  milestone_long <- reactive({
    req(!is.null(milestone_full_data()))
    get_milestone_longitudinal_data(milestone_full_data())
  })

  # Loading indicators (shared between sub-tabs) --------------------------------
  output$ms_loading_indicator <- renderUI({
    if (isTRUE(milestone_loading())) {
      div(class = "alert alert-info mb-3",
          icon("circle-notch", class = "fa-spin"),
          " Loading historical milestone data (including archived residents)...")
    } else if (is.null(milestone_full_data())) {
      div(class = "alert alert-secondary mb-3",
          icon("hourglass-half"),
          " Milestone data will load when you visit this tab for the first time.")
    }
  })

  output$ms_ind_loading_indicator <- renderUI({
    if (isTRUE(milestone_loading())) {
      div(class = "alert alert-info mb-3",
          icon("circle-notch", class = "fa-spin"),
          " Loading historical milestone data (including archived residents)...")
    } else if (is.null(milestone_full_data())) {
      div(class = "alert alert-secondary mb-3",
          icon("hourglass-half"),
          " Milestone data will load when you visit this tab.")
    }
  })

  # Populate individual-resident selector (all residents inc. archived) ---------
  observe({
    req(!is.null(milestone_full_data()))
    all_res <- milestone_full_data()$residents
    req(nrow(all_res) > 0, "full_name" %in% names(all_res))
    choices <- setNames(as.character(all_res$record_id), all_res$full_name)
    choices <- choices[order(names(choices))]
    updateSelectizeInput(session, "ms_resident", choices = choices, server = TRUE)
  })

  # Update "Specific Subcompetency" dropdown when category changes --------------
  observe({
    req(!is.null(milestone_long()))
    ld <- milestone_long()
    req(nrow(ld) > 0)

    cat_filter <- input$ms_category
    if (!is.null(cat_filter) && cat_filter != "All") {
      milestones <- sort(unique(ld$milestone[ld$category == cat_filter]))
    } else {
      milestones <- sort(unique(ld$milestone))
    }

    # Map milestone codes to full names for the dropdown
    mile_choices <- setNames(milestones, sapply(milestones, get_competency_full_name))

    updateSelectInput(session, "ms_specific",
      choices  = c("All (Average)" = "__all__", mile_choices),
      selected = "__all__"
    )
  })

  # ===========================================================================
  # MILESTONE ANALYSIS — Program Trends box plot
  # ===========================================================================

  output$ms_program_plot <- plotly::renderPlotly({
    req(!is.null(milestone_long()))
    ld <- milestone_long()
    if (nrow(ld) == 0) return(ms_empty_plot("No milestone data found."))

    cat_filter   <- input$ms_category
    spec_filter  <- input$ms_specific
    show_cohorts <- isTRUE(input$ms_show_cohorts)
    show_points  <- isTRUE(input$ms_show_points)

    period_order <- c(
      "Mid Intern", "End Intern",
      "Mid PGY2",   "End PGY2",
      "Mid PGY3",   "Graduating"
    )

    # -- Filter by category
    if (!is.null(cat_filter) && cat_filter != "All") {
      ld <- ld %>% filter(category == cat_filter)
    }
    if (nrow(ld) == 0) return(ms_empty_plot("No data for the selected category."))

    # -- Aggregate or single subcompetency
    use_specific <- !is.null(spec_filter) &&
                    spec_filter != "__all__"  &&
                    spec_filter != "All (Average)"

    if (use_specific) {
      plot_ld <- ld %>% filter(milestone == spec_filter)
    } else {
      # Average across all selected subcompetencies per resident per period
      plot_ld <- ld %>%
        group_by(record_id, full_name, grad_yr, prog_mile_period, period_num) %>%
        summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
    }

    if (nrow(plot_ld) == 0) return(ms_empty_plot("No data for the selected subcompetency."))

    # Drop rows with missing period (keeps factor ordering clean)
    plot_ld <- plot_ld %>% filter(!is.na(prog_mile_period))

    # Build box point style
    box_pts <- if (show_points) "all" else "outliers"

    plot_title <- if (use_specific) {
      paste0("Milestone: ", get_competency_full_name(spec_filter))
    } else if (!is.null(cat_filter) && cat_filter != "All") {
      paste0("Competency Category: ", cat_filter, " (average across subcompetencies)")
    } else {
      "All Competencies (average per resident)"
    }

    # -- Build plotly
    if (show_cohorts && "grad_yr" %in% names(plot_ld) &&
        any(!is.na(plot_ld$grad_yr))) {

      plot_ld_c <- plot_ld %>% filter(!is.na(grad_yr))

      p <- plotly::plot_ly(
        data       = plot_ld_c,
        x          = ~prog_mile_period,
        y          = ~score,
        color      = ~as.factor(grad_yr),
        type       = "box",
        boxpoints  = box_pts,
        jitter     = 0.35,
        pointpos   = 0
      ) %>%
        plotly::layout(boxmode = "group")

    } else {
      p <- plotly::plot_ly(
        data      = plot_ld,
        x         = ~prog_mile_period,
        y         = ~score,
        type      = "box",
        boxpoints = box_pts,
        jitter    = 0.35,
        pointpos  = 0,
        marker    = list(color = "rgba(0,48,135,0.80)", size = 5),
        line      = list(color = "#003087", width = 2),
        fillcolor = "rgba(0,48,135,0.38)",
        name      = "All residents"
      )
    }

    # Level-4 target line
    p <- p %>%
      plotly::add_lines(
        x       = period_order,
        y       = rep(4, length(period_order)),
        line    = list(color = "#dc3545", width = 2.5, dash = "dash"),
        name    = "Level 4 Target",
        inherit = FALSE,
        showlegend = TRUE
      ) %>%
      plotly::layout(
        title  = list(text = plot_title, font = list(size = 16, color = "#1a202c")),
        xaxis  = list(
          title         = list(text = "Review Period", font = list(size = 15, color = "#1a202c")),
          categoryorder = "array",
          categoryarray = period_order,
          tickfont      = list(size = 15, color = "#1a202c"),
          gridcolor     = "#c7d2da",
          linecolor     = "#cbd5e0",
          linewidth     = 1
        ),
        yaxis  = list(
          title     = list(text = "Milestone Score (1–9)", font = list(size = 15, color = "#1a202c")),
          range     = c(0.5, 9.5),
          dtick     = 1,
          tickfont  = list(size = 13, color = "#2d3748"),
          gridcolor = "#e2e8f0",
          linecolor = "#cbd5e0",
          linewidth = 1
        ),
        legend = list(
          orientation = "h",
          yanchor     = "bottom",
          y           = 1.02,
          xanchor     = "right",
          x           = 1,
          font        = list(size = 13)
        ),
        plot_bgcolor  = "#ffffff",
        paper_bgcolor = "#ffffff",
        font          = list(family = "Inter, sans-serif", size = 14),
        margin        = list(l = 65, r = 20, t = 55, b = 80)
      )

    p
  })

  # ===========================================================================
  # MILESTONE ANALYSIS — Individual Resident trajectory
  # ===========================================================================

  output$ms_individual_plot <- plotly::renderPlotly({
    req(input$ms_resident, !is.null(milestone_long()))
    ld <- milestone_long()
    if (nrow(ld) == 0) return(ms_empty_plot("No milestone data found."))

    rid        <- input$ms_resident
    cat_filter <- input$ms_ind_category

    period_order <- c(
      "Mid Intern", "End Intern",
      "Mid PGY2",   "End PGY2",
      "Mid PGY3",   "Graduating"
    )

    # -- Filter by category (keeps individual subcompetency lines manageable)
    ld_cat <- if (!is.null(cat_filter) && cat_filter != "All") {
      ld %>% filter(category == cat_filter)
    } else {
      ld
    }
    if (nrow(ld_cat) == 0) return(ms_empty_plot("No data for the selected category."))

    # -- Resident display name
    res_name <- tryCatch({
      r <- milestone_full_data()$residents %>%
        filter(as.character(record_id) == as.character(rid))
      if (nrow(r) > 0) as.character(r$full_name[1]) else as.character(rid)
    }, error = function(e) as.character(rid))

    # -- Individual scores per subcompetency per period ----------------------
    ind_by_ms <- ld_cat %>%
      filter(as.character(record_id) == as.character(rid),
             !is.na(prog_mile_period)) %>%
      arrange(period_num)

    if (nrow(ind_by_ms) == 0) {
      return(ms_empty_plot(paste0("No milestone data found for ", res_name, ".")))
    }

    # -- Program IQR ribbon (average per-person per period, selected category)
    prog_per_person <- ld_cat %>%
      filter(!is.na(prog_mile_period)) %>%
      group_by(record_id, prog_mile_period, period_num) %>%
      summarise(avg = mean(score, na.rm = TRUE), .groups = "drop")

    prog_summ <- prog_per_person %>%
      group_by(prog_mile_period, period_num) %>%
      summarise(
        med = median(avg, na.rm = TRUE),
        q25 = quantile(avg, 0.25, na.rm = TRUE),
        q75 = quantile(avg, 0.75, na.rm = TRUE),
        n   = n(),
        .groups = "drop"
      ) %>%
      arrange(period_num) %>%
      filter(!is.na(prog_mile_period))

    # -- Color palette (one color per subcompetency) -------------------------
    ms_palette <- c(
      "PC1"   = "#003087", "PC2"   = "#1a5fbf", "PC3"   = "#3678d8",
      "PC4"   = "#5491e8", "PC5"   = "#72abf5", "PC6"   = "#90c5ff",
      "MK1"   = "#155d27", "MK2"   = "#1e8136", "MK3"   = "#28a745",
      "SBP1"  = "#7d3800", "SBP2"  = "#b55200", "SBP3"  = "#fd7e14",
      "PBL1"  = "#4a0e8f", "PBL2"  = "#7c3aed",
      "PROF1" = "#7c0012", "PROF2" = "#a80018", "PROF3" = "#cc001e",
      "PROF4" = "#dc3545",
      "ICS1"  = "#005e6d", "ICS2"  = "#00859a", "ICS3"  = "#0dcaf0"
    )

    milestones_present <- sort(unique(ind_by_ms$milestone))

    # -- Build plot ----------------------------------------------------------
    p <- plotly::plot_ly()

    # Program IQR ribbon + median (uses correct ymin/ymax API)
    if (nrow(prog_summ) > 0) {
      x_prog <- as.character(prog_summ$prog_mile_period)

      p <- p %>%
        plotly::add_ribbons(
          x         = x_prog,
          ymin      = prog_summ$q25,
          ymax      = prog_summ$q75,
          fillcolor = "rgba(100,116,139,0.35)",
          line      = list(color = "rgba(71,85,105,0.7)", width = 1.5),
          name      = paste0("Program IQR",
                             if (!is.null(cat_filter) && cat_filter != "All")
                               paste0(" (", cat_filter, " avg)") else ""),
          showlegend = TRUE,
          hoverinfo  = "skip"
        ) %>%
        plotly::add_trace(
          x         = x_prog,
          y         = prog_summ$med,
          type      = "scatter",
          mode      = "lines",
          line      = list(color = "#475569", width = 2.5, dash = "dot"),
          name      = "Program Median",
          text      = paste0("Period: ", x_prog,
                             "<br>Program Median: ", round(prog_summ$med, 2),
                             "<br>N = ", prog_summ$n),
          hoverinfo = "text",
          showlegend = TRUE
        )
    }

    # One line+markers per subcompetency (individual scores)
    for (ms in milestones_present) {
      ms_data <- ind_by_ms %>%
        filter(milestone == ms) %>%
        arrange(period_num)
      if (nrow(ms_data) == 0) next

      col   <- if (ms %in% names(ms_palette)) ms_palette[[ms]] else "#444444"
      x_ms  <- as.character(ms_data$prog_mile_period)
      label <- get_competency_full_name(ms)

      p <- p %>%
        plotly::add_trace(
          x         = x_ms,
          y         = ms_data$score,
          type      = "scatter",
          mode      = "lines+markers",
          name      = label,
          line      = list(color = col, width = 3),
          marker    = list(color = col, size = 10,
                           line = list(color = "#ffffff", width = 2)),
          text      = paste0(label,
                             "<br>Period: ", x_ms,
                             "<br>Score: ", ms_data$score),
          hoverinfo = "text"
        )
    }

    # Level-4 reference line
    if (nrow(prog_summ) > 0) {
      x_prog <- as.character(prog_summ$prog_mile_period)
      p <- p %>%
        plotly::add_trace(
          x         = x_prog,
          y         = rep(4, length(x_prog)),
          type      = "scatter",
          mode      = "lines",
          line      = list(color = "#dc3545", width = 2.5, dash = "dash"),
          name      = "Level 4 Target",
          showlegend = TRUE,
          hoverinfo  = "skip"
        )
    }

    cat_label <- if (!is.null(cat_filter) && cat_filter != "All")
      paste0(" (", cat_filter, ")") else ""

    p %>% plotly::layout(
      title  = list(
        text = paste0(res_name, " — Milestone Trajectory", cat_label),
        font = list(size = 16, color = "#1a202c")
      ),
      xaxis  = list(
        title         = list(text = "Review Period", font = list(size = 15, color = "#1a202c")),
        categoryorder = "array",
        categoryarray = period_order,
        tickfont      = list(size = 13, color = "#2d3748"),
        gridcolor     = "#e2e8f0",
        linecolor     = "#cbd5e0",
        linewidth     = 1
      ),
      yaxis  = list(
        title     = list(text = "Milestone Score (1–9)", font = list(size = 15, color = "#1a202c")),
        range     = c(0.5, 9.5),
        dtick     = 1,
        tickfont  = list(size = 15, color = "#1a202c"),
        gridcolor = "#c7d2da",
        linecolor = "#cbd5e0",
        linewidth = 1
      ),
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        font        = list(size = 13, color = "#1a202c")
      ),
      plot_bgcolor  = "#ffffff",
      paper_bgcolor = "#ffffff",
      font          = list(family = "Inter, sans-serif", size = 14),
      margin        = list(l = 65, r = 190, t = 65, b = 80)
    )
  })

  }  # End of inner server function
}  # End of create_server function
