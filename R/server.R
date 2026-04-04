# server.R - Server logic for CCC Dashboard
# Uses ONLY wrapper functions - never accesses data directly
#
# Note: This function expects rdm_data to be passed as a parameter

create_server <- function(initial_data) {
  function(input, output, session) {

    # Reactive values
    app_data <- reactiveVal(initial_data)
    selected_resident_id <- reactiveVal(NULL)
    show_list_view <- reactiveVal(TRUE)
    authenticated <- reactiveVal(FALSE)
    filtered_review_table <- reactiveVal(NULL)

    # Filter reactive values
    filter_completion <- reactiveVal("all")
    filter_pgy <- reactiveVal("all")
    filter_coach <- reactiveVal("all")
    filter_second <- reactiveVal("all")

  # ===========================================================================
  # AUTHENTICATION
  # ===========================================================================

  # Output authenticated status for conditional panels
  output$authenticated <- reactive({
    authenticated()
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  # Handle access code submission
  observeEvent(input$submit_access_code, {
    # Get the expected access code from environment variable
    expected_code <- Sys.getenv("CCC_ACCESS_CODE")

    if (expected_code == "") {
      showNotification(
        "Access code not configured. Please set CCC_ACCESS_CODE environment variable.",
        type = "error",
        duration = 10
      )
      return()
    }

    # Check if entered code matches
    if (!is.null(input$access_code) && input$access_code == expected_code) {
      authenticated(TRUE)
      showNotification(
        "Access granted. Welcome to the CCC Dashboard!",
        type = "message",
        duration = 3
      )
    } else {
      output$access_error_message <- renderUI({
        tags$div(
          style = "color: #dc3545; margin-top: 15px; font-size: 14px;",
          icon("exclamation-circle"),
          " Invalid access code. Please try again."
        )
      })
    }
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
  })

  # ===========================================================================
  # LAZY MILESTONE WORKFLOW
  # Computed once the first time a resident is selected (not at startup).
  # Stored back into app_data() so spider plots pick it up automatically.
  # ===========================================================================

  observe({
    rid <- selected_resident_id()
    req(rid)

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

  # Dynamic coach filter buttons
  output$filter_coach_buttons <- renderUI({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) == 0) {
      return(tags$div(style = "margin: 10px 0;", actionButton("filter_coach_all", "All", class = "btn-sm")))
    }

    # Get unique coach names
    coaches <- unique(review_table$coach_name)
    coaches <- coaches[!is.na(coaches) & coaches != ""]
    coaches <- sort(coaches)

    # Create buttons
    buttons <- list(
      actionButton("filter_coach_all", "All", class = "btn-sm", style = "margin: 5px 5px 5px 0;")
    )

    for (coach in coaches) {
      btn_id <- paste0("filter_coach_", gsub("[^A-Za-z0-9]", "_", coach))
      buttons <- c(buttons, list(
        actionButton(btn_id, coach, class = "btn-sm", style = "margin: 5px 5px 5px 0;")
      ))
    }

    tags$div(style = "margin: 10px 0;", buttons)
  })

  # Dynamic second reviewer filter buttons
  output$filter_second_buttons <- renderUI({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) == 0) {
      return(tags$div(style = "margin: 10px 0;", actionButton("filter_second_all", "All", class = "btn-sm")))
    }

    # Get unique second reviewer names
    second_revs <- unique(review_table$second_rev_name)
    second_revs <- second_revs[!is.na(second_revs) & second_revs != ""]
    second_revs <- sort(second_revs)

    # Create buttons
    buttons <- list(
      actionButton("filter_second_all", "All", class = "btn-sm", style = "margin: 5px 5px 5px 0;")
    )

    for (second_rev in second_revs) {
      btn_id <- paste0("filter_second_", gsub("[^A-Za-z0-9]", "_", second_rev))
      buttons <- c(buttons, list(
        actionButton(btn_id, second_rev, class = "btn-sm", style = "margin: 5px 5px 5px 0;")
      ))
    }

    tags$div(style = "margin: 10px 0;", buttons)
  })

  # Observe coach button clicks
  observe({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) > 0) {
      coaches <- unique(review_table$coach_name)
      coaches <- coaches[!is.na(coaches) & coaches != ""]

      # All button
      observeEvent(input$filter_coach_all, {
        filter_coach("all")
      }, ignoreInit = TRUE)

      # Individual coach buttons
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
  })

  # Observe second reviewer button clicks
  observe({
    current_period <- get_current_ccc_period()
    review_table <- get_ccc_review_table(app_data(), current_period)

    if (nrow(review_table) > 0) {
      second_revs <- unique(review_table$second_rev_name)
      second_revs <- second_revs[!is.na(second_revs) & second_revs != ""]

      # All button
      observeEvent(input$filter_second_all, {
        filter_second("all")
      }, ignoreInit = TRUE)

      # Individual second reviewer buttons
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
  })

  # Resident review table
  output$resident_review_table <- DT::renderDT({
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
  })

  # Review statistics
  output$review_stats <- renderUI({
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

  # Resident detail panel
  output$resident_detail_panel <- renderUI({
    rid <- selected_resident_id()

    if (is.null(rid)) {
      return(
        div(
          class = "alert alert-info",
          "Select a resident from the table above to view details and conduct CCC review"
        )
      )
    }

    # Get resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    tagList(
      # Resident info header with Log Ad Hoc Meeting button
      fluidRow(
        column(width = 10,
          gmed::gmed_resident_panel(
            resident_name = resident_info$full_name,
            level = resident_info$current_period,
            coach = if ("coach_name" %in% names(resident_info)) resident_info$coach_name else NULL
          )
        ),
        column(width = 2,
          div(style = "padding-top: 15px; text-align: right;",
            actionButton(
              "log_adhoc_meeting",
              "Log Ad Hoc Meeting",
              icon  = icon("calendar-plus"),
              class = "btn-warning"
            )
          )
        )
      ),
      tags$hr(),

      h4("Milestone Review and Data Entry", style = "margin-top: 20px;"),
      fluidRow(
        # Left column (1/3): Editable milestone table
        column(
          width = 4,
          gmed::gmed_card(
            title = "Edit Program Milestones",
            p(
              class = "text-muted",
              style = "font-size: 0.9em;",
              "Edit milestone ratings (1-9 scale) for this period."
            ),
            DT::DTOutput("milestone_edit_table"),
            tags$br(),
            actionButton(
              "save_milestone_edits",
              "Save Changes",
              class = "btn-warning w-100",
              icon = icon("save")
            )
          )
        ),

        # Right column (2/3): Spider plots
        column(
          width = 8,
          # Three spider plots stacked vertically
          h5("ACGME Milestones (Previous Period)"),
          plotly::plotlyOutput("plot_acgme_spider", height = "450px"),
          br(),
          h5("Program Milestones (Current Period)"),
          plotly::plotlyOutput("plot_program_spider", height = "450px"),
          br(),
          h5("Self-Evaluation (Current Period)"),
          plotly::plotlyOutput("plot_self_spider", height = "450px")
        )
      ),
      tags$br(),

      # Additional Data Buttons
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Additional Data",
            actionButton(
              "view_plus_delta",
              "View Plus/Delta Comments",
              class = "btn-info",
              icon = icon("comments"),
              style = "margin-right: 10px;"
            ),
            actionButton(
              "view_ilp_goals",
              "View Current ILP Goals",
              class = "btn-info",
              icon = icon("bullseye")
            )
          )
        )
      ),
      tags$br(),

      # Milestone Descriptions - Full Width
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Milestone Descriptions",
            p(
              class = "text-muted",
              style = "font-size: 0.9em;",
              "Descriptions provided for specific competencies during this period."
            ),
            DT::DTOutput("milestone_descriptions_table")
          )
        )
      ),
      hr(),

      h4("Previous Reviews", style = "margin-top: 30px;"),
      fluidRow(
        column(
          width = 6,
          gmed::gmed_card(
            title = "Coach Review",
            uiOutput("coach_review_summary")
          )
        ),
        column(
          width = 6,
          gmed::gmed_card(
            title = "Second Review",
            uiOutput("second_review_summary")
          )
        )
      ),
      tags$hr(),

      h4("CCC Review Form", style = "margin-top: 30px;"),
      p(
        class = "text-muted",
        "Complete the following fields based on the committee's discussion and review of the resident's performance."
      ),

      # Action Data Table
      gmed::gmed_card(
        title = "Previous Action Items",
        DT::DTOutput("action_data_table")
      ),
      tags$br(),

      # ILP Section
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Individual Learning Plan",
            uiOutput("coach_ilp_display"),
            tags$br(),
            uiOutput("second_comments_display"),
            tags$br(),
            textAreaInput(
              "ccc_ilp",
              "CCC ILP:",
              value = "",
              rows = 4,
              width = "100%",
              placeholder = "Enter CCC Individual Learning Plan..."
            )
          )
        )
      ),

      # Milestone Discussion
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Milestone Discussion",
            radioButtons(
              "ccc_mile",
              "Discuss Milestones?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = "0",
              inline = TRUE
            ),
            uiOutput("milestone_discussion_content")
          )
        )
      ),

      # Follow-up Issues
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Follow-up Issues",
            radioButtons(
              "ccc_issues_yn",
              "Any Follow-up Issues?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = "0",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.ccc_issues_yn == '1'",
              textAreaInput(
                "ccc_issues_follow_up",
                "Describe Follow-up Issues:",
                value = "",
                rows = 3,
                width = "100%",
                placeholder = "Describe any issues requiring follow-up..."
              )
            )
          )
        )
      ),

      # Concerns
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Concerns",
            radioButtons(
              "ccc_concern",
              "Any Concerns?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = "0",
              inline = TRUE
            ),
            uiOutput("concern_content")
          )
        )
      ),

      br(),

      fluidRow(
        column(
          width = 12,
          actionButton(
            "submit_ccc_review",
            "Submit CCC Review",
            class = "btn-primary btn-lg",
            icon = icon("check")
          ),
          span(
            class = "text-muted",
            style = "margin-left: 15px;",
            "This will save the review to REDCap"
          )
        )
      )
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
        pageLength = 5,
        dom = 't',
        ordering = TRUE,
        searching = FALSE
      ),
      rownames = FALSE,
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

    if (input$ccc_mile == "0") {
      return(NULL)
    }

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

    tagList(
      if (nrow(second_data) > 0 && "second_approve" %in% names(second_data)) {
        if (!is.na(second_data$second_approve[1]) && second_data$second_approve[1] == "1") {
          div(
            p(strong("Second Reviewer Approved Milestones:"), " Yes")
          )
        } else if (!is.na(second_data$second_miles_comment[1]) && nchar(trimws(second_data$second_miles_comment[1])) > 0) {
          div(
            p(strong("Second Reviewer Milestone Comments:")),
            p(second_data$second_miles_comment[1])
          )
        }
      },
      br(),
      textAreaInput(
        "ccc_mile_notes",
        "CCC Milestone Notes:",
        value = "",
        rows = 3,
        width = "100%",
        placeholder = "Enter notes about milestone discussion..."
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
    action_choices <- get_field_choices(app_data()$data_dict, "ccc_action", for_ui = TRUE)
    status_choices <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    tagList(
      textAreaInput(
        "ccc_comments",
        "CCC Comments:",
        value = "",
        rows = 3,
        width = "100%",
        placeholder = "Describe the concerns..."
      ),
      checkboxGroupInput(
        "ccc_competency",
        "Competency Areas:",
        choices = competency_choices,
        selected = NULL
      ),
      checkboxGroupInput(
        "ccc_action",
        "Action Required:",
        choices = action_choices,
        selected = NULL
      ),
      checkboxGroupInput(
        "ccc_action_status",
        "Action Status:",
        choices = status_choices,
        selected = NULL
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
      return(p(
        class = "text-muted",
        icon("info-circle"),
        " No coach review available for this period"
      ))
    }

    # Display key fields from coach review
    tagList(
      if ("coach_ilp_final" %in% names(coach_data) && !is.na(coach_data$coach_ilp_final[1])) {
        div(
          p(strong("Coach ILP:")),
          p(coach_data$coach_ilp_final[1])
        )
      },
      # Fallback: show completion status
      if (!("coach_ilp_final" %in% names(coach_data))) {
        p(
          icon("check", class = "text-success"),
          " Coach review completed for ", resident_info$current_period
        )
      }
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
      return(p(
        class = "text-muted",
        icon("info-circle"),
        " No second review available for this period"
      ))
    }

    # Display key fields from second review
    tagList(
      if ("second_comments" %in% names(second_data) && !is.na(second_data$second_comments[1])) {
        div(
          p(strong("Comments:")),
          p(second_data$second_comments[1])
        )
      },
      if ("second_approve" %in% names(second_data) && !is.na(second_data$second_approve[1])) {
        div(
          p(strong("Milestones Approved:")),
          p(if_else(second_data$second_approve[1] == "1", "Yes", "No"))
        )
      },
      if ("second_miles_comment" %in% names(second_data) && !is.na(second_data$second_miles_comment[1])) {
        div(
          p(strong("Milestone Comments:")),
          p(second_data$second_miles_comment[1])
        )
      },
      # Fallback: show completion status if no specific fields available
      if (!any(c("second_comments", "second_approve", "second_miles_comment") %in% names(second_data))) {
        p(
          icon("check", class = "text-success"),
          " Second review completed for ", resident_info$current_period
        )
      }
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
  output$milestone_edit_table <- DT::renderDT({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    milestone_values <- get_milestone_values_for_edit(
      app_data(),
      rid,
      resident_info$current_period
    )

    # If no data exists, create empty template for administrative entry
    if (nrow(milestone_values) == 0) {
      # Create template with all standard REP milestone competencies (21 total)
      template_competencies <- c(
        "PC1", "PC2", "PC3", "PC4", "PC5", "PC6",
        "MK1", "MK2", "MK3",
        "SBP1", "SBP2", "SBP3",
        "PBL1", "PBL2",
        "PROF1", "PROF2", "PROF3", "PROF4",
        "ICS1", "ICS2", "ICS3"
      )

      display_data <- data.frame(
        Subcompetency = sapply(template_competencies, get_competency_full_name),
        Value = rep(NA, length(template_competencies)),
        Image = paste0('<a href="#" onclick="showImage(\'milestones/', tolower(template_competencies), '.png\'); return false;">View</a>'),
        stringsAsFactors = FALSE
      )
      editable <- TRUE  # Enable editing for administrative entry
    } else {
      # Add full names and image links
      display_data <- data.frame(
        Subcompetency = sapply(milestone_values$competency, get_competency_full_name),
        Value = milestone_values$value,
        Image = paste0('<a href="#" onclick="showImage(\'milestones/', tolower(milestone_values$competency), '.png\'); return false;">View</a>'),
        stringsAsFactors = FALSE
      )
      editable <- TRUE
    }

    DT::datatable(
      display_data,
      escape = FALSE,  # Allow HTML in Image column
      options = list(
        pageLength = 25,
        dom = 't',
        ordering = FALSE,
        searching = FALSE,
        columnDefs = list(
          list(width = '200px', targets = 0),  # Subcompetency column
          list(width = '50px', targets = 1),   # Value column (editable)
          list(width = '50px', targets = 2)    # Image link column
        )
      ),
      rownames = FALSE,
      selection = 'none',
      editable = if (editable) list(target = 'cell', disable = list(columns = c(0, 2))) else FALSE
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

  # Submit CCC Review
  observeEvent(input$submit_ccc_review, {
    rid <- selected_resident_id()
    req(rid)

    # Get resident info for current period
    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
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

    # Determine review type (semi-annual vs ad hoc)
    review_type <- if (resident_info$current_period %in% c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating")) {
      "1"  # Semi-annual
    } else {
      "2"  # Ad hoc
    }

    # Calculate next instance number for this resident
    existing_ccc <- get_ccc_review_data(app_data(), rid, resident_info$current_period)
    if (nrow(existing_ccc) > 0 && "redcap_repeat_instance" %in% names(existing_ccc)) {
      next_instance <- max(as.numeric(existing_ccc$redcap_repeat_instance), na.rm = TRUE) + 1
    } else {
      # Get all CCC reviews for this resident
      all_ccc <- app_data()$all_forms$ccc_review %>%
        filter(record_id == rid, redcap_repeat_instrument == "ccc_review")
      if (nrow(all_ccc) > 0 && "redcap_repeat_instance" %in% names(all_ccc)) {
        next_instance <- max(as.numeric(all_ccc$redcap_repeat_instance), na.rm = TRUE) + 1
      } else {
        next_instance <- 1
      }
    }

    # Build base data frame
    ccc_data <- data.frame(
      record_id = as.character(rid),
      redcap_repeat_instrument = "ccc_review",
      redcap_repeat_instance = as.character(next_instance),
      ccc_date = as.character(Sys.Date()),
      ccc_rev_type = as.character(review_type),
      ccc_session = as.character(period_code),
      ccc_ilp = if (!is.null(input$ccc_ilp) && nchar(trimws(input$ccc_ilp)) > 0) as.character(input$ccc_ilp) else "",
      ccc_mile = if (!is.null(input$ccc_mile)) as.character(input$ccc_mile) else "0",
      ccc_mile_notes = if (!is.null(input$ccc_mile_notes) && nchar(trimws(input$ccc_mile_notes)) > 0) as.character(input$ccc_mile_notes) else "",
      ccc_issues_follow_up = if (!is.null(input$ccc_issues_follow_up) && nchar(trimws(input$ccc_issues_follow_up)) > 0) as.character(input$ccc_issues_follow_up) else "",
      ccc_concern = if (!is.null(input$ccc_concern)) as.character(input$ccc_concern) else "0",
      ccc_comments = if (!is.null(input$ccc_comments) && nchar(trimws(input$ccc_comments)) > 0) as.character(input$ccc_comments) else "",
      ccc_review_complete = "2",  # Complete status
      stringsAsFactors = FALSE
    )

    # Handle checkbox fields - ccc_competency
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency")
    if (length(competency_choices) > 0) {
      for (code in names(competency_choices)) {
        col_name <- paste0("ccc_competency___", code)
        ccc_data[[col_name]] <- if (!is.null(input$ccc_competency) && code %in% input$ccc_competency) "1" else "0"
      }
    }

    # Handle checkbox fields - ccc_action
    action_choices <- get_field_choices(app_data()$data_dict, "ccc_action")
    if (length(action_choices) > 0) {
      for (code in names(action_choices)) {
        col_name <- paste0("ccc_action___", code)
        ccc_data[[col_name]] <- if (!is.null(input$ccc_action) && code %in% input$ccc_action) "1" else "0"
      }
    }

    # Handle checkbox fields - ccc_action_status
    status_choices <- get_field_choices(app_data()$data_dict, "ccc_action_status")
    if (length(status_choices) > 0) {
      for (code in names(status_choices)) {
        col_name <- paste0("ccc_action_status___", code)
        ccc_data[[col_name]] <- if (!is.null(input$ccc_action_status) && code %in% input$ccc_action_status) "1" else "0"
      }
    }

    # Validate required fields
    if (is.null(input$ccc_ilp) || nchar(trimws(input$ccc_ilp)) == 0) {
      showNotification(
        "Please enter CCC ILP before submitting.",
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
          paste("CCC Review saved successfully for", resident_info$full_name),
          type = "message",
          duration = 5
        )

        # Refresh data to show updated completion status
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
        updateTextAreaInput(session, "ccc_ilp", value = "")
        updateRadioButtons(session, "ccc_mile", selected = "0")
        updateTextAreaInput(session, "ccc_mile_notes", value = "")
        updateRadioButtons(session, "ccc_issues_yn", selected = "0")
        updateTextAreaInput(session, "ccc_issues_follow_up", value = "")
        updateRadioButtons(session, "ccc_concern", selected = "0")
        updateTextAreaInput(session, "ccc_comments", value = "")

        # Return to list view
        show_list_view(TRUE)
        selected_resident_id(NULL)

      } else {
        showNotification(
          paste("Error saving review:", result$outcome_message),
          type = "error",
          duration = 10
        )
      }
    }, error = function(e) {
      showNotification(
        paste("Error saving CCC review:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # Save Milestone Edits
  observeEvent(input$save_milestone_edits, {
    rid <- selected_resident_id()
    req(rid)

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

        # Try to save to REDCap
        tryCatch({
          result <- REDCapR::redcap_write(
            ds_to_write = update_data,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          if (result$success) {
            message_text <- if (creating_new_entry) {
              paste("New milestone entry created successfully for", resident_info$full_name)
            } else {
              paste("Milestone values updated successfully for", resident_info$full_name)
            }

            showNotification(
              message_text,
              type = "message",
              duration = 5
            )

            # Refresh data
            tryCatch({
              new_data <- load_ccc_data()
              app_data(new_data)
            }, error = function(e) {
              showNotification(
                "Values saved, but could not refresh data. Please use Refresh Data button.",
                type = "warning",
                duration = 5
              )
            })
          } else {
            showNotification(
              paste("Error saving milestone values:", result$outcome_message),
              type = "error",
              duration = 10
            )
          }
        }, error = function(e) {
          showNotification(
            paste("Error saving milestone values:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    } else {
      showNotification(
        "No edits detected. Make changes to the table before saving.",
        type = "info",
        duration = 5
      )
    }
  })

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

    milestone_badge <- function(res, label) {
      period_tag <- if (res$period != "—")
        tags$small(class = "text-muted d-block mt-1", res$period)
      else NULL
      if (nrow(res$df) > 0) {
        tagList(gmed::gmed_status_badge("Available", "complete"), period_tag)
      } else {
        tagList(gmed::gmed_status_badge("No data", "incomplete"), period_tag)
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

    coach_label <- paste0("Coach Notes",
      if (nchar(ctx$coach_period) > 0) paste0(" (", ctx$coach_period, ")") else "")
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
        info_row(coach_label,        ctx$coach_summary),
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

    has_any_context <- any(nchar(c(ctx$coach_summary, ctx$coach_ilp,
                                   ctx$ccc_ilp, ctx$ccc_issues, ctx$last_concern_notes)) > 0) ||
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
          gmed::gmed_card(title = "Program Milestones",  milestone_badge(prog,  "Program"))
        ),
        column(width = 4,
          gmed::gmed_card(title = "Self-Evaluation",     milestone_badge(self,  "Self"))
        ),
        column(width = 4,
          gmed::gmed_card(title = "ACGME Milestones",    milestone_badge(acgme, "ACGME"))
        )
      ),

      br(),
      gmed::gmed_card(
        title = "Previous Reviews",
        tags$p(class = "text-muted", style = "font-size:0.8rem; margin-bottom:10px;",
               "Coach and CCC notes from the most recent relevant reviews."),
        if (has_any_context)
          context_table
        else
          tags$p(class = "text-muted", style = "font-style:italic;",
                 icon("info-circle"), " No prior coach notes or CCC review data found for this resident.")
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

  # Ad Hoc Action Data Table
  output$adhoc_action_data_table <- DT::renderDT({
    req(input$adhoc_resident)

    action_data <- get_action_data_table(app_data(), input$adhoc_resident)

    DT::datatable(
      action_data,
      options = list(
        pageLength = 5,
        dom = 't',
        ordering = TRUE,
        searching = FALSE
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
  # FOLLOW-UP TRACKER — CCC Review full table (Tab 2)
  # ===========================================================================

  # Populate session filter choices from loaded data
  observe({
    ccc_all <- app_data()$all_forms$ccc_review
    if (is.null(ccc_all)) return()

    sessions <- unique(ccc_all$ccc_session)
    sessions <- sessions[!is.na(sessions) & nchar(trimws(sessions)) > 0]
    sessions <- sort(sessions)

    updateSelectInput(session, "tracker_session_filter",
                      choices  = c("All", sessions),
                      selected = "All")
  })

  output$tracker_ccc_review_table <- DT::renderDT({
    all_reviews <- get_ccc_review_all(app_data())

    if (is.null(all_reviews) || nrow(all_reviews) == 0) {
      return(DT::datatable(
        data.frame(Resident = character(0), Session = character(0),
                   Date = character(0), Type = character(0),
                   Concern = character(0), Actions = character(0),
                   Status = character(0), `Follow-up Notes` = character(0),
                   check.names = FALSE),
        rownames = FALSE,
        options  = list(dom = "t", pageLength = 50)
      ))
    }

    # Apply type filter
    type_filter <- input$tracker_rev_type_filter
    if (!is.null(type_filter) && type_filter != "All") {
      all_reviews <- all_reviews[all_reviews$Type == type_filter, ]
    }

    # Apply session filter
    sess_filter <- input$tracker_session_filter
    if (!is.null(sess_filter) && sess_filter != "All") {
      all_reviews <- all_reviews[all_reviews$Session == sess_filter, ]
    }

    display_cols <- c("Resident", "Session", "Date", "Type", "Concern",
                      "Actions", "Status", "Follow-up Notes")

    DT::datatable(
      all_reviews[, display_cols, drop = FALSE],
      selection = "none",
      rownames  = FALSE,
      escape    = TRUE,
      options   = list(
        pageLength = 50,
        dom        = "ftp",
        scrollX    = TRUE,
        order      = list(list(2, "desc")),  # sort by Date desc
        rowCallback = DT::JS(
          "function(row, data, index) {",
          "  // data[3] = Type column (0-indexed)",
          "  if (data[3] === 'Interim') {",
          "    $(row).css({'background-color':'#ede7f6','color':'#4a1d8e'});",
          "  }",
          "  // data[4] = Concern column",
          "  if (data[4] === 'Yes' && data[3] !== 'Interim') {",
          "    $(row).css({'background-color':'#fff3cd'});",
          "  }",
          "}"
        )
      )
    ) %>%
      DT::formatStyle("Status",
        backgroundColor = DT::styleEqual(
          c("Initiation", "Ongoing", "Resolved", "Recurring"),
          c("#fff3cd",    "#cff4fc",  "#d1e7dd",  "#f8d7da")
        )
      ) %>%
      DT::formatStyle("Concern",
        color = DT::styleEqual(c("Yes", "No"), c("#dc3545", "inherit")),
        fontWeight = DT::styleEqual(c("Yes", "No"), c("bold", "normal"))
      )
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

  # Load full data (archived + active) the first time the tab is visited --------
  observeEvent(input$main_mode, {
    req(input$main_mode == "milestone_analysis")
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
        marker    = list(color = "rgba(0,48,135,0.55)", size = 5),
        line      = list(color = "#003087"),
        fillcolor = "rgba(0,48,135,0.15)",
        name      = "All residents"
      )
    }

    # Level-4 target line
    p <- p %>%
      plotly::add_lines(
        x       = period_order,
        y       = rep(4, length(period_order)),
        line    = list(color = "#dc3545", width = 2, dash = "dash"),
        name    = "Level 4 Target",
        inherit = FALSE,
        showlegend = TRUE
      ) %>%
      plotly::layout(
        title  = list(text = plot_title, font = list(size = 15)),
        xaxis  = list(
          title         = "Review Period",
          categoryorder = "array",
          categoryarray = period_order
        ),
        yaxis  = list(
          title = "Milestone Score (1–9)",
          range = c(0.5, 9.5),
          dtick = 1
        ),
        legend = list(
          orientation = "h",
          yanchor     = "bottom",
          y           = 1.02,
          xanchor     = "right",
          x           = 1
        ),
        plot_bgcolor  = "#fafafa",
        paper_bgcolor = "#ffffff",
        font          = list(family = "Inter, sans-serif", size = 15),
        margin        = list(l = 60, r = 20, t = 50, b = 80)
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
          fillcolor = "rgba(107,114,128,0.13)",
          line      = list(color = "transparent"),
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
          line      = list(color = "#9ca3af", width = 1.5, dash = "dot"),
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
          line      = list(color = col, width = 2.5),
          marker    = list(color = col, size = 8,
                           line = list(color = "#ffffff", width = 1.5)),
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
          line      = list(color = "#dc3545", width = 1.5, dash = "dash"),
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
        font = list(size = 15)
      ),
      xaxis  = list(
        title         = "Review Period",
        categoryorder = "array",
        categoryarray = period_order
      ),
      yaxis  = list(
        title = "Milestone Score (1–9)",
        range = c(0.5, 9.5),
        dtick = 1
      ),
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        font        = list(size = 13)
      ),
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff",
      font          = list(family = "Inter, sans-serif", size = 12),
      margin        = list(l = 60, r = 180, t = 60, b = 80)
    )
  })

  }  # End of inner server function
}  # End of create_server function
