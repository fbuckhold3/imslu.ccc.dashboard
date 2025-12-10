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
      # Ad hoc resident selector
      choices <- setNames(residents$record_id, residents$full_name)
      updateSelectizeInput(
        session,
        "adhoc_resident",
        choices = choices,
        server = TRUE
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

  # Resident review table
  output$resident_review_table <- DT::renderDT({
    # Use automatic period detection
    current_period <- get_current_ccc_period()

    review_table <- get_ccc_review_table(
      app_data(),
      current_period
    )

    if (nrow(review_table) == 0) {
      return(data.frame(
        Resident = character(0),
        Level = character(0),
        Coach = character(0),
        Second = character(0),
        CCC = character(0)
      ))
    }

    # Format for display
    display_table <- review_table %>%
      select(
        Resident = resident,
        Level = level,
        Coach = coach_complete,
        Second = second_complete,
        CCC = ccc_complete
      ) %>%
      mutate(
        Coach = ifelse(Coach, "✓", "✗"),
        Second = ifelse(Second, "✓", "✗"),
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
        'Coach',
        color = DT::styleEqual(c("✓", "✗"), c('#28a745', '#dc3545'))
      ) %>%
      DT::formatStyle(
        'Second',
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

    total <- nrow(review_table)
    coach_completed <- sum(review_table$coach_complete, na.rm = TRUE)
    second_completed <- sum(review_table$second_complete, na.rm = TRUE)
    ccc_completed <- sum(review_table$ccc_complete, na.rm = TRUE)

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

    current_period <- get_current_ccc_period()

    review_table <- get_ccc_review_table(
      app_data(),
      current_period
    )

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
      # Resident info header
      gmed::gmed_resident_panel(
        resident_name = resident_info$full_name,
        level = resident_info$current_period,
        coach = if("coach_name" %in% names(resident_info)) resident_info$coach_name else NULL
      ),
      tags$hr(),

      h4("Milestone Review and Data Entry", style = "margin-top: 20px;"),
      fluidRow(
        # Left column (1/3): Editable milestone table and descriptions
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
          ),
          tags$br(),
          gmed::gmed_card(
            title = "Milestone Descriptions",
            p(
              class = "text-muted",
              style = "font-size: 0.9em;",
              "Descriptions provided for specific competencies during this period."
            ),
            DT::DTOutput("milestone_descriptions_table")
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
        "PBLI1", "PBLI2",
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
        paste0("rep_pbli", 1:2),
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
        paste0("PBLI", 1:2),
        paste0("PROF", 1:4),
        paste0("ICS", 1:3)
      )

      template_fields <- c(
        paste0("rep_pc", 1:6),
        paste0("rep_mk", 1:3),
        paste0("rep_sbp", 1:3),
        paste0("rep_pbli", 1:2),
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
        milestone_cols <- grep("^rep_(pc|mk|sbp|pbli|prof|ics)\\d+$",
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

  # ===========================================================================
  # MODE 2: AD HOC DISCUSSION
  # ===========================================================================

  output$adhoc_review_panel <- renderUI({
    req(input$adhoc_resident)

    # Get resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == input$adhoc_resident) %>%
      slice(1)

    if (nrow(resident_info) == 0) {
      return(p("Resident not found"))
    }

    # Use resident's current period
    current_period <- resident_info$current_period

    # Get milestones for current period
    milestones <- get_resident_milestones(
      app_data(),
      input$adhoc_resident,
      current_period
    )

    tagList(
      # Resident info header
      gmed::gmed_resident_panel(
        resident_name = resident_info$full_name,
        level = current_period,
        coach = if("type" %in% names(resident_info)) paste0("Type: ", resident_info$type) else NULL
      ),
      tags$hr(),

      h4("Available Data", style = "margin-top: 20px;"),
      fluidRow(
        column(
          width = 4,
          gmed::gmed_card(
            title = "Program Milestones",
            if (nrow(milestones$program) > 0) {
              gmed::gmed_status_badge("Complete", "complete")
            } else {
              gmed::gmed_status_badge("No data", "incomplete")
            }
          )
        ),
        column(
          width = 4,
          gmed::gmed_card(
            title = "Self-Evaluation",
            if (nrow(milestones$self) > 0) {
              gmed::gmed_status_badge("Complete", "complete")
            } else {
              gmed::gmed_status_badge("No data", "incomplete")
            }
          )
        ),
        column(
          width = 4,
          gmed::gmed_card(
            title = "ACGME Milestones",
            if (nrow(milestones$acgme) > 0) {
              gmed::gmed_status_badge("Complete", "complete")
            } else {
              gmed::gmed_status_badge("No data", "incomplete")
            }
          )
        )
      ),
      tags$hr(),

      h4("Ad Hoc Review Form", style = "margin-top: 30px;"),

      # Action Data Table for Ad Hoc
      gmed::gmed_card(
        title = "Previous Action Items",
        DT::DTOutput("adhoc_action_data_table")
      ),
      tags$br(),

      # Interim Update
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Interim Update",
            textAreaInput(
              "adhoc_ccc_interim",
              "Interim Notes:",
              value = "",
              rows = 4,
              width = "100%",
              placeholder = "Enter interim update notes..."
            )
          )
        )
      ),

      # ILP Section
      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Individual Learning Plan",
            textAreaInput(
              "adhoc_ccc_ilp",
              "CCC ILP:",
              value = "",
              rows = 4,
              width = "100%",
              placeholder = "Enter CCC Individual Learning Plan..."
            )
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
              "adhoc_ccc_issues_yn",
              "Any Follow-up Issues?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = "0",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.adhoc_ccc_issues_yn == '1'",
              textAreaInput(
                "adhoc_ccc_issues_follow_up",
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
              "adhoc_ccc_concern",
              "Any Concerns?",
              choices = c("No" = "0", "Yes" = "1"),
              selected = "0",
              inline = TRUE
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
        icon = icon("check")
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

  # Ad Hoc Concern Content
  output$adhoc_concern_content <- renderUI({
    req(input$adhoc_ccc_concern)

    if (input$adhoc_ccc_concern == "0") {
      return(NULL)
    }

    # Get checkbox choices from data dictionary (for_ui = TRUE to get labels as names)
    competency_choices <- get_field_choices(app_data()$data_dict, "ccc_competency", for_ui = TRUE)
    action_choices <- get_field_choices(app_data()$data_dict, "ccc_action", for_ui = TRUE)
    status_choices <- get_field_choices(app_data()$data_dict, "ccc_action_status", for_ui = TRUE)

    tagList(
      textAreaInput(
        "adhoc_ccc_comments",
        "CCC Comments:",
        value = "",
        rows = 3,
        width = "100%",
        placeholder = "Describe the concerns..."
      ),
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
      ccc_ilp = if (!is.null(input$adhoc_ccc_ilp) && nchar(trimws(input$adhoc_ccc_ilp)) > 0) as.character(input$adhoc_ccc_ilp) else "",
      ccc_mile = "0",  # Not applicable for ad hoc
      ccc_mile_notes = "",
      ccc_issues_follow_up = if (!is.null(input$adhoc_ccc_issues_follow_up) && nchar(trimws(input$adhoc_ccc_issues_follow_up)) > 0) as.character(input$adhoc_ccc_issues_follow_up) else "",
      ccc_concern = if (!is.null(input$adhoc_ccc_concern)) as.character(input$adhoc_ccc_concern) else "0",
      ccc_comments = if (!is.null(input$adhoc_ccc_comments) && nchar(trimws(input$adhoc_ccc_comments)) > 0) as.character(input$adhoc_ccc_comments) else "",
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
        updateTextAreaInput(session, "adhoc_ccc_ilp", value = "")
        updateRadioButtons(session, "adhoc_ccc_issues_yn", selected = "0")
        updateTextAreaInput(session, "adhoc_ccc_issues_follow_up", value = "")
        updateRadioButtons(session, "adhoc_ccc_concern", selected = "0")
        updateTextAreaInput(session, "adhoc_ccc_comments", value = "")

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

  }  # End of inner server function
}  # End of create_server function
