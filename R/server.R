# server.R - Server logic for CCC Dashboard
# Uses ONLY wrapper functions - never accesses data directly
#
# Note: This function expects rdm_data to be passed as a parameter

create_server <- function(initial_data) {
  function(input, output, session) {

    # Reactive values
    app_data <- reactiveVal(initial_data)
    selected_resident_id <- reactiveVal(NULL)

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

  # Resident review table
  output$resident_review_table <- DT::renderDT({
    req(input$review_period)

    review_table <- get_ccc_review_table(
      app_data(),
      input$review_period
    )

    if (nrow(review_table) == 0) {
      return(data.frame(
        Resident = character(0),
        Level = character(0),
        Period = character(0),
        Completed = character(0)
      ))
    }

    # Format for display
    display_table <- review_table %>%
      select(
        Resident = resident,
        Level = level,
        Period = expected_period,
        Completed = ccc_complete
      ) %>%
      mutate(
        Completed = ifelse(Completed, "✓ Yes", "✗ No")
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
        'Completed',
        backgroundColor = DT::styleEqual(
          c("✓ Yes", "✗ No"),
          c('#d4edda', '#f8d7da')
        )
      )
  })

  # Review statistics
  output$review_stats <- renderUI({
    req(input$review_period)

    review_table <- get_ccc_review_table(
      app_data(),
      input$review_period
    )

    total <- nrow(review_table)
    completed <- sum(review_table$ccc_complete, na.rm = TRUE)
    remaining <- total - completed

    tagList(
      p(
        strong("Total Residents: "), total, br(),
        strong("Completed: "), completed, br(),
        strong("Remaining: "), remaining
      )
    )
  })

  # Handle table row selection - show resident details
  observeEvent(input$resident_review_table_rows_selected, {
    req(input$resident_review_table_rows_selected)

    review_table <- get_ccc_review_table(
      app_data(),
      input$review_period
    )

    selected_rid <- review_table$record_id[input$resident_review_table_rows_selected]
    selected_resident_id(selected_rid)
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

    # Get milestones
    milestones <- get_resident_milestones(
      app_data(),
      rid,
      resident_info$current_period
    )

    # Get existing CCC review
    ccc_review <- get_resident_ccc_review(
      app_data(),
      rid,
      resident_info$current_period
    )

    tagList(
      h3(paste("CCC Review:", resident_info$full_name)),
      p(strong("Level: "), resident_info$current_period),
      hr(),

      h4("Milestone Data"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h5("ACGME Milestones (Previous Period)"),
            if (nrow(milestones$acgme) > 0) {
              p("Data available")
            } else {
              p(em("No data"))
            }
          )
        ),
        column(
          width = 4,
          wellPanel(
            h5("Program Milestones (Current Period)"),
            if (nrow(milestones$program) > 0) {
              p("Data available")
            } else {
              p(em("No data"))
            }
          )
        ),
        column(
          width = 4,
          wellPanel(
            h5("Self-Evaluation (Current Period)"),
            if (nrow(milestones$self) > 0) {
              p("Data available")
            } else {
              p(em("No data"))
            }
          )
        )
      ),
      hr(),

      h4("CCC Review Form"),
      p(em("CCC review form interface will be added here")),
      actionButton(
        "submit_ccc_review",
        "Submit CCC Review",
        class = "btn-primary"
      )
    )
  })

  # ===========================================================================
  # MODE 2: AD HOC DISCUSSION
  # ===========================================================================

  output$adhoc_review_panel <- renderUI({
    req(input$adhoc_resident, input$adhoc_period)

    # Get resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == input$adhoc_resident) %>%
      slice(1)

    if (nrow(resident_info) == 0) {
      return(p("Resident not found"))
    }

    # Get milestones for selected period
    milestones <- get_resident_milestones(
      app_data(),
      input$adhoc_resident,
      input$adhoc_period
    )

    tagList(
      h3(paste("Ad Hoc Discussion:", resident_info$full_name)),
      p(strong("Discussion Period: "), input$adhoc_period),
      hr(),

      h4("Available Data"),
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h5("Program Milestones"),
            if (nrow(milestones$program) > 0) {
              p("✓ Data available")
            } else {
              p("✗ No data")
            }
          )
        ),
        column(
          width = 4,
          wellPanel(
            h5("Self-Evaluation"),
            if (nrow(milestones$self) > 0) {
              p("✓ Data available")
            } else {
              p("✗ No data")
            }
          )
        ),
        column(
          width = 4,
          wellPanel(
            h5("ACGME Milestones"),
            if (nrow(milestones$acgme) > 0) {
              p("✓ Data available")
            } else {
              p("✗ No data")
            }
          )
        )
      ),
      hr(),

      h4("Ad Hoc Review Notes"),
      p(em("Ad hoc review interface will be added here")),
      textAreaInput(
        "adhoc_notes",
        "Discussion Notes:",
        rows = 5,
        width = "100%"
      ),
      actionButton(
        "save_adhoc_notes",
        "Save Notes",
        class = "btn-primary"
      )
    )
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
      residents <- review_table %>%
        select(record_id, full_name = resident, current_period = level)
    } else {
      review_table <- get_ccc_review_table(app_data(), "End Year")
      residents <- review_table %>%
        select(record_id, full_name = resident, current_period = level)
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
