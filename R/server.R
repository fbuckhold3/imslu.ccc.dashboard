# server.R - Server logic for CCC Dashboard
# Uses ONLY wrapper functions - never accesses data directly

server <- function(input, output, session) {

  # Reactive values
  app_data <- reactiveVal(data)

  # Initialize UI elements with default values
  observe({
    # Set academic year choices
    # TODO: Generate list of academic years from data
    updateSelectInput(
      session,
      "academic_year",
      choices = current_academic_year,
      selected = current_academic_year
    )

    # Set current period as default
    updateSelectInput(
      session,
      "period",
      selected = current_period
    )

    # Populate resident list
    residents <- get_resident_list(app_data())
    updateSelectInput(
      session,
      "selected_resident",
      choices = residents,
      selected = if (length(residents) > 0) residents[1] else NULL
    )
  })

  # Handle data refresh
  observeEvent(input$refresh_data, {
    tryCatch({
      new_data <- load_app_data()
      app_data(new_data)
      showNotification("Data refreshed successfully", type = "message")
    }, error = function(e) {
      showNotification(
        paste("Error refreshing data:", e$message),
        type = "error"
      )
    })
  })

  # Resident review table
  output$resident_table <- renderDT({
    req(input$academic_year, input$period)

    review_table <- get_ccc_review_table(
      app_data(),
      input$academic_year,
      as.integer(input$period)
    )

    datatable(
      review_table,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = 'ft'
      )
    ) %>%
      formatStyle(
        'completed',
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c('#d4edda', '#f8d7da')
        )
      )
  })

  # Review statistics
  output$review_stats <- renderUI({
    req(input$academic_year, input$period)

    review_table <- get_ccc_review_table(
      app_data(),
      input$academic_year,
      as.integer(input$period)
    )

    total <- nrow(review_table)
    completed <- sum(review_table$completed, na.rm = TRUE)
    remaining <- total - completed

    tagList(
      h4("Review Progress"),
      p(
        strong("Total Residents: "), total, br(),
        strong("Completed: "), completed, br(),
        strong("Remaining: "), remaining
      )
    )
  })

  # ACGME milestones plot (previous period)
  output$plot_acgme <- renderPlot({
    req(input$selected_resident, input$period)

    milestones <- get_resident_acgme_milestones(
      app_data(),
      input$selected_resident,
      as.integer(input$period)
    )

    if (nrow(milestones) == 0) {
      plot.new()
      text(0.5, 0.5, "No ACGME data available", cex = 1.2)
      return()
    }

    # TODO: Create milestone visualization
    plot.new()
    text(0.5, 0.5, "ACGME Milestone Plot", cex = 1.2)
  })

  # Program milestones plot (current period)
  output$plot_program <- renderPlot({
    req(input$selected_resident, input$period)

    milestones <- get_resident_program_milestones(
      app_data(),
      input$selected_resident,
      as.integer(input$period)
    )

    if (nrow(milestones) == 0) {
      plot.new()
      text(0.5, 0.5, "No program data available", cex = 1.2)
      return()
    }

    # TODO: Create milestone visualization
    plot.new()
    text(0.5, 0.5, "Program Milestone Plot", cex = 1.2)
  })

  # Self-evaluation milestones plot (current period)
  output$plot_self <- renderPlot({
    req(input$selected_resident, input$period)

    milestones <- get_resident_self_milestones(
      app_data(),
      input$selected_resident,
      as.integer(input$period)
    )

    if (nrow(milestones) == 0) {
      plot.new()
      text(0.5, 0.5, "No self-evaluation data available", cex = 1.2)
      return()
    }

    # TODO: Create milestone visualization
    plot.new()
    text(0.5, 0.5, "Self-Evaluation Milestone Plot", cex = 1.2)
  })

  # CCC review form
  output$ccc_form <- renderUI({
    req(input$selected_resident, input$period)

    # TODO: Build CCC review form
    tagList(
      p("CCC review form will be displayed here"),
      actionButton("submit_review", "Submit Review", class = "btn-primary")
    )
  })

  # Handle table row selection - switch to details tab
  observeEvent(input$resident_table_rows_selected, {
    req(input$resident_table_rows_selected)

    review_table <- get_ccc_review_table(
      app_data(),
      input$academic_year,
      as.integer(input$period)
    )

    selected_resident <- review_table$resident[input$resident_table_rows_selected]

    updateSelectInput(
      session,
      "selected_resident",
      selected = selected_resident
    )

    updateTabsetPanel(session, "main_tabs", selected = "details_tab")
  })
}
