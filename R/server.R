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
      h3(paste("CCC Review:", resident_info$full_name)),
      p(strong("Level: "), resident_info$current_period),
      hr(),

      h4("Milestone Visualizations"),
      fluidRow(
        column(
          width = 4,
          h5("ACGME Milestones (Previous Period)"),
          plotly::plotlyOutput("plot_acgme_spider", height = "400px")
        ),
        column(
          width = 4,
          h5("Program Milestones (Current Period)"),
          plotly::plotlyOutput("plot_program_spider", height = "400px")
        ),
        column(
          width = 4,
          h5("Self-Evaluation (Current Period)"),
          plotly::plotlyOutput("plot_self_spider", height = "400px")
        )
      ),
      hr(),

      h4("Previous Reviews"),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            h5("Coach Review"),
            uiOutput("coach_review_summary")
          )
        ),
        column(
          width = 6,
          wellPanel(
            h5("Second Review"),
            uiOutput("second_review_summary")
          )
        )
      ),
      hr(),

      h4("Milestone Descriptions"),
      p(
        class = "text-muted",
        "Descriptions provided for specific competencies during this period."
      ),
      DT::DTOutput("milestone_descriptions_table"),
      br(),

      h4("Edit Program Milestones"),
      p(
        class = "text-muted",
        "Review and edit milestone ratings for this period (1-9 scale). Changes will be saved to the same REDCap instance."
      ),
      DT::DTOutput("milestone_edit_table"),
      br(),
      fluidRow(
        column(
          width = 12,
          actionButton(
            "save_milestone_edits",
            "Save Milestone Changes",
            class = "btn-warning",
            icon = icon("save")
          ),
          span(
            class = "text-muted",
            style = "margin-left: 15px;",
            "This will update the milestone entry in REDCap"
          )
        )
      ),
      hr(),

      h4("CCC Review Form"),
      p(
        class = "text-muted",
        "Complete the following fields based on the committee's discussion and review of the resident's performance."
      ),

      fluidRow(
        column(
          width = 12,
          textAreaInput(
            "ccc_discussion_notes",
            "CCC Discussion Notes:",
            value = "",
            rows = 4,
            width = "100%",
            placeholder = "Summarize the key points discussed by the committee..."
          )
        )
      ),

      fluidRow(
        column(
          width = 6,
          textAreaInput(
            "ccc_areas_of_strength",
            "Areas of Strength:",
            value = "",
            rows = 4,
            width = "100%",
            placeholder = "Highlight the resident's strengths and positive performance areas..."
          )
        ),
        column(
          width = 6,
          textAreaInput(
            "ccc_areas_of_concern",
            "Areas of Concern:",
            value = "",
            rows = 4,
            width = "100%",
            placeholder = "Note any areas requiring improvement or monitoring..."
          )
        )
      ),

      fluidRow(
        column(
          width = 12,
          textAreaInput(
            "ccc_recommendations",
            "CCC Recommendations:",
            value = "",
            rows = 4,
            width = "100%",
            placeholder = "Provide specific recommendations for the resident's continued development..."
          )
        )
      ),

      fluidRow(
        column(
          width = 12,
          textAreaInput(
            "ccc_action_items",
            "Action Items / Follow-up:",
            value = "",
            rows = 3,
            width = "100%",
            placeholder = "List any specific action items or follow-up required..."
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

  # Coach Review Summary
  output$coach_review_summary <- renderUI({
    rid <- selected_resident_id()
    req(rid)

    resident_info <- app_data()$residents %>%
      filter(record_id == rid) %>%
      slice(1)

    coach_data <- get_resident_coach_review(
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
    # Field names may vary - adapt based on actual REDCap form
    tagList(
      if ("coach_strengths" %in% names(coach_data) && !is.na(coach_data$coach_strengths[1])) {
        div(
          p(strong("Strengths:")),
          p(coach_data$coach_strengths[1])
        )
      },
      if ("coach_concerns" %in% names(coach_data) && !is.na(coach_data$coach_concerns[1])) {
        div(
          p(strong("Concerns:")),
          p(coach_data$coach_concerns[1])
        )
      },
      if ("coach_recommendations" %in% names(coach_data) && !is.na(coach_data$coach_recommendations[1])) {
        div(
          p(strong("Recommendations:")),
          p(coach_data$coach_recommendations[1])
        )
      },
      # Fallback: show completion status if no specific fields available
      if (!any(c("coach_strengths", "coach_concerns", "coach_recommendations") %in% names(coach_data))) {
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

    second_data <- get_resident_second_review(
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
    # Field names may vary - adapt based on actual REDCap form
    tagList(
      if ("second_strengths" %in% names(second_data) && !is.na(second_data$second_strengths[1])) {
        div(
          p(strong("Strengths:")),
          p(second_data$second_strengths[1])
        )
      },
      if ("second_concerns" %in% names(second_data) && !is.na(second_data$second_concerns[1])) {
        div(
          p(strong("Concerns:")),
          p(second_data$second_concerns[1])
        )
      },
      if ("second_recommendations" %in% names(second_data) && !is.na(second_data$second_recommendations[1])) {
        div(
          p(strong("Recommendations:")),
          p(second_data$second_recommendations[1])
        )
      },
      # Fallback: show completion status if no specific fields available
      if (!any(c("second_strengths", "second_concerns", "second_recommendations") %in% names(second_data))) {
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
        Source = "",
        Description = "",
        stringsAsFactors = FALSE
      )
    } else {
      names(descriptions) <- c("Competency", "Source", "Description")
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

    if (nrow(milestone_values) == 0) {
      milestone_values <- data.frame(
        Competency = "No milestone data available",
        Value = NA,
        stringsAsFactors = FALSE
      )
      editable <- FALSE
    } else {
      # Keep field_name for later use but don't display it
      display_data <- data.frame(
        Competency = milestone_values$competency,
        Value = milestone_values$value,
        stringsAsFactors = FALSE
      )
      milestone_values <- display_data
      editable <- TRUE
    }

    DT::datatable(
      milestone_values,
      options = list(
        pageLength = 25,
        dom = 't',
        ordering = TRUE,
        searching = FALSE
      ),
      rownames = FALSE,
      selection = 'none',
      editable = if (editable) list(target = 'cell', disable = list(columns = c(0))) else FALSE
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

    # Collect form data
    ccc_data <- data.frame(
      record_id = rid,
      redcap_repeat_instrument = "ccc_review",
      redcap_repeat_instance = NA,  # REDCap will auto-assign
      ccc_session = resident_info$current_period,
      ccc_discussion_notes = input$ccc_discussion_notes,
      ccc_areas_of_strength = input$ccc_areas_of_strength,
      ccc_areas_of_concern = input$ccc_areas_of_concern,
      ccc_recommendations = input$ccc_recommendations,
      ccc_action_items = input$ccc_action_items,
      ccc_review_complete = 2,  # Complete status
      stringsAsFactors = FALSE
    )

    # Validate required fields
    if (nchar(trimws(input$ccc_discussion_notes)) == 0) {
      showNotification(
        "Please enter discussion notes before submitting.",
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

    if (nrow(milestone_entry_data) == 0) {
      showNotification(
        "No milestone entry found for this period.",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Get the milestone values with field names
    milestone_values <- get_milestone_values_for_edit(
      app_data(),
      rid,
      resident_info$current_period
    )

    # Get current table data (with edits)
    # Note: DT editable tables update via input$milestone_edit_table_cell_edit
    # We need to get the proxy data or rebuild from edits
    # For now, let's just show a message that edits are being processed

    # Build data frame for REDCap submission
    # Start with the original milestone entry data
    update_data <- milestone_entry_data[1, ]

    # Check if there were any cell edits
    if (!is.null(input$milestone_edit_table_cell_edit)) {
      # Process cell edits
      edit_info <- input$milestone_edit_table_cell_edit

      # Get the row and column that was edited
      row_idx <- edit_info$row
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

        # Update the value in update_data
        update_data[[field_name]] <- numeric_value

        # Try to save to REDCap
        tryCatch({
          result <- REDCapR::redcap_write(
            ds_to_write = update_data,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          if (result$success) {
            showNotification(
              paste("Milestone values updated successfully for", resident_info$full_name),
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
      h3(paste("Ad Hoc Discussion:", resident_info$full_name)),
      p(
        strong("Level: "), current_period, br(),
        strong("Type: "), resident_info$type, br(),
        strong("Graduation Year: "), resident_info$grad_yr
      ),
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
