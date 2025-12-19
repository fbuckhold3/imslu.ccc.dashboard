# ui.R - UI definition for CCC Dashboard

ui <- gmed::gmed_page(
  title = "CCC Dashboard",
  theme_variant = "slucare",
  base_font = "Inter",
  heading_font = "Inter",

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
              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "filter_completion",
                    label = "Review Status:",
                    choices = c(
                      "All" = "all",
                      "All Done" = "all_done",
                      "Coach Done" = "coach_done",
                      "Coach & Second Done" = "coach_second_done"
                    ),
                    selected = "all"
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    inputId = "filter_pgy",
                    label = "PGY Level:",
                    choices = c(
                      "All" = "all",
                      "Intern" = "Intern",
                      "PGY2" = "PGY2",
                      "PGY3" = "PGY3"
                    ),
                    selected = "all"
                  )
                ),
                column(
                  width = 3,
                  uiOutput("filter_coach_ui")
                ),
                column(
                  width = 3,
                  uiOutput("filter_second_ui")
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

      fluidRow(
        column(
          width = 12,
          gmed::gmed_card(
            title = "Milestone Analysis Dashboard",
            tags$div(
              style = "padding: 40px; text-align: center;",
              tags$h4("Milestone Analysis Coming Soon", style = "color: #6c757d; margin-bottom: 20px;"),
              tags$p(
                "This section will provide comprehensive milestone analysis tools including:",
                style = "color: #6c757d; font-size: 16px; margin-bottom: 10px;"
              ),
              tags$ul(
                style = "list-style: none; padding: 0; color: #6c757d; font-size: 14px;",
                tags$li(icon("chart-line"), " Cohort performance trends"),
                tags$li(icon("users"), " Comparative resident analysis"),
                tags$li(icon("graduation-cap"), " Competency progression tracking"),
                tags$li(icon("file-export"), " Custom report generation")
              )
            )
          )
        )
      )
    )
  )
  )  # Close conditionalPanel for authenticated content
)
