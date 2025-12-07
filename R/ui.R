# ui.R - UI definition for CCC Dashboard

ui <- fluidPage(
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # Title
  titlePanel("SLU Internal Medicine - Clinical Competency Committee Dashboard"),

  # Sidebar for filters
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Academic year selector
      selectInput(
        inputId = "academic_year",
        label = "Academic Year:",
        choices = NULL,  # Will be populated in server
        selected = NULL
      ),

      # Period selector
      selectInput(
        inputId = "period",
        label = "Evaluation Period:",
        choices = c(
          "Period 1 (Jul-Aug)" = 1,
          "Period 2 (Sep-Oct)" = 2,
          "Period 3 (Nov-Dec)" = 3,
          "Period 4 (Jan-Feb)" = 4,
          "Period 5 (Mar-Apr)" = 5,
          "Period 6 (May-Jun)" = 6
        ),
        selected = NULL  # Will be set to current period in server
      ),

      hr(),

      # Refresh data button
      actionButton(
        inputId = "refresh_data",
        label = "Refresh Data",
        icon = icon("sync")
      )
    ),

    # Main panel
    mainPanel(
      width = 9,

      # Tab panel for different views
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",

        # Tab 1: Resident Review Table
        tabPanel(
          title = "Resident Review",
          value = "review_tab",
          br(),
          DT::DTOutput("resident_table"),
          br(),
          uiOutput("review_stats")
        ),

        # Tab 2: Individual Resident View
        tabPanel(
          title = "Resident Details",
          value = "details_tab",
          br(),
          fluidRow(
            column(
              width = 12,
              selectInput(
                inputId = "selected_resident",
                label = "Select Resident:",
                choices = NULL,  # Will be populated in server
                selected = NULL
              )
            )
          ),
          hr(),
          h4("Milestone Evaluations"),
          br(),
          fluidRow(
            column(
              width = 4,
              h5("ACGME Milestones (Previous Period)"),
              plotOutput("plot_acgme")
            ),
            column(
              width = 4,
              h5("Program Milestones (Current Period)"),
              plotOutput("plot_program")
            ),
            column(
              width = 4,
              h5("Self-Evaluation (Current Period)"),
              plotOutput("plot_self")
            )
          ),
          hr(),
          h4("CCC Review Form"),
          uiOutput("ccc_form")
        )
      )
    )
  )
)
