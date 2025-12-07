# ui.R - UI definition for CCC Dashboard

ui <- fluidPage(
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # Title
  titlePanel("SLU Internal Medicine - Clinical Competency Committee Dashboard"),

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

      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Review Period"),
            selectInput(
              inputId = "review_period",
              label = "Select Review Period:",
              choices = c("Mid Year", "End Year"),
              selected = get_current_ccc_period()
            ),
            hr(),
            actionButton(
              inputId = "refresh_data",
              label = "Refresh Data",
              icon = icon("sync"),
              class = "btn-primary"
            )
          ),

          wellPanel(
            h4("Review Progress"),
            uiOutput("review_stats")
          )
        ),

        column(
          width = 9,
          h3("Residents for Review"),
          DT::DTOutput("resident_review_table"),
          br(),
          hr(),

          # Resident detail panel (shown when resident selected)
          uiOutput("resident_detail_panel")
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
          wellPanel(
            h4("Select Resident"),
            selectizeInput(
              inputId = "adhoc_resident",
              label = "Resident:",
              choices = NULL,  # Populated in server
              selected = NULL,
              options = list(
                placeholder = "Type to search...",
                maxOptions = 100
              )
            ),
            hr(),
            selectInput(
              inputId = "adhoc_period",
              label = "Discussion Period:",
              choices = PERIOD_NAMES,
              selected = NULL
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
    # MODE 3: ADMIN PAGE
    # ===========================================================================
    tabPanel(
      title = "Admin",
      value = "admin",

      br(),

      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Admin Functions"),
            selectInput(
              inputId = "admin_view",
              label = "View:",
              choices = c(
                "All Residents" = "all",
                "Mid Year Reviews" = "mid",
                "End Year Reviews" = "end"
              ),
              selected = "all"
            ),
            hr(),
            actionButton(
              inputId = "admin_add_data",
              label = "Enter Data",
              icon = icon("plus"),
              class = "btn-success"
            )
          )
        ),

        column(
          width = 9,
          h3("Resident Data"),
          DT::DTOutput("admin_resident_table"),
          br(),
          hr(),

          # Data entry panel (shown when entering data)
          uiOutput("admin_data_entry_panel")
        )
      )
    )
  )
)
