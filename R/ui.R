# ui.R - UI definition for CCC Dashboard

ui <- fluidPage(
  # Custom CSS
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

      # List View
      conditionalPanel(
        condition = "output.show_resident_list == true",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              h4("Current Review Period"),
              textOutput("current_period_display"),
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
            DT::DTOutput("resident_review_table")
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
