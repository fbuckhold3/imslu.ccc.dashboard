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
            gmed::gmed_selector_container(
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
          gmed::gmed_card(
            title = "Admin Functions",
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
            tags$hr(style = "margin: 15px 0;"),
            actionButton(
              inputId = "admin_add_data",
              label = "Enter Data",
              icon = icon("plus"),
              class = "btn-success w-100"
            )
          )
        ),

        column(
          width = 9,
          gmed::gmed_card(
            title = "Resident Data",
            DT::DTOutput("admin_resident_table")
          ),
          tags$br(),

          # Data entry panel (shown when entering data)
          uiOutput("admin_data_entry_panel")
        )
      )
    )
  )
)
