# ui.R - CCC Dashboard
# Thin shell — all content driven by server-side nav state.

ui <- gmed::gmed_page(
  title         = "CCC Dashboard",
  theme_variant = "slucare",
  base_font     = "Inter",
  heading_font  = "Inter",

  shinyjs::useShinyjs(),

  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
    ),
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
      window.onclick = function(event) {
        var modal = document.getElementById('imageModal');
        if (event.target == modal) modal.style.display = 'none';
      }

      // ── Capture all unhandled JavaScript errors and send to R for logging ──
      window.onerror = function(msg, src, line, col, err) {
        try {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('js_error_log', {
              message : msg || '(no message)',
              source  : (src  || '').replace(/.*\\//, ''),
              line    : line || 0,
              col     : col  || 0,
              stack   : (err && err.stack) ? err.stack.substring(0, 500) : ''
            }, {priority: 'event'});
          }
        } catch(e) {}
        return false;   // don't suppress the error
      };

      // ── Shiny disconnect event — fires just before the page grays ──────────
      $(document).on('shiny:disconnected', function(e) {
        // Best-effort: store disconnect info in localStorage for next load
        try {
          localStorage.setItem('shiny_disconnect_at', new Date().toISOString());
        } catch(e2) {}
      });

      // ── Shiny error events ─────────────────────────────────────────────────
      $(document).on('shiny:error', function(e) {
        try {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('js_error_log', {
              message : 'shiny:error ' + (e.message || ''),
              source  : 'shiny-framework',
              line    : 0, col: 0, stack: ''
            }, {priority: 'event'});
          }
        } catch(e2) {}
      });
    ")),

    tags$style(HTML("
      .gmed-nav-grid {
        display: grid !important;
        grid-template-columns: repeat(3, 1fr) !important;
        gap: 20px !important;
      }
      .gmed-nav-block {
        flex-direction: column !important;
        align-items: center !important;
        justify-content: center !important;
        text-align: center !important;
        min-height: 240px !important;
        padding: 40px 28px !important;
        gap: 0 !important;
      }
      .gmed-nav-block-icon {
        font-size: 3.6rem !important;
        padding-top: 0 !important;
        margin-bottom: 20px !important;
      }
      .gmed-nav-block-label {
        font-size: 1.15rem !important;
        margin-bottom: 8px !important;
      }
      .gmed-nav-block-desc {
        font-size: 0.84rem !important;
        line-height: 1.45 !important;
      }
      @media (max-width: 992px) {
        .gmed-nav-grid { grid-template-columns: repeat(2, 1fr) !important; }
      }
    "))
  ),

  # Image modal (used in resident detail view)
  tags$div(id = "imageModal", class = "image-modal",
    tags$span(class = "image-modal-close", onclick = "closeModal()", HTML("&times;")),
    tags$img(id = "modalImage", class = "image-modal-content")
  ),

  div(
    class = "container-fluid py-4 px-4",
    uiOutput("main_view")
  )
)
