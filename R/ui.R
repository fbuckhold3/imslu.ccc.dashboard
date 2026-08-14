# ui.R - CCC Dashboard
# Thin shell — all content driven by server-side nav state.

ui <- roundsui::roundsui_page(
  title        = "CCC Dashboard",
  base_font    = "Inter",
  heading_font = "Inter",
  # include_shinyjs defaults TRUE and calls shinyjs::useShinyjs() itself -
  # the app's own explicit call (previously here) would have double-inserted it.

  # gmed::load_gmed_styles() removed - every --gmed-*/.gmed-* reference in
  # this app has been recolored onto roundsui tokens (see server.R,
  # helpers.R, custom.css), so nothing needs it anymore. If a future change
  # reintroduces a --gmed-* reference, it will silently lose its styling
  # with no error - grep for "--gmed-" across R/ and www/ before assuming
  # that's safe.

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

      // ── Shiny error events — captures output ID + message ─────────────────
      // Shiny extends the jQuery event with {name: outputId, error: {message, call}}
      $(document).on('shiny:error', function(e) {
        try {
          var outputId  = e.name  || '(unknown output)';
          var errMsg    = (e.error && e.error.message) ? e.error.message : '(no message)';
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('js_error_log', {
              message : 'shiny:error output=' + outputId + ' | ' + errMsg,
              source  : 'shiny-output',
              line    : 0, col: 0, stack: ''
            }, {priority: 'event'});
          }
        } catch(e2) {}
      });
    "))
  ),
  # The .gmed-nav-grid/.gmed-nav-block column-count override that used to
  # live here is gone - the home nav grid now renders via
  # roundsui::roundsui_nav_blocks(), whose own .roundsui-nav-grid is
  # already a genuinely responsive auto-fit grid (no manual breakpoint
  # needed, unlike the fixed 3/2-column rules this replaces).

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
