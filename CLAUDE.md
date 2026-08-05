# imslu.ccc.dashboard

CCC (Clinical Competency Committee) review dashboard for IMSLU Internal
Medicine. See [README.md](README.md) for setup, deployment, and project
structure — this file covers architecture and gotchas not obvious from
the code.

**Repo:** `fbuckhold3/imslu.ccc.dashboard`. There is a similarly-named
`imres.ccc.dashboard` repo that a prior session accidentally pushed work
to — that repo is NOT this project. Always double-check the remote
before pushing.

## Data loading

Two-phase load, not a single blocking pull:
- Phase 1 (~4-6s): initial data needed to render the resident list.
- Phase 2 (background `future`, 15-30s total): the rest, loaded async.
- `app_data <- reactiveVal(initial_data)` holds current state; a polling
  observer swaps in the Phase 2 result when it completes.
- `server_state` (an environment) is passed from `app.R` into
  `create_server()` so state is shared consistently across the session.

All server logic lives in `R/server.R`; data access goes through
`R/wrappers.R` per the README's wrapper convention — server code should
never reach into `data$table_name` directly.

## Toggle buttons (Evaluations / Plus-Delta / Board / Milestone History)

These buttons are deliberately **static** inside the `resident_detail_panel`
`renderUI`, with `shinyjs::toggleClass` flipping visibility via CSS rather
than re-rendering the buttons themselves. An earlier version put all four
buttons inside a single `renderUI` that re-ran on click, which reset each
button's click count and caused a double-fire feedback loop. Don't move
toggle state back into a `renderUI` without re-solving that.

## gmed modules wired into the server

- `gmed::mod_eval_feedback_server()` — replaces the old local
  `mod_eval_table`; matches the evaluations view in `imslu.ind.dash`.
- `gmed::mod_plus_delta_table_server()`
- `gmed::mod_seval_boards_display_server()`

## Milestone history

`ccc_sel_mile` selects one milestone (PC1, MK1, …); a plotly chart shows
that milestone's score across periods. The graduation-target dotted line
is at **y=7**, not y=4 — matches the ACGME benchmark, not a generic
midpoint.
