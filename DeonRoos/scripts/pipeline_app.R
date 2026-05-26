# Purpose: Shiny app for running the camtrapR detection history pipeline.
# Inputs:  Epicollect deployment CSV, Epicollect retrieval CSV,
#          classifications CSV from classify_app.R
# Outputs: record_table_all_species.csv, <focal>_detection_history.csv,
#          <focal>_effort.csv

library(shiny)
library(tidyverse)
library(lubridate)
library(camtrapR)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&display=swap');

      * { box-sizing: border-box; }
      body { background: #161616; color: #e0e0e0;
             font-family: 'IBM Plex Mono', monospace; margin: 0; }

      .app-title { font-size: 14px; letter-spacing: 4px; text-transform: uppercase;
                   color: #78a849; padding: 18px 24px 0 24px; margin-bottom: 0; }

      .panel { background: #1f1f1f; border: 1px solid #2a2a2a;
               border-radius: 3px; padding: 18px; }

      .sec-label { font-size: 10px; color: #555; letter-spacing: 3px;
                   text-transform: uppercase; margin: 16px 0 8px; }
      .sec-label.first { margin-top: 0; }

      label { font-size: 10px !important; color: #888 !important;
              letter-spacing: 1px; }
      .form-control { background: #2a2a2a !important; border: 1px solid #333 !important;
                      color: #e0e0e0 !important; border-radius: 3px !important;
                      font-family: inherit !important; font-size: 11px !important; }
      .form-control:focus { border-color: #78a849 !important; box-shadow: none !important; }

      /* File upload widget */
      .input-group-btn .btn { background: #2a2a2a; border: 1px solid #333;
                               color: #aaa; font-family: inherit; font-size: 10px;
                               letter-spacing: 1px; }
      .input-group-btn .btn:hover { border-color: #78a849; color: #78a849; }

      hr.div { border: none; border-top: 1px solid #2a2a2a; margin: 16px 0; }

      /* Run button */
      .btn-run { width: 100%; background: #1e3a0f; border: 1px solid #78a849;
                 color: #78a849; font-family: inherit; font-size: 11px;
                 letter-spacing: 2px; text-transform: uppercase; padding: 10px;
                 border-radius: 3px; cursor: pointer; margin-top: 20px;
                 transition: all 0.15s; }
      .btn-run:hover:not(:disabled) { background: #78a849; color: #161616; }
      .btn-run:disabled { opacity: 0.3; cursor: not-allowed; }
      /* Override Shiny's default actionButton blue */
      .btn-run.btn-default { background: #1e3a0f !important; color: #78a849 !important;
                              border-color: #78a849 !important; }
      .btn-run.btn-default:hover { background: #78a849 !important; color: #161616 !important; }

      /* Log */
      .log-wrap { font-size: 10px; line-height: 2; min-height: 48px; }
      .log-ok  { color: #78a849; }
      .log-err { color: #c0392b; }
      .log-inf { color: #555; }

      /* Download buttons */
      .shiny-download-link {
        display: block;
        background: #2a2a2a;
        border: 1px solid #333;
        color: #aaa !important;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px;
        letter-spacing: 1.5px;
        text-transform: uppercase;
        padding: 8px 12px;
        border-radius: 3px;
        cursor: pointer;
        margin-bottom: 6px;
        text-align: left;
        transition: all 0.15s;
        width: 100%;
        text-decoration: none !important;
      }
      .shiny-download-link:hover { border-color: #78a849; color: #78a849 !important; }

      /* Preview table */
      .preview-wrap { overflow-x: auto; margin-top: 6px; }
      .preview-wrap table { width: 100%; border-collapse: collapse; font-size: 10px; }
      .preview-wrap th { color: #78a849; border-bottom: 1px solid #333;
                         padding: 4px 8px; text-align: left; letter-spacing: 1px;
                         white-space: nowrap; }
      .preview-wrap td { color: #888; border-bottom: 1px solid #222;
                         padding: 3px 8px; white-space: nowrap; }
    "))
  ),

  tags$h1(class = "app-title", "Camera Trap Pipeline"),

  fluidRow(
    style = "margin: 0; padding: 12px 24px;",

    # ── Left: inputs ───────────────────────────────────────────
    column(4, style = "padding-right: 10px;",
      div(class = "panel",

        div(class = "sec-label first", "Epicollect Exports"),
        fileInput("deploy_csv",   "Deployment CSV",   accept = ".csv"),
        fileInput("retrieve_csv", "Retrieval CSV",    accept = ".csv"),

        tags$hr(class = "div"),

        div(class = "sec-label", "Classifications"),
        fileInput("classif_csv", "Classifications CSV (from classifier app)", accept = ".csv"),

        tags$hr(class = "div"),

        div(class = "sec-label", "Settings"),
        textInput("focal",   "Focal species",          value = "Fox",  width = "100%"),
        numericInput("occ_len", "Occasion length (days)", value = 1, min = 1, step = 1, width = "100%"),

        actionButton("run_btn", "Run pipeline", class = "btn-run")
      )
    ),

    # ── Right: outputs ─────────────────────────────────────────
    column(8, style = "padding-left: 10px;",
      div(class = "panel",
        div(class = "sec-label first", "Status"),
        div(class = "log-wrap", uiOutput("log")),

        uiOutput("download_ui"),
        uiOutput("preview_ui")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  results <- reactiveValues(
    ok           = FALSE,
    log          = character(),
    record_table = NULL,
    det_hist     = NULL,
    effort       = NULL,
    focal        = "Fox"
  )

  add_log <- function(msg, type = "inf") {
    cls <- switch(type, ok = "log-ok", err = "log-err", "log-inf")
    results$log <- c(results$log, sprintf('<span class="%s">%s</span>', cls, htmltools::htmlEscape(msg)))
  }

  observeEvent(input$run_btn, {
    results$ok           <- FALSE
    results$log          <- character()
    results$record_table <- NULL
    results$det_hist     <- NULL
    results$effort       <- NULL

    # ── Validate uploads ────────────────────────────────────────
    missing_files <- c(
      if (is.null(input$deploy_csv))   "deployment CSV",
      if (is.null(input$retrieve_csv)) "retrieval CSV",
      if (is.null(input$classif_csv))  "classifications CSV"
    )
    if (length(missing_files) > 0) {
      for (f in missing_files) add_log(paste("✗ Missing:", f), "err")
      return()
    }

    focal      <- trimws(input$focal)
    occ_length <- as.integer(input$occ_len)

    add_log("Reading input files…")

    # ── Read CSVs ───────────────────────────────────────────────
    deployment <- tryCatch(
      read_csv(input$deploy_csv$datapath,   show_col_types = FALSE),
      error = function(e) { add_log(paste("✗ Deployment CSV:", conditionMessage(e)), "err"); NULL }
    )
    if (is.null(deployment)) return()

    retrieval <- tryCatch(
      read_csv(input$retrieve_csv$datapath, show_col_types = FALSE),
      error = function(e) { add_log(paste("✗ Retrieval CSV:", conditionMessage(e)), "err"); NULL }
    )
    if (is.null(retrieval)) return()

    classifications <- tryCatch(
      read_csv(input$classif_csv$datapath,  show_col_types = FALSE),
      error = function(e) { add_log(paste("✗ Classifications CSV:", conditionMessage(e)), "err"); NULL }
    )
    if (is.null(classifications)) return()

    add_log(sprintf("  deployment:      %d rows", nrow(deployment)))
    add_log(sprintf("  retrieval:       %d rows", nrow(retrieval)))
    add_log(sprintf("  classifications: %d rows", nrow(classifications)))

    # ── Join deployment + retrieval ─────────────────────────────
    add_log("Joining deployment and retrieval records…")

    stations <- tryCatch({
      dep <- deployment |>
        transmute(
          Station       = `2_Site_ID`,
          Latitude      = lat_1_Location,
          Longitude     = long_1_Location,
          Setup_date    = dmy(`4_Date_of_deployment`),
          Camera_ID     = `7_Camera_ID`,
          Camera_model  = `6_Camera_type`,
          Habitat       = `9_Dominant_habitat`,
          Field_of_view = `10_Field_of_view`,
          Deploy_issues = `11_Deployment_issues`,
          ec5_uuid      = ec5_uuid
        )
      ret <- retrieval |>
        transmute(
          Retrieval_date   = dmy(`13_Date`),
          Camera_status    = `14_Camera_status`,
          Retrieval_issues = `16_Retrieval_issues`,
          ec5_uuid         = ec5_parent_uuid
        )
      dep |> left_join(ret, by = "ec5_uuid") |> select(-ec5_uuid)
    }, error = function(e) {
      add_log(paste("✗ Failed to join CSVs:", conditionMessage(e)), "err")
      NULL
    })
    if (is.null(stations)) return()

    no_retrieval <- stations |> filter(is.na(Retrieval_date))
    if (nrow(no_retrieval) > 0) {
      add_log(sprintf("  ⚠ %d site(s) with no retrieval record (excluded): %s",
                      nrow(no_retrieval), paste(no_retrieval$Station, collapse = ", ")))
    }

    faulty <- stations |> filter(!is.na(Camera_status) & Camera_status != "Functioning")
    if (nrow(faulty) > 0) {
      add_log(sprintf("  ⚠ %d site(s) with non-functioning camera: %s",
                      nrow(faulty), paste(faulty$Station, collapse = ", ")))
    }

    stations <- stations |> filter(!is.na(Retrieval_date))
    add_log(sprintf("  %d stations included", nrow(stations)), "ok")

    # camtrapR requires Setup_date < Retrieval_date; nudge same-day deployments
    same_day <- stations |> filter(Setup_date == Retrieval_date)
    if (nrow(same_day) > 0) {
      add_log(sprintf(
        "  ⚠ %d site(s) deployed and retrieved on the same day — retrieval nudged to +1 day: %s",
        nrow(same_day), paste(same_day$Station, collapse = ", ")
      ))
      stations <- stations |>
        mutate(Retrieval_date = if_else(Setup_date == Retrieval_date,
                                        Retrieval_date + days(1),
                                        Retrieval_date))
    }

    # ── Camera operation matrix ─────────────────────────────────
    add_log("Building camera operation matrix…")
    cam_op <- tryCatch(
      cameraOperation(
        CTtable      = stations,
        stationCol   = "Station",
        setupCol     = "Setup_date",
        retrievalCol = "Retrieval_date",
        hasProblems  = FALSE,
        dateFormat   = "ymd"
      ),
      error = function(e) {
        add_log(paste("✗ cameraOperation failed:", conditionMessage(e)), "err"); NULL
      }
    )
    if (is.null(cam_op)) return()
    add_log("  done", "ok")

    # ── Record table ────────────────────────────────────────────
    add_log("Building species record table…")
    record_table <- tryCatch({
      classifications |>
        mutate(DateTimeOriginal = ymd_hms(DateTime)) |>
        group_by(Station, Burst_ID, Species) |>
        summarise(
          DateTimeOriginal = min(DateTimeOriginal),
          Count            = max(Count),
          .groups          = "drop"
        ) |>
        arrange(Station, DateTimeOriginal)
    }, error = function(e) {
      add_log(paste("✗ Record table failed:", conditionMessage(e)), "err"); NULL
    })
    if (is.null(record_table)) return()

    n_sp <- n_distinct(record_table$Species)
    add_log(sprintf("  %d detection events across %d species", nrow(record_table), n_sp), "ok")

    # ── Temporal overlap check ──────────────────────────────────
    # Warn if any station's detections fall entirely outside its deployment window
    det_range <- record_table |>
      group_by(Station) |>
      summarise(first_det = min(as.Date(DateTimeOriginal)),
                last_det  = max(as.Date(DateTimeOriginal)), .groups = "drop")

    overlap_check <- stations |>
      select(Station, Setup_date, Retrieval_date) |>
      left_join(det_range, by = "Station") |>
      filter(!is.na(first_det)) |>
      filter(last_det < Setup_date | first_det > Retrieval_date)

    if (nrow(overlap_check) > 0) {
      for (i in seq_len(nrow(overlap_check))) {
        m <- overlap_check[i, ]
        add_log(sprintf(
          "  ⚠ %s: detections %s – %s are outside deployment window %s – %s",
          m$Station,
          format(m$first_det,    "%Y-%m-%d"),
          format(m$last_det,     "%Y-%m-%d"),
          format(m$Setup_date,   "%Y-%m-%d"),
          format(m$Retrieval_date, "%Y-%m-%d")
        ))
      }
      add_log(paste0("  ⚠ Detections fall outside the deployment window.",
                     " Check that your deployment/retrieval CSV is from the",
                     " same field season as the classifications CSV."))
    }

    # ── Detection history ───────────────────────────────────────
    add_log(sprintf("Generating detection history for %s…", focal))
    record_focal <- record_table |> filter(Species == focal)

    det_hist_obj <- NULL
    if (nrow(record_focal) == 0) {
      add_log(sprintf("  ⚠ No detections of %s found", focal))
    } else {
      det_hist_obj <- tryCatch(
        detectionHistory(
          recordTable       = record_focal,
          camOp             = cam_op,
          stationCol        = "Station",
          speciesCol        = "Species",
          recordDateTimeCol = "DateTimeOriginal",
          species           = focal,
          occasionLength    = occ_length,
          day1              = "station",
          includeEffort     = TRUE,
          timeZone          = "Europe/London"
        ),
        error = function(e) {
          add_log(paste("✗ detectionHistory failed:", conditionMessage(e)), "err"); NULL
        }
      )
      if (!is.null(det_hist_obj)) {
        add_log(sprintf("  %d sites × %d occasions",
                        nrow(det_hist_obj$detection_history),
                        ncol(det_hist_obj$detection_history)), "ok")
      }
    }

    results$record_table <- record_table
    results$det_hist  <- if (!is.null(det_hist_obj)) as.data.frame(det_hist_obj$detection_history) else NULL
    results$effort    <- if (!is.null(det_hist_obj)) as.data.frame(det_hist_obj$effort) else NULL
    results$focal     <- focal
    results$ok        <- TRUE
    add_log("Pipeline complete.", "ok")
  })

  # ── Log ─────────────────────────────────────────────────────
  output$log <- renderUI({
    if (length(results$log) == 0)
      return(tags$span(class = "log-inf", "Upload files and click Run pipeline."))
    HTML(paste(results$log, collapse = "<br>"))
  })

  # ── Download buttons ─────────────────────────────────────────
  output$download_ui <- renderUI({
    req(results$ok)
    focal <- results$focal
    tagList(
      tags$hr(class = "div"),
      div(class = "sec-label", "Downloads"),
      downloadButton("dl_record",  "record_table_all_species.csv"),
      if (!is.null(results$det_hist))
        downloadButton("dl_dethist", paste0(focal, "_detection_history.csv")),
      if (!is.null(results$effort))
        downloadButton("dl_effort",  paste0(focal, "_effort.csv"))
    )
  })

  output$dl_record <- downloadHandler(
    filename = "record_table_all_species.csv",
    content  = function(f) write_csv(results$record_table, f)
  )
  output$dl_dethist <- downloadHandler(
    filename = function() paste0(results$focal, "_detection_history.csv"),
    content  = function(f) write_csv(results$det_hist, f)
  )
  output$dl_effort <- downloadHandler(
    filename = function() paste0(results$focal, "_effort.csv"),
    content  = function(f) write_csv(results$effort, f)
  )

  # ── Record table preview ─────────────────────────────────────
  output$preview_ui <- renderUI({
    req(results$ok, !is.null(results$record_table))
    tbl <- head(results$record_table, 8)
    tbl$DateTimeOriginal <- format(tbl$DateTimeOriginal, "%Y-%m-%d %H:%M")
    header <- paste0("<tr>", paste(sprintf("<th>%s</th>", names(tbl)), collapse = ""), "</tr>")
    rows   <- paste(apply(tbl, 1, function(r) {
      paste0("<tr>", paste(sprintf("<td>%s</td>", r), collapse = ""), "</tr>")
    }), collapse = "")
    tagList(
      tags$hr(class = "div"),
      div(class = "sec-label", "Record table preview (first 8 rows)"),
      div(class = "preview-wrap", HTML(paste0("<table>", header, rows, "</table>")))
    )
  })
}

shinyApp(ui, server)
