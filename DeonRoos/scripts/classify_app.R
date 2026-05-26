# Purpose: Shiny app for label-by-label review of AddaxAI camera trap output.
# Workflow:
#   1. Browse to image folder and results.xlsx
#   2. For each AddaxAI label (person first, then by frequency), review every
#      image with that label, confirm or correct the species, adjust count
#   3. Classifications are saved to classifications.csv after every image

library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinyFiles)
library(jsonlite)

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

BURST_THRESHOLD <- 10   # seconds

# Flat vector used for validation (suggested label must be in this list)
SPECIES_CHOICES <- c(
  "Fallow deer", "Red deer", "Roe deer",
  "Badger", "Fox", "Mustelid", "Otter",
  "Beaver", "Hedgehog", "Lagomorph", "Micromammal", "Squirrel", "Wild boar",
  "Cat", "Cow", "Dog", "Sheep",
  "Bird", "Other mammal", "Vehicle", "Unknown"
)

# Grouped list used for the dropdown — renders as HTML optgroups
SPECIES_GROUPS <- list(
  "Deer"                 = c("Fallow deer", "Red deer", "Roe deer"),
  "Carnivores"           = c("Badger", "Fox", "Mustelid", "Otter"),
  "Other wild mammals"   = c("Beaver", "Hedgehog", "Lagomorph", "Micromammal", "Squirrel", "Wild boar"),
  "Domestic / livestock" = c("Cat", "Cow", "Dog", "Sheep"),
  "Other"                = c("Bird", "Other mammal", "Vehicle", "Unknown")
)

ADDAX_MAP <- c(
  "fox"         = "Fox",
  "badger"      = "Badger",
  "otter"       = "Otter",
  "roe deer"    = "Roe deer",
  "red deer"    = "Red deer",
  "fallow deer" = "Fallow deer",
  "lagomorph"   = "Lagomorph",
  "squirrel"    = "Squirrel",
  "mustelid"    = "Mustelid",
  "hedgehog"    = "Hedgehog",
  "micromammal" = "Micromammal",
  "beaver"      = "Beaver",
  "wild boar"   = "Wild boar",
  "cat"         = "Cat",
  "dog"         = "Dog",
  "sheep"       = "Sheep",
  "cow"         = "Cow",
  "bird"        = "Bird",
  "vehicle"     = "Vehicle"
)

map_addax <- function(label) {
  m <- ADDAX_MAP[tolower(trimws(as.character(label)))]
  ifelse(is.na(m), "Unknown", unname(m))
}

# -----------------------------------------------------------------------------
# Data loading
# -----------------------------------------------------------------------------

load_data <- function(xlsx_path, img_dir) {
  # Copy to a local temp file first — handles OneDrive stubs and locked files
  tmp <- file.path(tempdir(), paste0("ct_results_", format(Sys.time(), "%H%M%S"), ".xlsx"))
  ok  <- file.copy(xlsx_path, tmp, overwrite = TRUE)
  if (!ok) stop("Could not copy results.xlsx. Check the file is not open in another application.")
  on.exit(unlink(tmp), add = TRUE)

  # Files sheet: one row per image, provides DateTimeOriginal + burst IDs
  files_df <- read_excel(tmp, sheet = "files") |>
    transmute(
      Filename         = as.character(relative_path),
      DateTimeOriginal = as.POSIXct(DateTimeOriginal)
    ) |>
    arrange(DateTimeOriginal) |>
    mutate(
      td       = as.numeric(difftime(DateTimeOriginal,
                   lag(DateTimeOriginal, default = first(DateTimeOriginal)),
                   units = "secs")),
      Burst_ID = cumsum(td > BURST_THRESHOLD)
    ) |>
    select(-td)

  # Detections sheet: one row per bounding-box detection
  det_df <- read_excel(tmp, sheet = "detections") |>
    transmute(
      Filename   = as.character(relative_path),
      label      = as.character(label),
      confidence = as.numeric(confidence)
    )

  # Collapse to one row per (label, image): count detections, max confidence
  img_label <- det_df |>
    group_by(label, Filename) |>
    summarise(
      n_dets   = n(),
      max_conf = max(confidence),
      .groups  = "drop"
    ) |>
    left_join(files_df, by = "Filename") |>
    mutate(
      Station   = basename(img_dir),
      img_path  = file.path(img_dir, Filename),
      suggested = if_else(label == "person", "Nothing present",
                          map_addax(label))
    ) |>
    filter(file.exists(img_path) & file.size(img_path) > 0)  # skip missing/unsynced stubs

  # Label order: person first, then by image count descending
  label_order <- img_label |>
    count(label) |>
    arrange(desc(label == "person"), desc(n)) |>
    pull(label)

  # Bounding boxes from image_recognition_file.json (optional — gracefully absent)
  bboxes_list <- list()
  json_path <- file.path(img_dir, "image_recognition_file.json")
  if (file.exists(json_path)) {
    tryCatch({
      raw      <- fromJSON(json_path, simplifyDataFrame = FALSE)
      det_cats <- raw$detection_categories
      for (entry in raw$images) {
        boxes <- Filter(Negate(is.null), lapply(entry$detections, function(d) {
          if (d$conf < 0.2) return(NULL)
          cat_nm <- det_cats[[d$category]]
          color  <- if (d$category == "2") "#c0392b" else
                    if (d$category == "3") "#e6a817" else "#f500bd"
          list(
            bbox  = as.numeric(unlist(d$bbox)),
            label = sprintf("%s %.0f%%",
                            if (cat_nm == "unidentified animal") "?" else cat_nm,
                            d$conf * 100),
            color = color
          )
        }))
        if (length(boxes) > 0) bboxes_list[[entry$file]] <- boxes
      }
    }, error = function(e) {})   # silently skip if JSON malformed or absent
  }

  list(img_label = img_label, label_order = label_order, bboxes = bboxes_list)
}

# -----------------------------------------------------------------------------
# Save helper
# -----------------------------------------------------------------------------

save_row <- function(csv_path, station, filename, datetime, species, count, burst_id) {
  row <- tibble(
    Station  = station,
    Filename = filename,
    DateTime = format(datetime, "%Y-%m-%d %H:%M:%S"),
    Species  = species,
    Count    = as.integer(count),
    Burst_ID = as.integer(burst_id)
  )
  write_csv(row, csv_path, append = file.exists(csv_path))
}

# -----------------------------------------------------------------------------
# CSS
# -----------------------------------------------------------------------------

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&display=swap');

* { box-sizing: border-box; }
body { background: #161616; color: #e0e0e0;
       font-family: 'IBM Plex Mono', monospace; margin: 0; }
.container-fluid { padding: 0; }

.app-title { font-size: 14px; letter-spacing: 4px; text-transform: uppercase;
             color: #78a849; padding: 18px 24px 6px; margin: 0; }

/* Setup */
.setup-wrap { max-width: 500px; margin: 32px auto; padding: 0 24px; }
.panel { background: #1f1f1f; border: 1px solid #2a2a2a; border-radius: 3px; padding: 20px; }
.sec-label { font-size: 10px; color: #555; letter-spacing: 3px;
             text-transform: uppercase; margin: 16px 0 5px; }
.sec-label.first { margin-top: 0; }
label { font-size: 10px !important; color: #888 !important; letter-spacing: 1px; }
.form-control { background: #2a2a2a !important; border: 1px solid #333 !important;
                color: #e0e0e0 !important; border-radius: 3px !important;
                font-family: inherit !important; font-size: 11px !important; }
.form-control:focus { border-color: #78a849 !important; box-shadow: none !important; }
select.form-control { height: 34px !important; }
.hint     { font-size: 9px; color: #444; margin-top: 3px; letter-spacing: 0.5px; }
.path-box { font-size: 10px; color: #78a849; margin-top: 5px; min-height: 16px;
            letter-spacing: 0.5px; word-break: break-all; }
.status-err { font-size: 11px; color: #c0392b; margin-top: 12px; }

/* shinyFiles button styling */
.btn-browse { background: #2a2a2a !important; border: 1px solid #444 !important;
              color: #aaa !important; font-family: inherit !important;
              font-size: 10px !important; letter-spacing: 1.5px !important;
              text-transform: uppercase !important; padding: 7px 16px !important;
              border-radius: 3px !important; cursor: pointer !important;
              transition: all 0.15s !important; }
.btn-browse:hover,
.btn-browse:focus { border-color: #78a849 !important;
                    color: #78a849 !important; background: #2a2a2a !important; }

.btn-load { width: 100%; background: #1e3a0f; border: 1px solid #78a849;
            color: #78a849; font-family: inherit; font-size: 11px;
            letter-spacing: 2px; text-transform: uppercase; padding: 10px;
            border-radius: 3px; cursor: pointer; margin-top: 18px;
            transition: all 0.15s; }
.btn-load:hover { background: #78a849; color: #161616; }

/* Label header bar */
.label-bar { background: #1f1f1f; border-bottom: 1px solid #2a2a2a;
             padding: 9px 24px; display: flex; align-items: center; gap: 16px;
             flex-wrap: wrap; }
.label-name { font-size: 13px; color: #78a849; font-weight: 500;
              letter-spacing: 1px; white-space: nowrap; }
.label-meta { font-size: 10px; color: #555; letter-spacing: 1px; white-space: nowrap; }
.lbar-track { flex: 1; min-width: 60px; background: #2a2a2a;
              height: 3px; border-radius: 2px; }
.lbar-fill  { background: #78a849; height: 3px; border-radius: 2px;
              transition: width 0.3s; }
.back-link  { font-size: 10px; color: #444; cursor: pointer;
              text-decoration: underline; letter-spacing: 1px; white-space: nowrap; }
.back-link:hover { color: #78a849; }

/* Image panel */
.img-meta { font-size: 10px; color: #555; letter-spacing: 1.5px; margin-bottom: 8px; }
.img-wrap  { background: #0d0d0d; border: 1px solid #2a2a2a; border-radius: 2px;
             min-height: 360px; display: flex; align-items: center;
             justify-content: center; overflow: hidden; position: relative; }
.img-wrap img { max-width: 100%; max-height: 500px; display: block; }

/* Mid-panel messages */
.msg-panel { text-align: center; padding: 60px 20px; }
.msg-panel .msg-big { color: #78a849; font-size: 14px; letter-spacing: 2px;
                      margin-bottom: 20px; }
.msg-panel .msg-sub { color: #555; font-size: 11px; letter-spacing: 1px;
                      margin-bottom: 24px; }

/* Suggestion badge */
.suggest-box { border-radius: 3px; padding: 8px 12px; margin-bottom: 14px;
               font-size: 10px; letter-spacing: 0.5px; }
.suggest-sp    { background: #1a2a14; border: 1px solid #3a5a1a; color: #aaa; }
.suggest-human { background: #2a0f0f; border: 1px solid #5a1a1a; color: #aaa; }
.suggest-box .sp       { color: #78a849; font-weight: 500; font-size: 12px; }
.suggest-box .sp.human { color: #c0392b; }
.suggest-box .cf       { color: #555; font-size: 9px; margin-left: 6px; }

/* Count */
.count-row  { display: flex; align-items: center; gap: 10px; margin: 10px 0 14px; }
.count-lbl  { font-size: 10px; color: #555; letter-spacing: 1px;
              text-transform: uppercase; white-space: nowrap; }
.count-ctrl { display: flex; align-items: center; }
.count-btn  { background: #2a2a2a; border: 1px solid #333; color: #aaa;
              width: 28px; height: 28px; font-size: 16px; cursor: pointer;
              display: flex; align-items: center; justify-content: center;
              transition: all 0.15s; }
.count-btn:hover { background: #333; color: #e0e0e0; }
.count-btn.minus { border-radius: 3px 0 0 3px; }
.count-btn.plus  { border-radius: 0 3px 3px 0; }
.count-val  { background: #1a1a1a; border-top: 1px solid #333;
              border-bottom: 1px solid #333; border-left: none; border-right: none;
              color: #e0e0e0; width: 40px; height: 28px; text-align: center;
              font-family: inherit; font-size: 13px; }

/* Action buttons */
.btn-confirm { width: 100%; background: #1e3a0f; border: 1px solid #78a849;
               color: #78a849; font-family: inherit; font-size: 11px;
               letter-spacing: 2px; text-transform: uppercase; padding: 10px;
               border-radius: 3px; cursor: pointer; margin-bottom: 8px;
               transition: all 0.15s; }
.btn-confirm:hover { background: #78a849; color: #161616; }
.btn-next   { background: #1e3a0f; border: 1px solid #78a849; color: #78a849;
              font-family: inherit; font-size: 11px; letter-spacing: 2px;
              text-transform: uppercase; padding: 10px 24px; border-radius: 3px;
              cursor: pointer; transition: all 0.15s; }
.btn-next:hover { background: #78a849; color: #161616; }

.btn-nothing { width: 100%; background: #1f1f1f; border: 1px solid #444;
               color: #888; font-family: inherit; font-size: 11px;
               letter-spacing: 2px; text-transform: uppercase; padding: 10px;
               border-radius: 3px; cursor: pointer; margin-bottom: 8px;
               transition: all 0.15s; }
.btn-nothing:hover { border-color: #78a849; color: #78a849; }

.btn-back-img { width: 100%; background: #1f1f1f; border: 1px solid #2a2a2a;
                color: #444; font-family: inherit; font-size: 10px;
                letter-spacing: 2px; text-transform: uppercase; padding: 8px;
                border-radius: 3px; cursor: pointer; margin-bottom: 8px;
                transition: all 0.15s; }
.btn-back-img:hover:not(:disabled) { border-color: #555; color: #777; }
.btn-back-img:disabled { opacity: 0.25; cursor: not-allowed; }

.btn-human  { width: 100%; background: #2a0f0f; border: 1px solid #8b2020;
              color: #c0392b; font-family: inherit; font-size: 11px;
              letter-spacing: 2px; text-transform: uppercase; padding: 10px;
              border-radius: 3px; cursor: pointer; margin-bottom: 8px;
              transition: all 0.15s; }
.btn-human:hover   { background: #8b2020; color: #fff; }
.btn-human.flagged { background: #3a0f0f; border-color: #c0392b; color: #e05555; }

/* Footer */
.footer-info { margin-top: 14px; padding-top: 12px; border-top: 1px solid #2a2a2a; }
.footer-info p { font-size: 10px; color: #444; margin: 3px 0; letter-spacing: 0.5px; }
.shiny-download-link {
  display: block; background: #2a2a2a; border: 1px solid #333;
  color: #aaa !important; font-family: 'IBM Plex Mono', monospace;
  font-size: 10px; letter-spacing: 1.5px; text-transform: uppercase;
  padding: 8px 12px; border-radius: 3px; margin-top: 6px;
  text-align: left; text-decoration: none !important;
  transition: all 0.15s; width: 100%; }
.shiny-download-link:hover { border-color: #78a849; color: #78a849 !important; }

/* Modal */
.modal-content { background: #1f1f1f; border: 1px solid #333; color: #e0e0e0;
                 font-family: 'IBM Plex Mono', monospace; }
.modal-header  { border-bottom: 1px solid #2a2a2a; }
.modal-footer  { border-top: 1px solid #2a2a2a; }
.modal-title   { color: #c0392b; font-size: 13px; letter-spacing: 2px; }
"

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      function adjustCount(d) {
        var el = document.getElementById('count_display');
        var v  = Math.min(99, Math.max(1, parseInt(el.value) + d));
        el.value = v;
        Shiny.setInputValue('count_val', v, {priority:'event'});
      }
      document.addEventListener('keydown', function(e) {
        var tag = (document.activeElement || {}).tagName || '';
        if (/^(input|select|textarea)$/i.test(tag)) return;
        if (e.key === 'Enter' && !document.querySelector('.modal.show')) {
          e.preventDefault();
          Shiny.setInputValue('btn_confirm', Math.random(), {priority:'event'});
        }
        if (e.key === '+' || e.key === '.') { e.preventDefault(); adjustCount(1); }
        if (e.key === '-' || e.key === ',') { e.preventDefault(); adjustCount(-1); }
      });
    "))
  ),

  tags$h1(class = "app-title", "Camera Trap Classifier"),

  # ── Setup ────────────────────────────────────────────────────────────
  conditionalPanel("output.app_phase === 'setup'",
    div(class = "setup-wrap",
      div(class = "panel",

        div(class = "sec-label first", "Image folder"),
        shinyDirButton("btn_dir", "Browse", title = "Select image folder",
                       class = "btn-browse"),
        div(class = "path-box", textOutput("dir_display", inline = TRUE)),
        div(class = "hint", "The site folder containing the JPG images (e.g. 2025/CT_001/)."),

        div(class = "sec-label", "AddaxAI results"),
        shinyFilesButton("btn_xlsx", "Browse", title = "Select results.xlsx",
                         multiple = FALSE, class = "btn-browse"),
        div(class = "path-box", textOutput("xlsx_display", inline = TRUE)),
        div(class = "hint", "The results.xlsx written by AddaxAI into the image folder."),

        tags$button(class = "btn-load",
                    onclick = "Shiny.setInputValue('btn_load', Math.random())",
                    "Load"),
        uiOutput("setup_status")
      )
    )
  ),

  # ── Classifier ───────────────────────────────────────────────────────
  conditionalPanel("output.app_phase === 'classify'",

    div(class = "label-bar",
      div(class = "lbar-track",
          uiOutput("label_bar_fill", inline = TRUE)),
      tags$span(class = "label-name", textOutput("label_name_txt",  inline = TRUE)),
      tags$span(class = "label-meta", textOutput("label_meta_txt",  inline = TRUE)),
      tags$span(class = "back-link",
                onclick = "Shiny.setInputValue('btn_back', Math.random())",
                "Change folder")
    ),

    fluidRow(
      style = "margin:0; padding:8px 24px;",

      # Left: image
      column(8, style = "padding-right:10px;",
        div(class = "panel",
          div(class = "img-meta", textOutput("img_meta_txt", inline = TRUE)),
          div(class = "img-wrap", uiOutput("image_display"))
        )
      ),

      # Right: controls
      column(4, style = "padding-left:10px;",
        div(class = "panel",
          uiOutput("suggest_ui"),

          div(class = "sec-label", "Species"),
          uiOutput("species_select_ui"),

          div(class = "count-row",
            tags$span(class = "count-lbl", "Individuals"),
            div(class = "count-ctrl",
              tags$button(class = "count-btn minus", onclick = "adjustCount(-1)", "-"),
              tags$input(id = "count_display", class = "count-val", type = "number",
                         value = "1", min = "1", max = "99", readonly = "readonly"),
              tags$button(class = "count-btn plus",  onclick = "adjustCount(1)",  "+")
            )
          ),
          tags$p(class = "hint",
                 style = "margin: -6px 0 10px; letter-spacing: 0.5px;",
                 "+ / . to increase   – / , to decrease"),

          tags$button(class = "btn-confirm",
                      onclick = "Shiny.setInputValue('btn_confirm', Math.random())",
                      "Confirm"),

          tags$button(class = "btn-nothing",
                      onclick = "Shiny.setInputValue('btn_nothing', Math.random())",
                      "Nothing present"),

          uiOutput("btn_delete_ui"),

          uiOutput("btn_back_img_ui"),

          div(class = "footer-info",
            tags$p(textOutput("csv_label", inline = TRUE)),
            tags$p("Human images are permanently deleted from disk."),
            downloadButton("dl_csv", "Download classifications.csv")
          )
        )
      )
    )
  ),

  # Custom message handlers
  tags$script(HTML("
    Shiny.addCustomMessageHandler('resetCount', function(msg) {
      var el = document.getElementById('count_display');
      if (el) el.value = msg.val;
    });

    var bboxData = [];

    function drawBBoxes() {
      var wrap = document.querySelector('.img-wrap');
      if (!wrap) return;
      wrap.querySelectorAll('.bbox-overlay').forEach(function(el) { el.remove(); });
      var img = wrap.querySelector('img');
      if (!img || bboxData.length === 0) return;
      var ox = (wrap.clientWidth  - img.clientWidth)  / 2;
      var oy = (wrap.clientHeight - img.clientHeight) / 2;
      bboxData.forEach(function(b) {
        var iw = img.clientWidth, ih = img.clientHeight;
        var div = document.createElement('div');
        div.className = 'bbox-overlay';
        div.style.cssText = 'position:absolute;box-sizing:border-box;pointer-events:none;border:2px solid ' + b.color + ';';
        div.style.left   = (ox + b.bbox[0] * iw) + 'px';
        div.style.top    = (oy + b.bbox[1] * ih) + 'px';
        div.style.width  = (b.bbox[2] * iw) + 'px';
        div.style.height = (b.bbox[3] * ih) + 'px';
        var lbl = document.createElement('span');
        lbl.style.cssText = 'position:absolute;top:-17px;left:-1px;font-size:9px;line-height:16px;' +
          'background:' + b.color + ';color:#161616;padding:0 4px;' +
          'font-family:IBM Plex Mono,monospace;white-space:nowrap;';
        lbl.textContent = b.label;
        div.appendChild(lbl);
        wrap.appendChild(div);
      });
    }

    Shiny.addCustomMessageHandler('drawBBoxes', function(msg) {
      bboxData = msg.boxes || [];
      var img = document.querySelector('.img-wrap img');
      if (img && img.complete && img.naturalWidth > 0) {
        drawBBoxes();
      } else if (img) {
        img.addEventListener('load', drawBBoxes, { once: true });
      }
    });
  "))
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # shinyFiles setup
  volumes <- c(getVolumes()())
  shinyDirChoose(input,  "btn_dir",  roots = volumes, session = session)
  shinyFileChoose(input, "btn_xlsx", roots = volumes, session = session,
                  filetypes = c("xlsx"))

  # Phase
  phase <- reactiveVal("setup")
  output$app_phase <- reactive(phase())
  outputOptions(output, "app_phase", suspendWhenHidden = FALSE)

  # Parsed paths
  img_dir_rv   <- reactiveVal("")
  xlsx_path_rv <- reactiveVal("")

  observe({
    req(input$btn_dir)
    d <- parseDirPath(volumes, input$btn_dir)
    if (length(d) > 0 && nzchar(d)) img_dir_rv(as.character(d))
  })

  observe({
    req(input$btn_xlsx)
    f <- parseFilePaths(volumes, input$btn_xlsx)
    if (nrow(f) > 0) xlsx_path_rv(as.character(f$datapath[1]))
  })

  output$dir_display  <- renderText({
    d <- img_dir_rv()
    if (nzchar(d)) d else "No folder selected"
  })
  output$xlsx_display <- renderText({
    x <- xlsx_path_rv()
    if (nzchar(x)) x else "No file selected"
  })

  # Data state
  img_label_rv   <- reactiveVal(NULL)
  label_order_rv <- reactiveVal(character())
  label_idx_rv   <- reactiveVal(1L)
  done_rv        <- reactiveVal(character())   # "label||Filename" keys
  history_rv     <- reactiveVal(list())        # undo stack: list(key, wrote_csv, label_idx)
  active_dir_rv  <- reactiveVal("")
  active_csv_rv  <- reactiveVal("")
  bboxes_rv      <- reactiveVal(list())        # filename → list of bbox objects from JSON

  # ── Load ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_load, {
    img_dir <- img_dir_rv()
    xlsx    <- xlsx_path_rv()

    errs <- c(
      if (!nzchar(img_dir) || !dir.exists(img_dir)) "Image folder not found.",
      if (!nzchar(xlsx)    || !file.exists(xlsx))    "results.xlsx not found."
    )
    if (length(errs)) {
      output$setup_status <- renderUI(
        div(class = "status-err", paste(errs, collapse = " ")))
      return()
    }

    res <- tryCatch(
      load_data(xlsx, img_dir),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(res$error)) {
      output$setup_status <- renderUI(
        div(class = "status-err", paste("Error reading file:", res$error)))
      return()
    }

    csv_out <- file.path(img_dir, "classifications.csv")

    # Pre-mark previously classified filenames as done across all their labels
    prior_done <- if (file.exists(csv_out)) {
      unique(read_csv(csv_out, show_col_types = FALSE)$Filename)
    } else character()

    done_keys <- if (length(prior_done) > 0) {
      res$img_label |>
        filter(Filename %in% prior_done) |>
        mutate(key = paste0(label, "||", Filename)) |>
        pull(key)
    } else character()

    img_label_rv(res$img_label)
    label_order_rv(res$label_order)
    bboxes_rv(res$bboxes)
    done_rv(done_keys)
    history_rv(list())
    active_dir_rv(img_dir)
    active_csv_rv(csv_out)
    label_idx_rv(1L)
    phase("classify")
    output$setup_status <- renderUI(NULL)
  })

  observeEvent(input$btn_back, phase("setup"))

  # ── Derived state ──────────────────────────────────────────────────────

  current_label <- reactive({
    lo <- label_order_rv()
    i  <- label_idx_rv()
    if (length(lo) == 0 || i > length(lo)) return(NULL)
    lo[i]
  })

  # Images remaining for the current label (already-done excluded)
  label_queue <- reactive({
    req(img_label_rv(), current_label())
    img_label_rv() |>
      filter(label == current_label()) |>
      filter(!paste0(label, "||", Filename) %in% done_rv()) |>
      arrange(DateTimeOriginal)
  })

  # First unprocessed image for the current label
  current_img <- reactive({
    q <- label_queue()
    if (nrow(q) == 0) return(NULL)
    q[1, ]
  })

  # Advance to next label
  advance_label <- function() {
    lo <- label_order_rv()
    li <- label_idx_rv()
    label_idx_rv(min(li + 1L, length(lo) + 1L))
  }

  # Reset count and update bounding boxes when the current image changes
  observeEvent(current_img(), {
    row <- current_img()
    val <- if (!is.null(row)) as.integer(row$n_dets) else 1L
    session$sendCustomMessage("resetCount", list(val = val))

    boxes <- list()
    if (!is.null(row)) {
      fname <- basename(row$Filename)
      boxes <- bboxes_rv()[[fname]]
      if (is.null(boxes)) boxes <- list()
    }
    session$sendCustomMessage("drawBBoxes", list(boxes = boxes))
  }, ignoreNULL = FALSE)

  # ── Label bar ──────────────────────────────────────────────────────────

  output$label_bar_fill <- renderUI({
    lo <- label_order_rv()
    li <- label_idx_rv()
    J  <- length(lo)
    pct <- if (J > 0) round((li - 1) / J * 100) else 0
    div(class = "lbar-fill", style = paste0("width:", pct, "%;"))
  })

  output$label_name_txt <- renderText({
    lbl <- current_label()
    if (is.null(lbl)) "Complete" else lbl
  })

  output$label_meta_txt <- renderText({
    lo  <- label_order_rv()
    li  <- label_idx_rv()
    J   <- length(lo)
    lbl <- current_label()
    if (is.null(lbl)) return(sprintf("%d/%d labels reviewed", J, J))

    total_in_label <- nrow(img_label_rv() |> filter(label == lbl))
    done_in_label  <- sum(startsWith(done_rv(), paste0(lbl, "||")))

    sprintf("Label %d/%d  |  Image %d/%d",
            li, J,
            min(done_in_label + 1L, total_in_label),
            total_in_label)
  })

  # ── Image panel ────────────────────────────────────────────────────────

  output$img_meta_txt <- renderText({
    row <- current_img()
    if (is.null(row)) "" else paste(row$Station, row$Filename, sep = "  |  ")
  })

  output$image_display <- renderUI({
    lbl <- current_label()

    # All labels complete
    if (is.null(lbl)) {
      return(div(class = "msg-panel",
        div(class = "msg-big", "All labels reviewed"),
        div(class = "msg-sub", "Download the classifications CSV below.")
      ))
    }

    row <- current_img()

    # This label is complete — show next-label button
    if (is.null(row)) {
      lo <- label_order_rv()
      li <- label_idx_rv()
      has_next <- li < length(lo)
      return(div(class = "msg-panel",
        div(class = "msg-big", paste0("'", lbl, "' complete")),
        if (has_next)
          tags$button(class = "btn-next",
                      onclick = "Shiny.setInputValue('btn_next_label', Math.random())",
                      "Next label")
        else
          tags$button(class = "btn-next",
                      onclick = "Shiny.setInputValue('btn_next_label', Math.random())",
                      "Finish")
      ))
    }

    # Guard: file may have been deleted (e.g. person image that also had an
    # animal detection — it was deleted under the 'person' label pass)
    if (!file.exists(row$img_path)) {
      return(div(class = "msg-panel",
        div(class = "msg-big", style = "color:#c0392b;", "Image deleted"),
        div(class = "msg-sub",
            "This image was permanently deleted because it contained a person."),
        tags$button(class = "btn-nothing",
                    onclick = "Shiny.setInputValue('btn_nothing', Math.random())",
                    "Mark as reviewed and continue")
      ))
    }

    addResourcePath("imgs", active_dir_rv())
    tags$img(src = paste0("imgs/", row$Filename), alt = row$Filename)
  })

  # ── Right-panel controls ───────────────────────────────────────────────

  output$suggest_ui <- renderUI({
    row <- current_img()
    if (is.null(row)) return(NULL)

    is_person <- row$label == "person"
    conf_str  <- sprintf("%.0f%%", row$max_conf * 100)

    div(
      class = paste("suggest-box", if (is_person) "suggest-human" else "suggest-sp"),
      "AddaxAI: ",
      tags$span(
        class = paste("sp", if (is_person) "human" else ""),
        if (is_person) "person detected" else row$label
      ),
      tags$span(class = "cf", conf_str)
    )
  })

  output$species_select_ui <- renderUI({
    row <- current_img()
    sel <- if (is.null(row)) SPECIES_CHOICES[1] else {
      s <- row$suggested
      if (!s %in% SPECIES_CHOICES) "Unknown" else s
    }
    selectInput("species_sel", NULL, choices = SPECIES_GROUPS,
                selected = sel, width = "100%")
  })

  output$btn_delete_ui <- renderUI({
    row    <- current_img()
    person <- !is.null(row) && row$label == "person"
    tags$button(
      class   = paste("btn-human", if (person) "flagged" else ""),
      onclick = "Shiny.setInputValue('btn_delete', Math.random())",
      if (person) "Person detected — delete image" else "Delete image"
    )
  })

  output$csv_label <- renderText(basename(active_csv_rv()))

  output$btn_back_img_ui <- renderUI({
    can_go_back <- length(history_rv()) > 0
    tags$button(
      class    = "btn-back-img",
      onclick  = "Shiny.setInputValue('btn_back_img', Math.random())",
      disabled = if (!can_go_back) NA else NULL,
      "Back"
    )
  })

  # ── Actions ────────────────────────────────────────────────────────────

  observeEvent(input$btn_confirm, {
    row <- current_img(); req(row)
    sp  <- if (!is.null(input$species_sel)) input$species_sel else row$suggested
    cnt <- if (!is.null(input$count_val)) as.integer(input$count_val) else as.integer(row$n_dets)

    save_row(active_csv_rv(), row$Station, row$Filename,
             row$DateTimeOriginal, sp, cnt, row$Burst_ID)

    key <- paste0(row$label, "||", row$Filename)
    history_rv(c(history_rv(), list(list(key = key, wrote_csv = TRUE,
                                         label_idx = label_idx_rv()))))
    done_rv(c(done_rv(), key))
    if (nrow(label_queue()) == 0) advance_label()
  })

  observeEvent(input$btn_nothing, {
    row <- current_img(); req(row)
    key <- paste0(row$label, "||", row$Filename)
    history_rv(c(history_rv(), list(list(key = key, wrote_csv = FALSE,
                                         label_idx = label_idx_rv()))))
    done_rv(c(done_rv(), key))
    if (nrow(label_queue()) == 0) advance_label()
  })

  observeEvent(input$btn_back_img, {
    hist <- history_rv()
    if (length(hist) == 0) return()

    last <- hist[[length(hist)]]
    history_rv(hist[-length(hist)])

    # Remove from done
    done_rv(setdiff(done_rv(), last$key))

    # Remove last CSV row if a record was written
    if (isTRUE(last$wrote_csv)) {
      csv <- active_csv_rv()
      if (file.exists(csv)) {
        rows <- read_csv(csv, show_col_types = FALSE)
        if (nrow(rows) > 0) write_csv(rows[-nrow(rows), ], csv)
      }
    }

    # Restore the label index (handles going back across label boundaries)
    label_idx_rv(last$label_idx)
  })

  observeEvent(input$btn_next_label, {
    advance_label()
  })

  observeEvent(input$btn_delete, {
    row <- current_img(); req(row)
    showModal(modalDialog(
      title = "Confirm deletion",
      tags$p("This image will be permanently deleted from disk to comply with GDPR.",
             style = "font-size:12px;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete image",
          style = paste("background:#8b2020; border-color:#c0392b; color:#fff;",
                        "font-family:'IBM Plex Mono',monospace;",
                        "font-size:11px; letter-spacing:1px;"))
      )
    ))
  })

  observeEvent(input$confirm_delete, {
    row <- current_img(); removeModal(); req(row)
    fp  <- file.path(active_dir_rv(), row$Filename)
    if (file.exists(fp)) file.remove(fp)

    # Mark EVERY label that references this filename as done so it won't
    # appear as a broken image in any subsequent label queue
    all_keys <- img_label_rv() |>
      filter(Filename == row$Filename) |>
      mutate(key = paste0(label, "||", Filename)) |>
      pull(key)
    done_rv(union(done_rv(), all_keys))

    if (nrow(label_queue()) == 0) advance_label()
  })

  # Download: serve the CSV written to disk, or an empty template
  output$dl_csv <- downloadHandler(
    filename = "classifications.csv",
    content  = function(f) {
      csv <- active_csv_rv()
      if (nzchar(csv) && file.exists(csv)) {
        file.copy(csv, f)
      } else {
        write_csv(
          tibble(Station = character(), Filename = character(),
                 DateTime = character(), Species = character(),
                 Count = integer(), Burst_ID = integer()),
          f
        )
      }
    }
  )
}

shinyApp(ui, server)
