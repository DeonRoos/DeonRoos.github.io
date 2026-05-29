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

BURST_THRESHOLD  <- 10   # seconds
BLANK_BATCH_SIZE <- 15L  # blank-gallery tiles shown per batch

# Bounding-box overlay colours — chosen for contrast against natural landscape
# (avoid dark greens, browns, dark greys which blend with vegetation / soil)
BBOX_COLORS <- c(
  "person"              = "#FF2D20",   # vivid red       — GDPR signal
  "vehicle"             = "#FF8C00",   # orange
  "roe deer"            = "#00FFFF",   # cyan
  "red deer"            = "#FFD700",   # gold
  "fallow deer"         = "#FF69B4",   # hot pink
  "fox"                 = "#FF4500",   # orange-red
  "badger"              = "#ADFF2F",   # green-yellow
  "otter"               = "#00FA9A",   # spring green
  "mustelid"            = "#40E0D0",   # turquoise
  "hedgehog"            = "#FF1493",   # deep pink
  "lagomorph"           = "#9400D3",   # violet
  "squirrel"            = "#1E90FF",   # dodger blue
  "micromammal"         = "#DA70D6",   # orchid
  "beaver"              = "#00BFFF",   # deep sky blue
  "wild boar"           = "#FF6347",   # tomato
  "cat"                 = "#FFFF00",   # yellow
  "dog"                 = "#7B68EE",   # slate blue
  "sheep"               = "#FFFFFF",   # white
  "cow"                 = "#FF00FF",   # magenta
  "bird"                = "#7FFF00",   # chartreuse
  "unidentified animal" = "#AAAAAA"    # neutral grey
)

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

  # Label order: person first, then known species by image count descending,
  # then anything AddaxAI labelled that isn't in ADDAX_MAP (unidentified etc.) last.
  # Showing unknowns last gives context from other labels in the same image.
  label_order <- img_label |>
    count(label) |>
    mutate(is_unknown = !label %in% c("person", names(ADDAX_MAP))) |>
    arrange(desc(label == "person"), is_unknown, desc(n)) |>
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
          cat_nm  <- det_cats[[d$category]]
          lbl_key <- tolower(trimws(cat_nm))
          color   <- if (!is.na(BBOX_COLORS[lbl_key])) unname(BBOX_COLORS[lbl_key]) else "#f500bd"
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

  list(img_label = img_label, label_order = label_order,
       bboxes = bboxes_list, files_df = files_df)
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

/* ── Site-complete phase ─────────────────────────────────────────────── */
.complete-phase  { max-width: 560px; margin: 48px auto; padding: 0 24px; }
.complete-title  { font-size: 20px; color: #78a849; letter-spacing: 4px;
                   text-transform: uppercase; margin-bottom: 6px; }
.complete-sub    { font-size: 11px; color: #555; letter-spacing: 1px;
                   margin-bottom: 28px; }
.complete-stat   { font-size: 10px; color: #555; letter-spacing: 2px;
                   text-transform: uppercase; margin-bottom: 12px; }
.summary-row     { display: flex; justify-content: space-between;
                   padding: 6px 0; border-bottom: 1px solid #1f1f1f; }
.summary-sp      { font-size: 11px; color: #e0e0e0; letter-spacing: 0.5px; }
.summary-ct      { font-size: 11px; color: #78a849; letter-spacing: 1px; font-weight:500; }
.complete-btns   { display: flex; gap: 10px; margin-top: 24px; }

/* Previously-recorded panel (multi-label images) */
.prev-box       { background: #161f10; border: 1px solid #2a3a1a; border-radius: 3px;
                  padding: 8px 12px; margin-bottom: 14px; }
.prev-title     { font-size: 9px; color: #555; letter-spacing: 2px;
                  text-transform: uppercase; margin-bottom: 6px; }
.prev-row       { font-size: 11px; color: #78a849; letter-spacing: 0.5px;
                  margin-bottom: 2px; }
.prev-nothing   { font-size: 10px; color: #555; letter-spacing: 0.3px;
                  font-style: italic; margin-bottom: 2px; }
.prev-guidance  { font-size: 9px; color: #444; margin-top: 8px; line-height: 1.5;
                  letter-spacing: 0.3px; }
.prev-guidance b { color: #666; font-weight: 500; }

/* Modal */
.modal-content { background: #1f1f1f; border: 1px solid #333; color: #e0e0e0;
                 font-family: 'IBM Plex Mono', monospace; }
.modal-header  { border-bottom: 1px solid #2a2a2a; }
.modal-footer  { border-top: 1px solid #2a2a2a; }
.modal-title   { color: #c0392b; font-size: 13px; letter-spacing: 2px; }

/* ── Blank gallery & review phases ──────────────────────────────────── */
.blank-phase     { display: flex; flex-direction: column; height: calc(100vh - 52px); }
.blank-hbar      { background: #1f1f1f; border-bottom: 1px solid #2a2a2a;
                   padding: 9px 24px; display: flex; align-items: center;
                   gap: 16px; flex-wrap: wrap; flex-shrink: 0; }
/* grid-wrap becomes a flex column so the grid below can claim all remaining height */
.blank-grid-wrap { flex: 1; min-height: 0; overflow-y: auto;
                   padding: 16px 24px; display: flex; flex-direction: column; }
/* .blank-grid passes height through to the Shiny output wrapper */
.blank-grid      { flex: 1; min-height: 0; display: flex; flex-direction: column; }
/* #blank_tile_grid is the Shiny output div — give it the grid layout */
#blank_tile_grid { flex: 1; min-height: 0;
                   display: grid !important;
                   grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
                   grid-auto-rows: 1fr;
                   gap: 10px; }
.blank-fbar      { background: #1f1f1f; border-top: 1px solid #2a2a2a;
                   padding: 10px 24px; display: flex; align-items: center;
                   gap: 8px; flex-shrink: 0; }
/* Tiles — flex column so image fills cell and label sits at bottom */
.blank-thumb      { display: flex; flex-direction: column;
                    cursor: pointer; position: relative; min-height: 0; }
.blank-thumb img  { flex: 1; min-height: 0; width: 100%; object-fit: cover;
                    display: block; border: 2px solid #2a2a2a; border-radius: 2px;
                    background: #0d0d0d; transition: border-color 0.12s; }
.blank-thumb:hover img    { border-color: #555; }
.blank-thumb.selected img { border-color: #78a849; }
.blank-thumb span { flex-shrink: 0; display: block; padding: 2px 0;
                    font-size: 8px; color: #444;
                    word-break: break-all; line-height: 1.3; }
.blank-thumb .chk { display: none; position: absolute; top: 5px; right: 5px;
                    background: #78a849; color: #161616; font-size: 9px; font-weight: bold;
                    width: 16px; height: 16px; border-radius: 2px;
                    align-items: center; justify-content: center; pointer-events: none; }
.blank-thumb.selected .chk { display: flex; }
/* Footer buttons */
.btn-sel { background: #2a2a2a; border: 1px solid #444; color: #888;
           font-family: inherit; font-size: 10px; letter-spacing: 1.5px;
           text-transform: uppercase; padding: 7px 14px; border-radius: 3px;
           cursor: pointer; transition: all 0.15s; }
.btn-sel:hover { border-color: #78a849; color: #78a849; }

/* ── Image zoom lightbox ────────────────────────────────────────────── */
.img-wrap img     { cursor: zoom-in; }
#zoom-overlay     { display: none; position: fixed; top: 0; left: 0;
                    width: 100vw; height: 100vh; background: rgba(0,0,0,0.93);
                    z-index: 9999; align-items: center; justify-content: center;
                    cursor: zoom-out; }
#zoom-overlay img { max-width: 96vw; max-height: 96vh; object-fit: contain;
                    pointer-events: none; }
#zoom-hint        { position: fixed; bottom: 14px; left: 50%;
                    transform: translateX(-50%); font-size: 9px; color: #555;
                    letter-spacing: 2px; text-transform: uppercase;
                    pointer-events: none; font-family: 'IBM Plex Mono', monospace; }

/* ── Edit-previous button (prev-box rows) ───────────────────────────── */
.btn-edit-prev { background: none; border: 1px solid #2a2a2a; color: #444;
                 font-family: inherit; font-size: 8px; letter-spacing: 1px;
                 text-transform: uppercase; padding: 2px 6px; border-radius: 2px;
                 cursor: pointer; margin-left: 8px; transition: all 0.15s;
                 vertical-align: middle; }
.btn-edit-prev:hover { border-color: #78a849; color: #78a849; }
"

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("
      function adjustCount(d) {
        // Find the visible count input (works across both classify and blank_review phases)
        var els = document.querySelectorAll('.count-val'), el = null;
        for (var i = 0; i < els.length; i++) {
          if (els[i].offsetParent !== null) { el = els[i]; break; }
        }
        if (!el) return;
        var v = Math.min(99, Math.max(1, parseInt(el.value) + d));
        el.value = v;
        Shiny.setInputValue('count_val', v, {priority:'event'});
      }

      // ── Zoom lightbox ───────────────────────────────────────────────
      function openZoom(src) {
        document.getElementById('zoom-img').src = src;
        document.getElementById('zoom-overlay').style.display = 'flex';
      }
      function closeZoom() {
        document.getElementById('zoom-overlay').style.display = 'none';
      }

      document.addEventListener('keydown', function(e) {
        var tag = (document.activeElement || {}).tagName || '';
        if (/^(input|select|textarea)$/i.test(tag)) return;
        // Escape / Enter close zoom first if it is open
        if (e.key === 'Escape' || e.key === 'Enter') {
          var zo = document.getElementById('zoom-overlay');
          if (zo && zo.style.display === 'flex') {
            closeZoom(); e.preventDefault(); return;
          }
        }
        if (e.key === 'Enter' && !document.querySelector('.modal.show')) {
          e.preventDefault();
          // Fire both — whichever phase is active will act; the other will req() silently
          Shiny.setInputValue('btn_confirm',   Math.random(), {priority:'event'});
          Shiny.setInputValue('blank_confirm', Math.random(), {priority:'event'});
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
        div(class = "hint", "The site folder containing the JPG images and results.xlsx (e.g. 2025/Site_012/)."),

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
          uiOutput("previous_records_ui"),

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

  # ── Blank gallery ────────────────────────────────────────────────────
  conditionalPanel("output.app_phase === 'blank_gallery'",
    div(class = "blank-phase",
      div(class = "blank-hbar",
        tags$span(class = "label-name", "Blank images"),
        uiOutput("blank_gallery_meta", inline = TRUE),
        tags$span(style = "flex:1;"),
        tags$span(class = "back-link",
                  onclick = "Shiny.setInputValue('btn_back_from_gallery', Math.random())",
                  "Back to summary")
      ),
      div(class = "blank-grid-wrap",
        tags$p(class = "hint",
               style = "margin-bottom:14px; color:#555; letter-spacing:0.5px;",
               "These images were not flagged by AddaxAI. Click any you want to inspect manually — unselected images in this batch will be deleted."),
        div(class = "blank-grid", uiOutput("blank_tile_grid"))
      ),
      div(class = "blank-fbar",
        tags$button(class = "btn-sel",
                    onclick = "blankSelectAll(true)",  "Select all"),
        tags$button(class = "btn-sel",
                    onclick = "blankSelectAll(false)", "Deselect all"),
        tags$span(style = "flex:1;"),
        uiOutput("blank_fbar_btns", inline = TRUE)
      )
    )
  ),

  # ── Blank review ─────────────────────────────────────────────────────
  conditionalPanel("output.app_phase === 'blank_review'",
    div(class = "label-bar",
      div(class = "lbar-track",
          uiOutput("blank_review_bar_fill", inline = TRUE)),
      tags$span(class = "label-name", "Manual review"),
      tags$span(class = "label-meta",
                textOutput("blank_review_meta_txt", inline = TRUE)),
      tags$span(style = "flex:1;"),
      tags$span(class = "back-link",
                onclick = "Shiny.setInputValue('btn_back_to_gallery', Math.random())",
                "Back to gallery")
    ),
    fluidRow(
      style = "margin:0; padding:8px 24px;",
      column(8, style = "padding-right:10px;",
        div(class = "panel",
          div(class = "img-meta",
              textOutput("blank_review_img_meta", inline = TRUE)),
          div(class = "img-wrap", uiOutput("blank_review_image"))
        )
      ),
      column(4, style = "padding-left:10px;",
        div(class = "panel",
          uiOutput("blank_review_suggest_ui"),
          div(class = "sec-label first", "Species"),
          uiOutput("blank_review_species_ui"),
          div(class = "count-row",
            tags$span(class = "count-lbl", "Individuals"),
            div(class = "count-ctrl",
              tags$button(class = "count-btn minus",
                          onclick = "adjustCount(-1)", "-"),
              tags$input(id = "blank_count_display", class = "count-val",
                         type = "number", value = "1", min = "1", max = "99",
                         readonly = "readonly"),
              tags$button(class = "count-btn plus",
                          onclick = "adjustCount(1)", "+")
            )
          ),
          tags$p(class = "hint",
                 style = "margin:-6px 0 10px; letter-spacing:0.5px;",
                 "+ / . to increase   – / , to decrease"),
          tags$button(class = "btn-confirm",
                      onclick = "Shiny.setInputValue('blank_confirm', Math.random())",
                      "Confirm"),
          tags$button(class = "btn-nothing",
                      onclick = "Shiny.setInputValue('blank_nothing', Math.random())",
                      "Nothing present")
        )
      )
    )
  ),

  # ── Site complete ────────────────────────────────────────────────────
  conditionalPanel("output.app_phase === 'complete'",
    div(class = "complete-phase",
      div(class = "panel",
        div(class = "complete-title", "Site complete"),
        div(class = "complete-sub",  textOutput("complete_sub_txt", inline = TRUE)),

        div(class = "complete-stat", "Classifications"),
        uiOutput("complete_summary_ui"),

        div(class = "complete-btns",
          downloadButton("dl_csv", "Download classifications.csv"),
          tags$button(class   = "btn-load",
                      style   = "margin-top:0;",
                      onclick = "Shiny.setInputValue('btn_new_site', Math.random())",
                      "Start new site")
        )
      )
    )
  ),

  # ── Zoom lightbox (always in DOM; position:fixed so it overlays everything) ──
  tags$div(id = "zoom-overlay", onclick = "closeZoom()",
    tags$img(id = "zoom-img", src = ""),
    tags$span(id = "zoom-hint", "click  ·  esc  ·  enter  to close")
  ),

  # Custom message handlers
  tags$script(HTML("
    Shiny.addCustomMessageHandler('resetCount', function(msg) {
      document.querySelectorAll('.count-val').forEach(function(el) {
        el.value = msg.val;
      });
      // Keep the Shiny input in sync so btn_confirm always reads the displayed value,
      // not a stale value left over from a previous image's +/- adjustments.
      Shiny.setInputValue('count_val', msg.val);
    });

    // Blank-image gallery tile selection (per-batch; resets between batches)
    function toggleBlankThumb(el) {
      el.classList.toggle('selected');
      syncBlankSelected();
    }
    function blankSelectAll(sel) {
      document.querySelectorAll('.blank-thumb').forEach(function(t) {
        if (sel) t.classList.add('selected'); else t.classList.remove('selected');
      });
      syncBlankSelected();
    }
    function syncBlankSelected() {
      var sel = Array.from(document.querySelectorAll('.blank-thumb.selected'))
        .map(function(t) { return t.dataset.filename; });
      Shiny.setInputValue('blank_selected_files', sel, {priority: 'event'});
    }
    Shiny.addCustomMessageHandler('resetBlanksSelection', function(msg) {
      document.querySelectorAll('.blank-thumb').forEach(function(t) {
        t.classList.remove('selected');
      });
      Shiny.setInputValue('blank_selected_files', [], {priority: 'event'});
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
  shinyDirChoose(input, "btn_dir", roots = volumes, session = session)

  # Phase
  phase <- reactiveVal("setup")
  output$app_phase <- reactive(phase())
  outputOptions(output, "app_phase", suspendWhenHidden = FALSE)

  # Parsed paths
  img_dir_rv <- reactiveVal("")

  observe({
    req(input$btn_dir)
    d <- parseDirPath(volumes, input$btn_dir)
    if (length(d) > 0 && nzchar(d)) img_dir_rv(as.character(d))
  })

  output$dir_display <- renderText({
    d <- img_dir_rv()
    if (nzchar(d)) d else "No folder selected"
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
  files_df_rv    <- reactiveVal(NULL)          # all files sheet rows (incl. AddaxAI blanks)
  blank_candidates_rv   <- reactiveVal(character()) # remaining unprocessed blanks
  blank_current_batch_rv <- reactiveVal(character()) # the N tiles shown in gallery now
  blank_total_rv        <- reactiveVal(0L)           # total blanks when gallery entered
  blanks_deleted_rv     <- reactiveVal(0L)           # cumulative deletion count
  blank_review_queue_rv <- reactiveVal(character()) # selected from current batch
  blank_review_idx_rv   <- reactiveVal(1L)
  deletion_result_rv    <- reactiveVal(NULL)
  edit_prev_context_rv  <- reactiveVal(NULL)   # row being edited in the prev-box modal
  prev_edit_trigger_rv  <- reactiveVal(0L)      # bumped to force prev-box re-render after edit

  # ── Load ──────────────────────────────────────────────────────────────
  observeEvent(input$btn_load, {
    img_dir <- img_dir_rv()
    xlsx    <- file.path(img_dir, "results.xlsx")

    errs <- c(
      if (!nzchar(img_dir) || !dir.exists(img_dir)) "Image folder not found.",
      if (!file.exists(xlsx)) "results.xlsx not found in the selected folder."
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
    files_df_rv(res$files_df)
    done_rv(done_keys)
    history_rv(list())
    active_dir_rv(img_dir)
    active_csv_rv(csv_out)
    label_idx_rv(1L)
    deletion_result_rv(NULL)
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

  # Images AddaxAI processed but found no detections in — derived from the
  # files sheet rather than list.files() to avoid picking up images from
  # other sites that happen to share the same parent folder.
  compute_blanks <- reactive({
    fd  <- files_df_rv()
    dir <- active_dir_rv()
    if (is.null(fd) || !nzchar(dir) || is.null(img_label_rv())) return(character())
    detected_files <- unique(img_label_rv()$Filename)
    candidates     <- setdiff(unique(fd$Filename), detected_files)
    # Only return files that still exist on disk
    candidates[file.exists(file.path(dir, candidates))]
  })

  # Detected images where the reviewer chose "Nothing present" for EVERY label
  # (all labels are in done_rv but no CSV row was written for the file).
  # Depends on done_rv() so it updates as the session progresses.
  compute_nothing_present <- reactive({
    img_lbl <- img_label_rv()
    dir     <- active_dir_rv()
    if (is.null(img_lbl) || !nzchar(dir)) return(character())
    done_keys  <- done_rv()
    fully_done <- img_lbl |>
      mutate(key = paste0(label, "||", Filename)) |>
      group_by(Filename) |>
      summarise(all_done = all(key %in% done_keys), .groups = "drop") |>
      filter(all_done) |>
      pull(Filename)
    csv <- active_csv_rv()
    confirmed <- if (nzchar(csv) && file.exists(csv)) {
      tryCatch(unique(read_csv(csv, show_col_types = FALSE)$Filename),
               error = function(e) character())
    } else character()
    np <- setdiff(fully_done, confirmed)
    np[file.exists(file.path(dir, np))]
  })

  # Site-specific URL prefix for image serving — prevents browser cache collisions
  # when the same filename (e.g. IMG_0025.JPG) exists across multiple sites.
  # Each site gets its own prefix so the browser treats them as distinct resources.
  imgs_key <- reactive({
    dir <- active_dir_rv()
    if (!nzchar(dir)) return("imgs_default")
    key <- paste0("site_", gsub("[^a-zA-Z0-9]", "_", basename(dir)))
    addResourcePath(key, dir)
    key
  })

  # Advance to next label
  advance_label <- function() {
    lo <- label_order_rv()
    li <- label_idx_rv()
    label_idx_rv(min(li + 1L, length(lo) + 1L))
  }

  # Current image in blank manual-review queue
  current_blank_img <- reactive({
    q   <- blank_review_queue_rv()
    idx <- blank_review_idx_rv()
    if (length(q) == 0 || idx > length(q)) return(NULL)
    q[idx]
  })

  # Reset count and update bounding boxes when the current image changes
  observeEvent(current_blank_img(), {
    session$sendCustomMessage("resetCount", list(val = 1L))
    f     <- current_blank_img()
    boxes <- if (!is.null(f)) { b <- bboxes_rv()[[basename(f)]]; if (is.null(b)) list() else b } else list()
    session$sendCustomMessage("drawBBoxes", list(boxes = boxes))
  }, ignoreNULL = FALSE)

  observeEvent(current_img(), {
    row   <- current_img()
    val   <- 1L
    boxes <- list()
    if (!is.null(row)) {
      fname <- basename(row$Filename)
      bxs   <- bboxes_rv()[[fname]]
      if (!is.null(bxs) && length(bxs) > 0) {
        boxes     <- bxs
        lbl_lower <- tolower(trimws(row$label))
        n_match   <- sum(sapply(bxs, function(b) startsWith(tolower(b$label), lbl_lower)))
        if (n_match > 0L) val <- as.integer(n_match)
      }
    }
    session$sendCustomMessage("resetCount", list(val = val))
    session$sendCustomMessage("drawBBoxes",  list(boxes = boxes))
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
      n_blanks <- length(compute_blanks()) + length(compute_nothing_present())
      return(div(class = "msg-panel",
        div(class = "msg-big", "All labels reviewed"),
        div(class = "msg-sub",
            if (n_blanks > 0)
              paste0(n_blanks, " unreviewed image",
                     if (n_blanks == 1) "" else "s",
                     " remaining in folder — process them below, then download.")
            else
              "All images accounted for. Download your classifications below."
        ),
        if (n_blanks > 0)
          tags$button(
            class   = "btn-nothing",
            style   = "max-width:300px; margin:0 auto;",
            onclick = "Shiny.setInputValue('btn_start_blank_gallery', Math.random())",
            paste0("Review / delete ", n_blanks, " unreviewed image",
                   if (n_blanks == 1) "" else "s")
          ),
        if (n_blanks == 0)
          tags$button(
            class   = "btn-confirm",
            style   = "max-width:300px; margin:12px auto 0;",
            onclick = "Shiny.setInputValue('btn_go_complete', Math.random())",
            "View site summary"
          )
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

    tags$img(src     = paste0(imgs_key(), "/", row$Filename),
             alt     = row$Filename,
             onclick = "openZoom(this.src)")
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

  output$previous_records_ui <- renderUI({
    prev_edit_trigger_rv()   # re-render whenever a prev record is edited
    row <- current_img()
    if (is.null(row)) return(NULL)

    filename  <- row$Filename
    lbl       <- row$label
    done_keys <- done_rv()

    # Other AddaxAI labels for this image already reviewed in this session
    other_done <- img_label_rv() |>
      filter(Filename == filename, label != lbl) |>
      mutate(key = paste0(label, "||", Filename)) |>
      filter(key %in% done_keys) |>
      pull(label)

    # Confirmed entries from CSV for this image, keeping track of their
    # absolute row index in the full CSV (needed for targeted in-place edits)
    csv <- active_csv_rv()
    all_csv <- if (nzchar(csv) && file.exists(csv)) {
      tryCatch(read_csv(csv, show_col_types = FALSE), error = function(e) tibble())
    } else tibble()
    img_indices <- if (nrow(all_csv) > 0) which(all_csv$Filename == filename) else integer(0)
    prev        <- if (length(img_indices) > 0) all_csv[img_indices, ] else tibble()

    if (nrow(prev) == 0 && length(other_done) == 0) return(NULL)

    div(class = "prev-box",
      div(class = "prev-title", "Already reviewed for this image"),

      # Confirmed species rows — each has an Edit button
      if (nrow(prev) > 0)
        tagList(lapply(seq_len(nrow(prev)), function(i)
          div(class = "prev-row",
            paste0(prev$Species[i], " × ", prev$Count[i]),
            tags$button(
              class   = "btn-edit-prev",
              onclick = paste0("Shiny.setInputValue('btn_edit_prev', ",
                               img_indices[i], ", {priority:'event'})"),
              "Edit"
            )
          )
        )),

      # Labels reviewed and nothing recorded
      if (length(other_done) > 0)
        div(class = "prev-nothing",
            if (nrow(prev) == 0)
              paste0("'", paste(other_done, collapse = "', '"),
                     "' reviewed — nothing recorded")
            else
              paste0("Also reviewed: '",
                     paste(other_done, collapse = "', '"), "'")),

      div(class = "prev-guidance",
        tags$b("Confirm"), " — there is an additional ", lbl,
        " in this image not yet counted above.", tags$br(),
        tags$b("Nothing present"), " — all animals in this image are",
        " already recorded above."
      )
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
    cnt <- if (!is.null(input$count_val)) as.integer(input$count_val) else 1L

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

  # Enter blank gallery from completion screen
  observeEvent(input$btn_start_blank_gallery, {
    blanks <- unique(c(compute_blanks(), compute_nothing_present()))
    if (length(blanks) == 0) return()
    blank_total_rv(length(blanks))
    blank_candidates_rv(blanks)
    blank_current_batch_rv(blanks[seq_len(min(BLANK_BATCH_SIZE, length(blanks)))])
    blanks_deleted_rv(0L)
    session$sendCustomMessage("resetBlanksSelection", list())
    phase("blank_gallery")
  })

  # Return to summary without deleting anything
  observeEvent(input$btn_back_from_gallery, {
    phase("classify")
  })

  # Delete current batch (no manual review) — advance to next batch or complete
  observeEvent(input$btn_delete_all_blanks, {
    batch <- blank_current_batch_rv()
    dir   <- active_dir_rv()
    n_del <- 0L
    for (f in batch) {
      fp <- file.path(dir, f)
      if (file.exists(fp)) { file.remove(fp); n_del <- n_del + 1L }
    }
    blanks_deleted_rv(blanks_deleted_rv() + n_del)
    remaining <- setdiff(blank_candidates_rv(), batch)
    if (length(remaining) == 0) {
      blank_candidates_rv(character())
      blank_current_batch_rv(character())
      phase("complete")
    } else {
      next_batch <- remaining[seq_len(min(BLANK_BATCH_SIZE, length(remaining)))]
      blank_candidates_rv(remaining)
      blank_current_batch_rv(next_batch)
      session$sendCustomMessage("resetBlanksSelection", list())
      phase("blank_gallery")
    }
  })

  # Delete unselected from current batch, send selected to manual review queue
  observeEvent(input$btn_proceed_review, {
    batch    <- blank_current_batch_rv()
    selected <- if (length(input$blank_selected_files) > 0)
                  input$blank_selected_files else character()
    to_del   <- setdiff(batch, selected)
    dir      <- active_dir_rv()
    n_del    <- 0L
    for (f in to_del) {
      fp <- file.path(dir, f)
      if (file.exists(fp)) { file.remove(fp); n_del <- n_del + 1L }
    }
    blanks_deleted_rv(blanks_deleted_rv() + n_del)
    queue <- selected[file.exists(file.path(dir, selected))]
    blank_review_queue_rv(queue)
    blank_review_idx_rv(1L)
    phase("blank_review")
  })

  # Back to gallery from review (keep current batch state, just reset selection)
  observeEvent(input$btn_back_to_gallery, {
    session$sendCustomMessage("resetBlanksSelection", list())
    phase("blank_gallery")
  })

  # Blank review: Confirm species → save to CSV, advance
  observeEvent(input$blank_confirm, {
    f <- current_blank_img(); req(f)
    sp  <- if (!is.null(input$blank_species_sel)) input$blank_species_sel else "Unknown"
    cnt <- if (!is.null(input$count_val))         as.integer(input$count_val) else 1L

    # Try detection row first; fall back to files sheet (truly-blank images have no detection)
    meta <- img_label_rv() |> filter(Filename == f) |> slice(1)
    if (nrow(meta) > 0) {
      save_row(active_csv_rv(), meta$Station, f,
               meta$DateTimeOriginal, sp, cnt, meta$Burst_ID)
    } else {
      fmeta <- files_df_rv() |> filter(Filename == f) |> slice(1)
      dt    <- if (nrow(fmeta) > 0) fmeta$DateTimeOriginal else Sys.time()
      bid   <- if (nrow(fmeta) > 0) fmeta$Burst_ID         else 0L
      save_row(active_csv_rv(), basename(active_dir_rv()), f, dt, sp, cnt, bid)
    }

    idx <- blank_review_idx_rv()
    if (idx >= length(blank_review_queue_rv())) finish_blank_review()
    else blank_review_idx_rv(idx + 1L)
  })

  # Blank review: Nothing present → stays blank, advance
  observeEvent(input$blank_nothing, {
    idx <- blank_review_idx_rv()
    if (idx >= length(blank_review_queue_rv())) finish_blank_review()
    else blank_review_idx_rv(idx + 1L)
  })

  # ── Edit a previously-confirmed record (from the prev-box Edit button) ──────

  observeEvent(input$btn_edit_prev, {
    csv_row_idx <- as.integer(input$btn_edit_prev)
    csv         <- active_csv_rv()
    if (!nzchar(csv) || !file.exists(csv)) return()
    all_csv <- tryCatch(read_csv(csv, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(all_csv) || csv_row_idx < 1 || csv_row_idx > nrow(all_csv)) return()
    the_row <- all_csv[csv_row_idx, ]
    edit_prev_context_rv(list(idx = csv_row_idx,
                              species = the_row$Species,
                              count   = as.integer(the_row$Count)))
    showModal(modalDialog(
      title = "Edit classification",
      tags$p(style = "font-size:10px; color:#555; margin-bottom:14px;",
             basename(the_row$Filename)),
      selectInput("edit_prev_species", "Species",
                  choices = SPECIES_GROUPS, selected = the_row$Species, width = "100%"),
      numericInput("edit_prev_count", "Individuals",
                   value = as.integer(the_row$Count), min = 1L, max = 99L, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_prev", "Save",
          style = paste("background:#1e3a0f; border-color:#78a849; color:#78a849;",
                        "font-family:'IBM Plex Mono',monospace;",
                        "font-size:11px; letter-spacing:1px;"))
      )
    ))
  })

  observeEvent(input$confirm_edit_prev, {
    ctx <- edit_prev_context_rv(); removeModal()
    if (is.null(ctx)) return()
    csv     <- active_csv_rv()
    all_csv <- tryCatch(read_csv(csv, show_col_types = FALSE), error = function(e) NULL)
    if (is.null(all_csv) || ctx$idx < 1 || ctx$idx > nrow(all_csv)) return()
    all_csv$Species[ctx$idx] <- input$edit_prev_species
    all_csv$Count[ctx$idx]   <- as.integer(input$edit_prev_count)
    write_csv(all_csv, csv)
    edit_prev_context_rv(NULL)
    prev_edit_trigger_rv(prev_edit_trigger_rv() + 1L)
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

  # ── Blank gallery outputs ──────────────────────────────────────────────

  output$blank_gallery_meta <- renderUI({
    total     <- blank_total_rv()
    n_cands   <- length(blank_candidates_rv())
    n_batch   <- length(blank_current_batch_rv())
    n_batches <- max(1L, ceiling(total / BLANK_BATCH_SIZE))
    cur_batch <- floor((total - n_cands) / BLANK_BATCH_SIZE) + 1L
    tagList(
      tags$span(class = "label-meta",
                paste0("Batch ", cur_batch, " of ", n_batches)),
      tags$span(class = "label-meta",
                paste0(n_batch, " image", if (n_batch == 1) "" else "s"))
    )
  })

  output$blank_tile_grid <- renderUI({
    batch <- blank_current_batch_rv()
    if (length(batch) == 0) {
      return(div(class = "msg-panel",
        div(class = "msg-big", "No blank images remaining")))
    }
    key <- imgs_key()
    lapply(batch, function(f) {
      tags$div(
        class           = "blank-thumb",
        `data-filename` = f,
        onclick         = "toggleBlankThumb(this)",
        div(class = "chk", "✓"),
        tags$img(src = paste0(key, "/", f), alt = basename(f), loading = "lazy"),
        tags$span(basename(f))
      )
    })
  })

  output$blank_fbar_btns <- renderUI({
    n_sel   <- length(input$blank_selected_files)
    batch   <- blank_current_batch_rv()
    n_batch <- length(batch)
    n_del   <- n_batch - n_sel
    tagList(
      if (n_sel > 0)
        tags$button(class   = "btn-next",
                    onclick = "Shiny.setInputValue('btn_proceed_review', Math.random())",
                    paste0("Review ", n_sel, " & delete ", n_del))
      else
        tags$button(class    = "btn-next",
                    disabled = NA,
                    style    = "opacity:0.35; cursor:not-allowed;",
                    paste0("Review 0 & delete ", n_del))
      ,
      tags$button(class   = "btn-human",
                  style   = "width:auto; padding:10px 20px;",
                  onclick = "Shiny.setInputValue('btn_delete_all_blanks', Math.random())",
                  paste0("Delete batch (", n_batch, ")"))
    )
  })

  # ── Blank review outputs ──────────────────────────────────────────────

  output$blank_review_bar_fill <- renderUI({
    n   <- length(blank_review_queue_rv())
    idx <- blank_review_idx_rv()
    pct <- if (n > 0) round((idx - 1) / n * 100) else 0
    div(class = "lbar-fill", style = paste0("width:", pct, "%;"))
  })

  output$blank_review_meta_txt <- renderText({
    q   <- blank_review_queue_rv()
    idx <- blank_review_idx_rv()
    sprintf("%d / %d", min(idx, max(length(q), 1L)), length(q))
  })

  output$blank_review_img_meta <- renderText({
    f <- current_blank_img()
    if (is.null(f)) "" else f
  })

  output$blank_review_image <- renderUI({
    f <- current_blank_img()
    if (is.null(f)) {
      return(div(class = "msg-panel",
        div(class = "msg-big", "Review complete"),
        div(class = "msg-sub", "All selected images reviewed.")
      ))
    }
    fp <- file.path(active_dir_rv(), f)
    if (!file.exists(fp)) {
      return(div(class = "msg-panel",
        div(class = "msg-big", style = "color:#c0392b;", "Image not found"),
        tags$button(class = "btn-nothing",
                    onclick = "Shiny.setInputValue('blank_nothing', Math.random())",
                    "Skip")
      ))
    }
    tags$img(src     = paste0(imgs_key(), "/", f),
             alt     = basename(f),
             onclick = "openZoom(this.src)")
  })

  output$blank_review_suggest_ui <- renderUI({
    f <- current_blank_img()
    if (is.null(f)) return(NULL)
    row <- img_label_rv() |> filter(Filename == f) |> slice(1)
    if (nrow(row) == 0) {
      # Truly blank — AddaxAI found no detection at all
      return(div(class = "suggest-box suggest-sp",
        "AddaxAI: ",
        tags$span(class = "sp", style = "color:#555;", "no detection")
      ))
    }
    div(class = "suggest-box suggest-sp",
      "AddaxAI: ",
      tags$span(class = "sp", row$label),
      tags$span(class = "cf", sprintf("%.0f%%", row$max_conf * 100))
    )
  })

  output$blank_review_species_ui <- renderUI({
    f   <- current_blank_img()
    # Default to Unknown for images AddaxAI never flagged
    sel <- "Unknown"
    if (!is.null(f)) {
      row <- img_label_rv() |> filter(Filename == f) |> slice(1)
      if (nrow(row) > 0 && row$suggested %in% SPECIES_CHOICES) sel <- row$suggested
    }
    selectInput("blank_species_sel", NULL,
                choices = SPECIES_GROUPS, selected = sel, width = "100%")
  })

  # ── Helper: advance to next batch after review, or go to complete ──────
  finish_blank_review <- function() {
    remaining <- setdiff(blank_candidates_rv(), blank_current_batch_rv())
    if (length(remaining) == 0) {
      blank_candidates_rv(character())
      blank_current_batch_rv(character())
      phase("complete")
    } else {
      next_batch <- remaining[seq_len(min(BLANK_BATCH_SIZE, length(remaining)))]
      blank_candidates_rv(remaining)
      blank_current_batch_rv(next_batch)
      session$sendCustomMessage("resetBlanksSelection", list())
      phase("blank_gallery")
    }
  }

  # ── Complete phase outputs ──────────────────────────────────────────────

  output$complete_sub_txt <- renderText({
    dir     <- active_dir_rv()
    deleted <- blanks_deleted_rv()
    paste0(basename(dir), " — ",
           deleted, " blank image", if (deleted == 1) "" else "s", " deleted")
  })

  output$complete_summary_ui <- renderUI({
    csv <- active_csv_rv()
    if (!nzchar(csv) || !file.exists(csv))
      return(div(class = "msg-sub", "No classifications recorded."))
    df <- read_csv(csv, show_col_types = FALSE)
    if (nrow(df) == 0)
      return(div(class = "msg-sub", "No classifications recorded."))
    summary_df <- df |>
      group_by(Species) |>
      summarise(total = sum(Count, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total))
    tagList(lapply(seq_len(nrow(summary_df)), function(i) {
      div(class = "summary-row",
        div(class = "summary-sp", summary_df$Species[i]),
        div(class = "summary-ct", summary_df$total[i])
      )
    }))
  })

  observeEvent(input$btn_go_complete, {
    phase("complete")
  })

  observeEvent(input$btn_new_site, {
    img_label_rv(NULL)
    label_order_rv(character())
    label_idx_rv(1L)
    done_rv(character())
    history_rv(list())
    active_dir_rv("")
    active_csv_rv("")
    bboxes_rv(list())
    files_df_rv(NULL)
    blank_candidates_rv(character())
    blank_current_batch_rv(character())
    blank_total_rv(0L)
    blanks_deleted_rv(0L)
    blank_review_queue_rv(character())
    blank_review_idx_rv(1L)
    deletion_result_rv(NULL)
    img_dir_rv("")
    phase("setup")
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
