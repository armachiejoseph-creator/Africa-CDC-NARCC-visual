library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(readxl)
library(openxlsx)

# ── 1. DATABASE SETUP ─────────────────────────────────────────────────────────
# IMPORTANT for Shiny Server deployment: keep the database OUTSIDE the app's
# code directory. If the app folder ever gets overwritten/rsynced during a
# redeploy, a db file living next to app.R would be wiped out along with it.
# Point this at a dedicated, persistent, shiny-user-writable directory instead,
# e.g. /srv/shiny-server-data/results_framework/  (create it once, then:
#   sudo chown -R shiny:shiny /srv/shiny-server-data/results_framework
#   sudo chmod 750 /srv/shiny-server-data/results_framework
# — adjust the user/group to whatever Shiny Server runs as on your VM).
data_dir <- Sys.getenv("RESULTS_DATA_DIR", unset = ".")  # falls back to "." for local testing
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
db_name <- file.path(data_dir, "results_framework_db_v1.sqlite")
con <- dbConnect(SQLite(), db_name)

# WAL mode lets reads and writes happen concurrently instead of the whole
# file locking on every write — cheap insurance once more than one person
# might submit a report at the same moment.
dbExecute(con, "PRAGMA journal_mode=WAL;")

# Registered once at app-process level (NOT inside server(), which runs
# per-session and would try to disconnect this shared connection repeatedly).
onStop(function() {
  if (dbIsValid(con)) dbDisconnect(con)
})

AU <- list(green = "#0E4B43", gold = "#b38e00", red = "#952038",
           blue = "#025891", muted = "#6b7280")

# One-time migration: if a database from before this column rename exists,
# drop just the indicators table and let it get recreated below with the new
# schema. Reports are untouched — same "framework can be reset without losing
# reports" behavior the admin hard-reset already relies on.
old_schema <- tryCatch({
  info <- dbGetQuery(con, "PRAGMA table_info(indicators)")
  "ahss_pillar_no" %in% info$name || "indicator_code" %in% info$name
}, error = function(e) FALSE)
if (old_schema) dbExecute(con, "DROP TABLE indicators")

# Column names here map 1:1 to your real source columns:
#   #                                   -> indicator_id
#   Pillar(s)                          -> pillar
#   Big Ticket                         -> big_ticket
#   Result(s)                          -> result
#   Indicator & Code (Link from PIRS)  -> indicator_and_code
#   Tier                               -> tier
#   2026 Target                        -> target_2026
#   Mid-Year 2026 Milestone            -> midyear_2026_milestone
#   Delivery Model                     -> delivery_model
#   Success Factors                    -> success_factors
#   Primary Responsibility BU          -> primary_bu
#   Contributing BUs                   -> contributing_bus
dbExecute(con, "CREATE TABLE IF NOT EXISTS indicators (
  indicator_id TEXT PRIMARY KEY,
  pillar TEXT, big_ticket TEXT, result TEXT, indicator_and_code TEXT, tier TEXT,
  target_2026 REAL, midyear_2026_milestone REAL, delivery_model TEXT,
  success_factors TEXT, primary_bu TEXT, contributing_bus TEXT
)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS reports (
  report_id INTEGER PRIMARY KEY AUTOINCREMENT,
  contribution_id TEXT, indicator_id TEXT, indicator_code TEXT, bu_name TEXT,
  status TEXT, target REAL, achieved REAL, performance_pct REAL,
  progress TEXT, challenges TEXT, next_steps TEXT, na_reason TEXT, timestamp TEXT
)")
# Safe for databases created before status/na_reason existed — errors if the
# column is already there, which we just ignore.
try(dbExecute(con, "ALTER TABLE reports ADD COLUMN status TEXT"), silent = TRUE)
try(dbExecute(con, "ALTER TABLE reports ADD COLUMN na_reason TEXT"), silent = TRUE)

# Maps your real Excel headers -> internal column names used above. This is
# the ONLY place that needs to change if your spreadsheet's headers change —
# everything else in the app just reads the internal names on the right.
HEADER_MAP <- c(
  "#"                                  = "indicator_id",
  "Pillar(s)"                          = "pillar",
  "Big Ticket"                         = "big_ticket",
  "Result(s)"                          = "result",
  "Indicator & Code (Link from PIRS)"  = "indicator_and_code",
  "Tier"                               = "tier",
  "2026 Target"                        = "target_2026",
  "Mid-Year 2026 Milestone"            = "midyear_2026_milestone",
  "Delivery Model"                     = "delivery_model",
  "Success Factors"                    = "success_factors",
  "Primary Responsibility BU"          = "primary_bu",
  "Contributing BUs"                   = "contributing_bus"
)

# Seed with sample rows the first time the DB is created, so the app is
# testable before an admin ever uploads anything.
if (dbGetQuery(con, "SELECT COUNT(*) n FROM indicators")$n == 0) {
  seed <- data.frame(
    indicator_id = c("1", "2", "3"),
    pillar = c("Pillar 1 - Public Health Emergency Management",
               "Pillar 2 - Laboratory Systems & Networks",
               "Pillar 3 - Partnerships & External Relations"),
    big_ticket = c("Regional Rapid Response", "RISLNET Expansion", "Regional Stakeholder Platform"),
    result = c("Member States receive timely technical assistance for outbreak readiness",
               "Strengthened laboratory networks across North Africa",
               "Regional bodies actively engaged in health security planning"),
    indicator_and_code = c("RCC.12.3-6 — Number of Member States provided with TA through Country engagement visits",
                           "RCC.12.2-5 — Number of programmatic activities implemented under RISLNET in Member States",
                           "RCC.12.3-8 — Number of RECs and regional health stakeholders engaged at least once per year"),
    tier = c("Tier 1", "Tier 2", "Tier 3"),
    target_2026 = c(10, 8, 6),
    midyear_2026_milestone = c(5, 4, 3),
    delivery_model = c("Country engagement visits", "Regional workshops & lab assessments", "Stakeholder convenings"),
    success_factors = c("Member State willingness to engage; travel funding; RCC staff availability",
                        "Member State lab readiness; reagent supply chain; partner co-funding",
                        "REC calendar alignment; leadership buy-in"),
    primary_bu = c("Surveillance Division", "Laboratory Systems Division", "Partnerships Division"),
    contributing_bus = c("Surveillance Division, Laboratory Systems Division, Emergency Response Division",
                         "Laboratory Systems Division, Research Division",
                         "Partnerships Division, Communications Division"),
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "indicators", seed, append = TRUE)
}

# Split "Contributing BUs" (comma-separated) into individual rows, one call
# site so both the dashboard and the modal stay in sync with the same logic.
# The Primary/Lead BU always gets a reporting row too — even if whoever filled
# in the spreadsheet didn't repeat its name inside the Contributing BUs cell —
# since the lead is expected to report on its own portion like any other BU.
split_contributions <- function(ind_df) {
  if (nrow(ind_df) == 0) return(data.frame(indicator_id=character(), bu_name=character(),
                                           contribution_id=character(), is_lead=logical(), stringsAsFactors=FALSE))
  bind_rows(lapply(seq_len(nrow(ind_df)), function(i) {
    bus  <- trimws(strsplit(ind_df$contributing_bus[i], ",")[[1]])
    lead <- trimws(ind_df$primary_bu[i])
    if (nzchar(lead) && !(tolower(lead) %in% tolower(bus))) bus <- c(lead, bus)  # prepend if missing
    data.frame(indicator_id = ind_df$indicator_id[i], bu_name = bus,
               contribution_id = paste0(ind_df$indicator_id[i], "_bu", seq_along(bus)),
               is_lead = tolower(bus) == tolower(lead),
               stringsAsFactors = FALSE)
  }))
}

status_for <- function(achieved, target) {
  if (is.na(achieved) || is.na(target)) return(list(label="Not started", color=AU$muted, bg="#f1f2f4"))
  ratio <- achieved / target
  if (ratio >= 1)   return(list(label="Achieved", color=AU$green, bg="#e3efe3"))
  if (ratio >= 0.5) return(list(label="On track", color=AU$gold,  bg="#f7f0d9"))
  return(list(label="Behind", color=AU$red, bg="#f5e3e6"))
}
NA_STATUS <- list(label = "Not applicable", color = "#6b7280", bg = "#e9eaec")
tier_style <- function(tier) {
  switch(tier,
         "Tier 1" = list(bg="#f5e3e6", color=AU$red),
         "Tier 2" = list(bg="#f7f0d9", color=AU$gold),
         "Tier 3" = list(bg="#d0e4f5", color=AU$blue),
         list(bg="#eee", color="#666"))
}
meta_item <- function(label, value) tags$div(tags$div(class="meta-item-label", label), tags$div(class="meta-item-value", value))
ref_line  <- function(label, value) tags$div(style="margin-bottom:8px;",
                                             tags$div(style="font-size:9.5px;font-weight:700;color:#9ca3af;text-transform:uppercase;letter-spacing:.04em;", label),
                                             tags$div(style="font-size:12.5px;color:#1f2937;margin-top:1px;", value))

# ── 2. UI ─────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  # Requires an image file at www/africa_cdc_logo.png next to this app.R —
  # Shiny serves anything in a local "www" folder automatically. Swap the
  # filename below if yours is named differently.
  title = tagList(
    tags$img(src = "cdc.png", height = "28px",
             style = "margin-right:10px;vertical-align:middle;"),
    tags$span("Africa CDC Big Ticket indicators", style = "vertical-align:middle;")
  ),
  id = "main_nav",
  position = "fixed-top",
  theme = bs_theme(bootswatch = "flatly", primary = AU$green, secondary = AU$gold),
  
  header = tagList(
    tags$style(HTML("
      body { padding-top: 90px !important; background:#f4f5f7; }
      .page-sub { font-size: 13px; color:#5B6E6A; margin-bottom: 20px; }

      .ind-card { background:white; border-radius:12px; border:1px solid #e5e7eb;
        box-shadow:0 1px 8px rgba(0,0,0,.05); margin-bottom:20px; overflow:hidden; }
      .ind-head { padding:18px 22px 16px; border-bottom:1px solid #f1f2f4; }
      .tag-row { display:flex; gap:6px; flex-wrap:wrap; margin-bottom:10px; }
      .tag-pill { font-size:10px; font-weight:700; padding:3px 10px; border-radius:10px; white-space:nowrap; }
      .labeled-line { font-size:12px; color:#374151; margin-bottom:6px; line-height:1.45; }
      .labeled-line-tag { font-size:9.5px; font-weight:700; letter-spacing:.04em; margin-right:8px; white-space:nowrap; }
      .labeled-line-text { word-break: break-word; }

      .pillar-callout {
        background: linear-gradient(135deg, #E4EEEC 0%, #f4f9f8 100%);
        border-left: 4px solid #0E4B43; border-radius: 8px;
        padding: 10px 14px; margin-bottom: 10px;
      }
      .pillar-callout-label { font-size:9.5px; font-weight:800; color:#0E4B43;
        text-transform:uppercase; letter-spacing:.06em; margin-bottom:3px; }
      .pillar-callout-text { font-size:12.5px; color:#1f2937; line-height:1.45; font-weight:500; }

      .result-callout {
        background: linear-gradient(135deg, #FBF2E3 0%, #fffdf8 100%);
        border-left: 4px solid #b38e00; border-radius: 8px;
        padding: 12px 16px; margin-bottom: 14px;
      }
      .result-callout-label { font-size:9.5px; font-weight:800; color:#b38e00;
        text-transform:uppercase; letter-spacing:.06em; margin-bottom:4px; }
      .result-callout-text { font-size:16px; font-weight:700; color:#111827; line-height:1.4; }
      .ind-result { font-size:15.5px; font-weight:700; color:#111827; line-height:1.35; margin-bottom:4px; }
      .ind-indicator-line { font-size:11.5px; color:#6b7280; margin-bottom:14px; }
      .ind-indicator-line a { color:#0E4B43; text-decoration:none; }
      .ind-code { font-family:Consolas, monospace; font-weight:700; color:#0E4B43; }
      .meta-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:12px;
        background:#f8f9fa; border-radius:8px; padding:12px 14px; margin-bottom:12px; }
      .meta-item-label { font-size:9px; font-weight:700; color:#9ca3af; text-transform:uppercase; letter-spacing:.04em; }
      .meta-item-value { font-size:12.5px; font-weight:600; color:#1f2937; margin-top:3px; line-height:1.3; }
      .success-line { font-size:11px; color:#6b7280; margin-bottom:12px; line-height:1.45; }
      .success-line b { color:#4b5563; }
      .ind-progress-track { background:#eef1f0; border-radius:6px; height:6px; overflow:hidden; }
      .ind-progress-fill { height:100%; border-radius:6px; }
      .ind-progress-label { font-size:10.5px; font-weight:700; color:#6b7280; margin-top:6px; text-align:right; }
      .bu-section-label { font-size:10px; font-weight:700; color:#9ca3af; text-transform:uppercase;
        letter-spacing:.05em; padding:12px 22px 2px; }
      .div-row { display:flex; align-items:center; justify-content:space-between;
        padding:12px 22px; border-bottom:1px solid #f7f8f9; }
      .div-row:last-child { border-bottom:none; }
      .div-name { font-size:13px; color:#1B2B29; font-weight:500; }
      .lead-badge { font-size:9px; font-weight:800; color:#b38e00; background:#FBF2E3;
        padding:2px 7px; border-radius:8px; margin-left:8px; text-transform:uppercase; letter-spacing:.03em; }
      .div-value { font-size:11.5px; color:#6b7280; margin-top:2px; }
      .div-right { display:flex; align-items:center; gap:10px; }
      .status-pill { font-size:10.5px; font-weight:700; padding:4px 10px; border-radius:12px; white-space:nowrap; }
      .report-btn { background:#0E4B43 !important; color:white !important; border:none !important;
        font-size:11.5px !important; font-weight:600 !important; padding:6px 14px !important; border-radius:16px !important; }
      .report-btn:hover { background:#156358 !important; }
      .modal-ref-box { background:#f8f9fa; border-radius:8px; padding:12px 14px; margin-bottom:16px; }
      .modal-dialog.modal-lg { max-width: 780px; width: 92vw; }
      .modal-body { padding: 22px 26px; }
      .modal-header { background:#0E4B43; }
      .modal-header .modal-title { color:white; font-weight:600; }
      .modal-header .close { color:white; opacity:.8; }
      .pct-readout { background:#eef1f0; border-radius:8px; padding:10px 14px; margin-bottom:14px;
        display:flex; align-items:center; justify-content:space-between; }
      .pct-readout-label { font-size:11px; font-weight:700; color:#6b7280; text-transform:uppercase; letter-spacing:.04em; }
      .pct-readout-value { font-size:20px; font-weight:700; }
    ")),
    # Delegated click listener — fires for ANY .report-btn, including ones
    # rendered after an admin hard-reset. One listener, one Shiny input,
    # instead of one observeEvent per button wired only at startup.
    tags$script(HTML(
      "$(document).on('click', '.report-btn', function() {
         Shiny.setInputValue('report_btn_trigger', this.id, {priority: 'event'});
       });"
    ))
  ),
  
  nav_panel("Results Dashboard",
            div(style="max-width:920px; margin:0 auto; padding:15px;",
                div(class="page-sub", "Indicators and the business units contributing to each. Click Report to log progress."),
                layout_column_wrap(width = 1/2,
                                   selectInput("f_tier", "Filter by Tier:", choices = c("All", "Tier 1", "Tier 2", "Tier 3")),
                                   textInput("f_search", "Search indicator code, name, or result:")
                ),
                uiOutput("indicator_list")
            )
  ),
  nav_spacer(),
  nav_item(actionButton("btn_admin_login", "Admin", class="btn-outline-light"))
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  refresh_val <- reactiveVal(0)
  is_admin    <- reactiveVal(FALSE)
  active_bu   <- reactiveVal(NULL)
  
  indicators_data <- reactive({ refresh_val(); dbGetQuery(con, "SELECT * FROM indicators") })
  contributions_data <- reactive({ split_contributions(indicators_data()) })
  latest_reports <- reactive({
    refresh_val()
    df <- dbGetQuery(con, "SELECT * FROM reports")
    if (nrow(df) == 0) return(df)
    df %>% mutate(timestamp = as.POSIXct(timestamp)) %>%
      group_by(contribution_id) %>% slice_max(timestamp, n = 1, with_ties = FALSE) %>% ungroup()
  })
  
  # ── Dashboard: indicator cards + contributing-BU report rows ───────────────
  output$indicator_list <- renderUI({
    data <- indicators_data()
    if (nrow(data) == 0) return(h5("No indicators loaded. Use Admin > Upload to add the framework."))
    
    if (input$f_tier != "All") data <- data %>% filter(tier == input$f_tier)
    if (nzchar(input$f_search)) {
      q <- input$f_search
      data <- data %>% filter(grepl(q, indicator_and_code, ignore.case=TRUE) |
                                grepl(q, result, ignore.case=TRUE))
    }
    if (nrow(data) == 0) return(h5("No indicators match the current filters."))
    
    latest <- latest_reports()
    all_bus <- contributions_data()
    
    lapply(seq_len(nrow(data)), function(i) {
      ind    <- data[i, ]
      bus    <- all_bus %>% filter(indicator_id == ind$indicator_id)
      tstyle <- tier_style(ind$tier)
      
      bu_rows <- lapply(seq_len(nrow(bus)), function(j) {
        b       <- bus[j, ]
        rep_row <- if (nrow(latest) == 0) latest else latest %>% filter(contribution_id == b$contribution_id)
        has_rep <- nrow(rep_row) > 0
        is_na_status <- has_rep && !is.na(rep_row$status[1]) && rep_row$status[1] == "Not Applicable"
        
        if (is_na_status) {
          st <- NA_STATUS
          val_txt <- "Marked not applicable this period"
        } else if (has_rep) {
          st <- status_for(rep_row$achieved[1], rep_row$target[1])
          val_txt <- paste0(rep_row$achieved[1], " / ", rep_row$target[1], "  (", rep_row$performance_pct[1], "%)")
        } else {
          st <- status_for(NA_real_, NA_real_)
          val_txt <- "No report yet"
        }
        
        tags$div(class="div-row",
                 tags$div(
                   tags$div(class="div-name",
                            b$bu_name,
                            if (isTRUE(b$is_lead)) tags$span(class="lead-badge", "LEAD")
                   ),
                   tags$div(class="div-value", val_txt)
                 ),
                 tags$div(class="div-right",
                          tags$span(class="status-pill", style=paste0("color:",st$color,";background:",st$bg,";"), st$label),
                          # id encodes the contribution_id; the delegated listener reads this.id
                          actionButton(inputId = paste0("report_", b$contribution_id), label = "Report", class = "report-btn")
                 )
        )
      })
      
      bu_ratios <- sapply(bus$contribution_id, function(cid) {
        if (nrow(latest) == 0) return(NA_real_)
        r <- latest %>% filter(contribution_id == cid)
        if (nrow(r) == 0) return(NA_real_)
        if (!is.na(r$status[1]) && r$status[1] == "Not Applicable") return(NA_real_)  # excluded, not counted as 0
        if (is.na(r$target[1]) || r$target[1] == 0) return(NA_real_)
        min(r$achieved[1] / r$target[1], 1)
      })
      pct <- if (all(is.na(bu_ratios))) 0 else round(mean(bu_ratios, na.rm = TRUE) * 100)
      bar_color <- if (pct >= 100) AU$green else if (pct >= 50) AU$gold else AU$red
      
      tags$div(class="ind-card",
               tags$div(class="ind-head",
                        tags$div(class="tag-row",
                                 tags$span(class="tag-pill", style=paste0("background:",tstyle$bg,";color:",tstyle$color,";"), ind$tier)
                        ),
                        tags$div(class="pillar-callout",
                                 tags$div(class="pillar-callout-label", "Pillar"),
                                 tags$div(class="pillar-callout-text", ind$pillar)
                        ),
                        tags$div(class="labeled-line",
                                 tags$span(class="labeled-line-tag", style=paste0("color:",AU$blue,";"), "BIG TICKET"),
                                 tags$span(class="labeled-line-text", ind$big_ticket)
                        ),
                        tags$div(class="result-callout",
                                 tags$div(class="result-callout-label", "Result"),
                                 tags$div(class="result-callout-text", ind$result)
                        ),
                        tags$div(class="ind-indicator-line", ind$indicator_and_code),
                        tags$div(class="meta-grid",
                                 meta_item("2026 Target", ind$target_2026),
                                 meta_item("Mid-Year Milestone", ind$midyear_2026_milestone),
                                 meta_item("Delivery Model", ind$delivery_model),
                                 meta_item("Primary BU", ind$primary_bu)
                        ),
                        tags$div(class="success-line", tags$b("Success factors: "), ind$success_factors),
                        tags$div(class="ind-progress-track",
                                 tags$div(class="ind-progress-fill", style=paste0("width:",pct,"%;background:",bar_color,";"))),
                        tags$div(class="ind-progress-label", paste0(pct, "% overall"))
               ),
               tags$div(class="bu-section-label", "Contributing BUs"),
               bu_rows
      )
    })
  })
  
  # ── ONE delegated observer handles every Report click, present or future ──
  observeEvent(input$report_btn_trigger, {
    cid <- sub("^report_", "", input$report_btn_trigger)
    active_bu(cid)
    
    b   <- contributions_data() %>% filter(contribution_id == cid)
    req(nrow(b) == 1)
    ind <- indicators_data() %>% filter(indicator_id == b$indicator_id[1])
    
    latest <- latest_reports()
    rep_row <- if (nrow(latest) == 0) latest else latest %>% filter(contribution_id == cid)
    was_na             <- nrow(rep_row) > 0 && !is.na(rep_row$status[1]) && rep_row$status[1] == "Not Applicable"
    prefill_target     <- if (nrow(rep_row) == 0) ind$midyear_2026_milestone[1] else rep_row$target[1]
    prefill_achieved   <- if (nrow(rep_row) == 0) 0 else rep_row$achieved[1]
    prefill_progress   <- if (nrow(rep_row) == 0) "" else rep_row$progress[1]
    prefill_challenges <- if (nrow(rep_row) == 0) "" else rep_row$challenges[1]
    prefill_nextsteps  <- if (nrow(rep_row) == 0) "" else rep_row$next_steps[1]
    prefill_na_reason  <- if (nrow(rep_row) == 0) "" else rep_row$na_reason[1]
    if (is.na(prefill_target))   prefill_target   <- ind$midyear_2026_milestone[1]
    if (is.na(prefill_achieved)) prefill_achieved <- 0
    if (is.na(prefill_na_reason)) prefill_na_reason <- ""
    
    showModal(modalDialog(
      title = paste0("Report progress — ", b$bu_name[1]), size = "l",
      tags$div(class="modal-ref-box",
               ref_line("Pillar(s)", ind$pillar[1]),
               ref_line("Big Ticket", ind$big_ticket[1]),
               ref_line("Result(s)", ind$result[1]),
               ref_line("Indicator & Code (Link from PIRS)", ind$indicator_and_code[1])
      ),
      checkboxInput(paste0("na_input_", cid), "Not applicable for reporting this period", value = was_na),
      conditionalPanel(
        condition = sprintf("!input.na_input_%s", cid),
        fluidRow(
          column(6, numericInput(paste0("target_input_", cid), "Target", value = prefill_target, min = 0)),
          column(6, numericInput(paste0("achieved_input_", cid), "Achieved", value = prefill_achieved, min = 0))
        ),
        uiOutput("performance_pct_display"),
        textAreaInput(paste0("progress_input_", cid), "Progress milestones", value = prefill_progress, rows = 2),
        textAreaInput(paste0("challenges_input_", cid), "Challenges", value = prefill_challenges, rows = 2),
        textAreaInput(paste0("nextsteps_input_", cid), "Next steps", value = prefill_nextsteps, rows = 2)
      ),
      conditionalPanel(
        condition = sprintf("input.na_input_%s", cid),
        textAreaInput(paste0("na_reason_input_", cid), "Reason (optional)", value = prefill_na_reason, rows = 2,
                      placeholder = "e.g., no activity planned for this BU this period")
      ),
      footer = tagList(modalButton("Cancel"), actionButton("submit_report_btn", "Submit report", class="btn report-btn")),
      easyClose = TRUE
    ))
  })
  
  output$performance_pct_display <- renderUI({
    cid <- active_bu(); req(cid)
    tgt <- input[[paste0("target_input_", cid)]]; ach <- input[[paste0("achieved_input_", cid)]]
    req(!is.null(tgt), !is.null(ach))
    pct <- if (is.na(tgt) || tgt == 0) 0 else round(ach / tgt * 100, 1)
    pcolor <- if (pct >= 100) AU$green else if (pct >= 50) AU$gold else AU$red
    tags$div(class="pct-readout",
             tags$span(class="pct-readout-label", "Performance"),
             tags$span(class="pct-readout-value", style=paste0("color:",pcolor,";"), paste0(pct, "%")))
  })
  
  observeEvent(input$submit_report_btn, {
    cid <- active_bu(); req(cid)
    b   <- contributions_data() %>% filter(contribution_id == cid)
    ind <- indicators_data() %>% filter(indicator_id == b$indicator_id[1])
    is_na <- isTRUE(input[[paste0("na_input_", cid)]])
    
    if (is_na) {
      dbExecute(con, "INSERT INTO reports
        (contribution_id, indicator_id, indicator_code, bu_name, status, target, achieved,
         performance_pct, progress, challenges, next_steps, na_reason, timestamp) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
                list(cid, ind$indicator_id[1], ind$indicator_and_code[1], b$bu_name[1], "Not Applicable",
                     NA_real_, NA_real_, NA_real_, NA_character_, NA_character_, NA_character_,
                     input[[paste0("na_reason_input_", cid)]], format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    } else {
      tgt <- input[[paste0("target_input_", cid)]]; ach <- input[[paste0("achieved_input_", cid)]]
      pct <- if (is.na(tgt) || tgt == 0) 0 else round(ach / tgt * 100, 1)
      dbExecute(con, "INSERT INTO reports
        (contribution_id, indicator_id, indicator_code, bu_name, status, target, achieved, performance_pct,
         progress, challenges, next_steps, na_reason, timestamp) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)",
                list(cid, ind$indicator_id[1], ind$indicator_and_code[1], b$bu_name[1], "Reported", tgt, ach, pct,
                     input[[paste0("progress_input_", cid)]], input[[paste0("challenges_input_", cid)]],
                     input[[paste0("nextsteps_input_", cid)]], NA_character_, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    }
    
    refresh_val(refresh_val() + 1)
    removeModal()
    showNotification("Report submitted.", type = "message", duration = 2.5)
  })
  
  # ── ADMIN LOGIN ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_admin_login, {
    showModal(modalDialog(passwordInput("ap", "Enter Admin Code"), footer = actionButton("do_al", "Login")))
  })
  observeEvent(input$do_al, {
    admin_secret <- Sys.getenv("RESULTS_ADMIN_CODE")
    if (admin_secret == "") { showNotification("Error: RESULTS_ADMIN_CODE env var is empty.", type="error"); return() }
    if (input$ap == admin_secret) { is_admin(TRUE); removeModal(); showNotification("Admin access granted.", type="message") }
    else showNotification("Incorrect code", type = "error")
  })
  
  observe({
    if (is_admin()) nav_insert("main_nav", nav_panel("Admin",
                                                     div(style="max-width:800px;margin:20px auto;",
                                                         card(
                                                           card_header("Indicator Framework Management"),
                                                           p(style="font-size:12.5px;color:#6b7280;",
                                                             "Upload an Excel file with these exact column headers: ",
                                                             tags$code("#, Pillar(s), Big Ticket, Result(s), Indicator & Code (Link from PIRS), Tier, ",
                                                                       "2026 Target, Mid-Year 2026 Milestone, Delivery Model, Success Factors, ",
                                                                       "Primary Responsibility BU, Contributing BUs"), ". ",
                                                             tags$b("Submitted progress reports are preserved"), " across a reset."),
                                                           layout_column_wrap(width = 1/2,
                                                                              fileInput("m_file", "1. Upload Excel to reset the framework", accept = ".xlsx"),
                                                                              div(
                                                                                downloadButton("download_indicators_backup", "2. Download indicators backup", class="btn-info", style="width:100%;margin-bottom:8px;"),
                                                                                downloadButton("download_reports_backup", "3. Download reports backup", class="btn-info", style="width:100%;")
                                                                              )
                                                           ),
                                                           actionButton("do_reset", "Execute Hard Reset (Replaces Indicator Framework)", class="btn-danger", width="100%")
                                                         )
                                                     )
    ))
  })
  
  observeEvent(input$do_reset, {
    req(input$m_file)
    df <- as.data.frame(read_excel(input$m_file$datapath))
    
    # Validate against your ACTUAL spreadsheet headers (the ones you gave —
    # "#", "Pillar(s)", "Big Ticket", etc.), not the internal names.
    real_headers <- names(HEADER_MAP)
    missing <- setdiff(real_headers, names(df))
    if (length(missing) > 0) {
      showNotification(paste("Upload is missing columns:", paste(missing, collapse=", ")), type="error")
      return()
    }
    
    # Select in the expected order, then translate headers -> internal names.
    df <- df[, real_headers]
    names(df) <- unname(HEADER_MAP[names(df)])
    
    df$indicator_id <- as.character(df$indicator_id)
    df$target_2026 <- as.numeric(df$target_2026)
    df$midyear_2026_milestone <- as.numeric(df$midyear_2026_milestone)
    
    dbBegin(con)
    tryCatch({
      dbExecute(con, "DELETE FROM indicators")
      dbWriteTable(con, "indicators", df, append = TRUE)
      dbCommit(con)
      refresh_val(refresh_val() + 1)
      showNotification("Hard reset complete. Framework replaced; reports preserved.", type = "message")
    }, error = function(e) {
      dbRollback(con)
      showNotification(paste("Reset error:", e$message), type = "error")
    })
  })
  
  output$download_indicators_backup <- downloadHandler(
    filename = function() paste0("indicators_backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content  = function(file) write.xlsx(dbGetQuery(con, "SELECT * FROM indicators"), file, rowNames = FALSE)
  )
  output$download_reports_backup <- downloadHandler(
    filename = function() paste0("reports_backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx"),
    content  = function(file) write.xlsx(dbGetQuery(con, "SELECT * FROM reports"), file, rowNames = FALSE)
  )
}

shinyApp(ui, server)