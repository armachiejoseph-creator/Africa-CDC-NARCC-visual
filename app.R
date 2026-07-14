library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(readxl)
library(openxlsx)

# ── 1. DATABASE SETUP (Supabase / Postgres) ───────────────────────────────────
# Credentials come from environment variables — NEVER hardcode them here.
# Set these on the VM (e.g. in a startup script, Renviron.site, or whatever
# Shiny Server's env config uses), the same way RESULTS_ADMIN_CODE is set:
#   SUPABASE_HOST, SUPABASE_PORT, SUPABASE_DB, SUPABASE_USER, SUPABASE_PASSWORD
get_db_conn <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = "aws-1-eu-central-1.pooler.supabase.com",
    port     = 5432,
    dbname   = "postgres",
    user     = "postgres.mvxbhugcfpxwmyblkgqc",
    password = Sys.getenv("SUPABASE_DB_PASSWORD")
  )
}

AU <- list(green = "#0E4B43", gold = "#b38e00", red = "#952038",
           blue = "#025891", muted = "#6b7280")

# ── One-time bootstrap: runs once when the app process starts, using a
# short-lived connection that's opened and closed immediately — NOT the
# connection every user session will use (that's opened per-session below,
# inside server()). This mirrors your own snippet's pattern exactly.
{
  boot_con <- get_db_conn()
  
  # One-time migration: if a database from before this column rename exists,
  # drop just the indicators table and let it get recreated below with the
  # current schema. Reports are untouched — same "framework can be reset
  # without losing reports" behavior the admin hard-reset already relies on.
  old_schema <- tryCatch({
    info <- dbGetQuery(boot_con, "SELECT column_name FROM information_schema.columns WHERE table_name = 'indicators'")
    "ahss_pillar_no" %in% info$column_name || "indicator_code" %in% info$column_name
  }, error = function(e) FALSE)
  if (old_schema) dbExecute(boot_con, "DROP TABLE indicators")
  
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
  dbExecute(boot_con, "CREATE TABLE IF NOT EXISTS indicators (
    indicator_id TEXT PRIMARY KEY,
    pillar TEXT, big_ticket TEXT, result TEXT, indicator_and_code TEXT, tier TEXT,
    target_2026 REAL, midyear_2026_milestone REAL, delivery_model TEXT,
    success_factors TEXT, primary_bu TEXT, contributing_bus TEXT
  )")
  
  dbExecute(boot_con, "CREATE TABLE IF NOT EXISTS reports (
    report_id SERIAL PRIMARY KEY,
    contribution_id TEXT, indicator_id TEXT, indicator_code TEXT, bu_name TEXT,
    status TEXT, target REAL, achieved REAL, performance_pct REAL,
    progress TEXT, challenges TEXT, next_steps TEXT, na_reason TEXT, timestamp TEXT
  )")
  # Postgres supports IF NOT EXISTS directly on ADD COLUMN — no need for the
  # try()/silent trick SQLite required. Harmless no-op if already present.
  dbExecute(boot_con, "ALTER TABLE reports ADD COLUMN IF NOT EXISTS status TEXT")
  dbExecute(boot_con, "ALTER TABLE reports ADD COLUMN IF NOT EXISTS na_reason TEXT")
  
  # Seed with sample rows the first time the DB is created, so the app is
  # testable before an admin ever uploads anything.
  if (dbGetQuery(boot_con, "SELECT COUNT(*) n FROM indicators")$n == 0) {
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
    for (i in seq_len(nrow(seed))) {
      r <- seed[i, ]
      dbExecute(boot_con, "INSERT INTO indicators
        (indicator_id, pillar, big_ticket, result, indicator_and_code, tier,
         target_2026, midyear_2026_milestone, delivery_model, success_factors, primary_bu, contributing_bus)
        VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)",
                list(r$indicator_id, r$pillar, r$big_ticket, r$result, r$indicator_and_code, r$tier,
                     r$target_2026, r$midyear_2026_milestone, r$delivery_model, r$success_factors,
                     r$primary_bu, r$contributing_bus))
    }
  }
  
  dbDisconnect(boot_con)
}

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
    tags$span("Results Framework — Progress Reporting", style = "vertical-align:middle;")
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

      .report-table-wrap { overflow-x: auto; border-radius: 10px; box-shadow: 0 1px 8px rgba(0,0,0,.05); }
      .report-table { width: 100%; border-collapse: collapse; background: white; min-width: 900px; }
      .report-table thead th {
        background: #0E4B43; color: white; font-size: 10.5px; font-weight: 700;
        text-transform: uppercase; letter-spacing: .04em; text-align: left; padding: 11px 14px;
        white-space: nowrap;
      }
      .report-table tbody td { padding: 10px 14px; font-size: 12.5px; color: #1f2937;
        border-bottom: 1px solid #f1f2f4; vertical-align: top; }
      .report-table td.big-ticket-cell, .report-table td.indicator-cell {
        max-width: 220px; vertical-align: middle; font-weight: 500; border-right: 1px solid #f1f2f4;
      }
      .report-table tbody tr:last-child td { border-bottom: none; }
      .report-table td.units-not-reported { color: #6b7280; font-size: 11.5px; max-width: 240px; }
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
                uiOutput("dashboard_filters"),
                uiOutput("indicator_list")
            )
  ),
  nav_panel("Progress Reports",
            div(style="max-width:1150px; margin:0 auto; padding:15px;",
                div(class="page-sub", "One row per submitted report. Indicators with outstanding units show who hasn't reported yet."),
                uiOutput("report_filters"),
                uiOutput("report_viewer_list")
            )
  ),
  nav_spacer(),
  nav_item(actionButton("btn_admin_login", "Admin", class="btn-outline-light"))
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # One connection per user session (not shared globally) — the right pattern
  # for a pooled remote database. Every reactive/observer below just refers
  # to "con" as before; only where it comes from has changed.
  con <- get_db_conn()
  onSessionEnded(function() {
    if (dbIsValid(con)) dbDisconnect(con)
  })
  
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
  
  # ── Progress Reports: filters (choices rebuild whenever the data changes,
  # current selection preserved via isolate() so regenerating the dropdown
  # doesn't reset what the person had picked) ─────────────────────────────────
  output$report_filters <- renderUI({
    ind <- indicators_data()
    bus <- contributions_data()
    
    pillar_choices <- c("All", if (nrow(ind) > 0) sort(unique(ind$pillar)) else character(0))
    ind_choices <- c("All" = "All")
    if (nrow(ind) > 0) ind_choices <- c(ind_choices, setNames(ind$indicator_id, ind$indicator_and_code))
    bu_choices <- c("All", if (nrow(bus) > 0) sort(unique(bus$bu_name)) else character(0))
    
    keep_selected <- function(current, choices) if (!is.null(current) && current %in% unname(choices)) current else "All"
    sel_pillar <- keep_selected(isolate(input$rv_pillar), pillar_choices)
    sel_ind    <- keep_selected(isolate(input$rv_indicator), ind_choices)
    sel_bu     <- keep_selected(isolate(input$rv_bu), bu_choices)
    sel_status <- keep_selected(isolate(input$rv_status), c("All","Reported","Not Applicable","No report"))
    
    layout_column_wrap(width = 1/4,
                       selectInput("rv_pillar", "Pillar:", choices = pillar_choices, selected = sel_pillar),
                       selectInput("rv_indicator", "Indicator:", choices = ind_choices, selected = sel_ind),
                       selectInput("rv_bu", "Business Unit:", choices = bu_choices, selected = sel_bu),
                       selectInput("rv_status", "Status:", choices = c("All","Reported","Not Applicable","No report"), selected = sel_status)
    )
  })
  
  # One row per submitted report. Indicators where NO contributing BU has
  # reported yet get a single placeholder row instead (unit = "—") so the
  # indicator itself doesn't just disappear from the table.
  report_table_data <- reactive({
    ind   <- indicators_data()
    bus   <- contributions_data()
    latest <- latest_reports()
    if (nrow(ind) == 0) return(NULL)
    
    reported_ids <- if (nrow(latest) > 0) latest$contribution_id else character(0)
    
    rows <- lapply(seq_len(nrow(ind)), function(i) {
      this_ind <- ind[i, ]
      ind_bus  <- bus %>% filter(indicator_id == this_ind$indicator_id)
      if (nrow(ind_bus) == 0) return(NULL)
      
      reported_bus     <- ind_bus %>% filter(contribution_id %in% reported_ids)
      not_reported_bus <- ind_bus %>% filter(!(contribution_id %in% reported_ids))
      not_reported_txt <- if (nrow(not_reported_bus) == 0) "None — all reported" else paste(not_reported_bus$bu_name, collapse = ", ")
      
      if (nrow(reported_bus) == 0) {
        return(data.frame(
          indicator_id = this_ind$indicator_id, pillar = this_ind$pillar, big_ticket = this_ind$big_ticket,
          indicator = this_ind$indicator_and_code, unit = "—",
          target = NA_real_, achieved = NA_real_, performance = NA_real_,
          status = "No report", not_reported = not_reported_txt, stringsAsFactors = FALSE
        ))
      }
      
      bind_rows(lapply(seq_len(nrow(reported_bus)), function(j) {
        cid <- reported_bus$contribution_id[j]
        r   <- latest %>% filter(contribution_id == cid)
        is_na_status <- !is.na(r$status[1]) && r$status[1] == "Not Applicable"
        data.frame(
          indicator_id = this_ind$indicator_id, pillar = this_ind$pillar, big_ticket = this_ind$big_ticket,
          indicator = this_ind$indicator_and_code, unit = reported_bus$bu_name[j],
          target      = if (is_na_status) NA_real_ else r$target[1],
          achieved    = if (is_na_status) NA_real_ else r$achieved[1],
          performance = if (is_na_status) NA_real_ else r$performance_pct[1],
          status      = if (is_na_status) "Not Applicable" else "Reported",
          not_reported = not_reported_txt, stringsAsFactors = FALSE
        )
      }))
    })
    bind_rows(rows)
  })
  
  output$report_viewer_list <- renderUI({
    df <- report_table_data()
    if (is.null(df) || nrow(df) == 0) return(h5(style="color:#9ca3af;", "No indicators loaded. Use Admin > Upload to add the framework."))
    
    if (!is.null(input$rv_pillar) && input$rv_pillar != "All") df <- df %>% filter(pillar == input$rv_pillar)
    if (!is.null(input$rv_indicator) && input$rv_indicator != "All") df <- df %>% filter(indicator_id == input$rv_indicator)
    if (!is.null(input$rv_bu) && input$rv_bu != "All") df <- df %>% filter(unit == input$rv_bu)
    if (!is.null(input$rv_status) && input$rv_status != "All") df <- df %>% filter(status == input$rv_status)
    if (nrow(df) == 0) return(h5(style="color:#9ca3af;", "No rows match the current filters."))
    
    # Rows for the same indicator are always contiguous (built one indicator
    # at a time, and filtering only removes rows, never reorders) — so a
    # run-length encode on indicator_id is enough to know each group's size.
    rl <- rle(df$indicator_id)
    is_group_start <- unlist(mapply(function(n) c(TRUE, rep(FALSE, n - 1)), rl$lengths, SIMPLIFY = FALSE))
    group_span     <- unlist(mapply(function(n) c(n, rep(NA_integer_, n - 1)), rl$lengths, SIMPLIFY = FALSE))
    group_index    <- cumsum(is_group_start)
    
    tags$div(class="report-table-wrap",
             tags$table(class="report-table",
                        tags$thead(tags$tr(
                          tags$th("Big Ticket"), tags$th("Indicator"), tags$th("Reporting Unit"),
                          tags$th("Target / Achieved"), tags$th("Performance"), tags$th("Status"),
                          tags$th("Units Not Reported")
                        )),
                        tags$tbody(
                          lapply(seq_len(nrow(df)), function(i) {
                            r  <- df[i, ]
                            st <- if (r$status == "Reported") status_for(r$achieved, r$target)
                            else if (r$status == "Not Applicable") NA_STATUS
                            else list(label = "No report", color = AU$muted, bg = "#f1f2f4")
                            ta_txt   <- if (is.na(r$target) || is.na(r$achieved)) "—" else paste0(r$achieved, " / ", r$target)
                            perf_txt <- if (is.na(r$performance)) "—" else paste0(r$performance, "%")
                            row_bg   <- if (group_index[i] %% 2 == 0) "background:#fafbfb;" else ""
                            
                            tags$tr(style = row_bg,
                                    if (is_group_start[i]) tags$td(class="big-ticket-cell", rowspan = group_span[i], r$big_ticket),
                                    if (is_group_start[i]) tags$td(class="indicator-cell", rowspan = group_span[i], r$indicator),
                                    tags$td(r$unit),
                                    tags$td(ta_txt),
                                    tags$td(perf_txt),
                                    tags$td(tags$span(class="status-pill", style=paste0("color:",st$color,";background:",st$bg,";"), st$label)),
                                    tags$td(class="units-not-reported", r$not_reported)
                            )
                          })
                        )
             )
    )
  })
  
  # ── Dashboard filters — Pillar and Tier choices both rebuilt from whatever's
  # actually in the data (same pattern as the Progress Reports filters), so a
  # new pillar name or tier value in an uploaded file just shows up automatically.
  output$dashboard_filters <- renderUI({
    ind <- indicators_data()
    pillar_choices <- c("All", if (nrow(ind) > 0) sort(unique(ind$pillar)) else character(0))
    tier_choices   <- c("All", if (nrow(ind) > 0) sort(unique(ind$tier)) else character(0))
    
    keep_selected <- function(current, choices) if (!is.null(current) && current %in% choices) current else "All"
    sel_pillar <- keep_selected(isolate(input$f_pillar), pillar_choices)
    sel_tier   <- keep_selected(isolate(input$f_tier), tier_choices)
    cur_search <- isolate(input$f_search); if (is.null(cur_search)) cur_search <- ""
    
    layout_column_wrap(width = 1/3,
                       selectInput("f_pillar", "Filter by Pillar:", choices = pillar_choices, selected = sel_pillar),
                       selectInput("f_tier", "Filter by Tier:", choices = tier_choices, selected = sel_tier),
                       textInput("f_search", "Search indicator code, name, or result:", value = cur_search)
    )
  })
  
  # ── Dashboard: indicator cards + contributing-BU report rows ───────────────
  output$indicator_list <- renderUI({
    data <- indicators_data()
    if (nrow(data) == 0) return(h5("No indicators loaded. Use Admin > Upload to add the framework."))
    
    if (!is.null(input$f_pillar) && input$f_pillar != "All") data <- data %>% filter(pillar == input$f_pillar)
    if (!is.null(input$f_tier) && input$f_tier != "All") data <- data %>% filter(tier == input$f_tier)
    if (!is.null(input$f_search) && nzchar(input$f_search)) {
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
         performance_pct, progress, challenges, next_steps, na_reason, timestamp)
         VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)",
                list(cid, ind$indicator_id[1], ind$indicator_and_code[1], b$bu_name[1], "Not Applicable",
                     NA_real_, NA_real_, NA_real_, NA_character_, NA_character_, NA_character_,
                     input[[paste0("na_reason_input_", cid)]], format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    } else {
      tgt <- input[[paste0("target_input_", cid)]]; ach <- input[[paste0("achieved_input_", cid)]]
      pct <- if (is.na(tgt) || tgt == 0) 0 else round(ach / tgt * 100, 1)
      dbExecute(con, "INSERT INTO reports
        (contribution_id, indicator_id, indicator_code, bu_name, status, target, achieved, performance_pct,
         progress, challenges, next_steps, na_reason, timestamp)
         VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)",
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
    
    df <- tryCatch(as.data.frame(read_excel(input$m_file$datapath)), error = function(e) {
      showNotification(paste("Could not read the Excel file:", e$message), type = "error")
      NULL
    })
    req(df)
    
    # Validate against your ACTUAL spreadsheet headers (the ones you gave —
    # "#", "Pillar(s)", "Big Ticket", etc.), not the internal names.
    real_headers <- names(HEADER_MAP)
    missing <- setdiff(real_headers, names(df))
    if (length(missing) > 0) {
      showNotification(paste("Upload is missing columns:", paste(missing, collapse=", ")), type="error")
      return()
    }
    
    # Select in the expected order, then translate headers -> internal names.
    # Wrapped in tryCatch: this previously ran unprotected, so a subtle header
    # mismatch (duplicate column, hidden character) would crash silently with
    # Shiny's generic error screen instead of telling you anything useful.
    df <- tryCatch(df[, real_headers], error = function(e) {
      showNotification(paste("Column selection failed:", e$message), type = "error")
      NULL
    })
    req(df)
    names(df) <- unname(HEADER_MAP[names(df)])
    
    df$indicator_id <- as.character(df$indicator_id)
    df$target_2026 <- as.numeric(df$target_2026)
    df$midyear_2026_milestone <- as.numeric(df$midyear_2026_milestone)
    
    if (nrow(df) == 0) {
      showNotification("The file was read successfully but contains 0 data rows.", type = "warning")
      return()
    }
    showNotification(paste0("Parsed ", nrow(df), " row(s) from the file. Writing to database..."), type = "message", duration = 3)
    
    dbBegin(con)
    tryCatch({
      dbExecute(con, "DELETE FROM indicators")
      for (i in seq_len(nrow(df))) {
        r <- df[i, ]
        dbExecute(con, "INSERT INTO indicators
          (indicator_id, pillar, big_ticket, result, indicator_and_code, tier,
           target_2026, midyear_2026_milestone, delivery_model, success_factors, primary_bu, contributing_bus)
          VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)",
                  list(r$indicator_id, r$pillar, r$big_ticket, r$result, r$indicator_and_code, r$tier,
                       r$target_2026, r$midyear_2026_milestone, r$delivery_model, r$success_factors,
                       r$primary_bu, r$contributing_bus))
      }
      dbCommit(con)
      refresh_val(refresh_val() + 1)
      # Read back what's actually in the table right now, in this same
      # connection/transaction context — the definitive check, not an assumption.
      actual_count <- dbGetQuery(con, "SELECT COUNT(*) n FROM indicators")$n
      showNotification(paste0("Hard reset complete. ", actual_count, " indicator(s) now in the database."), type = "message")
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