library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx) 

# --- 1. DATABASE SETUP ---
db_name <- "pirs_db_v1.sqlite"
con <- dbConnect(SQLite(), db_name)
cdc_colors <- list(green = "#006838", yellow = "#FDB913", bg = "#f8f9fa")

dbExecute(con, "CREATE TABLE IF NOT EXISTS pirs_db (
  division_name TEXT, priority_area TEXT, goal TEXT, objective TEXT, id TEXT PRIMARY KEY, 
  indicator_name TEXT, results_level TEXT, indicator_tier TEXT, indicator_type TEXT, 
  rationale TEXT, Calculation_type TEXT, definition TEXT, Disaggregation TEXT, 
  Numerator TEXT, Denominator TEXT, similar_indicators TEXT, 
  frequency TEXT, collection_method TEXT, last_updated TEXT
)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS audit_log (
  indicator_id TEXT, field_name TEXT, old_value TEXT, new_value TEXT, changed_by TEXT, change_time TEXT
)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS users (username TEXT, role TEXT, email TEXT PRIMARY KEY)")
dbExecute(con, "INSERT OR IGNORE INTO users VALUES ('Admin', 'Admin', 'admin@moh.gov')")

# --- 2. UI ---
ui <- page_navbar(
  title = "Africa CDC | Strategic PIRS",
  id = "main_nav",
  position = "fixed-top", 
  theme = bs_theme(bootswatch = "flatly", primary = cdc_colors$green, secondary = cdc_colors$yellow),
  
  header = tagList(
    tags$style(HTML("
      body { padding-top: 100px !important; background-color: #f8f9fa; }
      .card-pirs { border-left: 10px solid #006838; margin-bottom: 25px; font-size: 0.85rem; }
      .section-title { color: #006838; font-weight: bold; border-bottom: 1px solid #eee; margin-bottom: 15px; margin-top: 20px; }
      .card-label { font-weight: bold; color: #555; text-transform: uppercase; font-size: 0.7rem; display: block; margin-top: 5px; }
      textarea { resize: both !important; min-height: 80px; width: 100%; }
      .hist-link { font-size: 0.72rem; float: right; color: #006838; text-decoration: underline; cursor: pointer; font-weight: normal; }
    ")),
    tags$script(HTML("$(document).on('click', '.edit-btn', function() { Shiny.setInputValue('card_id_trigger', this.id, {priority: 'event'}); });"))
  ),
  
  nav_panel("Gallery Registry", 
            div(style = "max-width: 1500px; margin: auto; padding: 15px;",
                layout_column_wrap(width = 1/3,
                                   selectInput("f_tier", "Filter by Tier:", choices = c("All", "tier 1", "tier 2", "tier 3", "tier 4")),
                                   # NEW AREA FILTER ADDED HERE
                                   selectInput("f_area", "Filter by Priority Area:", 
                                               choices = c("All", "Priority 1", "Priority 2", "Priority 3", 
                                                           "Priority 4", "Priority 5", "Priority 6",
                                                           "Enabler A", "Enabler B", "Enabler C", 
                                                           "Enabler D", "Enabler E", "Enabler F", "Enabler G")),
                                   textInput("f_search", "Search Code/Name:")
                ),
                uiOutput("gallery_ui")
            )
  ),
  nav_panel("Editor", div(style = "max-width: 1000px; margin: auto; padding-bottom: 50px;", uiOutput("edit_form_ui"))),
  nav_spacer(),
  nav_item(actionButton("btn_admin_login", "Staff Management", class="btn-outline-secondary"))
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  editing_id <- reactiveVal(NULL)
  refresh_val <- reactiveVal(0)
  is_admin <- reactiveVal(FALSE)
  
  safe_diff <- function(a, b) {
    if (is.na(a)) a <- ""
    if (is.na(b)) b <- ""
    return(as.character(a) != as.character(b))
  }
  
  show_hist <- function(label, db_field) {
    logs <- dbGetQuery(con, "SELECT old_value, new_value, changed_by, change_time FROM audit_log WHERE indicator_id = ? AND field_name = ? ORDER BY change_time DESC", list(editing_id(), db_field))
    showModal(modalDialog(title = paste("History:", label), if(nrow(logs)==0) "No history recorded." else renderTable(logs), easyClose = TRUE, size = "l"))
  }
  
  obs_fields <- list(
    "h_div"="division_name", "h_area"="priority_area", "h_goal"="goal", "h_obj"="objective", 
    "h_name"="indicator_name", "h_def"="definition", "h_rat"="rationale", "h_calc"="Calculation_type", 
    "h_dis"="Disaggregation", "h_num"="Numerator", "h_den"="Denominator", "h_tier"="indicator_tier", 
    "h_level"="results_level", "h_freq"="frequency", "h_type"="indicator_type", "h_sim"="similar_indicators"
  )
  lapply(names(obs_fields), function(id) { observeEvent(input[[id]], { show_hist(obs_fields[[id]], obs_fields[[id]]) }) })
  
  # --- GALLERY UI (WITH UPDATED FILTER LOGIC) ---
  output$gallery_ui <- renderUI({
    refresh_val()
    data <- dbGetQuery(con, "SELECT * FROM pirs_db")
    if(nrow(data) == 0) return(h3("No data. Use Admin tab to Reset/Upload Excel."))
    
    # Filtering logic
    if(input$f_tier != "All") data <- data %>% filter(indicator_tier == input$f_tier)
    if(input$f_area != "All") data <- data %>% filter(priority_area == input$f_area)
    if(nzchar(input$f_search)) data <- data %>% filter(grepl(input$f_search, indicator_name, ignore.case=T) | grepl(input$f_search, id, ignore.case=T))
    
    lapply(1:nrow(data), function(i) {
      card(class = "card-pirs",
           card_header(div(style="display:flex; justify-content:space-between; align-items:center;", 
                           tags$b(data$id[i]), span(class="badge bg-warning text-dark", data$indicator_tier[i]))),
           div(style="padding:10px;",
               h5(style="color:#006838; font-weight:bold;", data$indicator_name[i]),
               layout_column_wrap(width = 1/4,
                                  div(span(class="card-label", "Division"), data$division_name[i]),
                                  div(span(class="card-label", "Area"), data$priority_area[i]),
                                  div(span(class="card-label", "Frequency"), data$frequency[i]),
                                  div(span(class="card-label", "Level"), data$results_level[i])
               ),
               hr(),
               layout_column_wrap(width = 1/2,
                                  div(span(class="card-label", "Goal"), data$goal[i]),
                                  div(span(class="card-label", "Objective"), data$objective[i])
               ),
               layout_column_wrap(width = 1/3,
                                  div(span(class="card-label", "Numerator"), data$Numerator[i]),
                                  div(span(class="card-label", "Denominator"), data$Denominator[i]),
                                  div(span(class="card-label", "Calculation"), data$Calculation_type[i])
               ),
               span(class="card-label", "Definition"), p(data$definition[i]),
               span(class="card-label", "Rationale"), p(data$rationale[i]),
               span(class="card-label", "Disaggregation"), p(data$Disaggregation[i]),
               layout_column_wrap(width = 1/2,
                                  div(span(class="card-label", "Type"), data$indicator_type[i]),
                                  div(span(class="card-label", "Similar Indicators"), data$similar_indicators[i])
               )
           ),
           card_footer(actionButton(data$id[i], "Edit Details", class="btn-sm btn-primary edit-btn"))
      )
    })
  })
  
  # --- EDITOR UI ---
  output$edit_form_ui <- renderUI({
    req(editing_id())
    curr <- dbGetQuery(con, "SELECT * FROM pirs_db WHERE id = ?", list(editing_id()))
    card(
      card_header(paste("Reference Sheet:", editing_id())),
      h6(class="section-title", "1. Strategic Alignment"),
      layout_column_wrap(width = 1/2, 
                         div(actionLink("h_div", "History", class="hist-link"), textAreaInput("u_div", "Division", curr$division_name)), 
                         div(actionLink("h_area", "History", class="hist-link"), 
                             selectInput("u_area", "Area", 
                                         choices = c("Priority 1", "Priority 2", "Priority 3", "Priority 4", "Priority 5", "Priority 6",
                                                     "Enabler A", "Enabler B", "Enabler C", "Enabler D", "Enabler E", "Enabler F", "Enabler G"), 
                                         selected = curr$priority_area))),
      layout_column_wrap(width = 1/2, 
                         div(actionLink("h_goal", "History", class="hist-link"), textAreaInput("u_goal", "Goal", curr$goal)), 
                         div(actionLink("h_obj", "History", class="hist-link"), textAreaInput("u_obj", "Objective", curr$objective))),
      h6(class="section-title", "2. Technical & Metrics"),
      div(actionLink("h_name", "History", class="hist-link"), textAreaInput("u_name", "Indicator Name", curr$indicator_name)),
      div(actionLink("h_def", "History", class="hist-link"), textAreaInput("u_def", "Definition", curr$definition)),
      div(actionLink("h_rat", "History", class="hist-link"), textAreaInput("u_rat", "Rationale", curr$rationale)),
      layout_column_wrap(width = 1/3,
                         div(actionLink("h_tier", "History", class="hist-link"), selectInput("u_tier", "Tier", choices = c("tier 1", "tier 2", "tier 3", "tier 4"), selected = curr$indicator_tier)),
                         div(actionLink("h_level", "History", class="hist-link"), selectInput("u_level", "Level", choices = c("Impact", "Outcome", "Output", "Process"), selected = curr$results_level)),
                         div(actionLink("h_freq", "History", class="hist-link"), selectInput("u_freq", "Frequency", choices = c("Annual", "Semi-Annual", "Quarterly", "Monthly"), selected = curr$frequency))),
      layout_column_wrap(width = 1/2, 
                         div(actionLink("h_num", "History", class="hist-link"), textAreaInput("u_num", "Numerator", curr$Numerator)), 
                         div(actionLink("h_den", "History", class="hist-link"), textAreaInput("u_den", "Denominator", curr$Denominator))),
      layout_column_wrap(width = 1/2,
                         div(actionLink("h_calc", "History", class="hist-link"), textAreaInput("u_calc", "Calculation Type", curr$Calculation_type)),
                         div(actionLink("h_dis", "History", class="hist-link"), textAreaInput("u_dis", "Disaggregation", curr$Disaggregation))),
      h6(class="section-title", "3. Metadata"),
      div(actionLink("h_sim", "History", class="hist-link"), textAreaInput("u_sim", "Similar Indicators", curr$similar_indicators)),
      div(actionLink("h_type", "History", class="hist-link"), selectInput("u_type", "Indicator Type", choices = c("Core", "Non-Core", "Custom"), selected = curr$indicator_type)),
      hr(),
      textInput("check_email", "Authorized Email:", value = "admin@moh.gov"),
      card_footer(div(style="float:right", actionButton("save_changes", "Save & Audit", class="btn-success")))
    )
  })
  
  # --- SAVE LOGIC ---
  observeEvent(input$save_changes, {
    user <- dbGetQuery(con, "SELECT username FROM users WHERE LOWER(email) = LOWER(?)", list(trimws(input$check_email)))
    if(nrow(user) == 0) { showNotification("Unauthorized", type = "error"); return() }
    old <- dbGetQuery(con, "SELECT * FROM pirs_db WHERE id = ?", list(editing_id()))
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    fields <- list(
      "division_name"="u_div", "priority_area"="u_area", "goal"="u_goal", "objective"="u_obj",
      "indicator_name"="u_name", "definition"="u_def", "rationale"="u_rat", "indicator_tier"="u_tier",
      "results_level"="u_level", "frequency"="u_freq", "Numerator"="u_num", "Denominator"="u_den",
      "Calculation_type"="u_calc", "Disaggregation"="u_dis", "similar_indicators"="u_sim", "indicator_type"="u_type"
    )
    dbBegin(con)
    tryCatch({
      for(f in names(fields)) {
        if(safe_diff(input[[fields[[f]]]], old[[f]])) {
          dbExecute(con, "INSERT INTO audit_log VALUES (?, ?, ?, ?, ?, ?)", list(editing_id(), f, old[[f]], input[[fields[[f]]]], user$username[1], now))
        }
      }
      dbExecute(con, "UPDATE pirs_db SET division_name=?, priority_area=?, goal=?, objective=?, indicator_name=?, definition=?, rationale=?, indicator_tier=?, results_level=?, frequency=?, Numerator=?, Denominator=?, Calculation_type=?, Disaggregation=?, similar_indicators=?, indicator_type=?, last_updated=? WHERE id=?",
                list(input$u_div, input$u_area, input$u_goal, input$u_obj, input$u_name, input$u_def, input$u_rat, input$u_tier, input$u_level, input$u_freq, input$u_num, input$u_den, input$u_calc, input$u_dis, input$u_sim, input$u_type, now, editing_id()))
      dbCommit(con); refresh_val(refresh_val() + 1); showNotification("Success"); nav_select("main_nav", "Gallery Registry")
    }, error = function(e) { dbRollback(con); showNotification(paste("Save Error:", e$message), type="error") })
  })
  
  # --- ADMIN LOGIC ---
  observeEvent(input$btn_admin_login, { 
    showModal(modalDialog(passwordInput("ap", "Enter Admin Code"), footer=actionButton("do_al", "Login"))) 
  })
  
  observeEvent(input$do_al, { 
    admin_secret <- Sys.getenv("PIRS_ADMIN_CODE")
    if (admin_secret == "") { showNotification("Error: PIRS_ADMIN_CODE env var is empty.", type = "error"); return() }
    if(input$ap == admin_secret) { is_admin(TRUE); removeModal(); showNotification("Admin Access Granted", type = "message") } 
    else { showNotification("Incorrect Code", type = "error") }
  })
  
  observe({ 
    if(is_admin()) nav_insert("main_nav", target="Editor", nav_panel("Admin", 
                                                                     card(card_header("Database Management"),
                                                                          layout_column_wrap(width = 1/2,
                                                                                             fileInput("m_file", "1. Upload Excel for Reset", accept = ".xlsx"),
                                                                                             downloadButton("download_backup", "2. Download Current Backup", class = "btn-info")),
                                                                          actionButton("do_reset", "Execute Hard Reset (Deletes Old Data)", class="btn-danger", width="100%")),
                                                                     card(card_header("Users"), textInput("nu_n", "Name"), textInput("nu_e", "Email"), actionButton("su", "Add User"), hr(), DTOutput("st")))) 
  })
  
  output$download_backup <- downloadHandler(
    filename = function() { paste0("Africa_CDC_PIRS_Backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx") },
    content = function(file) {
      export_data <- dbGetQuery(con, "SELECT * FROM pirs_db")
      cols <- c("division_name", "priority_area", "goal", "objective", "id", "indicator_name", "results_level", "indicator_tier", "indicator_type", "rationale", "Calculation_type", "definition", "Disaggregation", "Numerator", "Denominator", "similar_indicators")
      write.xlsx(export_data[, cols], file, rowNames = FALSE)
    }
  )
  
  observeEvent(input$do_reset, {
    req(input$m_file)
    df <- read_excel(input$m_file$datapath)
    df[] <- lapply(df, as.character)
    cols <- c("division_name", "priority_area", "goal", "objective", "id", "indicator_name", "results_level", "indicator_tier", "indicator_type", "rationale", "Calculation_type", "definition", "Disaggregation", "Numerator", "Denominator", "similar_indicators")
    dbBegin(con)
    tryCatch({
      dbExecute(con, "DELETE FROM pirs_db")
      dbWriteTable(con, "pirs_db", df[, cols], append = TRUE)
      dbCommit(con); refresh_val(refresh_val()+1); showNotification("Reset Done")
    }, error = function(e) { dbRollback(con); showNotification("Reset Error: Check Columns", type="error") })
  })
  
  output$st <- renderDT({ refresh_val(); datatable(dbGetQuery(con, "SELECT username, email FROM users")) })
  observeEvent(input$su, { dbExecute(con, "INSERT INTO users VALUES (?, 'User', ?)", list(input$nu_n, input$nu_e)); refresh_val(refresh_val()+1) })
  observeEvent(input$card_id_trigger, { editing_id(input$card_id_trigger); nav_select("main_nav", "Editor") })
}

shinyApp(ui, server)