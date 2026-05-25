library(shiny)
library(bslib)
library(plotly)
library(bsicons)
library(googlesheets4)
library(dplyr)

# --- CONFIGURATION ---
# Replace with your actual Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1TVNHPi4xf2jR00mFKd471m_Sy5OfnaVak3jmqsPqzfU/edit?usp=sharing"
gs4_deauth() 

# --- AFRICA CDC COLOR PALETTE ---
cdc_maroon <- "#8B2332"
cdc_teal   <- "#2E5A47"
cdc_gold   <- "#C69214"
cdc_grey   <- "#D3D3D3"

# --- GAUGE COMPONENT (TIGHTER HEIGHT) ---
create_cdc_gauge <- function(value, title, color, target) {
  plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = value,
    number = list(suffix = "%", font = list(color = "#333333", size = 28)),
    title = list(text = title, font = list(size = 15, color = "#333333")),
    gauge = list(
      axis = list(
        range = list(0, 100), 
        tickcolor = "#444444",
        tickvals = c(50, target),
        ticktext = c("50", target),
        tickmode = "array"
      ),
      bar = list(color = color),
      bgcolor = "white",
      borderwidth = 1,
      bordercolor = "#cccccc",
      threshold = list(
        line = list(color = "black", width = 3),
        thickness = 0.8,
        value = target
      ),
      steps = list(list(range = list(0, 100), color = "#f9f9f9"))
    )
  ) %>%
    layout(
      height = 160, # Reduced height to fit snugly in card
      margin = list(l = 35, r = 35, b = 10, t = 45),
      paper_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = FALSE)
}

# --- UI ---
ui <- page_sidebar(
  title = "Africa CDC Strategic Presentation",
  theme = bs_theme(version = 5, bootswatch = "lux", primary = cdc_maroon),
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("bu_filter", "Select Business Unit (BU):", choices = "Loading..."),
    actionButton("refresh", "Refresh Data", icon = icon("sync")),
    hr(),
    div(class = "text-center", "Safeguarding Africa's Health")
  ),
  
  navset_hidden(
    id = "slide_container",
    nav_panel_hidden(
      value = "slide1",
      
      # TOP ROW: VALUE BOXES
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Broad Activities", value = textOutput("val_broad"),
          showcase = bs_icon("folder2-open"), theme = "primary"
        ),
        value_box(
          title = "Specific Activities", value = textOutput("val_spec"),
          showcase = bs_icon("list-stars"),
          style = paste0("background-color: ", cdc_teal, " !important; color: white;")
        ),
        value_box(
          title = "Tier 3/4 Indicators", value = textOutput("val_ind"),
          showcase = bs_icon("graph-up-arrow"), 
          style = paste0("background-color: ", cdc_gold, " !important; color: white;")
        )
      ),
      
      div(style = sprintf("height: 40px; background-color: %s; margin: 25px 0; border-radius: 5px;", cdc_grey)),
      
      # BOTTOM ROW: GAUGES (REDUCED CARD HEIGHT)
      layout_column_wrap(
        width = 1/4,
        card(full_screen = FALSE, style = "height: 200px;", plotlyOutput("gauge_burn")),
        card(full_screen = FALSE, style = "height: 200px;", plotlyOutput("gauge_comp")),
        card(full_screen = FALSE, style = "height: 200px;", plotlyOutput("gauge_q_perf")),
        card(full_screen = FALSE, style = "height: 200px;", plotlyOutput("gauge_ytd"))
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  
  sheet_data <- reactive({
    input$refresh 
    read_sheet(sheet_url)
  })
  
  observe({
    req(sheet_data())
    units <- unique(sheet_data()$Bu)
    updateSelectInput(session, "bu_filter", choices = c("All", units), selected = "All")
  })
  
  filtered_metrics <- reactive({
    req(sheet_data())
    df <- sheet_data()
    
    if (input$bu_filter != "All") {
      df <- df %>% filter(Bu == input$bu_filter)
    }
    
    df %>% summarize(
      broad_sum = sum(broad, na.rm = TRUE),
      spec_sum = sum(specific, na.rm = TRUE),
      ind_sum = sum(indicator, na.rm = TRUE),
      burn_avg = mean(burnrate, na.rm = TRUE),
      comp_avg = mean(completion, na.rm = TRUE),
      q_avg = mean(performance_q1, na.rm = TRUE),
      y_avg = mean(performance_y, na.rm = TRUE)
    )
  })
  
  # Render Value Boxes
  output$val_broad <- renderText({ filtered_metrics()$broad_sum })
  output$val_spec  <- renderText({ filtered_metrics()$spec_sum })
  output$val_ind   <- renderText({ filtered_metrics()$ind_sum })
  
  # Render Gauges
  output$gauge_burn   <- renderPlotly({ create_cdc_gauge(filtered_metrics()$burn_avg, "Burn Rate", cdc_maroon, 25) })
  output$gauge_comp   <- renderPlotly({ create_cdc_gauge(filtered_metrics()$comp_avg, "Completion Rate", cdc_teal, 90) })
  output$gauge_q_perf <- renderPlotly({ create_cdc_gauge(filtered_metrics()$q_avg, "Q1 Performance", cdc_gold, 90) })
  output$gauge_ytd    <- renderPlotly({ create_cdc_gauge(filtered_metrics()$y_avg, "YTD Performance", cdc_teal, 25) })
}

shinyApp(ui, server)