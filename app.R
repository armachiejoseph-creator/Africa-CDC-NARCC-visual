library(shiny)
library(shinydashboard)
library(deSolve)
library(ggplot2)
library(tidyr)
library(dplyr)
library(markdown)
# just a note

# 1. Model Definition
model_func <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + E + I + H + Q + R
    lambda <- (beta * I / N) + (epsilon * beta * Q / N)
    
    dS <- -lambda * S
    dE <- lambda * S - (alpha + omega) * E
    dI <- omega * E - (p + gamma_I) * I
    dH <- p * I - gamma_H * H
    dQ <- alpha * E + gamma_H * H + gamma_I * I - gamma_Q * Q
    dR <- gamma_Q * Q + gamma_H * H + gamma_I * I
    
    return(list(c(dS, dE, dI, dH, dQ, dR)))
  })
}
# 2. UI Layout
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Ebola Epidemic Policy Lab", 
    titleWidth = 350 # Adjust this pixel value until it fits your text perfectly
  ),
  
  # CSS to pin header/sidebar and enable internal scrolling for content
  tags$head(
    tags$style(HTML("
      /* Pin the header and sidebar */
      .main-header { position: fixed; width: 100%; top: 0; }
      .main-sidebar { position: fixed; height: 100%; top: 50px; }
      
      /* Make content area scroll independently */
      .content-wrapper {
        margin-top: 50px; 
        margin-left: 230px; 
        height: calc(100vh - 50px);
        overflow-y: auto;
      }
    "))
  ),
  
  # Explicitly assign to the 'sidebar' argument to fix tagAssert error
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Simulator", tabName = "sim", icon = icon("dashboard")),
      menuItem("Guide & Methodology", tabName = "guide", icon = icon("book"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "sim",
              fluidRow(
                column(width = 3, 
                       sliderInput("beta", "β Transmission Rate:", 0, 1, 0.5),
                       sliderInput("epsilon", "ε Quarantine Leakage:", 0, 0.2, 0.05),
                       sliderInput("q", "q Quarantine Rate (α):", 0, 1, 0.2),
                       sliderInput("p", "p Detection/Hosp Rate:", 0, 0.5, 0.1),
                       sliderInput("delta", "δ Leakage to Susceptible:", 0, 0.1, 0.01),
                       sliderInput("recovery", "Days to Recover:", 5, 50, 20)),
                column(width = 9,
                       fluidRow(valueBoxOutput("peakI", width = 4), 
                                valueBoxOutput("peakQ", width = 4), 
                                valueBoxOutput("peakH", width = 4)),
                       box(title = "Clinical and Quarantine Dynamics", width = 12, plotOutput("epiPlot")))
              )
      ),
      tabItem(tabName = "guide", uiOutput("guide_content"))
    )
  )
)
# 3. Server Logic
server <- function(input, output) {
  sim_data <- reactive({
    rate_gamma <- 1 / input$recovery
    init <- c(S = 999999, E = 1, I = 0, H = 0, Q = 0, R = 0)
    params <- c(beta = input$beta, epsilon = input$epsilon, 
                alpha = input$q, omega = (1 - input$q), 
                p = input$p, gamma_I = rate_gamma, 
                gamma_H = rate_gamma, gamma_Q = rate_gamma, delta = input$delta)
    out <- ode(y = init, times = 0:700, func = model_func, parms = params)
    as.data.frame(out)
  })
  
  output$peakI <- renderValueBox({
    df <- sim_data()
    valueBox(tags$span(style = "font-size: 40%;", paste0("Peak: ", round(max(df$I)), " | Total: ", round(max(df$I + df$R)))), 
             "Infectious (Community)", icon = icon("users"), color = "red")
  })
  
  output$peakQ <- renderValueBox({
    df <- sim_data()
    valueBox(tags$span(style = "font-size: 40%;", paste0("Peak: ", round(max(df$Q)), " | Total: ", round(max(df$Q + df$R)))), 
             "Quarantined", icon = icon("hospital-user"), color = "yellow")
  })
  
  output$peakH <- renderValueBox({
    df <- sim_data()
    valueBox(tags$span(style = "font-size: 40%;", paste0("Peak: ", round(max(df$H)), " | Total: ", round(max(df$H + df$R)))), 
             "Hospitalized", icon = icon("bed"), color = "blue")
  })
  
  output$epiPlot <- renderPlot({
    # Use fully qualified calls and ensure the data object is explicitly handled
    plot_df <- tidyr::pivot_longer(
      data = sim_data(), 
      cols = -time, 
      names_to = "compartment", 
      values_to = "count"
    )
    
    # Perform filtering and plotting using the explicit data frame
    dplyr::filter(plot_df, compartment %in% c("I", "Q", "H")) %>% 
      ggplot2::ggplot(aes(x = time, y = count, color = compartment)) +
      ggplot2::geom_line(size = 1.2) + 
      ggplot2::theme_minimal()
  })
  
  output$guide_content <- renderUI({
    withMathJax(includeMarkdown("guide.Rmd"))
  })
}

shinyApp(ui, server)