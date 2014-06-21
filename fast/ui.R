shinyUI(
  navbarPage("Fast", id = "nav_fast", collapsable = TRUE, 
             
             tabPanel("Data", uiOutput('data_ui_and_tabs')),
             
             navbarMenu("Forecasting",
                        tabPanel("ARIMA", uiOutput("autoarima")))
             
             ))