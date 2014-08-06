shinyUI(
  navbarPage("Fast", id = "nav_fast", collapsable = TRUE, 
             
             tabPanel("Data", uiOutput('data_ui_and_tabs')),
             
             navbarMenu("Forecasting",
                        tabPanel("ARIMA", uiOutput("autoarima"))),
             
             navbarMenu("Regression",
                        tabPanel("Tobit Regression", uiOutput("tobit_reg"))),
             
             navbarMenu("R",
                        tabPanel("Report", uiOutput("report")),
                        tabPanel("code", uiOutput("rcode")))
             
             ))