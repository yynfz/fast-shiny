output$uiArima_var1 <- renderUI({
  isTimeSeries <- "ts" == getdata_class_ts() | "mts" == getdata_class_ts()
  vars <- varnames_ts()[isTimeSeries]
  if(length(vars) ==  0) return()
  selectInput(inputId = "arima_var1", label = "Dependent variable:", choices = vars,
              selected = vars[1], multiple = FALSE)
})


output$autoarima <- renderUI ({
  sidebarLayout(
    sidebarPanel(
      div(class = "busy",
          p("Calculation in progress ..."),
          img(src="ajaxloaderq.gif")
      ),
      wellPanel(
        HTML(paste("<label><strong>Menu:", "Forecasting","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
      ),
      wellPanel(
        uiOutput("uiArima_var1")
      ),
      conditionalPanel(
        condition = "input.arimatab == 2",
        wellPanel(
          strong("Specification"),br(),br(),
          conditionalPanel(
            condition = "input.subtab == 1 || input.subtab == 2 ",
            wellPanel(
              numericInput(inputId = "lagdat", label = "Lags to include", value = 20, min = 1))),
          wellPanel(
            checkboxInput(
              inputId = "dfdat",
              label = "Apply Differencing?",
              value = FALSE),
            conditionalPanel(
              condition = "input.dfdat == true",
              selectInput(
                inputId = "dfd",
                label = "",
                choices = list(
                  "First-Order" = 1,
                  "Second-Order" = 2),
                selected = 1))),
          conditionalPanel(
            condition = "input.subtab == 1 || input.subtab == 2 ",
            wellPanel(
              sliderInput(inputId = "confval", label = "Confidence Interval (%)", min = 0, max = 100, value = 95)))
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 3",
        
        wellPanel(
          selectInput(
            inputId = "methods",
            label = "Modelling Method:",
            choices = list(
              "Automatic Modelling" = 1,
              "Manual Modelling" =2 ),
            selected = 1)
        ),
        conditionalPanel(
          condition = "input.methods == 1",
          wellPanel(
            selectInput(
              inputId = "ic",
              label = "Information Criteria",
              choices = list(
                "aicc",
                "aic",
                "bic"),
              selected = "aicc"
            )
          ), br(), br(), br(), br()
        ),
        conditionalPanel(
          condition = "input.methods == 2",
          wellPanel(
            numericInput(
              inputId = "ar",
              label = "Autoregressive",
              min = 0,
              value = 0),
            numericInput(
              inputId = "df",
              label = "Differencing",
              min = 0,
              value = 0),
            numericInput(
              inputId = "ma",
              label = "Moving Average",
              min = 0,
              value = 0))
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 5 && input.forecasttab == 1",
        wellPanel(
          numericInput(
            inputId = "period",
            label = "Number Of Period",
            min = 0,
            value = 10))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "arimatab",
        tabPanel(
          title = "Identification",
          #           helpText("Identify the model of the simulated data using the correlograms,
          #                  the", strong("Autocorrelation Function"), "(ACF) and the", 
          #                    strong("Partial Autocorrelation Function"), "(PACF).",
          #                    br(),br()),
          tabsetPanel(
            id = "subtab",
            tabPanel(
              title = "Historical Plot",
              plotOutput(outputId = "arimaplot"),
              helpText(strong("Differenced Series Plot"),br(), "Apply differencing to see the differenced series plot."),
              plotOutput(
                outputId = "h.new")
            ),
            tabPanel(
              title = " ACF",
              plotOutput(
                outputId = "acf"),
              verbatimTextOutput(outputId = "acfval"),
              value = 1),
            tabPanel(
              title = "PACF",
              plotOutput(
                outputId = "pacf"),
              verbatimTextOutput(outputId = "pacfval"),
              value = 2)),
          value = 2),
        tabPanel(
          title = "Estimation",
          helpText("Estimate the identified model of the data.", br(), br()),
          verbatimTextOutput(
            outputId = "est"),
          value = 3),
        tabPanel(
          title = "Diagnostic",
          helpText("Diagnose the model by testing the residuals for randomness."),
          br(),br(),
          withTags(
            div(
              class = "fluid-row",
              div(
                selectInput(
                  inputId = "test",
                  label = strong("PORTMANTEAU TEST"),
                  choices = c(
                    "Box-Pierce" = "Box",
                    "Ljung-Box" = "Ljung"),
                  selected = "Ljung"),
                br(),br(),
                helpText(
                  strong("H0:"),
                  "The data are independently distributed (i.e. the correlations in 
                       the population from which the sample is taken are 0, so that any 
                       observed correlations in the data result from randomness of the 
                       sampling process.",br(),
                  strong("H1:"),
                  "The data are not independently distributed.",br(),br(),br(),
                  strong("COMPUTATION:"),br(),
                  "The computed statistics is:",
                  textOutput(outputId = "diagx2"),
                  "The computed p-value is:",
                  textOutput(outputId = "diagpv"),br(),br(),
                  strong("DECISION:"),textOutput(outputId = "diagdc"),br(),br(),
                  strong("CONCLUSION:"),textOutput(outputId = "diagcn")
                ), class = "span5"),
              div(
                class = "span7",
                plotOutput(outputId = "resplot", height = "580px")))),br(),br(),
          value = 4),
        tabPanel(
          title = "Forecast",
          tabsetPanel(
            id = "forecasttab",
            tabPanel(
              title = "Forecastplot",
              plotOutput(outputId = "forecastplot"),
              verbatimTextOutput(outputId = "forecastvalue"),
              value = 1),
            tabPanel(
              title = "Fitted plot",
              helpText("Obtain the predicted values of the model, and plot this with the
                 original data."),
              plotOutput(outputId = "fitplot"),
              value = 2)),
          value = 5
        ),
        tabPanel(
          title = "HandsonTable",
          htable(outputId = "tbl", clickId = "tblClick",colHeaders = "provided" ,rowNames = "enabled", minCols = 4, minRows = 200, width = 1000, height = 650) #bagian handsonTable
        )
      )
    )
  )
})

# conversi data ke ts object dan mengambil variable yang dibutuhkan
getdata_ts <- reactive({
  dat <- ts(getdata())
  return(dat[,as.character(input$arima_var1)])
})

######################################################
## Bagian perhitungan
######################################################

# handsonTable
chacedTbl <- NULL

output$tbl <- renderHtable({
  if (is.null(input$tbl)){
  dat <- getdata()
  dat <- as.data.frame(dat)
  tbl <- dat
  chacedTbl <<- tbl
  print(tbl)
  return(tbl)
  } else{
    chacedTbl <<- input$tbl
    print(input$tbl)
    values[[input$datasets]] <- input$tbl
    return(input$tbl)
  }
})

# plot data awal
output$arimaplot <- renderPlot({
  par(mfcol = c(1,1), mar = c(5,4,1,2))
  plot(getdata_ts())
})

# plot acf
output$acf <- renderPlot({
  p <- qacf(getdata_ts(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(getdata_ts(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qacf(d, max.lag = input$lagdat)
  }
  print(p)
})

# nilai acf
output$acfval <- renderPrint({
  p <- acf(getdata_ts(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(getdata_ts(), lag = 1, differences = as.numeric(input$dfd))
    p <- acf(d, lag.max = input$lagdat, conf.level = (input$confval/100), plot = FALSE)
  }
  print(p)
})

# plot pacf
output$pacf <- renderPlot({
  p <- qpacf(getdata_ts(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(getdata_ts(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qpacf(d, max.lag = input$lagdat)
  }
  print(p)
})

# nilai pacf
output$pacfval <- renderPrint({
  p <- pacf(getdata_ts(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(getdata_ts(), lag = 1, differences = as.numeric(input$dfd))
    p <- pacf(d, lag.max = input$lagdat, conf.level = (input$confval/100), plot = FALSE)
  }
  print(p)
})

# plot setelah dilakukan differencing
output$h.new <- renderPlot({
  if(input$dfdat){
    val1 <- diff(getdata_ts(), differences = as.numeric(input$dfd))
    par(mfcol = c(1,1), mar = c(5,4,1,2))
    p <- plot(val1)
    print(p)
  }
})

# estimasi menggunakan auto.arima
estimate.auto <- reactive({
  if(as.numeric(input$methods) == 1){
    k <- auto.arima(getdata_ts(), ic = input$ic, stepwise = FALSE)
  }
  if(as.numeric(input$methods) == 2){
    k <- Arima(getdata_ts(), order = c(input$ar, input$df, input$ma), include.drift = TRUE)
  }
  return(k)
})

# menampilkan hasil estimasi
output$est <- renderPrint({
  estimate.auto()
})

# hasil uji dalam bentuk statistik
output$diagx2 <- renderPrint({
  m <- Box.test(estimate.auto()$residuals, type = input$test)
  as.numeric(as.matrix(m$statistic))
})

# hasil uji dalam bentuk p-value
output$diagpv <- renderPrint({
  m <- Box.test(estimate.auto()$residuals, type = input$test)
  as.numeric(as.matrix(m$p.value))
})

# keputusan dari uji diagnostic (decision)
output$diagdc <- renderPrint({
  m <- Box.test(estimate.auto()$residuals, type = input$test)
  j <- as.numeric(as.matrix(m$p.value))
  if(j >= 0.05){
    print("Do not reject the null hypothesis, since the p-value is greater than 0.05")
  }
  if(j < 0.05){
    print("Reject the null hypothesis, since the p-value is less than 0.05")
  }
})

# kesimpulan dari uji diagnostic (conclusion)
output$diagcn <- renderPrint({
  m <- Box.test(estimate.auto()$residuals, type = input$test)
  j <- as.numeric(as.matrix(m$p.value))
  if(j >= 0.05){
    print("Therefore, we do not have enough evidence to reject the null hypothesis. And thus, the residuals of the model exhibits randomness")
  }
  if(j < 0.05){
    print("Therefore, the residuals of the model are not independently distributed.")
  }
})

# plot residual
output$resplot <- renderPlot({
  p <- tsdisplay(estimate.auto()$residuals)
  print(p)
})

# output nilai peramalan terhadap fitted value
output$fitplot <- renderPlot({
  fit <- estimate.auto()
  plot(fit$x,col="red")
  lines(fitted.Arima(fit),col="blue")
})

# output plot forecast
output$forecastplot <- renderPlot({
  forecast1 <- forecast.Arima(estimate.auto(), h=input$period)
  p <- plot.forecast(forecast1)
  print(p)
})

# output nilai forecast
output$forecastvalue <- renderPrint({
  forecast1 <- forecast.Arima(estimate.auto(), h=input$period)
  print(forecast1)
})

#######################################################
# fungsi untuk plot acf dan pacf menggunakan ggplot2

qacf <- function(x, conf.level = 0.95, max.lag = NULL,
                 min.lag = 0) {
  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
  bacf <- acf(x, plot = FALSE, lag.max = max.lag)
  bacfdf <- with(bacf, data.frame(lag, acf))
  if (min.lag > 0) {
    bacfdf <- bacfdf[-seq(1, min.lag), ]
  }
  significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, significant)
  q <- qplot(
    lag, acf, data = bacfdf, geom = "bar",
    stat = "identity", position = "identity",
    ylab = "Autocorrelation",
    fill = factor(significant))
  q <- q + geom_hline(
    yintercept = -ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = 0, color = "red",
    size = 0.3)
  q <- q + scale_fill_hue(
    name = paste("Significant at the", conf.level, "level"),
    breaks = 0:1,
    labels = c("Not Significant", "Significant")) +
    theme(panel.background = element_rect(
      size = 3, 
      colour = "black",
      fill = "white"),
      axis.ticks = element_line(
        size = 2),
      axis.title.x = element_text(
        size = rel(1.2), 
        face = "bold"),
      axis.title.y = element_text(
        size = rel(1.2), 
        face = "bold"),
      plot.title = element_text(
        size = 20,
        face = "bold", 
        vjust = 1.5),
      legend.position = "bottom",
      legend.title = element_text(
        size=rel(1.2), 
        face="bold"),
      legend.text = element_text(
        colour="blue", 
        size = 13))
  return(q)
}

qpacf <- function(x, conf.level = 0.95, max.lag = NULL,
                  min.lag = 0) {
  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
  bacf <- pacf(x, plot = FALSE, lag.max = max.lag)
  bacfdf <- with(bacf, data.frame(lag, acf))
  if (min.lag > 0) {
    bacfdf <- bacfdf[-seq(1, min.lag), ]
  }
  significant <- (abs(bacfdf[, 2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, significant)
  q <- qplot(
    lag, acf, data = bacfdf, geom = "bar",
    stat = "identity", position = "identity",
    ylab = "Autocorrelation",
    fill = factor(significant))
  q <- q + geom_hline(
    yintercept = -ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = ciline,
    color = "blue", size = 0.2, linetype="dashed")
  q <- q + geom_hline(
    yintercept = 0, color = "red",
    size = 0.3)
  q <- q + scale_fill_hue(
    name = paste("Significant at the", conf.level, "level"), 
    breaks = 0:1, labels = c("Not Significant", "Significant")) +
    theme(panel.background = element_rect(
      size = 3, 
      colour = "black",
      fill = "white"),
      axis.ticks = element_line(
        size = 2),
      axis.title.x = element_text(
        size = rel(1.2), 
        face = "bold"),
      axis.title.y = element_text(
        size = rel(1.2), 
        face = "bold"),
      plot.title = element_text(
        size = 20,
        face = "bold", 
        vjust = 1.5),
      legend.position = "bottom",
      legend.title = element_text(
        size=rel(1.2), 
        face="bold"),
      legend.text = element_text(
        colour="blue", 
        size = 13))
  return(q)
}