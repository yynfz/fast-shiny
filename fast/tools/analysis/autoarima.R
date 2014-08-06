output$uiArima_var1 <- renderUI({
  isTimeSeries <- "ts" == getdata_class_ts() | "mts" == getdata_class_ts()
  vars <- varnames_ts()[isTimeSeries]
  if(length(vars) ==  0) return()
  selectInput(inputId = "arima_var1", label = "Dependent variable:", choices = vars,
              selected = vars[1], multiple = FALSE)
})

observe({
  if(!identical(input$nav_fast, "ARIMA")){
    print(input$nav_fast)
    updateTabsetPanel(session, "arimatab", selected = "datatab")
  }
})

output$uimenu <- renderUI({
  wellPanel(
    HTML(paste("<label><strong>Menu:", "Forecasting","</strong></label>")),
    HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
    HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
    #textOutput(outputId = "namadata")
    # actionButton('commit', 'Commit Change')
  )
})

output$autoarima <- renderUI ({
  sidebarLayout(
    sidebarPanel(
      div(class = "busy",
          p("Calculation in progress ..."),
          img(src="ajaxloaderq.gif")
      ),
      uiOutput("uimenu"),
      wellPanel(
        uiOutput("uiArima_var1")
      ),
      conditionalPanel(
        condition = "input.arimatab == 'datatab'",
        wellPanel(
          strong("Data attributes"),
          selectInput(
            inputId = "frequency",
            label = "Time period",
            choices = list(
              "Monthly" = 12,
              "Quartly" = 4,
              "Annual" = 1
            ),
            selected = 12),
          conditon = "input.frequency == 12",
          numericInput(inputId = "date",label = "Start Years", value = 1990, min = 1900)
        ),
        wellPanel(
          selectInput(
            inputId = "forecastperiod",
            label = "Forecast Period",
            choices = list(
              "In sample forecast" = 1,
              "Out of sample forecast" = 2
            ),
            selected = 2
          ),
          conditionalPanel(
            condition = "input.forecastperiod == 1",
            numericInput(inputId = "endYear", label = "Training-set End Year", value = 2012),
            numericInput(inputId = "endMonth", label = "Training-set End Month ", value = 6)
          )
        )
      ),
      conditionalPanel(
        condition = "input.arimatab == 2",
        wellPanel(
          strong("Specification"),br(),br(),
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
          wellPanel(
            sliderInput(inputId = "confval", label = "Confidence Interval (%)", min = 0, max = 100, value = 95)),
          conditionalPanel(
            condition = "input.subtab == 1 || input.subtab == 2 ",
            wellPanel(
              numericInput(inputId = "lagdat", label = "Lags to include", value = 20, min = 1))),
          conditionalPanel(
            condition = "input.uniroottest == 'adf'  &&  (input.subtab !=1 && input.subtab != 2 )",
            wellPanel(
              numericInput(inputId = "lagdf", label = "Lagged differences", value = 10, min = 1)))
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
            ),
            selectInput(
              inputId = "selRoot",
              label = "Unit root test",
              choices = list(
                "KPSS" = "kpss",
                "ADF" = "adf",
                "PP" = "pp"),
              selected = "kpss"
            ),
            selectInput(
              inputId = "seasonalTest",
              label = "Seasonal Test",
              choices = list(
                "ocsb",
                "ch"),
              selected = "ocsb"
            ),
            checkboxInput(
              inputId = "seasonal",
              label = "Seasonal Model",
              value = TRUE),
            checkboxInput(
              inputId = "stepwise",
              label = "Use Stepwise Selection",
              value = TRUE),
            checkboxInput(
              inputId = "trace",
              label = "Show considered ARIMA models",
              value = FALSE),
            checkboxInput(
              inputId = "allowDrift",
              label = "Allow drift in models",
              value = TRUE)
          )
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
      ),
      helpAndReport('Regression','regression', inclMD("tools/help/regression.md"))
    ),
    mainPanel(
      tabsetPanel(
        id = "arimatab",
        tabPanel(
          title = "Data",
          conditionalPanel(
            condition = "input.forecastperiod == 2",
            verbatimTextOutput("tampil_ts")
          ),
          conditionalPanel(
            condition = "input.forecastperiod == 1",
            helpText(strong("Training Set:")),
            verbatimTextOutput("trainingSet"),
            helpText(strong("Test Set:")),
            verbatimTextOutput("testSet")
          ),
          value = "datatab"
        ),
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
              withTags(
                div(
                  class = "fluid-row",
                  div(
                    class = "span5",
                    selectInput(
                      inputId = "uniroottest",
                      label = strong("Unit Root Test"),
                      choices = c(
                        "ADF" = "adf",
                        "KPSS" = "kpss",
                        "PP" = "pp"
                      ),
                      selected = "kpss"),
                    verbatimTextOutput("textUnitRoot"),
                    br(),br(),
                    helpText(
                      strong("H0:"),
                      textOutput(outputId = "rooth0"),br(),
                      strong("H1:"),
                      textOutput(outputId = "rooth1"),br(),br(),
                      strong("COMPUTATION:"),br(),
                      "The computed p-value is:",
                      textOutput(outputId = "rootpv"),br(),br(),
                      strong("DECISION"), textOutput(outputId = "rootdc"),br(),br(),
                      strong("CONCLUSION"), textOutput(outputId = "rootcn")
                    )
                  ),
                  div(
                    class = "span7",
                    bsCollapse(multiple = TRUE, open = c("plot1", "plot2"), id = "collapse1",
                               bsCollapsePanel("Series Plot", plotOutput(outputId = "arimaplot", height = "290px"),
                                               id = "plot1", value = "test1"),
                               bsCollapsePanel("Differenced Series Plot", helpText("Apply differencing to see the differenced series plot."),
                                               plotOutput(
                                                 outputId = "h.new", height = "290px")
                                               , id = "plot2", value = "test2")
                    ))))
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
          actionButton("addModel", "Add model for comparison"),
          htmlOutput(outputId = "modeltable"),
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
        )
      )
    )
  )
})

# conversi data ke ts object dan mengambil variable yang dibutuhkan
getdata_ts <- reactive({
  if(is.null(input$frequency) || is.null(input$date) || is.null(input$arima_var1)) return()
  else{
    dat <- ts(getdata(), frequency = nilaiTabel[["frequency"]], start=c(nilaiTabel[["start"]],1))
    return(dat[,as.character(input$arima_var1)])
  }
})

get_data <- reactive({
  if(is.null(input$frequency) || is.null(input$date) || is.null(input$arima_var1)) return()
  else{
    return (getdata()[,as.character(input$arima_var1)])
  }
})

######################################################
## Bagian perhitungan
######################################################

# handsonTable
#  cachedTbl <- NULL
# 
# output$tbl <- renderHtable({
#   if (is.null(input$tbl)){
#     dat <- getdata()
#     dat <- as.data.frame(dat)
#     tbl <- dat
#     chacedTbl <<- tbl
# #     print("masih kosong")
# #     print(tbl)
#     return(tbl)
#   }
#   else {
#     chacedTbl <<- input$tbl
# #     print("isi")
# #     print(input$tbl)
#     values[[input$datasets]] <- input$tbl
#     return(input$tbl)
#   }
# })


# observe({
#   if(is.null(input$commit) || input$commit ==0) return()
#   
#   isolate({
#     values[[input$datasets]] <- input$tbl
#   })
# })

# menampilkan data time series awal
output$tampil_ts <- renderPrint({
  if(is.null(input$arima_var1) || is.null(input$frequency) || is.null(input$date))  return ()
  isolate({
    mulai <- as.numeric(input$date)
    nilaiTabel[["start"]] <- mulai
    nilaiTabel[["frequency"]] <- as.numeric(input$frequency)
    ts(data = get_data(), frequency = as.numeric(input$frequency), start = c(input$date,1))
  })
})

# menampilkan training set
output$trainingSet <- renderPrint({
  window(getdata_ts(), end = c(input$endYear, input$endMonth))
})

# menampilkan test set
output$testSet <- renderPrint({
  window(getdata_ts(), start = c(input$endYear, input$endMonth+1))
})
# plot data awal
output$arimaplot <- renderPlot({
  par(mfcol = c(1,1), mar = c(5,4,1,2))
  plot(getdata_ts())
})


# hipotesis 0 dari unit root
output$rooth0 <- renderText({
  if(input$uniroottest =="kpss"){
    print("The data is stationary and doesn't need to be differenced.")
  }
  else {
    print("The data needs to be differenced to make it stationary.")
  }
})

# hipotesis 1 dari unit root
output$rooth1 <- renderText({
  if(input$uniroottest =="kpss"){
    "The data needs to be differenced to make it stationary."
  }
  else {
    "The data is stationary and doesn't need to be differenced."
  }
})

# menghitung nilai trunc/lag adf
observe({
  if(is.null(input$uniroottest)) return()
  isolate({
    k <- trunc((length(getdata_ts())-1)^(1/3))
    updateNumericInput(session, inputId = "lagdf", value = k) #(length(getdata_ts())-1)^(1/3)
  })
})

# menampilkan hasil uji unit root
output$textUnitRoot <- renderPrint({
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  print(dodiff)
})

# nilai p-value dari unit root test
# menampilkan hasil uji unit root
output$rootpv <- renderPrint({
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  dodiff$p.value
})

# keputusan dari uji unit root (decision)
output$rootdc <- renderPrint({
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }}
  
  if(dodiff$p.value >= (1-input$confval/100)){
    print(paste0("Do not reject the null hypothesis, since the p-value is greater than ", (1-input$confval/100))) 
  }
  if(dodiff$p.value < (1-input$confval/100)){
    print(paste0("Reject the null hypothesis, since the p-value is less than ", (1-input$confval/100))) 
  }
})

# kesimpulan dari uji unit root (conclusion)
output$rootcn <- renderPrint({
  
  if(input$uniroottest =="kpss"){
    dodiff <- tseries::kpss.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::kpss.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      print("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      print("Therefore, we reject the null hypothesis. And the data is not Stationary") 
    }}
  
  else if(input$uniroottest =="adf"){
    dodiff <- tseries::adf.test(getdata_ts(), k = input$lagdf)
    if(input$dfdat){
      dodiff <- tseries::adf.test(diff(getdata_ts(), differences = as.numeric(input$dfd)), k = input$lagdf)
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      print("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is not Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      print("Therefore, we reject the null hypothesis. And the data is Stationary") 
    }}
  
  else if(input$uniroottest =="pp"){
    dodiff <- tseries::pp.test(getdata_ts())
    if(input$dfdat){
      dodiff <- tseries::pp.test(diff(getdata_ts(), differences = as.numeric(input$dfd)))
    }
    
    if(dodiff$p.value >= (1-input$confval/100)){
      print("Therefore, we do not have enough evidence to reject the null hypothesis. And the data is not Stationary")
    }
    if(dodiff$p.value < (1-input$confval/100)){
      print("Therefore, we reject the null hypothesis. And the data is Stationary") 
    }}
  
})

# plot acf
output$acf <- renderPlot({
  p <- qacf(get_data(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qacf(d, max.lag = input$lagdat)
  }
  print(p)
})

# nilai acf
output$acfval <- renderPrint({
  p <- acf(get_data(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, differences = as.numeric(input$dfd))
    p <- acf(d, lag.max  = input$lagdat, conf.level = (input$confval/100), plot = FALSE)
  }
  print(p)
})

# plot pacf
output$pacf <- renderPlot({
  p <- qpacf(get_data(), conf.level = (input$confval/100), max.lag = input$lagdat)
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, conf.level = (input$confval/100), differences = as.numeric(input$dfd))
    p <- qpacf(d, max.lag = input$lagdat)
  }
  print(p)
})

# nilai pacf
output$pacfval <- renderPrint({
  p <- pacf(get_data(), lag.max = input$lagdat, conf.level = (input$confval/100),plot = FALSE )
  if(input$dfdat){
    d <- diff(get_data(), lag = 1, differences = as.numeric(input$dfd))
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
  #if(is.null(input$methods) || input$methods == 0) return()
  input$methods
  #isolate({
  if(as.numeric(input$methods) == 1){
    k <- auto.arima(getdata_ts(), ic = input$ic, seasonal = input$seasonal,stepwise = input$stepwise, 
                    test = input$selRoot, trace = input$trace, seasonal.test = input$seasonalTest, allowdrift = input$allowDrift)
  }
  if(as.numeric(input$methods) == 2){
    k <- Arima(getdata_ts(), order = c(input$ar, input$df, input$ma), include.drift = TRUE)
  }
  return(k)
  #})
})

# menampilkan hasil estimasi
output$est <- renderPrint({
  estimate.auto()
})

# menampilkan tabel dari beberapa model 
output$modeltable <- renderGvis({
  if(is.null(input$addModel) || input$addModel == 0) return()
  
  isolate({
    fit <- estimate.auto()
    sum <- summary(fit)
    order <- fit$arma[c(1,6,2,3,7,4,5)]
    result <- paste("ARIMA(",order[1],",",order[2],",",order[3],")",sep="")
    if(order[7]>1 & sum(order[4:6]) > 0)
      result <- paste(result,"(",order[4],",",order[5],",",order[6],")[",order[7],"]",sep="")
    if(is.element("constant",names(fit$coef)) | is.element("intercept",names(fit$coef)))
      result <- paste(result,"with non-zero mean")
    else if(is.element("drift",names(fit$coef)))
      result <- paste(result,"with drift        ")
    else if(order[2]==0 & order[5]==0)
      result <- paste(result,"with zero mean    ")
    else
      result <- paste(result,"                  ")
    
    # mengambil nilai aic, aicc, bic, ME, RMSE, MAE , MPE, MAPE, MASE
    data <- data.frame(result, fit$aic, fit$aicc, fit$bic, sum[2], sum[3], sum[5], sum[6])
    colnames(data) <- c("ARIMA MODEL", "AIC", "AICc", "BIC", "RMSE", "MAE", "MAPE", "MASE")
    
    nilaiTabel[["tmp"]] <- rbind(nilaiTabel[["tmp"]], data)
    
    colnames(nilaiTabel[["tmp"]]) <- c("ARIMA MODEL", "AIC", "AICc", "BIC", "RMSE", "MAE", "MAPE", "MASE")  
    gvisTable(nilaiTabel[["tmp"]], options = list(width = 1000))
  })
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

# menampilkan nama datasets
output$namadata <- renderText({
  nama <- paste0("Data: ", input$datasets)
  nama
})

# bagian report, save html, pdf word dll.
# observe({
#   if(is.null(input$regressionReport) || input$regressionReport == 0) return()
#   isolate({
#     inp <- list(input$datasets, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
#                 input$reg_interactions, input$reg_standardize, input$reg_vif, input$reg_stepwise, input$reg_plots)
#     updateReport(inp,"regression", round(7 * 650/650,2), round(7 * 650/650,2))
#   })
# })