#UI for input Survival Time
output$uiSurvTimeVar <- renderUI ({
  isSurvival <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isSurvival]
  #if(length(vars) ==  0) return()
  selectInput(inputId = "survTimeVar", label = "Survival Time Variable:", choices = vars,
  #selectInput(inputId = "survTimeVar", label = "Survival Time Variable:", selected = "survtime",
              selected = vars[1], multiple = FALSE)
})

#UI for input Censored variable
output$uiCensVar <- renderUI ({
  if(is.null(input$survTimeVar)) return()
  #isSurvival <- "integer" == getdata_class() | "factor" == getdata_class()
  isSurvival <- "character" != getdata_class()
  vars <- varnames()[isSurvival]
  vars <- vars[-which(vars == input$survTimeVar)]
  if(length(vars) == 0) return()
  selectInput(inputId = "survCensVar", label = "Censored Variable:", choices = vars,
  #selectInput(inputId = "survCensVar", label = "Censored Variable:", selected = "censdead",
              selected = vars[1], multiple = FALSE)
})

#UI for input independent variable
output$uiSurvVarIndep <- renderUI ({
  if(is.null(input$survTimeVar) & is.null(input$survCensVar)) return()
  isSurvival <- "character" != getdata_class() & ("numeric" == getdata_class() | "integer" == getdata_class())
  vars <- varnames()[isSurvival]
  vars <- vars[-which(vars == input$survCensVar)]
  if(length(vars) == 0) return()
  selectInput(inputId = "survVarIndep", label = "Independent Variables:", choices = vars, 
              multiple = TRUE, selectize = FALSE)
})

output$survRegFitting <- renderUI ({
  sidebarLayout(
    sidebarPanel(
      div(class = "busy",
          p("Calculation in progress ..."),
          img(src="ajaxloaderq.gif")
      ),
      wellPanel(
        HTML(paste("<label><strong>Menu:","Survival","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
      ),
      wellPanel(
        uiOutput("uiSurvTimeVar"),
        uiOutput("uiCensVar"),
        uiOutput("uiSurvVarIndep")
      )
    ),
	  mainPanel(
	    tabsetPanel(
	      id = "survRegFittingTabs",
	      tabPanel("Exponential",
	               uiOutput(outputId="expRegFit"),
                 value = 1
	      ),
	      tabPanel("Weibull",
	               uiOutput(outputId="weiRegFit"),
	               value = 2
	      ),
	      tabPanel("Lognormal",
	               uiOutput(outputId="logNormRegFit"),
	               value = 3
	      ),
	      tabPanel("Extended Gamma*",
	               verbatimTextOutput(outputId="exGammaRegFit"),
	               value = 4
	      ),
	      tabPanel("Log-logistic*",
	               verbatimTextOutput(outputId="logLogRegFit"),
	               value = 5
	      ),
	      tabPanel("Summary",
	               htmlOutput(outputId="summaryRegFit"),
	               value = 6
	      )
      )
	  )
  )
})

#get Survival Time Variable
survivalTimeVariable <- reactive({
  dat <- getdata()[,as.character(input$survTimeVar)]
})
#get Censored Variable
censoredVariable <- reactive({
  dat <- getdata()[,as.character(input$survCensVar)]
})
#get Independent Variable(s)
independentVariables <- reactive({
  dat <- getdata()[,as.character(input$survVarIndep)]
  datM <- data.matrix(dat, rownames.force = NA)
  return(datM)
})

warningSelection <- reactive ({
  #return("<strong><font color='red'>Please select one or more Independent Variable(s).</font></strong>")
  return("Please select one or more Independent Variable(s).")
})

output$expRegFit <- renderUI ({
  if (is.null(input$survVarIndep)) {
    warningSelection()
  }
  else {
    verbatimTextOutput("expRegFit2")
  }
})

output$expRegFit2 <- renderPrint ({
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  expo <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="exponential")
  summary(expo)
})

output$weiRegFit <- renderUI ({
  if (is.null(input$survVarIndep)) {
    warningSelection()
  }
  else {
    verbatimTextOutput("weiRegFit2")
  }
})

output$weiRegFit2 <- renderPrint ({
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  wei <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="weibull")
  summary(wei)
})

output$logNormRegFit <- renderUI ({
  if (is.null(input$survVarIndep)) {
    warningSelection()
  }
  else {
    verbatimTextOutput("logNormRegFit2")
  }
})

output$logNormRegFit2 <- renderPrint ({
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  lognorm <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="lognormal")
  summary(lognorm)
})

output$exGammaRegFit <- renderPrint ({
  
}) #Fungsi ini belum selesai sampai 14 Agustus 2014

output$logLogRegFit <- renderPrint({
  
}) #Fungsi ini belum selesai sampai 14 Agustus 2014

output$summaryRegFit <- renderUI ({
  if (is.null(input$survVarIndep)) {
    warningSelection()
  }
  else {
    htmlOutput("summaryRegFit2")
  }
})

output$summaryRegFit2 <- renderText ({
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  expo <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="exponential")
  expo.ll <- expo$loglik
  expo.AIC <- extractAIC(expo)[2]
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  wei <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="weibull")
  wei.ll <- wei$loglik
  wei.AIC <- extractAIC(wei)[2]
  indepVar_ <- data.matrix(independentVariables(), rownames.force = NA)
  lognorm <- survreg(Surv(survivalTimeVariable(),censoredVariable())~indepVar_, dist="lognormal")  
  lognorm.ll <- lognorm$loglik
  lognorm.AIC <- extractAIC(lognorm)[2]
  if (expo.AIC < wei.AIC & expo.AIC < lognorm.AIC) {
    expo.AIC <- paste("<font color='red'>",expo.AIC,"</font>")
  } else if (wei.AIC < expo.AIC & wei.AIC < lognorm.AIC) {
    wei.AIC <- paste("<font color='red'>",wei.AIC,"</font>")
  } else if (lognorm.AIC < expo.AIC & lognorm.AIC < wei.AIC) {
    lognorm.AIC <-paste("<font color='red'>",lognorm.AIC,"</font>")
  }
  paste0(
    "<table border='1'>
    <tr>
      <th width='125px'>Distribution</th>
      <th width='75px'>Log-likelihood</th> 
      <th width='75px'><i>p</i></th>
      <th width='75px'>AIC</th>
    </tr>
    <tr>
      <td>Exponential</td>
      <td>",expo.ll,"</td> 
      <td></td>
      <td>",expo.AIC,"</td>
    </tr>
    <tr>
      <td>Weibull</td>
      <td>",wei.ll,"</td> 
      <td></td>
      <td>",wei.AIC,"</td>
    </tr>
    <tr>
      <td>Lognormal</td>
      <td>",lognorm.ll,"</td> 
      <td></td>
      <td>",lognorm.AIC,"</td>
    </tr>
    <tr>
      <td>Extended Gamma</td>
      <td></td> 
      <td></td>
      <td></td>
    </tr>
    <tr>
      <td>Log-logistic</td>
      <td></td> 
      <td></td>
      <td></td>
    </tr>
    </table><br /><br />"
  )
})
