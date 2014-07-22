library(shiny)
library(datasets)
library(censReg)
library(GGally)
library(nortest)
library(tseries)
library(ggplot2)
library(R2wd)
library(xtable)
#library(stats)

#user interface untuk input variabel dependen
output$uiReg_varDep <- renderUI({
  isTobit <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isTobit]
  if(length(vars) ==  0) return()
  selectInput(inputId = "tobit_variableDep", label = "Dependent variable :", choices = vars,
              selected = vars[1], multiple = FALSE)
})

#user interface untuk input variabel independen
output$uiReg_varIndep <- renderUI({
  if(is.null(input$tobit_variableDep)) return()
	isTobit <- "character" != getdata_class()
  vars <- varnames()[isTobit]
 	vars <- vars[-which(vars == input$tobit_variableDep)]
  if(length(vars) == 0) return()
  selectInput(inputId = "tobit_variableIndep", label = "Independent variables :", choices = vars, 
  	 multiple = TRUE, selectize = FALSE)
})

#user interface
output$tobit_reg <- renderUI ({
  sidebarLayout(
     sidebarPanel(
      div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
	  wellPanel(
        HTML(paste("<label><strong>Menu:", "Regression","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
      ),
      wellPanel(
        uiOutput("uiReg_varDep"),
		uiOutput("uiReg_varIndep")
      ),
	  conditionalPanel(condition = "input.conditionedPanels == 'Estimation'",
	  wellPanel(
    
		 radioButtons("left", "Specify left-censoring limit :",
             c("No left-censoring limit" = "no",
			   "Left-censoring at minimum" = "min",
               "Specified left-censoring limit" = "yes"),"min"
			),
			uiOutput("censoring_left"),
			  		 		
		 
		 radioButtons("right", "Specify right-censoring limit :",
             c("No right-censoring limit" = "no",
			   "Right-censoring at maximum" = "max",
               "Specified right-censoring limit" = "yes")
			),
			uiOutput("censoring_right")
      ),
     
	wellPanel(
		selectInput(
            inputId = "iterative_method",
            label = "Specify iterative method :",
            choices = list(
              "Newton-Raphson" = "nr",
			   "Berndt-Hall-Hall-Hausman" = "bhhh",
               "Broyden-Fletcher-Goldfarb-Shanno" = "bfgs",
			   "Nelder-Mead" ="nm",
			   "BFGSR"="bfgsr",
			   "Simulated annealing" = "sann"),"nr"
	)
	)),
	conditionalPanel(condition = "input.conditionedPanels == 'Identification'",
	  wellPanel(
		selectInput(
            inputId = "identification_plot",
            label = "Identification plot : ",
            c("Censored Variable Plot"="cen","Histogram","Correletaion"='cor',"Scatter","Laverage plots"))
      )),
	  
      conditionalPanel(condition = "input.conditionedPanels == 'Postestimation'", 
		conditionalPanel(condition = "input.condPanels == 'Plot'", 
		wellPanel(
			selectInput(
            inputId = "asumsi_plot",
            label = "Regression assumptions : ",
            choices = list(
              "Linearity" = 'linear_plot', 
              "Normality" = 'normal_plot',
			  "Homoschedasticity"= 'homo_plot',
			  "Non-Autocorrelation"= 'nonauto_plot',
			  "Multicolinearity"='multicol_plot'))
		),
		uiOutput("normality_plot")
	)),
	conditionalPanel(condition = "input.conditionedPanels == 'Postestimation'", 
		conditionalPanel(condition = "input.condPanels == 'Formal Test'", 
		wellPanel(
			selectInput(
            inputId = "asumsi_formal",
            label = "Tobit regression assumptions : ",
            choices = list(
              "Linearity" = 'linear_formal', 
              "Normality" = 'normal_formal',
			  "Homoschedasticity"= 'homo_formal',
			  "Non-Autocorrelation"= 'nonauto_formal',
			  "Multicolinearity"='multicol_formal'))
		),
		uiOutput("normality_formal")
	))
	
	),
    mainPanel(
		tabsetPanel(
			  tabPanel("Identification", uiOutput('identification'), actionButton("export", "export to word")),
			  tabPanel("Estimation", htmlOutput('hasil')),
			  tabPanel("Postestimation", 
				tabsetPanel(
							tabPanel("Plot", plotOutput("plot")),
							tabPanel("Formal Test",
							h5("Test the assumption of Tobit Regression by formal test."), textOutput(outputId = "test_name"),
									br(),br(),
							verbatimTextOutput(outputId = "test")
							 ),id ="condPanels")
 						),
			 id = "conditionedPanels" 
		)
    )
  )
  
  })
  
 # fungsi untuk mengambil variable yang dibutuhkan

  variable_Dependent <- reactive({
  dat <- getdata()[,as.character(input$tobit_variableDep)]
})
  
   variable_Independent <- reactive({
  dat <- getdata()[,as.character(input$tobit_variableIndep)]
})
 
 #Jenis-jenis uji asumsi
 output$test_name<-reactive({
	if(input$asumsi_formal=='linear_formal') return("Linearity Test")
		else if(input$asumsi_formal=='normal_formal') return("Normality Test")
			else if(input$asumsi_formal=='homo_formal') return("Homoschedasticity Test")
				else if(input$asumsi_formal=='nonauto_formal') return("Non Autocorrelation Test")
					else if (input$asumsi_formal=='multicol_formal') return("Non Multicollinearity Test")
 })
 
  #################
  #Perhitungan
  #################
  
  #Fungsi untuk estimasi parameter
	parameter <- reactive({
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
			return("Please select one or more independent variables.")
		}
					
				
			if (input$left=='no'&& input$right=='no') { 
				return("Please specify at least one censoring limit.")
			}
			
			if (input$left=='no') { 
				left=-Inf
			}
			
			if (input$left=='min') { 
				left=min(variable_Dependent_)
				if(input$right=='yes'){
					if(left>=input$right2) {
					return("Right-censoring limit must be a larger number than left-censorinf limit.")
					}
				}
			}
			
			if (input$left=='yes') {
				left=input$left2
			}
			
			if (input$right=='no') {
				right=Inf
			}
			if (input$right=='max') { 
				right=max(variable_Dependent_)
				if(input$left=='yes'){
					if(right<=input$left2) {
					return("Right-ensoring limit must be a larger number than left-censorinf limit.")
					}
				}
				
			}
			if (input$right=='yes') {
				right=input$right2
			}
			
			if(input$left=='yes'&&input$right=='yes') {
			if (input$left2>=input$right2) { 
				return("Right-ensoring limit must be a larger number than left-censorinf limit.")
			}
			}
			
			if(input$iterative_method=="nr"){
				method="Newton-Raphson"
			}
			else if(input$iterative_method=="bhhh"){
				method="BHHH"
			}
			else if(input$iterative_method=="bfgs"){
				method="BFGS"
			}
			else if(input$iterative_method=="nm"){
				method="NM"
			}
			else if(input$iterative_method=="bfgsr"){
				method="BFGSR"
			}
			else if(input$iterative_method=="sann"){
				method="SANN"
			}
			
			#estimasi parameter
			result<- censReg(variable_Dependent_~variable_Independent_,left=left, right=right, method=method) 
			return(result)
			
	})
	
	#Fungsi untuk residual dan MSE
	residual <- reactive({
	
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
			return("Please select one or more independent variables.")
		}
					
				
			if (input$left=='no'&& input$right=='no') { 
				return("Please specify at least one censoring limit.")
			}
			
			if (input$left=='no') { 
				left=-Inf
			}
			
			if (input$left=='min') { 
				left=min(variable_Dependent_)
				if(input$right=='yes'){
					if(left>=input$right2) {
					return("Right-ensoring limit must be a larger number than left-censorinf limit.")
					}
				}
			}
			
			if (input$left=='yes') {
				left=input$left2
			}
			
			if (input$right=='no') {
				right=Inf
			}
			if (input$right=='max') { 
				right=max(variable_Dependent_)
				if(input$left=='yes'){
					if(right<=input$left2) {
					return("Right-ensoring limit must be a larger number than left-censorinf limit.")
					}
				}
				
			}
			if (input$right=='yes') {
				right=input$right2
			}
			
			if(input$left=='yes'&&input$right=='yes') {
			if (input$left2>=input$right2) { 
				return("Right-ensoring limit must be a larger number than left-censorinf limit.")
			}
			}
			
			if(input$iterative_method=="nr"){
				method="Newton-Raphson"
			}
			else if(input$iterative_method=="bhhh"){
				method="BHHH"
			}
			else if(input$iterative_method=="bfgs"){
				method="BFGS"
			}
			else if(input$iterative_method=="nm"){
				method="NM"
			}
			else if(input$iterative_method=="bfgsr"){
				method="BFGSR"
			}
			else if(input$iterative_method=="sann"){
				method="SANN"
			}
			
			result<- censReg(variable_Dependent_~variable_Independent_,left=left, right=right, method=method) 
			#menghitung residual
				number_row<-nrow(data.matrix(result$estimate))
				matrix_1<-data.matrix(result$estimate)
				matrix_2<-matrix_1[c(1:(number_row-1)),]
				matrix_3<-data.matrix(matrix_2)
				matrix_Indep<-cbind(1,variable_Independent_)
				fitted<-matrix_Indep%*%matrix_3
				residual<- variable_Dependent_-fitted
				return(residual)
				 
			
	})
  
  #menampilkan estimasi parameter dan mse
  output$hasil <- renderPrint({
		 
			variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
			variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
			
				
			if(typeof(residual())=="character"){
				residual()
			}
			else {
			mse<-sum((residual()^2))/(nrow(variable_Independent_)-(ncol(variable_Independent_)+1))
			#print(summary(parameter()))
			
			result<-summary(parameter())
			formula<-result$call
			observation<-result$nObs
			estimate<-result$estimate
			#table1<-xtable(formula)
			#table2<-xtable(observation)
			table3<-xtable(estimate)
			print(formula)
			print(observation)
			print(table3, type = "html")
			print("MSE :",quote=FALSE) 
			print(mse)
			}
				
			
  })
  
 #fungsi untuk menampilkan plot pada tab identification
 
 output$identification<- renderUI({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		if(is.null(input$tobit_variableIndep)){
			return("Please select one or more independent variables.")
		}
		else {
			plotOutput("identification1")
		}
							
	   })
 
 output$identification1<- renderPlot({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		dependent_variable<-variable_Dependent()
		independent_variable<-variable_Independent()
		data<-data.frame(cbind(dependent_variable,independent_variable))
			
				if(input$identification_plot=='cen') {
					f <- function(x, var, bw = 15) {
						dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
					}
					p <- ggplot(data, aes(x = dependent_variable))
					plot<-p + stat_bin(binwidth=5) +stat_function(fun = f, size = 1,args = list(var = data$dependent_variable))
					
					if (input$export==TRUE) { 
					wdGet()
					wdTitle("Plot of Censoring Variable")
					wdPlot(plot)
					}
					
				}
				else if (input$identification_plot=='cor') {
					plot<-ggpairs(cbind(variable_Dependent_,variable_Independent_),title="Correlation Plot")
				}
				print(plot)
				
							
	   })
 
 #fungsi penghitungan uji asumsi formal
   output$test <- renderPrint({
		
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
			if(typeof(residual())=="character"){
				residual()
			}
			else {
					if(input$asumsi_formal=='linear_formal') {
						
					}
					else if (input$asumsi_formal=='normal_formal') {
						if(input$normal_formalchoice=='ks') {
							test<-lillie.test(residual())						
						}
						else if (input$normal_formalchoice=='jb') {
							test<-jarque.bera.test(residual())
						}
						else if (input$normal_formalchoice=='ad') {
							test<-ad.test(residual())
						}
						else if (input$normal_formalchoice=='cvm') {
							test<-cvm.test(residual())
						}
						else if (input$normal_formalchoice=='pearson') {
							test<-pearson.test(residual())
						}
						else if (input$normal_formalchoice=='sw') {
							test<-shapiro.test(residual())
						}
						else if (input$normal_formalchoice=='sf') {
							test<-sf.test(residual())
						}
					}
					else if (input$asumsi_formal=='homo_formal') {
					
					}
					else if (input$asumsi_formal=='nonauto_formal') {
					
					}
					else {
					
					}
					return(test)
		}
					
	})
			output$normality_formal <- renderUI({
				if (input$asumsi_formal=='normal_formal') {
				wellPanel(
					selectInput(
					inputId = "normal_formalchoice",
					label = "Choose formal test of normality : ",
					choices = list(
					  "Lilliefors test" = 'ks',
					  "Jarque Bera test" = 'jb',
					  "Anderson Darling test" = 'ad',
					  "Cramer-von Mises test" = 'cvm',
					  "Pearson chi-square test" = 'pearson',
					  "Shapiro-Wilk test" = 'sw',
					  "Shapiro Francia test" = 'sf'
					  ))
				)}
			})

	
	#fungsi untuk menampilkan plot
	output$plot <- renderPlot({
			
		variable_Dependent_<-data.matrix(variable_Dependent(), rownames.force = NA)
		variable_Independent_<-data.matrix(variable_Independent(), rownames.force = NA)
		
		independent_variable<-variable_Independent()
		residuals<-residual()
			if(typeof(residual())=="character"){
				residual()
			}
			else {
			
				if(input$asumsi_plot=='linear_plot') {
						plots <- list()
						i<-0
						rdat <- cbind(residuals,independent_variable)
						rdat <- data.frame(rdat)
						while(i<ncol(independent_variable)){
						#if(getdata_class()[i] == 'factor') {
						#	if('factor' %in% class(dat[,i])) {
						#		plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .3)
						#	} else {
								print(plots[i] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point() + geom_smooth(size = .75, linetype = "dotdash"))
						#	}
						}
								i<-i+1

																
					}
				else if (input$asumsi_plot=='normal_plot') {
					if(input$normal_plotchoice=='hist') {
					print(hist(residual(),100,col="red"))
					}
					else {
					print(qqnorm(residual(),col="red"))
					print(qqline(residual(),col="black"))
					}
				}
				else if (input$asumsi_plot=='homo_plot') {
					fitted<-variable_Dependent_-residual()
					plot(residual(),fitted, col="red")
				}
				else if (input$asumsi_plot=='nonauto_plot') {
				
				}
				else {
				
				}
			}
	   })
	
	output$normality_plot <- renderUI({
		if (input$asumsi_plot=='normal_plot') {
		wellPanel(
			selectInput(
            inputId = "normal_plotchoice",
            label = "Choose plot of normality : ",
            choices = list(
              "Histogram" = 'hist', 
              "QQ-Plot" = 'qq'
			  ))
		)
		}
	})

	output$censoring_left <- renderUI({
  	 if(input$left == 'yes') {
			numericInput("left2", "Specified left censoring limit", " ", min = NA, max = NA,
				step = NA)
			}
	})
	
	output$censoring_right <- renderUI({
  	 if(input$right == 'yes') {
			numericInput("right2", "Specified right censoring limit", " ", min = NA, max = NA,
				step = NA)
			}
	})
