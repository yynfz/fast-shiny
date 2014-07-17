upload<-reactive({
  getdata()
})

output$clusVarH <- renderUI({
  dat <- upload()
	list(
		bsCollapse(multiple = TRUE, open = "colH1", id = "collapseH",
		bsCollapsePanel("Cluster Properties", 
			selectInput('varH', 'Select one or more variables to cluster :', names(dat), multiple=TRUE, selectize=FALSE),
			conditionalPanel(condition = "input.clusTabH == 1 || input.clusTabH == 2",
				selectInput('typeh', 'Select Cluster Method', c('Agglomerative','Divisive')),
				conditionalPanel(condition = "input.typeh == 'Agglomerative'",
				selectInput('distance', 'Select distance measurement', c('ward', 'single', 
																		'complete', 'average', 'mcquitty', 'median', 'centroid'))),
				selectInput('metricH', 'Select Cluster Metric', c('Euclidean','Manhattan'))
			),
			br(), br(),
            id="colH1"),
		bsCollapsePanel("Cluster Membership",
			conditionalPanel(condition = "input.clusTabH == 1 || input.clusTabH == 2",
                numericInput("cutH", "Choose number of cluster to view membership", 0, min = 0)
			),
            id="colH2"),
        bsCollapsePanel("Generate Your Report to Ms. Word Document", 
			#checkboxInput("dattt", "Data", TRUE),
            checkboxGroupInput("bgValue", "Select cluster results to generate :", 
					choices = c("Data", "Summary", "Plot", "Heatmap"), selected = NULL),
			#bsActionButton("generateH", "Generate"),
            id="colH3")
    )
	)
})

output$clusVarP <- renderUI({
  dat <- upload()
  
  bsCollapse(multiple = TRUE, open = "colP1", id = "collapseP",
		bsCollapsePanel("Cluster Properties", 
			selectInput('varP', 'Variables:', names(dat), 
                multiple=TRUE, selectize=FALSE),
			selectInput('typep', 'Select Cluster Method', c('K-Means', 'Pillar K-Means')),
			numericInput('clusters', 'Cluster count', 3, min = 1),
			numericInput('maxIter', 'Maximum Iteration', 10, min = 1),
			br(), br(),
            id="colP1"),
        bsCollapsePanel("Generate Your Report", 
			checkboxGroupInput("bgValue", "Select cluster results to generate :", 
					choices = c("Data", "Summary", "Plot"), selected = NULL),
			#bsActionButton("generateP", "Generate"),
            id="colP2")
    ) 
})

output$clusteringH<- renderUI ({
  sidebarLayout(
    sidebarPanel(
        wellPanel(
          HTML(paste("<label><strong>Menu:", "Clustering","</strong></label>")),
          HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
          HTML(paste("<label><strong>Data:",isolate(input$datasets),"</strong></label>"))
          ),
        wellPanel(
            uiOutput("clusVarH")
          )
      ),
    mainPanel(
      tabsetPanel(
        id = "clusTabH",
        tabPanel(
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          title = "Summary",
          helpText("Clustering is an effort to classify similar objects
                   in the same groups. Cluster analysis constructs", strong("good cluster"),
                   "when the members of a cluster have a", strong("high degree of similarity"),
                   "of each other (internal homogeneity) and are not like members of each other
                   clusters (external homogeneity)."),
          verbatimTextOutput("summaryH"),
		  tableOutput("cutTree"),
          value=1
          ),
        tabPanel(
          title = "Plot",
          helpText("Display the clustering results as a", 
                   strong("dendrogram plot"), "for hierarchical clustering."),
          plotOutput("plottyH"),
          value=2
          ),
        tabPanel(
          title = "Heatmap",
          helpText("Display and analyze your data with heatmap. Two dimensional image
                   which uses colors to represent data values. Available for", 
                   strong("agglomerative hierarchical clustering")," only."),
          plotOutput("heatmapH"),
          value=3
        )
        )
      )
    )  
})

output$clusteringP<- renderUI ({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:", "Clustering","</strong></label>")),
        HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
        HTML(paste("<label><strong>Data:",isolate(input$datasets),"</strong></label>"))
      ),
      wellPanel(
        uiOutput("clusVarP")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "clusTabP",
        tabPanel(
          div(class = "busy",
              p("Calculation in progress ..."),
              img(src="ajaxloaderq.gif")
          ),
          title = "Summary",
          helpText("Clustering is an effort to classify similar objects
                   in the same groups. Cluster analysis constructs", strong("good cluster"),
                   "when the members of a cluster have a", strong("high degree of similarity"),
                   "of each other (internal homogeneity) and are not like members of each other
                   clusters (external homogeneity)."),
          verbatimTextOutput("summaryP"),
          value=1
          ),
        tabPanel(
          title = "Plot",
          helpText("Display the clustering results as a", 
                   strong("silhouette plot"), "for partitional clustering."),
          plotOutput("plottyP"),
          value=2
        )
      )
    )
  )  
})

selectedData <- reactive({
  if(input$nav_fast=="Hierarchical"){
    datH<-upload()[, c(input$varH)]
  }
  else{
    datP<-upload()[, c(input$varP)]
  }
})

clusters<- reactive({
  if(input$nav_fast=="Hierarchical"){
    if(input$typeh == 'Agglomerative') {
      (agnes(selectedData(), diss=FALSE, method = input$distance, metric = input$metricH))
    }
    else{
      (diana(selectedData(), diss = FALSE, metric = input$metricH))
    }
  }
  else{
    if(input$typep=="K-Means"){
	  kmeans(selectedData(), input$clusters, iter.max = input$maxIter, nstart = 1)
    }
    else{
      return()
    }
  }
})

output$summaryH<-renderPrint({
  if(is.null(input$varH))return()
  sumH<-clusters()
  summary(sumH)
})

output$summaryP<-renderPrint({
  if(is.null(input$varP))return()
  sumP<-clusters()
  sumP
})

output$plottyH<-renderPlot({
	if(is.null(input$varH))return()
	treeH <- clusters()
	pltree(treeH, main="Cluster Dendrogram", xlab=input$dataset)
  if(is.null(input$cutH) || input$cutH==0){
		return()
  }
  else{
	rect.hclust(treeH, k = input$cutH, border = "red")
  }
})

output$plottyP<-renderPlot({
	if(is.null(input$varP))return()
	cl <- clusters()
	plot(selectedData(), col = cl$cluster)
	points(cl$centers, col = 1:5, pch = 8)
})

output$cutTree<-renderTable({
  if(input$nav_fast=="Hierarchical"){
    if(input$cutH==0){
      return()
    }
    else{
      tree <- clusters()
      cutden<-cutree(tree, k=input$cutH)
      cutTab<-as.data.frame(cutden)
      no<-as.data.frame(c(1:nrow(cutTab)))
      result<-data.frame(no,cutTab)
      colnames(result) <- c("no. observation","cluster membership")
      result
    }
  }
})

output$members<-renderTable({
  if(input$nav_fast=="Partitional"){
    clus<-clusters()
    mem<-as.data.frame(clus$clustering)
    no<-as.data.frame(c(1:nrow(mem)))
    result<-data.frame(no,mem)
    colnames(result) <- c("no. observation","cluster membership")
    result 
  }  
})

output$heatmapH <-renderPlot({
  if(is.null(input$varH))return()
  x <- as.matrix(selectedData())
  rc <- rainbow(nrow(x), start = 0, end = .5)
  cc <- rainbow(ncol(x), start = 0, end = .3)
  hv <- heatmap(x, col = cm.colors(256), scale = "column",
                RowSideColors = rc, ColSideColors = cc, margins = c(4,3), 
                width = 6, height = 10, main = "Heatmap", xlab=input$dataset)  
})

#observe({
#	if(is.null(input$generateH)||is.null(input$generateP)) return()
		
#		wdGet()
#		wdNewDoc()
#		wdTitle("Report for Clustering",label="R2wd")
#		wdSection("Summary")
#		wdBody("Clustering is an effort to classify similar objects in the same groups. Cluster analysis constructs good cluster when :
#1. The members of a cluster have a high degree of similarity of each other (internal homogeneity);
#2. The members of a cluster are not like members of each other clusters (external homogeneity).")
	
#	if(input$dattt==TRUE){
#		Hdat<-upload()
#		wdSection("Data")
#		wdTable(Hdat)
#	}
		
#		wdSave()
#})

#ploot <- function(x,y){
 # cls<-clusters()
  #pltree(cls, main = "Dendrogram")
  #rect.hclust(cls, k = isolate(input$cutH), border = "red") 
#}

#observe({
 #   if(is.null(input$dattt) | input$dattt==FALSE) {
 #   updateButton(session, "generateH", disabled = TRUE)    
#	}
#	else{
#	updateButton(session, "generateH", disabled = FALSE)
#}
 # })