shinyServer(function(input, output, session){
  
  # fungsi source untuk meload fungsi yang dipakai bersama
  source('fast.R', local = TRUE)
  setInitValues <- function() {
    # initialize state list and reactive values
    if(testingFast) {
      # load previous state for testing
      
    } else {
      
      state_list <<- list()
      values <<- reactiveValues()
      
      # initial plot height and width
      values$plotHeight <- 650
      values$plotWidth <- 650
      
      # Datasets can change over time (i.e. the changedata function). Therefore,
      # the data need to be a reactive value so the other reactive functions
      # and outputs that depend on these datasets will know when they are changed.
      # robj <- load("../base/data/data_init/diamonds.rda") 
      robj <- load("data/data_init/diamonds.rda") 
      df <- get(robj)
      values[["diamonds"]] <- df
      values[["diamonds_descr"]] <- attr(df,'description')
      values$datasetlist <- c("diamonds")
    }
  }
  
  setInitValues()   # using a function here so it can also be called from state.R to reset the app
  
  
  # source dari data & alat analisis
  R.utils::sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
  R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)
})