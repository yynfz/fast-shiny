################################################################
# fungsi yang digunakan bersama dalam aplikasi
################################################################
# mengganti data
changedata <- function(addCol, addColName = "") {
  if(nrow(getdata()) == nrow(addCol) && addColName[1] != "") {
    return(values[[input$datasets]][,addColName] <- addCol)
  }
}

changedata_names <- function(oldnames, newnames) {
  upnames <- colnames(values[[input$datasets]])
  upnames[which(upnames %in% oldnames)] <- newnames
  return(colnames(values[[input$datasets]]) <- upnames)
}

inChecker <- function(tocheck) {
  ifelse(sum(tocheck %in% varnames()) < length(tocheck), return(NULL), return('OK'))
}

# fungsi untuk mengambil data dari varaibel global berdasarkan pilihan user
getdata <- reactive({
  values[[input$datasets]]
})

# mendapatkan kelas dari data
getdata_class <- reactive({
  # don't use isolate here or values won't change when the dataset is changed
  cls <- sapply(getdata(), function(x) class(x)[1])
  gsub("ordered","factor", cls)
})

getdata_class_ts <- reactive({
  # don't use isolate here or values won't change when the dataset is changed
  cls <- sapply(ts(getdata()), function(x) class(x)[1])
  gsub("ordered","factor", cls)
})

# mengambil nama variabel dari data
varnames <- reactive({
  dat <- getdata_class()
  vars <- names(dat)
  names(vars) <- paste(vars, " {", dat, "}", sep = "")
  vars
})

varnames_ts <- reactive({
  dat <- getdata_class_ts()
  vars <- names(dat)
  names(vars) <- paste(vars, " {", dat, "}", sep = "")
  vars
})
# untuk mengubah tanggal menjadi karakter
date2character <- reactive({
  date2character_dat(getdata())
})

# proses mengubah tanggal menjadi karakter
date2character_dat <- function(dat) {
  # xtable tidak bisa memakai tipe dates
  isDate <- c(sapply(dat, is.Date))
  dat[,isDate] <- sapply(dat[,isDate], as.character)
  dat
}

################################################################
# fungsi untuk memudahkah membuat input dan output
################################################################
# mengatur lebar plot sesuai dengan input$viz_plot_width di visualize.R, defaultnya 650px
plotWidth <- function() {
  ifelse(is.null(input$viz_plot_width), return(values$plotWidth), return(input$viz_plot_width))
}

# mengatur tinggi plot sesuai dengan input$viz_plot_height di visualize.R, defaultnya 650px
plotHeight <- function() {
  ifelse(is.null(input$viz_plot_width), return(values$plotHeight), return(input$viz_plot_height))
}