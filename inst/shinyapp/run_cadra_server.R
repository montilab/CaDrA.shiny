
# R packages
library(shiny)
library(devtools)
load_all("/CaDrA-shiny")

id <- "myapp"
datalist_file <- "/CaDrA-shiny/inst/extdata/datalist.csv"

app <-  `CaDrA-shiny`::CaDrA_App(id=id, datalist_file=datalist_file)

shiny::runApp(app, host='0.0.0.0', port=3838)
