
# R packages
library(shiny)
library(devtools)
load_all(".")

id <- "myapp"
datalist_file <- system.file("extdata/datalist.csv", package = "CaDrA-shiny")

app <-  `CaDrA-shiny`::CaDrA_App(id=id, datalist_file=datalist_file)

shiny::runApp(app, host='0.0.0.0', port=3838)
