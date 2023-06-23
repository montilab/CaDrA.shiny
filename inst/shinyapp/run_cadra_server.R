
# R packages
library(shiny)
library(devtools)
load_all(".")

datalist <- "/CaDrA.shiny/inst/extdata/datalist.csv"

app <-  CaDrA.shiny::CaDrA_App(id="myapp", datalist=datalist)

shiny::runApp(app, host='0.0.0.0', port=3838)
