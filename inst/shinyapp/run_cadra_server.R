
# R packages
library(shiny)
library(devtools)
load_all(".")

app <-  CaDrA.shiny::CaDrA_App(id="myapp", datalist_file=NULL)

shiny::runApp(app, host='0.0.0.0', port=3838)
