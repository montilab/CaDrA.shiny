
# R packages
library(shiny)
library(devtools)
load_all()

# Read in datalist file in /data directory
datalist <- "/data/datalist.csv"

# If file exists, use the file. Otherwise, use datasets provided with the package.
if(!file.exists(datalist)){
  datalist <- NULL
}

# Run CaDrA.shiny with provided datasets
app <-  CaDrA.shiny::CaDrA_App(id="myapp", datalist=datalist)

# Deploy CaDrA.shiny on localhost:3838
shiny::runApp(app, host='0.0.0.0', port=3838)
