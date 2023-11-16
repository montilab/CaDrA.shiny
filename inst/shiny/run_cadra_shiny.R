
# Load R packages
library(shiny)
library(devtools)
load_all("/CaDrA", recompile=TRUE)
load_all("/CaDrA.shiny", recompile=TRUE)

# Get location of datalist file
# Required column names in datalist:
# - feature_set_name
# - feature_set_path
# - input_score_name, 
# - input_score_path
# - gene_expression_name
# - gene_expression_path
datalist <- system.file("extdata/datalist.csv", package="CaDrA.shiny")

# If file exists, use the file. Otherwise, use datasets provided with the package.
if(!file.exists(datalist)){
  datalist <- NULL
}

# Run CaDrA's shiny app with provided datasets
app <- CaDrA.shiny::CaDrA_App(id="myapp", datalist=datalist)

# Deploy CaDrA's shiny app on localhost at port 3838
shiny::runApp(app, host='0.0.0.0', port=3838)
