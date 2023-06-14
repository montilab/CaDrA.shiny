

# R packages
library(plumber)
library(devtools)
load_all(".")

cadra_api <- plumber::plumb("./inst/plumber/cadra_api.R")

cadra_api$run(host='0.0.0.0', port=3838)
