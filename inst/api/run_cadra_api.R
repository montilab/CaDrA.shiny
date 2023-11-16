
# Load R packages
library(plumber)
library(devtools)
load_all("/CaDrA", recompile=TRUE)
load_all("/CaDrA.shiny", recompile=TRUE)

# Get location of CaDrA API code
cadra_api_path <- system.file("api/api.R", package="CaDrA.shiny")

# Check if CaDrA API code exists
if(!file.exists(cadra_api_path)){
  
  stop(paste0("CaDrA API does not exists at ", cadra_api_path))
  
}else{
  
  # Start a CaDrA's Plumber API instance
  cadra_api <- plumber::plumb(cadra_api_path)
  
  # Deploy CaDrA's Plumber API on localhost at port 3838
  cadra_api$run(host='0.0.0.0', port=3838)
  
}