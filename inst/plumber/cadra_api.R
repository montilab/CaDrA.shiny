
# R packages ####
library(dplyr)
library(plumber)
library(jsonlite)

# Increase the memory limit for reading and downloading data from API
library(unix)
unix::rlimit_as(1e12, 1e12)

# API title and description ####

#* @apiTitle Plumber Example API
#* @apiDescription This is a server for generating NGS special report.

# Set-up header access ####

#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

# Create a list of serializers to return object ####
serializers <- list(
  "html" = plumber::serializer_html(),
  "json" = plumber::serializer_json(),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

# Create a page not found
page_not_found <- function(status, message){
  
  sprintf(
    '
    <!DOCTYPE html>
    <html lang="en">
    <head>
     <title>%s Error</title>
     <style>
       #error-404 {
        text-align: center;
          margin: 0;
          padding: 0.6em 2em 0.4em;
          border-bottom: 2px solid #000;
          color: #fff;
          background-color: #900;
          font-size: 0.9em;
          font-weight: normal;
          font-family: sans-serif, helvetica;
        }
     
        #error-message {
          padding: 1em 5em;
          text-align: center;
          border-bottom: 2px solid #000;
        }
     </style>
    </head>
    <body>
      <div id="error-404"><h1><strong>%s Error</strong></h1></div>
      <div id="error-message"><h3>%s</h3></div>
    </body>
    </html>
    ', status, status, message
  )
  
}

## Get a list of feature sets available on the database
#' @get /get_feature_sets
get_feature_sets <- function(res, req){
  
  # Get the path of datalist file which it contains all of the feature sets, input scores,
  # and gene expression sets that are available on the server
  fs_datalist_path <- system.file("extdata/datalist.csv", package="CaDrA.shiny")
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- read.csv(fs_datalist_path, header=TRUE) %>% 
    dplyr::select(description, feature_set_names)
  
  # Validate results
  if(nrow(fs_df) == 0){
    fs_df <- data.frame(
      warning = sprintf("There are currently no feature sets avaliable on our portal.")
    )
  }
  
  return(jsonlite::toJSON(fs_df, pretty=TRUE))
  
}

## Get a list of input scores that are associated with a given feature set
#* @param feature_set
#' @get /get_input_scores
get_input_scores <- function(res, req, feature_set){
  
  # parameters
  params <- c("feature_set")
  
  # check parameters
  if(base::missing(feature_set)){
    missing_params <- c(base::missing(feature_set))
    error_message <- sprintf("Missing required parameter: %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 500
    rest$body <- page_not_found(status="500", message=error_message)
  }
  
  # Validate parameters
  feature_set <- trimws(feature_set)
  
  # Get the path of datalist file which it contains all of the feature sets, input scores,
  # and gene expression sets that are available on the server
  fs_datalist_path <- system.file("extdata/datalist.csv", package="CaDrA.shiny")
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- read.csv(fs_datalist_path, header=TRUE) %>% 
    dplyr::filter(feature_set_names %in% feature_set)
  
  # Validate results
  if(nrow(fs_df) == 0){
    fs_df <- data.frame(
      warning = sprintf("There is no feature set with the name: %s avaliable on our portal.", feature_set)
    )
  }else{
    fs_df <- fs_df %>% 
      dplyr::distinct(feature_set_names, input_score_names, .keep_all = TRUE)
  }
  
  return(jsonlite::toJSON(fs_df, pretty=TRUE))
  
}

## Get a list of gene expression set that are associated with a given feature set
#* @param feature_set
#' @get /get_gene_expressions
get_gene_expressions <- function(res, req, feature_set){
  
  # parameters
  params <- c("feature_set")
  
  # check parameters
  if(base::missing(feature_set)){
    missing_params <- c(base::missing(feature_set))
    error_message <- sprintf("Missing required parameter: %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 500
    rest$body <- page_not_found(status="500", message=error_message)
  }
  
  # Validate parameters
  feature_set <- trimws(feature_set)
  
  # Get the path of datalist file which it contains all of the feature sets, input scores,
  # and gene expression sets that are available on the server
  fs_datalist_path <- system.file("extdata/datalist.csv", package="CaDrA.shiny")
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- read.csv(fs_datalist_path, header=TRUE) %>% 
    dplyr::filter(feature_set_names %in% feature_set)
  
  # Validate results
  if(nrow(fs_df) == 0){
    fs_df <- data.frame(
      warning = sprintf("There is no feature set with the name: %s avaliable on our portal.", feature_set)
    )
  }else{
    fs_df <- fs_df %>% 
      dplyr::distinct(feature_set_names, gene_expression_names, .keep_all = TRUE)
  }
  
  return(jsonlite::toJSON(fs_df, pretty=TRUE))
  
}

## Returns a Zip file that consists a list of given feature sets along with their associated input scores and gene expression sets
#* @param feature_sets
#* @param include_input_score
#* @param include_gene_expression
#* @serializer contentType list(type="application/zip")
#' @get /download_feature_sets
download_feature_sets <- function(res, req, feature_sets, include_input_score=TRUE, include_gene_expression=TRUE){
  
  # parameters
  params <- c("feature_set", "include_input_score", "include_gene_expression")
  
  # check parameters
  if(base::missing(feature_set) | base::missing(include_input_score) | base::missing(include_gene_expression)){
    
    missing_params <- c(base::missing(feature_set), base::missing(include_input_score), base::missing(include_gene_expression))
    error_message <- sprintf("Missing required parameter(s): %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 500
    rest$body <- page_not_found(status="500", message=error_message)
    
  }
  
  # Validate parameters
  feature_sets <- strsplit(as.character(feature_sets), ",", perl=TRUE) %>% unlist() %>% gsub("[[:space:]]", "", .)
  include_input_score <- ifelse(include_input_score==TRUE, 1, 0)
  include_gene_expression <- ifelse(include_gene_expression==TRUE, 1, 0)
  
  # Get the path of datalist file which it contains all of the feature sets, input scores,
  # and gene expression sets that are available on the server
  fs_datalist_path <- system.file("extdata/datalist.csv", package="CaDrA.shiny")
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- read.csv(fs_datalist_path, header=TRUE) %>% 
    dplyr::filter(feature_set_names %in% feature_sets)
  
  # Validate results
  if(nrow(fs_df) == 0){
    
    fs_df <- data.frame(
      warning = sprintf(
        "There %s no feature set%s with the name%s: %s avaliable on our portal.", 
        ifelse(length(feature_sets)==1, "is", "are"),
        ifelse(length(feature_sets)==1, "", "s"),
        ifelse(length(feature_sets)==1, "", "s"),
        paste0(feature_sets, collapse=", "))
    )
    
    return(jsonlite::toJSON(fs_df, pretty=TRUE))
    
  }else{
    
    # Create a temporary directory to store RDS files
    fs <- c()
    tmpdir <- file.path(tempdir(), paste0("download-fs-", Sys.Date()))
    dir.create(tmpdir, showWarnings=FALSE, recursive=TRUE)
    
    # Delete the directory after exiting the function
    on.exit({
      if (dir.exists(tmpdir)) {
        unlink(tmpdir, recursive = TRUE)
      }
    }, add = TRUE)
    
    # Create an empty object to store file names
    file_paths <- NULL
      
    # Read in feature sets 
    for(f in 1:nrow(fs_df)){
      #f=1;
      feature_set_name = fs_df$feature_set_names[f]
      feature_set_path = fs_df$feature_set_paths[f]

      # Create a directory
      file_dir <- file.path(tmpdir, feature_set_name, "feature_set")
      dir.create(file_dir, showWarnings=FALSE, recursive=TRUE)
      
      # Create a file path
      file_path <- file.path(file_dir, paste0(feature_set_name, ".RDS"))
      
      # Copy FS to zip directory
      file.copy(from=feature_set_path, to=file_path, overwrite=TRUE, recursive=FALSE)
      
      # Store file path
      file_paths <- c(file_paths, file_path)    
      
      # Whether to include input scores
      if(include_input_score == TRUE){
        input_score_name = fs_df$input_score_names[f]
        input_score_path = fs_df$input_score_paths[f]
        
        if(input_score_name != "" && !is.na(input_score_name) && !is.null(input_score_name)){

          # Create a directory
          file_dir <- file.path(tmpdir, feature_set_name, "input_score")
          dir.create(file_dir, showWarnings=FALSE, recursive=TRUE)
          
          # Create a file path
          file_path <- file.path(file_dir, paste0(input_score_name, ".RDS"))
          
          # Copy FS to zip directory
          file.copy(from=input_score_path, to=file_path, overwrite=TRUE, recursive=FALSE)
          
          # Store file path
          file_paths <- c(file_paths, file_path)  
          
        }
      }
      
      # Whether to include gene expression
      if(include_gene_expression == TRUE){
        gene_expression_name = fs_df$gene_expression_names[f]
        gene_expression_path = fs_df$gene_expression_paths[f]
        
        if(gene_expression_name != "" && !is.na(gene_expression_name) && !is.null(gene_expression_name)){
          
          # Create a directory
          file_dir <- file.path(tmpdir, feature_set_name, "gene_expression")
          dir.create(file_dir, showWarnings=FALSE, recursive=TRUE)
          
          # Create a file path
          file_path <- file.path(file_dir, paste0(gene_expression_name, ".RDS"))
          
          # Copy FS to zip directory
          file.copy(from=input_score_path, to=file_path, overwrite=TRUE, recursive=FALSE)
          
          # Store file path
          file_paths <- c(file_paths, file_path)  
          
        }
      }
      
    }
    
    # zip the files with predefined names
    for(path in file_paths){
      fs <- c(fs, path)
    } 
    
    zip_file = file.path(tmpdir, paste0("download-fs-", Sys.Date(), ".zip"))
    
    zip(zipfile=zip_file, files=fs, flags="-r9Xj")
    
    readBin(zip_file, what="raw", n=file.info(zip_file)$size)

  }  
}




