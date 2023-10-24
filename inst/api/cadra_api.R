
# Load R packages ####
library(plumber)
library(tidyverse)
library(jsonlite)

# Must load CaDrA.shiny to access dataset
library(devtools)
load_all()

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

# Convert datalist file into a data frame
datalist_df <- CaDrA.shiny:::get_extdata(datalist=datalist)

# Required column names for datalist
required_colnames <- CaDrA.shiny:::datalist_colnames

# Increase the memory limit for reading and downloading data from API
library(unix)
unix::rlimit_as(1e12, 1e12)

# API title and description ####

#* @apiTitle Plumber Example API
#* @apiDescription This is a server for retrieving pre-proccessed datasets to run CaDrA

# Set-up header access ####

#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

# Create a list of serializers to return object ####
serializers <- list(
  "application/zip" = plumber::serializer_content_type("application/zip"),
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
#' @get /get_feature_set
get_feature_set <- function(res, req){
  
  # Validate results
  if(nrow(datalist_df) == 0){
    
    error_message <- sprintf("Currently, there are no feature sets avaliable on our portal.")
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }else{
    
    fs_df <- datalist_df %>% 
      dplyr::select(tidyselect::starts_with("description"), tidyselect::starts_with("collection"), feature_set_name) %>% 
      dplyr::distinct(feature_set_name, .keep_all = TRUE)
    
    return(jsonlite::toJSON(fs_df, pretty=TRUE))
    
  }
  
}

## Get a list of input scores that are associated with a given feature set
#* @param feature_set
#' @get /get_input_score
get_input_score <- function(res, req, feature_set){
  
  # parameters
  params <- c("feature_set")
  
  # check parameters
  if(base::missing(feature_set)){
    
    missing_params <- c(base::missing(feature_set))
    error_message <- sprintf("Missing required parameter: %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }
  
  # Validate parameters
  feature_set <- trimws(feature_set)
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- datalist_df %>% 
    dplyr::filter(feature_set_name %in% feature_set)
  
  # Validate results
  if(nrow(fs_df) == 0){
    
    error_message <- sprintf("There is no feature set with the name: %s avaliable on our portal.", feature_set)
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }else{
    
    input_score_df <- fs_df %>%
      dplyr::distinct(feature_set_name, input_score_name, .keep_all = TRUE)
    
    if(nrow(input_score_df) == 0){
      
      error_message <- sprintf("Feature set, %s, does not have any associated input scores to download from our portal.", feature_set)
      
      ## Initialize the serializers
      res$serializer <- serializers[["html"]]
      res$status <- 400
      res$body <- page_not_found(status="400", message=error_message)
      
    }else{
      
      return(jsonlite::toJSON(input_score_df, pretty=TRUE))
      
    }
    
  }
  
}

## Get a list of gene expression set that are associated with a given feature set
#* @param feature_set
#' @get /get_gene_expression
get_gene_expression <- function(res, req, feature_set){
  
  # parameters
  params <- c("feature_set")
  
  # check parameters
  if(base::missing(feature_set)){
    
    missing_params <- c(base::missing(feature_set))
    error_message <- sprintf("Missing required parameter: %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }
  
  # Validate parameters
  feature_set <- trimws(feature_set)
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- datalist_df %>% 
    dplyr::filter(feature_set_name %in% feature_set)

  # Validate results
  if(nrow(fs_df) == 0){
    
    error_message <- sprintf("There is no feature set with the name: %s avaliable on our portal.", feature_set)
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }else{
    
    gene_expression_df <- fs_df %>%
      dplyr::distinct(feature_set_name, gene_expression_name)
    
    if(nrow(gene_expression_df) == 0){
      
      error_message <- sprintf("Feature set, %s, does not have any associated gene expression sets to download from our portal.", feature_set)
      
      ## Initialize the serializers
      res$serializer <- serializers[["html"]]
      res$status <- 400
      res$body <- page_not_found(status="400", message=error_message)
      
    }else{
      
      return(jsonlite::toJSON(gene_expression_df, pretty=TRUE))
      
    }  
  
  }
  
}

## Returns a Zip file that consists a list of given feature sets along with their associated input scores and gene expression sets
#* @param feature_set
#* @param include_input_score
#* @param include_gene_expression
#' @get /download_feature_set
download_feature_set <- function(res, req, feature_set, include_input_score=TRUE, include_gene_expression=TRUE){
  
  # parameters
  params <- c("feature_set", "include_input_score", "include_gene_expression")
  
  # check parameters
  if(base::missing(feature_set) | base::missing(include_input_score) | base::missing(include_gene_expression)){
    
    missing_params <- c(base::missing(feature_set), base::missing(include_input_score), base::missing(include_gene_expression))
    error_message <- sprintf("Missing required parameter(s): %s", paste(params[which(missing_params==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=error_message)
    
  }
  
  # Validate parameters
  feature_set <- trimws(feature_set)
  include_input_score <- ifelse(include_input_score==TRUE, 1, 0)
  include_gene_expression <- ifelse(include_gene_expression==TRUE, 1, 0)
  
  # Read in the datalist file that includes paths to feature sets, input scores, and gene expression sets
  fs_df <- datalist_df %>% 
    dplyr::filter(feature_set_name %in% feature_set)

  # Validate results
  if(nrow(fs_df) == 0){
    
    error_message <- sprintf("There is no feature set with the name: %s avaliable on our portal.", feature_set)
    
    ## Initialize the serializers
    res$serializer <- serializers[["html"]]
    res$status <- 400
    res$body <- page_not_found(status="400", message=error_message)
    
  }else{
    
    ## Initialize the serializers
    res$serializer <- serializers[["application/zip"]]
    res$status <- 200
    
    # This header is a convention that instructs browsers to present the response
    # as a download named filename rather than trying to render it inline.
    file_name <- paste0("download-fs-", Sys.Date())
    zip_file_name <- paste0(file_name, ".zip")
    attachmentString = paste0("attachment; filename=", zip_file_name)
    res$setHeader("Content-Disposition", attachmentString)
    
    # Create a temporary directory to store RDS files
    tmpdir <- file.path(tempdir())
    dir.create(tmpdir, showWarnings=FALSE, recursive=TRUE)
    
    # Delete the directory after exiting the function
    on.exit({
      if (dir.exists(tmpdir)) {
        unlink(tmpdir, recursive = TRUE)
      }
    }, add = TRUE)
    
    # Create a directory
    zip_file_dir <- file.path(tmpdir, file_name)
    dir.create(zip_file_dir, showWarnings=FALSE, recursive=TRUE)
    
    # Retrieve feature set name and path
    feature_set_name <- unique(fs_df$feature_set_name)
    feature_set_path <- unique(fs_df$feature_set_path)
    
    # Create a download data frame
    download_df <- NULL
    
    for(f in seq_along(feature_set_name)){
      #f=1;
      fs_name <- feature_set_name[f]
      
      # Create feature set directory path
      fs_file_dir <- file.path(zip_file_dir, feature_set_name[f], "feature_set")
      
      # Create feature set directory
      dir.create(path=fs_file_dir, showWarnings=FALSE, recursive=TRUE)
      
      # Create feature set file name
      file_path <- file.path(fs_file_dir, paste0(feature_set_name[f], ".RDS"))
      
      # Copy feature set to zip directory
      file.copy(from=feature_set_path[f], to=file_path, overwrite=TRUE, recursive=FALSE)
      
      # Whether to include input scores
      if(include_input_score == TRUE){
        
        input_score_df <- fs_df %>% 
          dplyr::filter(feature_set_name == fs_name) %>% 
          dplyr::distinct(feature_set_name, input_score_name, .keep_all = TRUE) %>% 
          dplyr::select(required_colnames)
        
        if(nrow(input_score_df) > 0){
          
          for(g in 1:nrow(input_score_df)){
            #g=1; 
            # Create a directory
            scores_file_dir <- file.path(zip_file_dir, fs_name, "input_score")
            dir.create(scores_file_dir, showWarnings=FALSE, recursive=TRUE)        
            
            score_name = input_score_df$input_score_name[g]
            score_path = input_score_df$input_score_path[g]
            
            # Create a file path
            file_path <- file.path(scores_file_dir, paste0(score_name, ".RDS"))
            
            # Copy input score to zip directory
            file.copy(from=score_path, to=file_path, overwrite=TRUE, recursive=FALSE)
          }
          
          download_df <- download_df %>% rbind(input_score_df)
          
        }
      }
      
      # Whether to include gene expression
      if(include_gene_expression == TRUE){
        
        gene_expression_df <- fs_df %>% 
          dplyr::filter(feature_set_name == fs_name) %>% 
          dplyr::distinct(feature_set_name, gene_expression_name, .keep_all = TRUE) %>% 
          dplyr::select(required_colnames)
        
        if(nrow(gene_expression_df) > 0){
          
          for(h in 1:nrow(gene_expression_df)){
            #h=1;
            # Create a directory
            ge_file_dir <- file.path(zip_file_dir, fs_name, "gene_expression")
            dir.create(ge_file_dir, showWarnings=FALSE, recursive=TRUE)        
            
            ge_name = gene_expression_df$gene_expression_name[h]
            ge_path = gene_expression_df$gene_expression_path[h]
            
            # Create a file path
            file_path <- file.path(ge_file_dir, paste0(ge_name, ".RDS"))
            
            # Copy gene expression to zip directory
            file.copy(from=ge_path, to=file_path, overwrite=TRUE, recursive=FALSE)

          }
          
          download_df <- download_df %>% rbind(gene_expression_df)
          
        }
      }
      
      # Save data information
      download_df <- download_df %>% 
        dplyr::transmute(
          feature_set_name = feature_set_name,
          feature_set_path = basename(feature_set_path), 
          input_score_name = sapply(seq_along(input_score_name), function(s){ ifelse(include_input_score==TRUE, input_score_name[s], "") }),
          input_score_path = sapply(seq_along(input_score_path), function(s){ ifelse(include_input_score==TRUE, basename(input_score_path[s]), "") }),
          gene_expression_name = sapply(seq_along(gene_expression_name), function(s){ ifelse(include_gene_expression==TRUE, gene_expression_name[s], "") }),
          gene_expression_path = sapply(seq_along(gene_expression_path), function(s){ ifelse(include_gene_expression==TRUE, basename(gene_expression_path[s]), "") }),
        )
      
      saveRDS(download_df, file.path(zip_file_dir, fs_name, "datalist.RDS"))
      
    }  
    
    zip_file <- file.path(tmpdir, zip_file_name)
    
    system(sprintf('bash -c "cd %s && zip -r %s %s"', tmpdir, zip_file_name, file_name))    
    
    return(readBin(zip_file, what="raw", n=file.info(zip_file)$size))
    
  }
}  



