
#' Get a list of pre-processed feature sets available on CaDrA API Portal
#' 
#' @param collection a specific collection of feature sets on CaDrA portal. Default is TCGA.
#' @param order_by sort results by ascending or descending order
#' 
#' @return a data frame with two columns: description, feature_set_name
#'
#' @examples
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(
#'   collection = "TCGA",
#'   order_by = "asc"
#' )
#' 
#' # View top 6 rows
#' head(fs_list)
#' 
#' @import httr dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @export 
get_feature_set <- function(collection="TCGA", order_by="asc"){

  # API server to retrieve a list of feature sets from CaDrA Portal
  API_Server <- "https://montilab.bu.edu/CaDrA_API/get_feature_set"
  
  # Call the api server
  res <- httr::GET(url = API_Server, encode = 'json')
  
  # Check the status of the api
  if(res$status_code == '200'){
    
    fs_df <- jsonlite::fromJSON(jsonlite::fromJSON(base::rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(order_by == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_name)
    }else if(order_by == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_name))
    }
    
    return(fs_df)
    
  }else{
    
    return(res$message)
    
  }
  
}

#' Pull a specific feature set available on CaDrA Portal
#' and determine whether to include gene expression set that is 
#' associated with that feature set
#' 
#' @param feature_set a list of feature sets to look up and download the 
#' appropriate data that are associated with it. 
#' @param include_gene_expression a Boolean value to determine whether to 
#' include gene expression sets that associated with the given feature sets. 
#' Default is TRUE.
#' 
#' @return a list of feature set and gene expression set if 
#' include_gene_expression = TRUE
#'
#' @examples
#' 
#' # Load R library
#' library(shiny)
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(order_by = "asc")
#' 
#' # Download feature sets and return a datalist with appropriate paths to its dataset
#' datasets <- pull_datasets(
#'   feature_set = fs_list$feature_set_name[1],
#'   include_gene_expression = TRUE
#' )
#' 
#' @import httr dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @export 
pull_datasets <- function(feature_set, include_gene_expression=FALSE){
  
  # API server to download a list of feature sets from CaDrA Portal
  API_Server <- "https://montilab.bu.edu/CaDrA_API/download_feature_set"
  
  # Create a temporarily directory
  out_dir <- tempdir()
  dir.create(out_dir, showWarnings = FALSE)
  
  # Check feature sets
  if(length(feature_set) == 0 | any(feature_set %in% ""))
    stop("feature_set cannot be empty.")
  
  # Check gene expression
  if(!include_gene_expression %in% c(TRUE, FALSE))
    stop("include_gene_expression must be Boolean value, TRUE or FALSE.")
  
  # API Server
  url <- paste0(API_Server, "?feature_set=", feature_set, "&include_input_score=FALSE&include_gene_expression=", include_gene_expression)
  
  # Check the status of the api
  tryCatch({
    
    file_dir = file.path(out_dir, paste0("download-fs-", Sys.Date()))
    
    zip_file = paste0(file_dir, ".zip")
    
    download.file(url, zip_file)
    
  }, error = function(e){
    
    stop(e)
    
  })
  
  # If download is successful, unzip files and return datalist with its new appropriate path
  unzip(zip_file, exdir=out_dir, list=FALSE)
  
  # Read in datalist for each downloaded feature set
  datalist <- readRDS(file.path(file_dir, feature_set, "datalist.RDS")) %>% distinct_all(., keep.all=TRUE)
  feature_set_path = file.path(file_dir, feature_set, "feature_set", datalist$feature_set_path)
  gene_expression_path = file.path(file_dir, feature_set, "gene_expression", datalist$gene_expression_path) 
  
  # Create a dataset list
  datasets <- list(
    feature_set = readRDS(feature_set_path)
  )
  
  if(include_gene_expression == TRUE)
    datasets <- c(datasets, gene_expression = readRDS(gene_expression_path))

  # Remove the temp directory
  on.exit(unlink(out_dir))
  
  # Return datalist with appropriate paths to its downloaded dataset
  return(datasets)
  
}

#' Get a specific feature set available on CaDrA API Portal
#' and determine whether to include input scores and gene expression that 
#' associated with that feature set
#' 
#' @param feature_set a list of feature sets to look up and download the 
#' appropriate data that are associated with it. 
#' @param include_input_score a Boolean value to determine whether to include 
#' input scores that associated with the given feature sets. Default is TRUE.
#' @param include_gene_expression a Boolean value to determine whether to 
#' include gene expression sets that associated with the given feature sets. 
#' Default is TRUE.
#' @param out_dir a directory to save the output. 
#' 
#' @return a zip file
#'
#' @examples
#' 
#' # Load R library
#' library(shiny)
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(order_by = "asc")
#' 
#' # Download feature sets and return a datalist with appropriate paths to its dataset
#' mydatafile <- download_feature_sets(
#'   feature_set = fs_list$feature_set_name[1],
#'   include_input_score = TRUE,
#'   include_gene_expression = TRUE,
#'   out_dir = "~/Github"
#' )
#' 
#' # Launch CaDrA Shiny app with your downloaded datalist
#' app <- CaDrA.shiny::CaDrA_App(id="myapp", datalist=mydatafile)
#' 
#' # Launch Shiny app (NOT RUN)
#' # shiny::runApp(app, host='0.0.0.0', port=3838)
#' 
#' @import httr dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @export 
download_feature_sets <- function(feature_set, include_input_score=TRUE, include_gene_expression=TRUE, out_dir=Sys.getenv("HOME")){
  
  # API server to download a list of feature sets from CaDrA Portal
  API_Server <- "https://montilab.bu.edu/CaDrA_API/download_feature_set"
  
  # Check feature sets
  if(length(feature_set) == 0 | any(feature_set %in% ""))
    stop("feature_set cannot be empty.")
  
  # Check input score 
  if(!include_input_score %in% c(TRUE, FALSE))
    stop("include_input_score must be a Boolean value, TRUE or FALSE.")
  
  # Check gene expression
  if(!include_gene_expression %in% c(TRUE, FALSE))
    stop("include_gene_expression must be Boolean value, TRUE or FALSE.")
  
  # Check output directory
  if(length(out_dir) == 0 | out_dir == "")
    stop("out_dir cannot be empty.")
  
  # Check if output directory exists
  if(!dir.exists(out_dir))
    stop(sprintf("Directory: %s does not exists"), out_dir)
  
  # Create empty list to store datalist
  datalist <- NULL; 
  
  # Required column names for datalist
  datalist_colnames <- c('feature_set_name', 'feature_set_path', 'input_score_name', 'input_score_path', 'gene_expression_name', 'gene_expression_path')
  
  # Create the server string
  for(d in seq_along(feature_set)){
    #d=1;
    # API Server
    url <- paste0(API_Server, "?feature_set=", feature_set[d], "&include_input_score=", include_input_score, "&include_gene_expression=", include_gene_expression)
    
    # Check the status of the api
    tryCatch({
      
      file_dir = file.path(out_dir, paste0("download-fs-", Sys.Date()))
      
      zip_file = paste0(file_dir, ".zip")
      
      download.file(url, zip_file)
      
    }, error = function(e){
      
      stop(e)
      
    })
    
    # If download is successful, unzip files and return datalist with its new appropriate path
    unzip(zip_file, exdir=out_dir, list=FALSE)
    
    # Append datalist for each downloaded feature set
    datalist <- datalist %>% 
      rbind(
        readRDS(file.path(file_dir, feature_set[d], "datalist.RDS")) %>% 
          dplyr::mutate(
            feature_set_path = file.path(file_dir, feature_set[d], "feature_set", feature_set_path),
            input_score_path = file.path(file_dir, feature_set[d], "input_score", input_score_path), 
            gene_expression_path = file.path(file_dir, feature_set[d], "gene_expression", gene_expression_path) 
          ) %>% 
          dplyr::select(datalist_colnames)
      )
    
    # Save datalist to its origin directory
    saveRDS(datalist, file.path(file_dir, feature_set[d], "datalist.RDS"))
    
  }
  
  # Save combined datalist to its output directory
  saveRDS(datalist, file.path(out_dir, paste0("download-fs-", Sys.Date()), "datalist.RDS"))
  
  # Return datalist with appropriate paths to its downloaded dataset
  return(datalist)
  
}
