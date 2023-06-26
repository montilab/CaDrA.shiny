
#' Get a list of pre-processed feature sets available on CaDrA API Portal
#' 
#' @param API_Server an URL link to where the API is hosted 
#' @param order_by sort results by ascending or descending order
#' 
#' @return a data frame with two columns: description, feature_set_name
#'
#' @examples
#' 
#' # API server to retrieve a list of feature sets from CaDrA Portal
#' API_Server <- "https://montilab.bu.edu/CaDrA_API/get_feature_set"
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(
#'   API_Server = API_Server, 
#'   order_by = "asc"
#' )
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_feature_set <- function(API_Server, order_by="asc"){
  
  # Call the api server
  res <- httr::GET(url = API_Server, encode = 'json')
  
  # Check the status of the api
  if(res$status_code == '200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
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

#' Get a list of input scores associated with a given feature set
#' 
#' @param API_Server an URL link to where the API is hosted 
#' @param feature_set a specific feature set to look up and retrieve 
#' appropriate input scores that are associated with it 
#' @param order_by sort results by ascending or descending order
#' 
#' @return a data frame with two columns: feature_set_name, input_score_name
#'
#' @examples
#' 
#' # API server to retrieve a list of feature sets from CaDrA Portal
#' fs_API_Server <- "https://montilab.bu.edu/CaDrA_API/get_feature_set"
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(
#'   API_Server = fs_API_Server, 
#'   order_by = "asc"
#' )
#' 
#' # API server to retrieve a list of input scores associated with a specific 
#' feature set from CaDrA Portal
#' scores_API_Server <- "https://montilab.bu.edu/CaDrA_API/get_input_score"
#' 
#' scores_list <- CaDrA.shiny::get_input_score(
#'   API_Server = scores_API_Server, 
#'   feature_set = fs_list$feature_set_name[1], 
#'   order_by = "asc"
#' )
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_input_score <- function(API_Server, feature_set, order_by="asc"){
  
  # Create the server string
  url <- paste0(API_Server, "?feature_set=", feature_set)
  
  # Call the api server
  res <- httr::GET(url = url, encode = 'json')
  
  # Check the status of the api
  if(res$status_code == '200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(order_by == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_name, input_score_name)
    }else if(order_by == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_name), desc(input_score_name))
    }
    
    return(fs_df)
    
  }else{
    
    stop(res$message)
    
  }
  
}

#' Get a list of gene expression sets associated with a given feature set
#' 
#' @param API_Server an URL link to where the API is hosted
#' @param feature_set a specific feature set to look up and retrieve 
#' appropriate gene expression sets that are associated with it 
#' @param order_by sort results by ascending or descending order
#' 
#' @return a data frame with two columns: feature_set_name, gene_expression_name
#'
#' @examples
#' 
#' # API server to retrieve a list of feature sets from CaDrA Portal
#' fs_API_Server <- "https://montilab.bu.edu/CaDrA_API/get_feature_set"
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(
#'   API_Server = fs_API_Server, 
#'   order_by = "asc"
#' )
#'  
#' # API server to retrieve a gene expression set that associated with a 
#' specific feature set from CaDrA Portal
#' ge_API_Server <- "https://montilab.bu.edu/CaDrA_API/get_gene_expression"
#' 
#' ge_list <- CaDrA.shiny::get_gene_expression(
#'   API_Server = ge_API_Server, 
#'   feature_set = fs_list$feature_set_name[1], 
#'   order_by = "asc"
#' )
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_gene_expression <- function(API_Server, feature_set, order_by="asc"){
  
  # Create the server string
  url <- paste0(API_Server, "?feature_set=", feature_set)
  
  # Call the api server
  res <- httr::GET(url = url, encode = 'json')
  
  # Check the status of the api
  if(res$status_code == '200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(order_by == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_name, gene_expression_name)
    }else if(order_by == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_name), desc(gene_expression_name))
    }
    
    return(fs_df)
    
  }else{
    
    stop(res$message)
    
  }
  
}

#' Get a specific feature set available on CaDrA API Portal
#' and determine whether to include input scores and gene expression that 
#' associated with that feature set
#' 
#' @param API_Server an URL link to where the API is hosted 
#' @param feature_sets a list of feature sets to look up and download the 
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
#' # API server to retrieve a list of feature sets from CaDrA Portal
#' fs_API_Server <- "https://montilab.bu.edu/CaDrA_API/get_feature_set"
#' 
#' # Retrieve a list of feature sets available from CaDrA Portal
#' fs_list <- CaDrA.shiny::get_feature_set(
#'   API_Server = fs_API_Server, 
#'   order_by = "asc"
#' )
#' 
#' # Download the feature set along with its input score and gene expression
#' dl_API_Server <- paste0("https://montilab.bu.edu/CaDrA_API/download_feature_set") 
#' 
#' # Define query parameters
#' feature_sets <- fs_list$feature_set_name[1:2]
#' include_input_score <- TRUE
#' include_gene_expression <- TRUE
#' out_dir <- "~/Github"
#' 
#' # Download feature sets and return a datalist with appropriate paths to its dataset
#' mydatafile <- download_feature_sets(
#'   API_Server = dl_API_Server,
#'   feature_sets = feature_sets,
#'   include_input_score = include_input_score,
#'   include_gene_expression = include_gene_expression,
#'   out_dir = out_dir
#' )
#' 
#' # Launch CaDrA Shiny app with your downloaded datalist
#' app <- CaDrA.shiny::CaDrA_App(id="myapp", datalist=mydatafile)
#' 
#' # Launch Shiny app (NOT RUN)
#' # shiny::runApp(app, host='0.0.0.0', port=3838)
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
download_feature_sets <- function(API_Server, feature_sets, include_input_score=TRUE, include_gene_expression=TRUE, out_dir){
  
  # Check feature sets
  if(length(feature_sets) == 0 | any(feature_sets %in% ""))
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
  for(d in seq_along(feature_sets)){
    #d=1;
    feature_set <- feature_sets[d]
    
    # API Server
    url <- paste0(API_Server, "?feature_set=", feature_set, "&include_input_score=", include_input_score, "&include_gene_expression=", include_gene_expression)
    
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
        readRDS(file.path(file_dir, feature_set, "datalist.RDS")) %>% 
          dplyr::mutate(
            feature_set_path = file.path(file_dir, feature_set, "feature_set", feature_set_path),
            input_score_path = file.path(file_dir, feature_set, "input_score", input_score_path), 
            gene_expression_path = file.path(file_dir, feature_set, "gene_expression", gene_expression_path) 
          ) %>% 
          dplyr::select(datalist_colnames)
      )
    
    # Save datalist to its origin directory
    saveRDS(datalist, file.path(file_dir, feature_set, "datalist.RDS"))
    
  }
  
  # Save combined datalist to its output directory
  saveRDS(datalist, file.path(out_dir, paste0("download-fs-", Sys.Date()), "datalist.RDS"))
  
  # Return datalist with appropriate paths to its downloaded dataset
  return(datalist)
  
}


