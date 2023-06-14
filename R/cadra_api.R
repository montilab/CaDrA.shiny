
#' Get a list of pre-processed feature sets available on CaDrA's API Portal
#' 
#' @param API_Server an URL link to where the API is hosted 
#' @param orderby sort results by ascending or descending order
#' 
#' @return a data frame with two columns: description, feature_set_names
#'
#' @examples
#' 
#' # Load R library
#' # examples will go here
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_feature_sets <- function(API_Server, orderby="asc"){
  
  # Call the api server
  res <- httr::GET(url = API_Server, encode = 'json')
  
  # Check the status of the api
  if(res$status_code=='200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(orderby == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_names)
    }else if(orderby == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_names))
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
#' @param orderby sort results by ascending or descending order
#' 
#' @return a data frame with two columns: feature_set_names, input_score_names
#'
#' @examples
#' 
#' # Load R library
#' # examples will go here
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_input_scores <- function(API_Server, feature_set, orderby="asc"){
  
  # Create the server string
  url <- paste0(API_Server, "?feature_set=", feature_set)
  
  # Call the api server
  res <- httr::GET(url = url, encode = 'json')
  
  # Check the status of the api
  if(res$status_code=='200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(orderby == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_names, input_score_names)
    }else if(orderby == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_names), desc(input_score_names))
    }
    
    return(fs_df)
    
  }else{
    
    return(res$message)
    
  }
  
}

#' Get a list of gene expression sets associated with a given feature set
#' 
#' @param API_Server an URL link to where the API is hosted
#' @param feature_set a specific feature set to look up and retrieve 
#' appropriate gene expression sets that are associated with it 
#' @param orderby sort results by ascending or descending order
#' 
#' @return a data frame with two columns: feature_set_names, gene_expression_names
#'
#' @examples
#' 
#' # Load R library
#' # examples will go here
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
get_gene_expressions <- function(API_Server, feature_set, orderby="asc"){
  
  # Create the server string
  url <- paste0(API_Server, "?feature_set=", feature_set)
  
  # Call the api server
  res <- httr::GET(url = url, encode = 'json')
  
  # Check the status of the api
  if(res$status_code=='200'){
    
    fs_df <- jsonlite::fromJSON(fromJSON(rawToChar(res$content)))  
    
    # Sort projects by ascending or descending order
    if(orderby == "asc"){
      fs_df <- fs_df %>% dplyr::arrange(feature_set_names, gene_expression_names)
    }else if(orderby == "desc"){
      fs_df <- fs_df %>% dplyr::arrange(desc(feature_set_names), desc(gene_expression_names))
    }
    
    return(fs_df)
    
  }else{
    
    return(res$message)
    
  }
  
}

#' Get a specific feature set available on CaDrA's API Portal
#' and determine whether to include input scores and gene expression that 
#' associated with that feature set
#' 
#' @param API_Server an URL link to where the API is hosted 
#' @param feature_sets a list of feature sets to look up and download the 
#' appropriate dataset that associated with it. 
#' @param include_input_score a Boolean value to determine whether to include input
#' scores that associated with the given feature sets
#' @param include_gene_expression a Boolean value to determine whether to include 
#' gene expression sets that associated with the given feature sets
#' 
#' @return a zip file
#'
#' @examples
#' 
#' # Load R library
#' # examples will go here
#' 
#' @import httr jsonlite dplyr
#' 
#' @export 
download_feature_sets <- function(API_Server, feature_sets, out_dir, include_input_score=TRUE, include_gene_expression=TRUE){
  
  # If the list of feature sets >= 2, concatenate them by a commas separator
  if(length(feature_sets) == 0){
    stop("feature_sets cannot be empty.")
  }else{
    feature_sets <- paste0(feature_sets, sep = ", ", collapse = TRUE)
  }

  # Check the input parameters
  if(!include_input_score %in% c(TRUE, FALSE))
    stop("include_input_score must be a Boolean value, TRUE or FALSE.")
  
  if(!include_gene_expression %in% c(TRUE, FALSE))
    stop("include_gene_expression must be Boolean value, TRUE or FALSE.")
  
  # Check output directory
  if(!dir.exists(out_dir))
    stop(sprintf("Directory: %s does not exists"), out_dir)
  
  # Create the server string
  url <- paste0(API_Server, "?feature_sets=", feature_sets, "&include_input_score=", include_input_score, "&include_gene_expression=", include_gene_expression)
  
  # Call the api server
  res <- httr::GET(url = url, encode = 'json')
  
  # Check the status of the api
  if(res$status_code == '200'){
    
    file_dir = file.path(out_dir, paste0("download-fs-", Sys.Date()))
    
    zip_file = paste0(file_dir, ".zip")
    
    download.file(url, zip_file)
    
    return(list.files(file_dir))
    
  }else{
    
    return(res$message)
    
  }
  
}


