

## Get a list of feature set available on database
#* @param orderby
feature_set <- function(orderby="asc"){
  
  # Combine extdata with global expression set and scores dataset if it was provided
  eset_choices <- get_extdata(dataset_choices, score_choices)[["eset_choices"]] %>% unlist() %>% names() 
  
  # Sort projects by ascending or descending order
  if(orderby == "asc"){
    eset_choices <- sort(eset_choices, decreasing = FALSE)
  }else if(orderby == "desc"){
    eset_choices <- sort(eset_choices, decreasing = TRUE)
  }
  
  return(toJSON(eset_choices, pretty=TRUE))
  
}

## Get expression set of the selected feature set
#* @param feature_set
expression_set <- function(res, req, feature_set, include_scores=FALSE){
  
  # Remove multiple spaces
  feature_set <- gsub("\\s+", " ", feature_set)
  
  # Combine extdata with global expression set and scores dataset if it was provided
  extdata <- get_extdata(dataset_choices, score_choices)
  eset_choices <- extdata[["eset_choices"]] %>% unlist()
  input_score_choices <- extdata[["input_score_choices"]] %>% unlist()
  eset_names <- names(eset_choices)
  
  # Check if the project is available on GeneHive, if not, return warning message
  if(toupper(feature_set) %in% toupper(eset_names)){
    
    dl_data <- list()
    
    dataset <- eset_choices[which(toupper(eset_names) == toupper(feature_set))]
    
    if(tools::file_ext(dataset) == "rda" | tools::file_ext(dataset) == "RData"){
      envir_name <- load(dataset)
      ES <- get(envir_name)
    }else{
      ES <- readRDS(system.file("extdata", "eset", dataset, package = "CaDrA"))
    }
    
    dl_data <- c(dl_data, ES=ES)
    
    if(include_scores){
      
      scores <- input_score_choices[which(eset_choices == dataset)] %>% unlist()
      
      if(!is.na(names(scores))){
        
        if(tools::file_ext(scores) == "rda" | tools::file_ext(scores) == "RData"){
          envir_name <- load(scores)
          input_score <- get(envir_name)
        }else{
          input_score <- readRDS(system.file("extdata", "input_score", scores, package = "CaDrA"))
        }
        
        dl_data <- c(dl_data, input_score=list(input_score))

      }else{
        
        dl_data <- c(dl_data, input_score=list("Currently, there are no input scores associated with this feature set on CaDrA portal."))
        
      }
      
    }
    
    return(as_attachment(dl_data, paste0(feature_set, ".rds")))
    
  }else{
    
    res$status <- 404  
    return(list(error=paste0(feature_set, " feature set is not available on the CaDrA portal")))
    
  }
  
}




