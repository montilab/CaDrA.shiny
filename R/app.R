
# function to hover columns in data table
create_hover_txt <- function(table, n_char=100){
  
  column_names <- colnames(table)
  
  th_tr <- lapply(seq_along(column_names), function(l){ 
    title <- column_names[l]
    name <- ifelse(nchar(title) > n_char, paste0(substr(title, 1, n_char), "..."), title)
    th <- sprintf('<th title = "%s">%s</th>\n', title, name) 
  }) %>% purrr::flatten_chr() %>% paste0(collapse = "")
  
  th_tr <- paste0('<th title=""></th>\n', th_tr) %>% HTML()
  sketch <- htmltools::withTags(
    tags$table(
      class = 'display',
      tags$thead(
        th_tr
      )
    )
  )
  
  return(sketch)
  
}

# Global expression sets ####
global_feature_set_path <- c(
  "CCLE SCNAs and Mutations" =  system.file("data/CCLE_MUT_SCNA.rda", package = "CaDrA"),
  "TCGA BrCa SCNAs and Mutations" = system.file("data/BRCA_GISTIC_MUT_SIG.rda", package = "CaDrA"),
  "Simulated Feature Set" = system.file("data/sim_FS.rda", package = "CaDrA")
)

# Global input scores ####
global_input_score_path <- c(
  "B-catenin Activity in CCLE" = system.file("data/CTNBB1_reporter.rda", package = "CaDrA"),
  "YAP/TAZ Activity in TCGA BrCa" = system.file("data/TAZYAP_BRCA_ACTIVITY.rda", package = "CaDrA"),
  "Simulated Input Score" =  system.file("data/sim_Scores.rda", package = "CaDrA")
)

# Global gene expression ####
global_gene_expression_path <- c(
  "CCLE Expression Set" = "",
  "BRCA Expression Set" = "",
  "Simulated Expression Set" = ""
)

# Description about the feature set
global_fs_description <- c(
  "CCLE SCNAs and Mutations" =  "Somatic copy number alterations and mutations from CCLE. See ?CaDrA::CCLE_MUT_SCNA",
  "TCGA BrCa SCNAs and Mutations" = "Somatic copy number alterations and mutations from BRCA TCGA. See ?CaDrA::BRCA_GISTIC_MUT_SIG",
  "Simulated Feature Set" = paste0(
    "Simulated feature set comprises of 1000 genomic features and 100 ",
    "sample profiles. This simulated data includes 10 left-skewed (i.e. True Positive or TP) and ", 
    "990 uniformly-distributed (i.e. True Null or TN) features. See ?CaDrA::BRCA_GISTIC_MUT_SIG"
  ) 
)

# Description about the feature set
global_fs_collection <- c(
  "CCLE SCNAs and Mutations" =  "CCLE",
  "TCGA BrCa SCNAs and Mutations" = "TCGA",
  "Simulated Feature Set" = "Simulated"
)

# Create global datalist options
global_datalist_options <- data.frame(
  collection = global_fs_collection,
  description = global_fs_description,
  feature_set_name = names(global_feature_set_path), 
  feature_set_path = global_feature_set_path , 
  input_score_name = names(global_input_score_path), 
  input_score_path = global_input_score_path, 
  gene_expression_name = names(global_gene_expression_path),
  gene_expression_path = global_gene_expression_path
)

# Create a list of scoring methods
scoring_methods <- c("ks_pval", "ks_score", "wilcox_pval", "wilcox_score", "revealer")
  
# Required column names for datalist
datalist_colnames <- c('feature_set_name', 'feature_set_path', 'input_score_name', 'input_score_path', 'gene_expression_name', 'gene_expression_path')

# function to obtain the external data
get_extdata <- function(datalist=NULL){
  
  if(is.null(datalist)){
    
    datalist <- global_datalist_options
    
  }else if(is.data.frame(datalist)){
    
    if(all(datalist_colnames %in% colnames(datalist))){
      
      datalist <- global_datalist_options %>% dplyr::bind_rows(datalist) 
      
    }else{
      
      stop("The provided datalist file must contain the following column names: ", paste0(datalist_colnames, collapse=", "))
      
    }
    
  }else{
    
    file_path <- as.character(datalist)
    
    if(file.exists(file_path)){
      if(tools::file_ext(datalist) == "csv"){
        datalist <- utils::read.csv(file_path, header=TRUE) %>% dplyr::mutate_all(., as.character)
      }else if(tools::file_ext(file_path) == "rds"){
        datalist <- base::readRDS(file_path) %>% dplyr::mutate_all(., as.character)
      }else{
        stop("datalist file file must have a csv or rds format")
      }
    }else{
      stop("File does not exist at ", file_path)
    } 
    
    if(all(datalist_colnames %in% colnames(datalist))){
      
      datalist <- global_datalist_options %>% dplyr::bind_rows(datalist) 
      
    }else{
      
      stop("The provided datalist file must contain the following column names: ", paste0(datalist_colnames, collapse=", "))
      
    }
    
  }
  
  # Check if datalist has appropriate column names
  if(all(datalist_colnames %in% colnames(datalist))){
    
    # Only retain feature set with data path exists
    fs_df <- datalist %>% 
      dplyr::filter(!is.null(feature_set_name) & !is.na(feature_set_name) & feature_set_name != "") %>% 
      dplyr::filter(!is.null(feature_set_path) & !is.na(feature_set_path) & feature_set_path != "") %>% 
      distinct(feature_set_name, feature_set_path) 
    
    if(nrow(fs_df) > 0){
      removed_fs_names <- fs_df$feature_set_name[which(file.exists(fs_df$feature_set_path) == FALSE)]
      if(length(removed_fs_names) > 0){
        stop("There ", ifelse(length(removed_fs_names) > 1, "are", "is"), " no feature sets exists at ", paste0(removed_fs_names, collapse=", "), ".\n",
             "Please check your files again.")      }
    }else{
      stop("The required feature_set_name and feature_set_path fields in datalist file cannot be empty")
    }
    
    input_score_df <- datalist %>% 
      dplyr::filter(!is.null(input_score_name) & !is.na(input_score_name) & input_score_name != "") %>% 
      dplyr::filter(!is.null(input_score_path) & !is.na(input_score_path) & input_score_path != "") %>% 
      dplyr::distinct(input_score_name, input_score_path)

    if(nrow(input_score_df) > 0){
      removed_score_names <- fs_df$input_score_name[which(file.exists(datalist$input_score_path) == FALSE)]
      if(length(removed_score_names) > 0){
        stop("There ", ifelse(length(removed_score_names) > 1, "are", "is"), " no input score exists at ", paste0(removed_score_names, collapse=", "), ".\n",
             "Please check your files again.")
      }
    }else{
      datalist <- datalist %>% 
        dplyr::mutate(
          input_score_name = "",
          input_score_path = ""
        )
    }
    
    gene_expression_df <- datalist %>% 
      dplyr::filter(!is.null(gene_expression_name) & !is.na(gene_expression_name) & gene_expression_name != "") %>% 
      dplyr::filter(!is.null(gene_expression_path) & !is.na(gene_expression_path) & gene_expression_path != "") %>% 
      dplyr::distinct(gene_expression_name, gene_expression_path)
    
    if(nrow(gene_expression_df) > 0){
      removed_gs_names <- fs_df$gene_expression_name[which(file.exists(datalist$gene_expression_path) == FALSE)]
      if(length(removed_gs_names) > 0){
        stop("There ", ifelse(length(removed_gs_names) > 1, "are", "is"), " no gene expression set exists at ", paste0(removed_gs_names, collapse=", "), ".\n",
             "Please check your files again.")
      }
    }else{
      datalist <- datalist %>% 
        dplyr::mutate(
          gene_expression_name = "",
          gene_expression_path = ""
        )      
    }
    
    app_options <- datalist %>% 
      dplyr::distinct_all(.keep_all = TRUE)
    
    return(app_options)
    
  }else{
    
    stop("The provided datalist file must contain the following column names: ", paste0(datalist_colnames, collapse=", "))
    
  }
  
}

#' Shiny UI modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist A data frame or path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist must contains the following variables:
#' 'feature_set' (required), 'feature_set_path' (repuired), 
#' 'input_score_name' (required), input_score_path' (required), 
#' 'gene_expression_name' (optional), 'gene_expression_path' (optional)
#' 
#' Default is NULL. If NULL, the app will start with DEFAULT dataset included 
#' in the package.
#' 
#' @return shiny ui elements
#'
#' @examples
#' 
#' # Load R library
#' library(shiny)
#'
#' # Create ui and server for Shiny app
#' id <- "myapp"
#' 
#' ui <- shiny::fluidPage(
#'    CaDrA.shiny::CaDrA_UI(id = id)
#' )
#' 
#' server <- function(input, output, session) {
#'    CaDrA.shiny::CaDrA_Server(id = id)
#' }
#' 
#' app <- shiny::shinyApp(ui=ui, server=server)
#' 
#' # Launch Shiny app (NOT RUN)
#' # shiny::runApp(app, host='0.0.0.0', port=3838)
#' 
#' @import dplyr DT htmltools 
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(shinyjs, except = c(runExample))
#'  
#' @export 
 CaDrA_UI <- function(id, datalist=NULL){
  
  # Combine extdata with global expression set and scores dataset if it was provided
  extdata <- get_extdata(datalist=datalist)

  cadra_fs_choices <- extdata %>% dplyr::distinct(feature_set_name, .keep_all=TRUE)
  cadra_feature_set_path <- cadra_fs_choices$feature_set_path
  names(cadra_feature_set_path) <- cadra_fs_choices$feature_set_name

  gsva_gs_choices <- extdata %>% dplyr::distinct(gene_expression_name, .keep_all=TRUE) %>% 
    dplyr::filter(gene_expression_path != "" & !is.na(gene_expression_path))
  gsva_gene_expression_path <- gsva_gs_choices$gene_expression_path
  names(gsva_gene_expression_path) <- gsva_gs_choices$gene_expression_name
  
  ns <- shiny::NS(id)

  fluidRow(
    style = "padding: 5px 10px 10px 10px;",

    tags$style(
      HTML(
        "
        .side-bar-options {
          border: 1px solid gray;
          border-radius: 3px;
          background: lightgrey;
          padding: 5px 10px 10px 10px;
          min-height: 850px;
        }

        .footer-info {
          text-align: center;
        }

        .tooltip-txt {
          color: red;
          font-weight: bold;
          width: 70px;
        }

        .loading_div {
          display: flex;
          text-align: center;
          align-items: center;
          justify-content: center;
          width: 100%;
          height: 400px;
        }

        .loading_text {
          text-align: center;
          margin-left: -30px;
        }

        .loader {
          border: 16px solid #f3f3f3;
          border-top: 16px solid #3498db;
          border-radius: 50%;
          width: 120px;
          height: 120px;
          animation: spin 2s linear infinite;
        }

        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        "
      )
    ),

    tags$script(
      HTML(
        paste0(
          "Shiny.addCustomMessageHandler('ToggleOperation',\n", 
            "function(message){ \n",
              "var x = document.getElementById(message.id);\n",
              "if(message.display === 'yes'){\n",
                "x.style.display = 'flex';\n",
              "}else{\n",
                "x.style.display = 'none';\n",
              "}\n",
            "}\n",
          ")\n"
        )
      )
    ),
    
    tags$script(
      HTML(
        paste0(
          "function select_all_genelist() {",
          "var allcheckboxes = document.getElementsByName('gsva_select_all');",
          "var checkboxes = document.getElementsByName('gsva_selected_row');",
          "if(allcheckboxes[0].checked === true){",
          "for(var i = 0; i < checkboxes.length; i++){",  
          "checkboxes[i].checked = true;",
          "}",
          "}else{",
          "for(var i = 0; i < checkboxes.length; i++){",  
          "checkboxes[i].checked = false;",
          "}",
          "}",
          "}"
        )
      )
    ),
    
    tags$script(
      HTML(
        paste0(
          "function gsva_selected_checkbox(module_id) {",
          "var id = module_id;",
          "var checkboxes = document.getElementsByName('gsva_selected_row');",
          "var allcheckboxes = document.getElementsByName('gsva_select_all');",
          "var row_number = [];",
          "for(var i = 0; i < checkboxes.length; i++){",
          "if(checkboxes[i].checked === true){",
          "row_number.push(checkboxes[i].value);",
          "}else{",
          "row_number.push(null);",
          "}",
          "}",
          "Shiny.onInputChange(id+'-checkbox_all', allcheckboxes[0].checked);",
          "Shiny.onInputChange(id+'-gsva_selected_row_number', row_number);",
          "}"
        )
      )
    ),
    
    shinyjs::useShinyjs(),
    
    column(
      width = 12,
      style = "padding: 5px 10px 10px 10px;",

      tabsetPanel(
        id = "tabs",

        ##### CADRA OPTIONS #######
        tabPanel(
          title = "Run CaDrA",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "running", lib = "font-awesome"),

          column(
            width = 3,
            class = "side-bar-options",

            h3("CaDrA Options", style="text-align: center;"),

            br(),
            
            # FS ####
            selectizeInput(
              inputId = ns("feature_set"),
              label = "Feature set",
              choices = c(cadra_feature_set_path, "Import Data"),
              width = "100%"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("feature_set")),

              fileInput(
                inputId = ns("ES_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Feature set file"),
                width = "100%"
              ),

              radioButtons(
                inputId = ns("ES_file_type"),
                label = HTML(paste0(
                  'File type ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement=',
                  '"top" title=\"NOTE: If file is in csv format, ',
                  'the \'Feature Set\' must be a data ',
                  'frame including a \'Feature\' column name ',
                  'that contains unique names or labels to ',
                  'search for best features. Otherwise, \'Feature ',
                  'Set\' must be an object of class SummarizedExperiment ',
                  'from SummarizedExperiment package.\">?</a>')),
                choices = c("csv", "rds"),
                selected = "csv",
                inline = TRUE
              )
            ),

            # input_score ####
            selectizeInput(
              inputId = ns("input_score"),
              label = "Input score",
              choices = c("Please select an option below" = ""),
              width = "100%"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("input_score")),

              fileInput(
                inputId = ns("input_score_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Input score file"),
                width = "100%"
              ),

              radioButtons(
                inputId = ns("input_score_file_type"),
                label = HTML(paste0(
                  'File type ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement=',
                  '"top" title=\"NOTE: If file is in csv format, ',
                  'then the \'Input Score\' file ',
                  'must be a data frame with two columns ',
                  '(Sample and Score) and the \'Sample\' column ',
                  'must match the colnames of \'Feature Set\'. ',
                  'Otherwise, \'Input Score\' must be a list of  ',
                  'vectors and have names or labels that match the ',
                  'colnames of the \'Feature Set\'.\">?</a>')),
                choices = c("csv", "rds"),
                selected = "csv",
                inline = TRUE
              )
            ),

            # min_cutoff ####
            numericInput(
              inputId = ns("min_cutoff"),
              label = HTML(paste0(
                '<strong>Min event frequency (n)</strong> ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"Minimum number of \'occurrences\' a feature ',
                '(e.g., a mutation) must have to be included in the ',
                '\'Feature Set\`. Features with fewer events than the ',
                'specified number will be removed.\n\nNOTE: \'Min event ',
                'frequency\' must be >= 5.\">?</a>')),
              value = 5,
              min = 5,
              max = Inf,
              step = 1,
              width = "100%"
            ),

            # max_cutoff ####
            numericInput(
              inputId = ns("max_cutoff"),
              label = HTML(paste0(
                '<strong>Max event frequency (%)</strong> ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"Maximum number (expressed as % of total) of ',
                '\'occurrences\' a feature (e.g., a mutation) can have ',
                'to be included in the \'Feature Set\`. Features with a ',
                'higher percentage of events than the specified number ',
                'will be removed.\n\nNOTE: \'Max event frequency\' must ',
                'be <= 90.\">?</a>')),
              value = 60,
              min = 0,
              max = 100,
              step = 1,
              width = "100%"
            ),

            # method ####
            selectInput(
              inputId = ns("method"),
              label = "Scoring method",
              choices = scoring_methods,
              selected = "ks", 
              width = "100%"
            ),
            
            # weighted KS ####
            conditionalPanel(
              condition = sprintf("input['%s'] == 'ks_pval' ||
                                   input['%s'] == 'ks_score'", 
                                  ns("method"), ns("method")),
              checkboxInput(
                inputId = ns("weighted_ks"),
                label = HTML(paste0(
                  'Compute weighted-KS ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"Whether or not to compute a weighted KS test.\">?</a>')),
                value = FALSE
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("weighted_ks")),
                
                fileInput(
                  inputId = ns("weight_file"),
                  label = strong(span(style = "color: red;", "*"),
                                 "Choose a weight file"),
                  width = "100%"
                ),
                
                radioButtons(
                  inputId = ns("weight_file_type"),
                  label = HTML(paste0(
                    'File type ',
                    '<a class="tooltip-txt" data-html="true" ',
                    'data-tooltip-toggle="tooltip" ',
                    'data-placement="top" title=\"NOTE: ',
                    'If file is in csv format, then ',
                    'the \'Weights\' file must be a data frame ',
                    'with two columns (Sample and Weights) and ',
                    'the \'Sample\' column must match the colnames of ',
                    '\'Feature Set\'. Otherwise, \'Weights\' in RDS file',
                    'must contain a list of vectors and have names or ',
                    'labels that match the colnames of \'Feature Set\'.\">?</a>')),
                  choices=c("csv", "rds"),
                  selected = "csv",
                  inline = TRUE
                )
              )
            ),
            
            # method alternative ####
            conditionalPanel(
              condition = sprintf("input['%s'] == 'ks_pval' || 
                                   input['%s'] == 'ks_score' ||
                                   input['%s'] == 'wilcox_pval' ||
                                   input['%s'] == 'wilcox_score'",
                                  ns("method"), ns("method"), ns("method"), ns("method")),
              selectInput(
                inputId = ns("method_alternative"),
                label = "Method alternative",
                choices = c("less"="less", "two-sided"="two.sided", "greater"="greater"),
                selected = "less", width = "100%"
              )
            ),
            
            # search_method ####
            radioButtons(
              inputId = ns("search_method"),
              label = "Search method",
              choices=c("Forward and Backward"="both",
                        "Forward Only"="forward"),
              selected = "both", inline = FALSE
            ),
            
            # max_size ####
            numericInput(
              inputId = ns("max_size"),
              label = HTML(paste0(
                '<span style=\"color:red;\">*</span> Max meta-feature size ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"Max possible number of features to be ',
                'included in the meta-feature (search will stop ',
                'after max is reached)\">?</a>')),
              min = 1,
              max = 100,
              step = 1,
              value = 7,
              width = "100%"
            ),

            # initial seeds ####
            radioButtons(
              inputId = ns("initial_seed"),
              label = HTML(paste0(
                'Search modality ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"\'Top N\' repeats the search starting from ',
                'each of the top N scoring features. \'Custom seeds\' ',
                'repeats the search starting from each of the custom ',
                'seeds. WARNING: If number of seeds specified is greater ',
                'than 10, this may result in a longer search time.\">?</a>')),
              choices = c("Top N seeds", "Custom seeds"),
              selected = "Top N seeds",
              inline = TRUE
            ),

            # top_N ####
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Top N seeds'",
                                  ns("initial_seed")),
              numericInput(
                inputId = ns("top_N"),
                label = NULL,
                min = 1,
                max = 100,
                step = 1,
                value = 3,
                width = "100%"
              )
            ),

            # search_start ####
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Custom seeds'",
                                  ns("initial_seed")),

              textAreaInput(
                inputId = ns("search_start"),
                label = strong(span(style = "color:red;", "*"),
                               paste0('Enter a list of features (separated by ',
                                      'a commas) corresponding to row names ',
                                      'of Feature Set object to start the ',
                                      'search with.')),
                value="",
                width="100%"
              )
            ),

            # permutation_test ####
            checkboxInput(
              inputId = ns("permutation_test"),
              label = HTML(paste0(
                'Perform permutation-based testing ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"Whether or not to perform permutation significance test.\">?</a>')),
              value = FALSE
            ),

            # n_perm ####
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("permutation_test")),
              
              # permutation-based alternative type
              selectInput(
                inputId = ns("perm_alternative"),
                label = HTML(paste0(
                  'Permutation-based alternative ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"An alternative hypothesis for calculating 
                  permutation-based p-value\">?</a>')),
                choices = c("one-sided"="one.sided", "two-sided"="two.sided"),
                selected = "one.sided", width = "100%"
              ),
              
              numericInput(
                inputId = ns("n_perm"),
                label = strong(span(style="color:red;", "*"),
                               paste0("Number of permutations to perform")),
                min = 1,
                max = Inf,
                step = 1,
                value = 100,
                width = "100%"
              ),

              # ncores ####
              numericInput(
                inputId = ns("ncores"),
                label = strong(span(style="color:red;", "*"),
                               paste0("Number of cores to perform parallelized calculation for permutation tests")),
                min = 1,
                max = Inf,
                step = 1,
                value = 1,
                width = "100%"
              ),
              
              # cache ####
              checkboxInput(
                inputId = ns("cache"),
                label = HTML(paste0(
                  'Use caching for future loading ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"If cache = TRUE, the cache path is set to ~/.Rcache
                  for future loading.\">?</a>')),
                value = TRUE,
                width = "100%"
              )
            ),

            br(),

            uiOutput(outputId = ns("cadra_error_message")),

            br(),

            actionButton(
              inputId = ns("run_cadra"),
              label = strong("RUN"),
              style="background: blue; color: white;"
            ),

            actionButton(
              inputId = ns("stop_cadra"),
              label = strong("STOP"),
              style="background: blue; color: white;"
            ),

            br(), br(), br(), br(),
            
            HTML(
              paste0(
                "<div style='position: absolute; width: 98%; bottom: 0px;'>",
                "<p style='text-align: center;'>",
                "<span class='footer-info'>&copy; Monti Lab &diams; ",
                "<script>document.write(new Date().getFullYear());</script> ",
                "&diams; All Rights Reserved.",
                "</span>", 
                "</p>",
                "</div>"
              )
            )
          ),
          
          ##### RUN CADRA #######
          column(
            width = 9,

            uiOutput(outputId = ns("cadra_instructions")),
            
            div(
              id = ns("loading_icon"), class = "loading_div", style="display: none;",
              span(
                div(class = "loader"),
                br(),
                p(class = "loading_text", "Running Candidate Search...")
              )
            ),

            div(
              uiOutput(outputId = ns("featureData_title"))
            ),

            div(
              uiOutput(outputId = ns("inputScoreData_title")),
              DT::dataTableOutput(outputId = ns("inputScoreData"))
            ),

            div(
              uiOutput(outputId = ns("bestFeatureData_title")),
              DT::dataTableOutput(outputId = ns("bestFeatureData"))
            ),

            div(
              uiOutput(outputId = ns("meta_plot_title")),
              plotOutput(outputId = ns("meta_plot"))
            ),

            div(
              uiOutput(outputId = ns("topn_plot_title")),
              plotOutput(outputId = ns("topn_plot"))
            ),

            div(
              id = ns("permutation_loading_icon"), class = "loading_div", style="display: none;",
              span(
                div(class = "loader"),
                br(),
                p(class = "loading_text", "Running Permutation-based Testing...")
              )
            ),

            div(
              uiOutput(outputId = ns("permutation_plot_title")),
              plotOutput(outputId = ns("permutation_plot"))
            )
          )
        ),

        ##### GSVA OPTIONS #######
        tabPanel(
          title = "Run GSVA",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "running", lib = "font-awesome"),

          column(
            width = 3,
            class = "side-bar-options",

            h3("GSVA Options", style="text-align: center;"),

            br(),

            selectizeInput(
              inputId = ns("gsva_gene_expression"),
              label = "Gene expression",
              choices = c(gsva_gene_expression_path, "Import Data"),
              width = "600px"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("gsva_gene_expression")),
              
              fileInput(
                inputId = ns("gsva_gene_expression_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Gene expression file"),
                width = "600px"
              ),
              
              radioButtons(
                inputId = ns("gsva_gene_expression_file_type"),
                label = HTML(paste0(
                  'File type ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement=',
                  '"top" title=\"NOTE: If file is in csv format, ',
                  'the \'Feature Set\' must be a data ',
                  'frame including a \'Feature\' column name ',
                  'that contains unique names or labels to ',
                  'search for best features. Otherwise, \'Feature ',
                  'Set\' must be an object of class SummarizedExperiment ',
                  'from SummarizedExperiment package.\">?</a>')),
                choices = c("csv", "rds"),
                selected = "csv",
                inline = TRUE,
                width = "600px"
              )
            ),
            
            br(),
            
            selectizeInput(
              inputId = ns("gsva_feature_set"),
              label = "Associated feature set",
              choices = "",
              width = "600px"
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("gsva_feature_set")),
              
              fileInput(
                inputId = ns("gsva_feature_set_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Feature set file"),
                width = "600px"
              ),
              
              radioButtons(
                inputId = ns("gsva_feature_set_file_type"),
                label = HTML(paste0(
                  'File type ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement=',
                  '"top" title=\"NOTE: If file is in csv format, ',
                  'the \'Feature Set\' must be a data ',
                  'frame including a \'Feature\' column name ',
                  'that contains unique names or labels to ',
                  'search for best features. Otherwise, \'Feature ',
                  'Set\' must be an object of class SummarizedExperiment ',
                  'from SummarizedExperiment package.\">?</a>')),
                choices = c("csv", "rds"),
                selected = "csv",
                inline = TRUE,
                width = "600px"
              )
            ),
            
            br(),

            fileInput(
              inputId = ns("gsva_geneset_file"),
              label = strong(span(style = "color: red;", "*"),
                             "Choose a geneset file to import"),
              width = "600px"
            ),

            radioButtons(
              inputId = ns("gsva_geneset_file_type"),
              label = HTML(paste0(
                'File type ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement=',
                '"top" title=\"NOTE: If file is in csv format, ',
                'the \'Feature Set\' must be a data ',
                'frame including a \'Feature\' column name ',
                'that contains unique names or labels to ',
                'search for best features. Otherwise, \'Feature ',
                'Set\' must be an object of class SummarizedExperiment ',
                'from SummarizedExperiment package.\">?</a>')),
              choices = c("csv", "gmt"),
              selected = "csv",
              inline = TRUE,
              width = "600px"
            ),

            br(),

            uiOutput(outputId = ns("gsva_cadra_error_message")),

            br(),

            actionButton(
              inputId = ns("run_gsva"),
              label = strong("RUN"),
              style="background: blue; color: white;"
            ),

            actionButton(
              inputId = ns("stop_gsva"),
              label = strong("STOP"),
              style="background: blue; color: white;"
            ),

            br(), br(), br(), br(),
            
            HTML(
              paste0(
                "<div style='position: absolute; width: 98%; bottom: 0px;'>",
                "<p style='text-align: center;'>",
                "<span class='footer-info'>&copy; Monti Lab &diams; ",
                "<script>document.write(new Date().getFullYear());",
                "</script> &diams; All Rights Reserved.</span></p></div>"
              )
            )
          ),

          ##### RUN GSVA #######
          column(
            width=9,

            uiOutput(outputId = ns("gsva_instructions")),
            
            div(
              id = ns("gsva_loading_icon"), class = "loading_div", style="display: none;",
              span(
                div(class = "loader"),
                br(),
                p(class = "loading_text", "Running GSVA...")
              )
            ),

            div(
              uiOutput(outputId = ns("gsva_enrichment_title")),
              DT::dataTableOutput(outputId = ns("gsva_enrichment_scores"))
            ),
            
            br(), br(),

            div(
              uiOutput(outputId = ns("add_enrichment_scores")),
            )

          )
        ),

        ##### DOWNLOAD DATASET #######
        tabPanel(
          title = "Download",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "database", lib = "font-awesome"),

          column(
            width = 4,
            class = "side-bar-options",

            h2("Download Feature Set"),

            selectizeInput(
              inputId = ns("download_fs_options"),
              label = NULL,
              choices = cadra_feature_set_path,
              width = "600px"
            ),

            selectInput(
              inputId = ns("download_fs_type"),
              label = "Type of Data to Download",
              choices = c("Feature Set", "Sample Names", "Feature Names"),
              width = "600px"
            ),

            div(
              id = ns("input_score_dl"), style="display: none;",
              checkboxInput(
                inputId = ns("include_scores"),
                label = HTML(paste0(
                  'Include Input Score ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"Whether to download \'Input Score\' that is associated with \'Feature Set\'\">?</a>')),
                value = FALSE
              )
            ),

            div(
              id = ns("gene_expression_dl"), style="display: none;",
              checkboxInput(
                inputId = ns("include_gene_expression"),
                label = HTML(paste0(
                  'Include Gene Expression Set ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"Whether to download \'Gene Expression Set\' that is associated with \'Feature Set\'\">?</a>')),
                value = FALSE
              )
            ),

            downloadButton(outputId = ns("download_fs"), label="Download", icon=icon("download"), style="background: blue; color: white;")

          )
        ),

        ##### PUBLICATION TAB #######
        tabPanel(
          title = "Publication",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "book", lib = "font-awesome"),

          h2("Citation"),
          p("Kartha VK, Kern JG, Sebastiani P, Zhang L, Varelas X, Monti S (2019) CaDrA: A computational framework for performing candidate driver analyses using genomic features.", a(href="https://www.frontiersin.org/articles/10.3389/fgene.2019.00121/full", "{Frontiers in Genetics}")),

          h2("Github"),
          p("Chau R, Bulekova K, Kartha V, Monti S (2022). CaDrA: Candidate Driver Analysis. R package version 2.0.0, ", a(target="_blank", href="https://github.com/montilab/CaDrA/", "https://github.com/montilab/CaDrA/"), "."),

          tags$pre(
            tags$code(
              "
              @Manual{,
                title = {CaDrA: Candidate Driver Analysis},
                author = {Reina Chau and Katia Bulekova and Vinay Kartha and Stefano Monti},
                year = {2022},
                note = {R package version 2.0.0},
                url = {https://github.com/montilab/CaDrA/},
              }
              "
            )
          ),
          
          h2("Acknowledgements"),
          
          HTML(
            paste0(
              "<p>",
              "This project is funded in part by the ",
              "<a href='https://www.nidcr.nih.gov/'>NIH/NIDCR</a> ",
              "(3R01DE030350-01A1S1, R01DE031831), ", 
              "<a href='https://findthecausebcf.org'>", 
              "Find the Cause Breast Cancer Foundation</a>, and ", 
              "<a href='https://findthecausebcf.org'>NIH/NIA</a> ",
              "(UH3 AG064704).",
              "</p>"
            )
          )
        ),

        ##### CONTRACT TAB #######
        tabPanel(
          title = "Contact Us",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "envelope", lib = "font-awesome"),

          h2("Developers"),

          p(a(href="mailto:rchau88@bu.edu", strong("Reina Chau")), em(". Author, maintainer.")),

          p(a(href="mailto:ktrn@bu.edu", strong("Katia Bulekova")), em(". Author")),

          p(a(href="mailto:vkartha@bu.edu", strong("Vinay Kartha")), em(". Author.")),

          p(a(href="mailto:smonti@bu.edu", strong("Stefano Monti")), em(". Author.")),
          
          br()
          
        )
      )
    )
  )
  
}

#' Shiny Server modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist A data frame or path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist must contains the following variables:
#' 'feature_set' (required), 'feature_set_path' (repuired), 
#' 'input_score_name' (required), input_score_path' (required), 
#' 'gene_expression_name' (optional), 'gene_expression_path' (optional)
#' 
#' Default is NULL. If NULL, the app will start with DEFAULT dataset included 
#' in CaDrA package.
#' 
#' @return Shiny server elements
#'
#' @examples
#' 
#' # Load R library
#' library(shiny)
#' 
#' # Create ui and server for Shiny app
#' id <- "myapp"
#' 
#' ui <- shiny::fluidPage(
#'    CaDrA.shiny::CaDrA_UI(id = id)
#' )
#' 
#' server <- function(input, output, session) {
#'    CaDrA.shiny::CaDrA_Server(id = id)
#' }
#' 
#' app <-  shiny::shinyApp(ui=ui, server=server)
#' 
#' # Launch and deploy Shiny app (NOT RUN)
#' # shiny::runApp(app, host='0.0.0.0', port=3838)
#'  
#' @import CaDrA dplyr DT htmltools parallel tibble tools utils
#' @importFrom GSVA gsva
#' @importFrom GSEABase getGmt
#' @rawNamespace import(methods, except = c(removeClass, show))
#' @rawNamespace import(SummarizedExperiment, except = c(show))
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' 
#' @export 
CaDrA_Server <- function(id, datalist=NULL){
  
  shiny::addResourcePath(
    "vignettes",
    system.file("vignettes", package="CaDrA.shiny")
  )
  
  rmarkdown::render(
    input = system.file("vignettes/create-cadra-instructions.Rmd", package="CaDrA.shiny"),
    output_format = "html_document",
    quiet = TRUE
  )
  
  rmarkdown::render(
    input = system.file("vignettes/create-gsva-instructions.Rmd", package="CaDrA.shiny"),
    output_format = "html_document",
    quiet = TRUE
  )
  
  shiny::moduleServer(
    id,
    function(input, output, session){
      
      ## Extract extdata ####
      extdata <- shiny::reactiveVal( get_extdata(datalist=datalist) )
      
      ## Detect number of cores on machine ####
      num_of_cores <- parallel::detectCores()

      ## Create reactive values for CaDrA search ####
      rVal <- shiny::reactiveValues()
      rVal$candidate_search_process <- NULL
      rVal$candidate_search_obs <- NULL
      rVal$cadra_permutation_process <- NULL
      rVal$cadra_permutation_obs <- NULL
      feature_set_description <- shiny::reactiveVal()
      feature_set_data <- shiny::reactiveVal()
      input_score_data <- shiny::reactiveVal()
      instructions_message <- shiny::reactiveVal(TRUE)
      cadra_error_message <- shiny::reactiveVal()

      ## Create reactive values for GSVA analysis ####
      gVal <- shiny::reactiveValues()
      gVal$gsva_search_process <- NULL
      gVal$gsva_search_obs <- NULL
      gsva_instructions_message <- shiny::reactiveVal(TRUE)
      gsva_cadra_error_message <- shiny::reactiveVal()
      enrichment_table_message <- shiny::reactiveVal()

      # Prevent Shiny from grayed out
      autoInvalidate <- shiny::reactiveTimer(10000)
      
      # Observe the auto invalidate timer
      shiny::observe({
        autoInvalidate()
        cat(".")
      })
      
      ## Output instructions message for running CaDrA search ####
      output$cadra_instructions <- shiny::renderUI({

        req(instructions_message())
        
        tags$iframe(src="vignettes/create-cadra-instructions.html", onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));', style="height:200px;width:100%;border:none;overflow:hidden;")
        
      })

      ## Updates feature set and input score choices for running CaDrA search ####
      observeEvent({
        input$feature_set
        extdata()
      }, {
        
        ns <- session$ns
        
        selected_fs <- isolate({ input$feature_set })
        
        if(selected_fs != "Import Data"){

          input_score_data <- extdata() %>% dplyr::filter(feature_set_path == selected_fs) %>% dplyr::distinct(input_score_path, .keep_all=TRUE)
          input_score_selection <- input_score_data$input_score_path
          names(input_score_selection) <- input_score_data$input_score_name

          if(all(is.na(input_score_selection)) || all(input_score_selection == "")){
            updateSelectizeInput(session, inputId = "input_score", choices = "Import Data")
          }else{
            input_score_selection <- input_score_selection[which(!is.na(input_score_selection) & input_score_selection != "")]
            updateSelectizeInput(session, inputId = "input_score", choices = c(input_score_selection, "Import Data"), selected = input_score_selection[1])
          }

        }else{

          updateSelectizeInput(session, inputId = "input_score", choices = "Import Data")

        }

      })
      
      
      ## Updates feature set and gene expression set choices for running GSVA analysis ####
      observeEvent({
        input$gsva_gene_expression
      }, {
        
        ns <- session$ns
        
        selected_fs <- isolate({ input$gsva_gene_expression })
        
        if(selected_fs != "Import Data"){
          
          feature_set_data <- extdata() %>% 
            dplyr::filter(gene_expression_path == selected_fs) %>% 
            dplyr::distinct(feature_set_path, .keep_all=TRUE)
          
          feature_set_selection <- feature_set_data$feature_set_path
          names(feature_set_selection) <- feature_set_data$feature_set_name
          
          if(all(is.na(feature_set_selection)) || all(feature_set_selection == "")){
            updateSelectizeInput(session, inputId = "gsva_feature_set", choices = "Import Data")
          }else{
            feature_set_selection <- feature_set_selection[which(!is.na(feature_set_selection) & feature_set_selection != "")]
            updateSelectizeInput(session, inputId = "gsva_feature_set", choices = c(feature_set_selection, "Import Data"), selected = feature_set_selection[1])
          }
          
        }else{
          
          updateSelectizeInput(session, inputId = "gsva_feature_set", choices = "Import Data")

        }
        
      })
      
      ## Updates feature set, input score, and gene expression set choices for downloading dataset ####
      observeEvent({
        input$download_fs_options
        extdata()
      }, {

        ns <- session$ns

        selected_fs <- isolate({ input$download_fs_options })

        input_score_data <- extdata() %>% dplyr::filter(feature_set_path == selected_fs) %>% dplyr::distinct(input_score_path, .keep_all=TRUE)
        input_score_selection <- input_score_data$input_score_path

        gene_expression_data <- extdata() %>% dplyr::filter(feature_set_path == selected_fs) %>% dplyr::distinct(gene_expression_path, .keep_all=TRUE)
        gene_expression_selection <- gene_expression_data$gene_expression_path

        #print(selected_fs); print(input_score_selection); print(gene_expression_selection);

        if(all(is.na(input_score_selection)) || all(input_score_selection == "")){
          ## Hide input score checkbox
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("input_score_dl"), display="no"))
          updateCheckboxInput(session, inputId = "include_scores", value=FALSE)
        }else{
          ## Show input score checkbox
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("input_score_dl"), display="yes"))
          updateCheckboxInput(session, inputId = "include_scores", value=TRUE)
        }

        if(all(is.na(gene_expression_selection)) || all(gene_expression_selection == "")){
          ## Hide gene expression checkbox
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gene_expression_dl"), display="no"))
          updateCheckboxInput(session, inputId = "include_gene_expression", value=FALSE)
        }else{
          ## Show gene expression checkbox
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gene_expression_dl"), display="yes"))
          updateCheckboxInput(session, inputId = "include_gene_expression", value=TRUE)
        }

      })

      ## Download feature set handler ####
      output$download_fs <- downloadHandler(

        filename = function() {
          selected_fs <- isolate({ input$download_fs_options })
          type <- isolate({ input$download_fs_type })
          filename <- extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)] %>% unique()
          paste0(filename, ".rds")
        },

        content = function(file) {
          selected_fs <- isolate({ input$download_fs_options })
          type <- isolate({ input$download_fs_type })
          include_scores <- isolate({ input$include_scores })
          include_gene_expression <- isolate({ input$include_gene_expression })

          # print(selected_fs); print(type); print(include_scores); print(include_gene_expression);

          if(tools::file_ext(selected_fs) == "rda"){
            envir_name <- base::load(selected_fs)
            FS <- base::get(envir_name)
          }else{
            FS <- base::readRDS(selected_fs)
          }

          dl_data <- list()

          if(type == "Feature Set"){
            dl_data <- c(dl_data, list(Feature_Set=FS))
          }else if(type == "Sample_Names"){
            dl_data <- c(dl_data, list(sample_names=colnames(FS)))
          }else if(type == "Feature_Names"){
            dl_data <- c(dl_data, list(feature_names=rownames(FS)))
          }

          if(include_scores){

            data <- extdata() %>% dplyr::filter(feature_set_path == selected_fs & !is.na(input_score_path) & input_score_path != "") %>% dplyr::distinct(input_score_path, .keep_all=TRUE)
            input_score_path <- data$input_score_path
            input_score_name <- data$input_score_name

            for(d in 1:length(input_score_path)){
              #d=1;
              if(tools::file_ext(input_score_path[d]) == "rda"){
                envir_name <- base::load(input_score_path[d])
                input_score <- base::get(envir_name)
              }else{
                input_score <- base::readRDS(input_score_path[d])
              }
              dl_data_names <- c(names(dl_data), input_score_name[d])
              dl_data <- c(dl_data, list(input_score))
              names(dl_data) <- dl_data_names
            }

          }

          if(include_gene_expression){

            data <- extdata() %>% dplyr::filter(feature_set_path == selected_fs & !is.na(gene_expression_path) & gene_expression_path != "") %>% dplyr::distinct(gene_expression_path, .keep_all=TRUE)
            gene_expression_path <- data$gene_expression_path
            gene_expression_name <- data$gene_expression_name

            for(d in 1:length(gene_expression_path)){
              #d=1;
              #print(gene_expression_path[d])
              if(tools::file_ext(gene_expression_path[d]) == "rda"){
                envir_name <- base::load(gene_expression_path[d])
                ge_set <- base::get(envir_name)
              }else{
                ge_set <- base::readRDS(gene_expression_path[d])
              }
              dl_data_names <- c(names(dl_data), gene_expression_name[d])
              dl_data <- c(dl_data, list(gene_expression=ge_set))
              names(dl_data) <- dl_data_names
            }

          }

          saveRDS(dl_data, file)

        }
      )

      #
      # START CADRA SEARCH ####
      #

      observeEvent(input$run_cadra, {

        ns <- session$ns

        if(!is.null(rVal$candidate_search_process)) return(NULL)

        rVal$candidate_search_result <- NULL
        rVal$cadra_permutation_result <- NULL
        feature_set_description(NULL)
        feature_set_data(NULL)
        input_score_data(NULL)
        instructions_message(FALSE)
        cadra_error_message(NULL)

        ## Show cadra loading icon
        session$sendCustomMessage(type="ToggleOperation", message=list(id=ns("loading_icon"), display="yes"))

        ## Get feature set ####
        feature_set <- isolate({ input$feature_set })

        if(feature_set == "Import Data"){

          inputfile <- input$ES_file;
          inputtype <- input$ES_file_type;

          if(is.null(inputfile)){
            cadra_error_message("Please choose a 'Feature Set' file to import.")
            return(NULL)
          }

          file_extension <-  tools::file_ext(inputfile$datapath)

          if(inputtype == "csv" && file_extension == "csv"){

            FS <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

            if("Feature" %in% colnames(FS)){
              FS <- FS %>% tibble::column_to_rownames(var="Feature") %>%
                dplyr::mutate_all(as.numeric) %>% 
                as.matrix()
            }else{
              cadra_error_message("The 'Feature Set' file must contain a 'Feature' column name which contains unique names or labels for the features.")
              return(NULL)
            }

          }else if(inputtype == "rds" && file_extension == "rds"){
            
            FS <- base::readRDS(inputfile$datapath)
            
          }else{
            
            cadra_error_message("Incorrect file format. Please check your 'Feature Set' file again.")
            return(NULL)
            
          }
          
        }else{
          
          if(tools::file_ext(feature_set) == "rda"){
            envir_name <- base::load(feature_set)
            FS <- base::get(envir_name)
          }else if(tools::file_ext(feature_set) == "rds"){
            FS <- base::readRDS(feature_set)
          }else if(tools::file_ext(feature_set) == "csv"){
            FS <- utils::read.csv(feature_set, header=TRUE, check.names=FALSE)
            if("Feature" %in% colnames(FS)){
              FS <- FS %>% tibble::column_to_rownames(var="Feature") %>%
                dplyr::mutate_all(as.numeric) %>% 
                as.matrix()
            }else{
              cadra_error_message("The 'Feature Set' file must contain a 'Feature' column name which contains unique names or labels for the features.")
              return(NULL)
            }
          }
          
        }

        ## Get input score ####
        input_score <- isolate({ input$input_score })

        if(input_score == "Import Data"){

          inputfile <- input$input_score_file;
          inputtype <- input$input_score_file_type;

          if(is.null(inputfile)){
            cadra_error_message("Please choose a 'Input Score' file to import.")
            return(NULL)
          }

          file_extension <-  tools::file_ext(inputfile$datapath)

          if(inputtype == "csv" && file_extension == "csv"){

            dat <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

            if(all(c("Sample", "Score") %in% colnames(dat))){
              input_score <- as.numeric(dat$Score)
              names(input_score) <- as.character(dat$Sample)
            }else{
              cadra_error_message("The 'Input Score' file must be a data frame with two columns: Sample and Score.")
              return(NULL)
            }

          }else if(inputtype == "rds" && file_extension == "rds"){

            input_score <- base::readRDS(inputfile$datapath)

          }else{

            cadra_error_message("Incorrect file format. Please check your 'Input Score' file again.")
            return(NULL)

          }

        }else{

          if(tools::file_ext(input_score) == "rda"){
            envir_name <- base::load(input_score)
            input_score <- base::get(envir_name)
          }else if(tools::file_ext(input_score) == "rds"){
            input_score <- base::readRDS(input_score)
          }else if(tools::file_ext(input_score) == "csv"){
            dat <- utils::read.csv(input_score, header = TRUE, check.names = FALSE)
            if(all(c("Sample", "Score") %in% colnames(dat))){
              input_score <- as.numeric(dat$Score)
              names(input_score) <- as.character(dat$Sample)
            }else{
              cadra_error_message("The 'Input Score' file must be a data frame with two columns: Sample and Score.")
              return(NULL)
            }
          }
          
        }
        
        # Obtain minimum cutoff parameter ####
        min_event_cutoff <- as.integer(input$min_cutoff)

        if(is.na(min_event_cutoff) || length(min_event_cutoff)==0 || min_event_cutoff < 5){
          cadra_error_message("Please specify an integer value for Min Event Frequency >= 5 \n")
          return(NULL)
        }else{
          if(min_event_cutoff > ncol(FS)){
            cadra_error_message(sprintf("There are not enough samples in \'Feature Set\' to meet Min Event Frequency = %s \n", min_event_cutoff))
            return(NULL)
          }
          min_cutoff <- round(min_event_cutoff/ncol(FS), 2)
        }
        

        # Obtain maximum cutoff parameter ####
        max_event_cutoff <- as.numeric(input$max_cutoff)
        
        if(is.na(max_event_cutoff) || length(max_event_cutoff)==0 || max_event_cutoff <= 0 || max_event_cutoff > 90){
          cadra_error_message("Please specify a Max Event Frequency between 1 and 90 \n")
          return(NULL)
        }else{
          max_cutoff <- round(max_event_cutoff/100, 2)
        }

        ## Keep a record of the number of features in original FS
        n_orig_features <- nrow(FS)
        
        ## Pre-filter FS based on occurrence frequency ####
        FS <- tryCatch({
          CaDrA::prefilter_data(
            FS = FS,
            min_cutoff = min_cutoff,
            max_cutoff = max_cutoff
          )
        }, error = function(e){
          cadra_error_message(e)
          return(NULL)
        })
        
        ## Samples to keep based on the overlap between the two inputs
        sample_overlap <- intersect(names(input_score), colnames(FS))
        input_score <- input_score[sample_overlap]
        FS <- FS[, sample_overlap, drop = FALSE]
        
        # Retrieve the binary feature matrix
        if(is(FS, "SummarizedExperiment")){
          FS_mat <- SummarizedExperiment::assay(FS)
        }else{
          FS_mat <- FS
        }
        
        # Check data inputs
        tryCatch({
          CaDrA:::check_data_input(
            FS_mat = FS_mat,
            input_score = input_score
          )
        }, error = function(e){
          cadra_error_message(e)
          return(NULL)
        })
 
        # Get scoring method ####
        method <- input$method
        
        # Whether to perform a weighted-KS test ####
        if(method == "ks_pval" || method == "ks_score"){
          
          if(input$weighted_ks == TRUE){

            inputfile <- input$weight_file;
            inputtype <- input$weight_file_type;

            if(is.null(inputfile)){
              cadra_error_message("Please choose a 'Weighted KS' file to import.")
              return(NULL)
            }

            file_extension <-  tools::file_ext(inputfile$datapath)
            
            if(inputtype == "csv" && file_extension == "csv"){

              dat <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

              if(all(c("Sample", "Weight") %in% colnames(dat))){
                weights <- as.numeric(dat$Weight)
                names(weights) <- as.character(dat$Sample)
              }else{
                cadra_error_message("The 'Weighted KS' file must be a data.frame with two columns: Sample and Weight.")
                return(NULL)
              }

            }else if(inputtype == "rds" && file_extension == "rds"){

              weights <- base::readRDS(inputfile$datapath)

            }else{

              cadra_error_message("Incorrect file format. Please check your 'Weighted KS' file again.")
              return(NULL)

            }

            # Check weights is provided and are continuous values with no NAs
            if(length(weights) == 0 || any(!is.numeric(weights)) || any(is.na(weights))){
              cadra_error_message("weights must be a vector of continous values (with no NAs) with the vector names matching the colnames of the \'Feature Set\'.\n")
              return(NULL)
            }

            # Make sure the weights has names or labels that are the same as the colnames of FS
            if(is.null(names(weights))){
              cadra_error_message("The weights object must have names or labels to track the samples by. Please provide unique sample names or labels that match the colnames of the \'Feature Set\'.\n")
              return(NULL)
            }

            # Make sure the weights has the same length as number of samples in FS
            if(length(weights) != ncol(FS)){

              cadra_error_message("The weights must have the same length as the number of columns in the \'Feature Set\'.\n")
              return(NULL)

            }else{

              if(any(!names(weights) %in% colnames(FS))) {
                cadra_error_message("The weights object must have names or labels that match the colnames of the \'Feature Set\'.\n")
                return(NULL)
              }

              # Match colnames of Feature Set with names of provided weights
              weights <- weights[colnames(FS)]

            }
          } else {

            weights <- NULL

          }
        }

        ## Get alternative hypothesis from a given scoring method ####
        method_alternative <- input$method_alternative

        ## Get search method ####
        search_method <- input$search_method;

        ## Get max size ####
        max_size <- as.integer(input$max_size)

        if(is.na(max_size) || length(max_size) == 0 || max_size <= 0){
          cadra_error_message("Please specify an integer value specifies a maximum size that a meta-feature can extend to do for a given search (max_size must be >= 1).\n")
          return(NULL)
        }

        if(max_size > nrow(FS)){
          cadra_error_message("Please specify a \'Max meta-feature size\' lesser than the number of features in the Feature Set.\n")
          return(NULL)
        }

        ## Get search start ####
        initial_seed <- input$initial_seed

        if(initial_seed == "Top N seeds"){

          search_start <- NULL
          top_N <- as.integer(input$top_N)

          if(is.na(top_N) || length(top_N) == 0 || top_N <= 0){
            cadra_error_message("Please specify an integer value to evaluate over Top N seeds (top_N must be >= 1).\n")
            return(NULL)
          }

          if(top_N > nrow(FS)){
            cadra_error_message("Please specify a Top N seeds lesser than the number of features in the Feature Set.\n")
            return(NULL)
          }

        }else if(initial_seed == "Custom seeds"){

          search_start <- strsplit(as.character(input$search_start), ",", fixed=TRUE) %>% unlist() %>% trimws()
          top_N <- NULL

          if(length(search_start) == 0 || any(!search_start %in% rownames(FS))){
            if(length(search_start) == 0){
              msg <- "There are no custom seeds provided.\n"
            }else{
              msg <- paste0(
                "The provided custom seeds: ",
                paste0(search_start[which(!search_start %in% rownames(FS))], collapse=", "),
                " do(es) not exist among the row names of FS object.\n"
              )
            }
            cadra_error_message(msg)
            return(NULL)
          }
          
        }

        ## Whether or not to perform permutation test ####
        permutation <- input$permutation_test

        if(permutation == TRUE){

          ## show permutation loading icon
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("permutation_loading_icon"), display="yes"))

          n_perm <- as.integer(input$n_perm)

          if(is.na(n_perm) || length(n_perm)==0 || n_perm <= 0){
            cadra_error_message("Please specify an INTEGER number of permutations (n_perm must be >= 1).\n")
            return(NULL)
          }

          ## Get the number of cores ####
          ncores <- as.integer(input$ncores)

          if(is.na(ncores) || length(ncores)==0 || ncores <= 0){
            cadra_error_message("Please specify the number of parallelization cores for permutation testing (ncores must be >= 1).\n")
            return(NULL)
          }

          if(ncores > num_of_cores){
            cadra_error_message(paste0("There are ONLY ", num_of_cores, " cores available on the system. Please specify the number of parallelization cores for permutation testing (ncores <= ", num_of_cores, ")."))
            return(NULL)
          }
          
          # Get caching option
          cache <- input$cache
          
          ## Get the permutation-based alternative ####
          perm_alternative <- as.character(input$perm_alternative)

          ## Perform permutation-based testings ####
          rVal$cadra_permutation_process <- parallel::mcparallel({

            CaDrA::CaDrA(
              FS = FS,
              input_score = input_score,
              method = method,
              method_alternative = method_alternative,
              custom_function = NULL,
              custom_parameters = NULL,
              weights = weights,
              top_N = top_N,
              search_start = search_start,
              search_method = search_method,
              max_size = max_size,
              n_perm = n_perm,
              perm_alternative = perm_alternative,
              smooth = TRUE,
              obs_best_score = NULL,
              plot = FALSE,
              ncores = ncores,
              cache = cache,
              cache_path = NULL
            )

          })

          print(paste0("cadra permutation process: ", rVal$cadra_permutation_process$pid, " started"))

        }

        # Export the FS and input_score to reactive datase
        feature_set_description(
          sprintf(
            paste0(
              'After filtering features with Min Event Frequency = %s ',
              '(or having < %s%% prevalence across all samples) and Max ',
              'Event Frequency = %s (or having > %s%% prevalance across ',
              'all samples), the \'Feature Set\' retained %s genomic ',
              'features out of %s supplied features across %s samples.'),
            min_event_cutoff,
            min_cutoff*100,
            max_event_cutoff,
            max_cutoff*100,
            format(nrow(FS), big.mark=","),
            format(n_orig_features, big.mark=","),
            format(ncol(FS), big.mark=",")
          )
        )

        feature_set_data(FS_mat)
        input_score_data(input_score)

        # Compute candidate search ####
        rVal$candidate_search_process <- parallel::mcparallel({

          CaDrA::candidate_search(
            FS = FS,
            input_score = input_score,
            method = method,
            method_alternative = method_alternative,
            custom_function = NULL,
            custom_parameters = NULL,
            weights = weights,
            search_start = search_start,
            top_N = top_N,
            search_method = search_method,
            max_size = max_size,
            best_score_only = FALSE,
            do_plot = FALSE
          )

        })

        print(paste0("candidate search process: ", rVal$candidate_search_process$pid, " started"))

        cadra_error_message("NONE")

      })

      ######################### CADRA SECTION ##################################
      
      #
      # Stop CaDrA Search ####
      #

      observeEvent(input$stop_cadra, {

        if(!is.null(rVal$candidate_search_process)) {
          #print(paste0("candidate search process: ", rVal$candidate_search_process$pid, " killed"))
          tools::pskill(rVal$candidate_search_process$pid)
          rVal$candidate_search_process <- NULL

          if (!is.null(rVal$candidate_search_process)) {
            rVal$candidate_search_obs$destroy()
          }
        }

        if(!is.null(rVal$cadra_permutation_process)) {
          #print(paste0("cadra permutation process: ", rVal$cadra_permutation_process$pid, " killed"))
          tools::pskill(rVal$cadra_permutation_process$pid)
          rVal$cadra_permutation_process <- NULL

          if (!is.null(rVal$cadra_permutation_process)) {
            rVal$cadra_permutation_obs$destroy()
          }
        }

        ns <- session$ns

        rVal$candidate_search_result <- NULL; rVal$cadra_permutation_result <- NULL;

        cadra_error_message("Your process has been interrupted")

        ## Hide cadra loading icon
        session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("loading_icon"), display="no"))

        ## Hide permutation loading icon
        session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("permutation_loading_icon"), display="no"))

        # Show instruction message
        instructions_message(TRUE)

      }, ignoreInit = TRUE)

      #
      # Handle CaDrA search process event ####
      #

      observeEvent(rVal$candidate_search_process, {

        rVal$candidate_search_obs <- observe({

          shiny::invalidateLater(500, session)

          isolate({

            ns <- session$ns
            result <- parallel::mccollect(rVal$candidate_search_process, wait = FALSE)
            
            if(!is.null(result)) {
              rVal$candidate_search_result <- result[[1]]
              rVal$candidate_search_obs$destroy()
              rVal$candidate_search_process <- NULL
              ## Hide cadra loading icon
              session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("loading_icon"), display="no"))
            }
            
          })
          
        })

      }, ignoreInit = TRUE)

      #
      # Handle CaDrA permutation process event ####
      #

      observeEvent(rVal$cadra_permutation_process, {

        rVal$cadra_permutation_obs <- observe({

          shiny::invalidateLater(500, session)

          isolate({

            ns <- session$ns
            result <- parallel::mccollect(rVal$cadra_permutation_process, wait = FALSE)

            if(!is.null(result)) {
              rVal$cadra_permutation_result <- result[[1]]
              rVal$cadra_permutation_obs$destroy()
              rVal$cadra_permutation_process <- NULL
              ## Hide permutation loading icon
              session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("permutation_loading_icon"), display="no"))
            }

          })
          
        })

      }, ignoreInit = TRUE)

      #
      # Render CaDrA's error messages ####
      #
      
      output$cadra_error_message <- shiny::renderUI({
        
        req(cadra_error_message())
        
        ns <- session$ns
        
        if(cadra_error_message() != "NONE"){
          
          ## Update loading icon
          session$sendCustomMessage(type="ToggleOperation", message=list(id=ns("loading_icon"), display="no"))
          
          ## Update loading icon for permutation test
          session$sendCustomMessage(type="ToggleOperation", message=list(id=ns("permutation_loading_icon"), display="no"))
          
          # Show instructions
          instructions_message(TRUE)
          
          # Display error message
          p(style="color: red; font-weight: bold; margin-bottom: 10px;", paste0("ERROR: ", cadra_error_message()))
          
        }
        
      })
      
      ## Feature Set Tittle for CaDrA Search ####
      output$featureData_title <- shiny::renderUI({
        
        req(rVal$candidate_search_result, feature_set_description())
        
        ns <- session$ns
        
        description <- feature_set_description()
        
        selected_fs <- isolate({ input$feature_set })
        
        if(selected_fs == "Import Data"){
          title <- "Dataset: Imported Data"
        }else{
          title <- paste0("Feature Set: ", extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)] %>% unique())
        }
        
        div(
          h3(title),
          br(),
          p(description),
          downloadButton(outputId = ns("download_featureset"), label="Download Filtered Feature Set")
        )
        
      })
      
      ## Download Filtered Feature Set ####
      output$download_featureset <- downloadHandler(
        filename = function() {
          paste0("CaDrA-Filtered-Feature-Set.csv")
        },
        
        content = function(file) {
          FS_table <- feature_set_data() %>% as.data.frame(.) %>% rownames_to_column(var="Feature")
          write.csv(FS_table, file, row.names=FALSE)
        }
      )
      
      ## Title for Best Meta-Feature ####
      output$bestFeatureData_title <- shiny::renderUI({
        
        req(rVal$candidate_search_result)
        
        h3("Best Meta-Feature Set")
        
      })
      
      ## Output Best Meta-Feature ####
      output$bestFeatureData <- DT::renderDataTable({
        
        req(rVal$candidate_search_result)
        
        ns <- session$ns
        
        topn_best_meta <- topn_best(topn_list=rVal$candidate_search_result)
        FS <- topn_best_meta[["feature_set"]]
        meta_indices <- topn_best_meta[["best_indices"]]
        marginal_scores <- topn_best_meta[["marginal_best_scores"]] %>% signif(., digits = 4)
        cumulative_best_scores <- topn_best_meta[["cumulative_best_scores"]] %>% signif(., digits = 4)
        
        # Retrieve the binary feature matrix
        if(is(FS, "SummarizedExperiment")){
          FS_table <- SummarizedExperiment::assay(FS)
        }else{
          FS_table <- as.data.frame(FS)
        }
        
        # Retrieve the binary feature matrix
        if(is(FS, "SummarizedExperiment")){
          FS_table <- SummarizedExperiment::assay(FS)
        }else{
          FS_table <- as.data.frame(FS)
        }
        
        FS_table <- data.frame(
          Score_Index = meta_indices,
          Marginal_Score = marginal_scores,
          Cumulative_Score = cumulative_best_scores
        ) %>% 
          cbind(FS_table)
        
        hover_columns <- create_hover_txt(table = FS_table)
        
        table <- FS_table  %>%
          DT::datatable(
            container = hover_columns,
            rownames = TRUE,
            extensions = 'Buttons',
            selection = "single",
            options = list(
              deferRender = FALSE,
              paging = TRUE,
              searching = TRUE,
              ordering = TRUE,
              pageLength = 20,
              scrollX = TRUE,
              scrollY = 400,
              scrollCollapse = TRUE,
              dom = 'T<"clear">Blfrtip',
              buttons = list(
                list(
                  extend = "collection",
                  text = 'Download Results',
                  action = DT::JS(
                    sprintf(
                      paste0(
                        "function ( e, dt, node, config ) {",
                        "Shiny.setInputValue('%s', true, {priority: 'event'});",
                        "}"
                      ), ns('Download_FS')
                    )
                  )
                )
              )
            )
          )
        
        return(table)
        
      })
      
      ## Downloading Best Meta-Feature Dialog ####
      observeEvent(input$Download_FS, {
        
        ns <- session$ns
        
        shiny::showModal(
          shiny::modalDialog(
            title = "Download Best Meta-Feature Set",
            downloadButton(outputId = ns("downloadFSCSV"),
                           "Download Table as CSV file"),
            br(), br(),
            downloadButton(outputId = ns("downloadFSRDS"),
                           "Download Table as RDS file"),
          )
        )
      })
      
      ## Download Best Feature Set in CSV format ####
      output$downloadFSCSV <- downloadHandler(
        
        filename = function() {
          paste0("CaDrA-Best-Meta-Feature-Set.csv")
        },
        
        content = function(file) {
          topn_best_meta <- topn_best(topn_list=rVal$candidate_search_result)
          FS <- topn_best_meta[["feature_set"]] 
          # Retrieve the binary feature matrix
          if(is(FS, "SummarizedExperiment")){
            FS_table <- SummarizedExperiment::assay(FS)
          }else{
            FS_table <- as.data.frame(FS)
          }
          write.csv(FS_table, file, row.names=FALSE)
        }
        
      )
      
      ## Download Best Meta-Feature in RDS format ####
      output$downloadFSRDS <- downloadHandler(
        
        filename = function() {
          paste0("CaDrA-Best-Meta-Feature-Set.rds")
        },
        
        content = function(file) {
          topn_best_meta <- topn_best(topn_list=rVal$candidate_search_result)
          FS <- topn_best_meta[["feature_set"]] 
          # Retrieve the binary feature matrix
          if(is(FS, "SummarizedExperiment")){
            FS_table <- SummarizedExperiment::assay(FS)
          }else{
            FS_table <- as.data.frame(FS)
          }
          saveRDS(FS_table, file)
        }
        
      )
      
      ## Title for Input Score ####
      output$inputScoreData_title <- shiny::renderUI({
        
        req(rVal$candidate_search_result, input_score_data())
        
        selected_input_score <- isolate({ input$input_score })
        
        if(selected_input_score == "Import Data"){
          title <- "Imported Data"
        }else{
          title <- extdata()$input_score_name[which(extdata()$input_score_path == selected_input_score)]
        }
        
        h3("Input Score:", title)
        
      })
      
      ## Output Input Score ####
      output$inputScoreData <- DT::renderDataTable({
        
        req(rVal$candidate_search_result, input_score_data())
        
        ns <- session$ns
        
        input_score <- input_score_data() %>% signif(., digits = 4)
        score_table <- matrix(input_score, nrow=1, ncol=length(input_score), byrow=TRUE, dimnames=list("input_score", names(input_score)))
        
        hover_columns <- create_hover_txt(table = score_table)
        
        table <- score_table  %>%
          DT::datatable(
            container = hover_columns,
            rownames = TRUE,
            extensions = 'Buttons',
            selection = "single",
            options = list(
              deferRender = FALSE,
              paging = FALSE,
              searching = TRUE,
              ordering = TRUE,
              pageLength = 20,
              scrollX = TRUE,
              scrollY = 400,
              scrollCollapse = TRUE,
              dom = 'T<"clear">Blfrtip',
              buttons = list(
                list(
                  extend = "collection",
                  text = 'Download Results',
                  action = DT::JS(
                    sprintf(
                      paste0(
                        "function ( e, dt, node, config ) {",
                        "Shiny.setInputValue('%s', true, {priority: 'event'});",
                        "}"
                      ), ns('Download_InputScore')
                    )
                  )
                )
              )
            )
          )
        
        return(table)
        
      })
      
      ## Show input score dialog ####
      observeEvent(input$Download_InputScore, {
        
        ns <- session$ns
        
        shiny::showModal(
          shiny::modalDialog(
            title = "Download Observed Input Score",
            downloadButton(outputId = ns("downloadScoreCSV"),
                           "Download Table as CSV file"),
            br(), br(),
            downloadButton(outputId = ns("downloadScoreRDS"),
                           "Download Table as RDS file"),
          )
        )
      })
      
      ## Download input scores in CSV format ####
      output$downloadScoreCSV <- downloadHandler(
        
        filename = function() {
          paste0("CaDrA-Observed-Input-Score.csv")
        },
        
        content = function(file) {
          
          input_score <- input_score_data() %>% signif(., digits = 4)
          
          score_table <- data.frame(
            Sample = names(input_score),
            Score = input_score,
            stringsAsFactors = FALSE
          )
          
          write.csv(score_table, file, row.names=FALSE)
          
        }
      )
      
      ## Download input score in RDS format ####
      output$downloadScoreRDS <- downloadHandler(
        
        filename = function() {
          paste0("CaDrA-Observed-Input-Score.rds")
        },
        
        content = function(file) {
          
          input_score <- input_score_data() %>% signif(., digits = 4)
          
          score_table <- data.frame(
            Sample = names(input_score),
            Score = input_score,
            stringsAsFactors = FALSE
          )
          
          saveRDS(score_table, file)
          
        }
      )
      
      ## Title for meta plot ####
      output$meta_plot_title <- shiny::renderUI({
        
        req(rVal$candidate_search_result)
        
        h3("Best Meta-Feature Plot")
        
      })
      
      ## Output meta-feature plot ####
      output$meta_plot <- shiny::renderPlot({
        
        req(rVal$candidate_search_result)
        
        topn_res <- rVal$candidate_search_result
        topn_best_meta <- CaDrA::topn_best(topn_res)
        
        CaDrA::meta_plot(topn_best_list = topn_best_meta)
        
      })
      
      ## Title for TopN plot ####
      output$topn_plot_title <- shiny::renderUI({
        
        req(rVal$candidate_search_result)
        
        if(length(rVal$candidate_search_result) == 1){
          
          div(
            h3("Top N Overlapping Heatmap"),
            br(),
            h4(style="color: red; font-weight: bold;",
               "NOTE: Cannot plot overlapping heatmap with provided top N seeds = 1 or number of provided custom seeds = 1.")
          )
          
        }else{
          
          h3("Top N Overlapping Heatmap")
          
        }
        
      })
      
      ## Output TopN plot ####
      output$topn_plot <- shiny::renderPlot({
        
        req(rVal$candidate_search_result)
        
        topn_res <- rVal$candidate_search_result
        
        CaDrA::topn_plot(topn_res)
        
      })
      
      ## Title for permutation plot ####
      output$permutation_plot_title <- shiny::renderUI({
        
        req(rVal$cadra_permutation_result)
        
        h3("Permutation Plot")
        
      })
      
      ## Output permutation plot ####
      output$permutation_plot <- shiny::renderPlot({
        
        req(rVal$cadra_permutation_result)
        
        perm_res <- rVal$cadra_permutation_result
        
        permutation_plot(perm_res)
        
      })
      
      ######################### GSVA SECTION ##################################
      
      ## Output instructions message for running GSVA analysis ####
      output$gsva_instructions <- shiny::renderUI({
        
        req(gsva_instructions_message())
        
        tags$iframe(src="vignettes/create-gsva-instructions.html", onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));', style="height:200px;width:100%;border:none;overflow:hidden;")
        
      })
      
      #
      # Start GSVA Analysis ####
      #

      observeEvent(input$run_gsva, {

        ns <- session$ns

        if(!is.null(gVal$gsva_search_process)) return(NULL)

        gVal$gsva_search_result <- NULL
        gsva_instructions_message(FALSE)
        gsva_cadra_error_message(NULL)
        enrichment_table_message(NULL)

        fset <- isolate({ input$gsva_feature_set })
        geset <- isolate({ input$gsva_gene_expression })

        #print(fset); print(geset)

        genesetcollection <- list()
        genesetname <- c()

        ## Show gsva loading icon
        session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gsva_loading_icon"), display="yes"))

        # Check feature set
        if(fset == "Import Data"){

          inputfile <- input$gsva_feature_set_file;
          inputtype <- input$gsva_feature_set_file_type;

          if(is.null(inputfile)){
            gsva_cadra_error_message("Please choose a 'Feature Set' file to import.")
            return(NULL)
          }

          file_extension <- tools::file_ext(inputfile$datapath)
          
          if(inputtype == "csv" && file_extension == "csv"){

            # read in the FS file
            FS <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

            if("Feature" %in% colnames(FS)){

              # Convert FS to SummarizedExperiment object
              FS <- FS %>% tibble::column_to_rownames(var="Feature")  %>%
                dplyr::mutate_all(as.numeric) %>% 
                as.matrix() %>% 
                SummarizedExperiment::SummarizedExperiment(
                  assays=.,
                  rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
                  colData=data.frame(samples=colnames(.), row.names = colnames(.))
                )
              
            }else{
              
              gsva_cadra_error_message("The 'Feature Set' file must contain a 'Feature' column name that contains unique names or labels to search for best features.")
              return(NULL)
              
            }
            
          }else if (inputtype == "rds" && file_extension == "rds"){
            
            feature_set <- base::readRDS(inputfile$datapath)

          }else{

            gsva_cadra_error_message("Incorrect file format. Please check your 'Feature Set' file again.")
            return(NULL)

          }

        }else{

          if(tools::file_ext(fset) == "rda"){
            envir_name <- base::load(fset)
            feature_set <- base::get(envir_name)
          }else{
            feature_set <- base::readRDS(fset)
          }

        }

        # Check gene expression set
        if(geset == "Import Data"){

          inputfile <- input$gsva_gene_expression_file;
          inputtype <- input$gsva_gene_expression_file_type;

          if(is.null(inputfile)){
            gsva_cadra_error_message("Please choose a 'gene expression' file to import.")
            return(NULL)
          }

          file_extension <- tools::file_ext(inputfile$datapath)
          
          if(inputtype == "csv" && file_extension == "csv") {

            gene_expression <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

            if("Feature" %in% colnames(gene_expression)){

              gene_expression <- gene_expression %>% 
                tibble::column_to_rownames(var="Feature")  %>%
                dplyr::mutate_all(as.numeric) %>%
                as.matrix() %>% 
                SummarizedExperiment::SummarizedExperiment(
                  assays=.,
                  rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
                  colData=data.frame(samples=colnames(.), row.names = colnames(.))
                )
              
            }else{

              gsva_cadra_error_message("The 'Feature Set' file must contain a 'Feature' column name that contains unique names or labels to search for best features.")
              return(NULL)

            }

          }else if(inputtype == "rds" && file_extension == "rds"){

            gene_expression <- base::readRDS(inputfile$datapath)

          }else {

            gsva_cadra_error_message("Incorrect file format. Please check your 'Gene Expression' file again.")
            return(NULL)

          }

        }else{

          if(tools::file_ext(geset) == "rda"){
            envir_name <- base::load(geset)
            gene_expression <- base::get(envir_name)
          }else{
            gene_expression <- base::readRDS(geset)
          }

        }

        ## Check geneset list
        inputfile <- input$gsva_geneset_file;
        inputtype <- input$gsva_geneset_file_type;

        if(is.null(inputfile)){
          gsva_cadra_error_message("Please choose a 'geneset' file to import.")
          return(NULL)
        }

        file_extension <- tools::file_ext(inputfile$datapath)

        if(inputtype == "csv" && file_extension == "csv"){

          genelist <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

          # Getting gene list for each geneset collection
          for(u in 1:ncol(genelist)){
            #u=1;
            genesetname <- c(genesetname, colnames(genelist)[u])
            genesetcollection <- c(genesetcollection, list(genelist=genelist[,u]))
            names(genesetcollection) <- genesetname
          }

        }else if(inputtype == "gmt" && file_extension == "gmt"){

          genesetcollection <- GSEABase::getGmt(inputfile$datapath)

        }else{

          gsva_cadra_error_message("Incorrect file format. Please check your 'Geneset' file again.")
          return(NULL)

        }

        #print(feature_set); print(gene_expression); print(genesetcollection)

        # Make sure the samples match between gene expression and feature set
        matching_samples <- colnames(feature_set)[which(colnames(feature_set) %in% colnames(gene_expression))]
        gene_expression <- gene_expression[, matching_samples]

        # Make sure data are numeric values
        gene_expression <- assay(gene_expression)  %>%
          as.data.frame()  %>%
          dplyr::mutate_if(is.character, as.numeric)

        # Create a gene expression matrix
        gene_expression <- as.matrix(
          gene_expression,
          nrow=nrow(gene_expression),
          ncol=ncol(gene_expression),
          byrow=TRUE,
          dimnames=list(rownames(gene_expression), colnames(gene_expression))
        )

        ## Remove genes with constant expression values
        if(any(rowSums(gene_expression) == 0) || any(rowSums(gene_expression) == ncol(gene_expression))){
          gene_expression <- gene_expression[!(rowSums(gene_expression)==0 | rowSums(gene_expression) == ncol(gene_expression)),]
        }

        # Run the gsva analysis
        gVal$gsva_search_process <- parallel::mcparallel({

          GSVA::gsva(expr=gene_expression, gset.idx.list=genesetcollection, method="gsva", mx.diff=TRUE)

        })

        gsva_cadra_error_message("NONE")

      })

      #
      # Stop GSVA Analysis ####
      #

      observeEvent(input$stop_gsva, {

        # Stop or kill the process
        if(!is.null(gVal$gsva_search_process)) {
          #print(paste0("GSVA process: ", gVal$gsva_search_process$pid, " killed"))
          tools::pskill(gVal$gsva_search_process$pid)
          gVal$gsva_search_process <- NULL

          if (!is.null(gVal$gsva_search_process)) {
            gVal$gsva_search_obs$destroy()
          }
        }

        # Get session info
        ns <- session$ns

        # Clear out search result
        gVal$gsva_search_result <- NULL;

        ## error mesage
        gsva_cadra_error_message("Your process has been interrupted")

        ## Hide gsva loading icon
        session$sendCustomMessage(type="ToggleOperation", message=list(id=ns("gsva_loading_icon"), display="no"))

        # Show instruction message
        gsva_instructions_message(TRUE)

      }, ignoreInit = TRUE)

      #
      # Handle GSVA Analysis process event ####
      #

      observeEvent(gVal$gsva_search_process, {

        gVal$gsva_search_obs <- observe({

          shiny::invalidateLater(500, session)

          isolate({

            ns <- session$ns
            result <- parallel::mccollect(gVal$gsva_search_process, wait = FALSE)

            if(!is.null(result)) {
              gVal$gsva_search_result <- result[[1]]
              gVal$gsva_search_obs$destroy()
              gVal$gsva_search_process <- NULL
              ## Hide cadra loading icon
              session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gsva_loading_icon"), display="no"))
            }

          })
        })

      }, ignoreInit = TRUE)

      #
      # Render GSVA's error messages ####
      #

      output$gsva_cadra_error_message <- shiny::renderUI({

        req(gsva_cadra_error_message())

        ns <- session$ns

        if(gsva_cadra_error_message() != "NONE"){

          ## Update loading icon
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gsva_loading_icon"), display="no"))

          # Show instructions
          gsva_instructions_message(TRUE)

          # Display error message
          p(style="color: red; font-weight: bold; margin-bottom: 10px;", gsva_cadra_error_message())

        }

      })

      # Title for GSVA ####
      output$gsva_enrichment_title <- shiny::renderUI({

        req(gVal$gsva_search_result)
        
        selected_fs = isolate({ input$gsva_feature_set })
        selected_fs_name = extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)] %>% unique()
        
        h3(paste0("Enrichment Score for ", selected_fs_name))

      })

      #
      # Render GSVA's Enrichment Table ####
      #

      output$gsva_enrichment_scores <- DT::renderDataTable({

        req(gVal$gsva_search_result)
        
        ns <- session$ns
        
        enrichment_table <- gVal$gsva_search_result %>% as.data.frame(.) %>%
          dplyr::mutate_if(is.character, as.numeric) %>%
          dplyr::mutate(across(where(is.numeric), ~signif(.x, digits = 4))) %>%           
          tibble::rownames_to_column(., var="Geneset") %>% 
          dplyr::mutate(checkbox = sapply(1:nrow(.), function(s){ sprintf('<input type="checkbox" name="gsva_selected_row" id="%s" value="%s">', ns(paste0('gsvarow', s)), s) })) %>% 
          dplyr::select(checkbox, everything()) 
        
        colnames(enrichment_table)[which(colnames(enrichment_table) == "checkbox")] <- sprintf('<input type="checkbox" name="gsva_select_all" id="%s" onclick="select_all_genelist()">', ns("gsva_select_all"))
        
        table <- enrichment_table %>%
          DT::datatable(
            escape = FALSE,
            rownames = FALSE, 
            extensions = 'Buttons',
            selection = "none",
            options = list(
              columnDefs = list(list(orderable = FALSE, targets = 0)),
              paging = FALSE,
              searching = TRUE,
              ordering = TRUE,
              pageLength = 20,
              scrollX = TRUE,
              scrollY = 500,
              scrollCollapse = TRUE,
              dom = 'T<"clear">Blfrtip',
              buttons = list(
                list(
                  extend = "collection",
                  text = 'Download Results',
                  action = DT::JS(
                    sprintf(
                      paste0(
                        "function ( e, dt, node, config ) {",
                        "Shiny.setInputValue('%s', true, {priority: 'event'});",
                        "}"
                      ), ns('Download_Enrichment')
                    )
                  )
                )
              )
            )
          )
        
        return(table)

      })

      ## Downloading enrichment scores dialog ####
      observeEvent(input$Download_Enrichment, {

        ns <- session$ns

        shiny::showModal(
          shiny::modalDialog(
            title = "Download Enrichment Score",
            downloadButton(outputId = ns("downloadEnrichmentCSV"),
                           "Download Table as CSV file"),
            br(), br(),
            downloadButton(outputId = ns("downloadEnrichmentRDS"),
                           "Download Table as RDS file"),
          )
        )
        
      })

      # Download enrichment scores in CSV format ####
      output$downloadEnrichmentCSV <- downloadHandler(

        filename = function() {
          selected_fs <- isolate({ input$gsva_feature_set })
          fsname <- extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)]
          paste0("CaDrA-", fsname, "-GSVA-Results.csv")
        },

        content = function(file) {
          enrichment_table <- gVal$gsva_search_result %>% as.data.frame(.) %>% tibble::rownames_to_column(., var="Geneset")
          write.csv(enrichment_table, file, row.names=FALSE)
        }

      )

      ## Download enrichment scores in RDS format ####
      output$downloadEnrichmentRDS <- downloadHandler(

        filename = function() {
          selected_fs <- isolate({ input$gsva_feature_set })
          fsname <- extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)]
          paste0("CaDrA-", fsname, "-GSVA-Results.rds")
        },

        content = function(file) {

          # Get gsva result to SummarizedExperiment object
          geneset <- gVal$gsva_search_result %>% 
            as.matrix() %>% 
            SummarizedExperiment::SummarizedExperiment(
              assays=.,
              rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
              colData=data.frame(samples=colnames(.), row.names = colnames(.))
            )
          
          saveRDS(geneset, file)
          
        }
        
      )

      #
      # Add GSVA's Enrichment Table to CaDrA ####
      #

      output$add_enrichment_scores <- shiny::renderUI({

        req(gVal$gsva_search_result)

        ns <- session$ns

        div(

          actionButton(
            inputId = ns("add_to_input_scores"),
            label = strong("Add to CaDrA's Input Score"),
            style = "background: blue; color: white;",
            onclick = paste0("gsva_selected_checkbox('", id, "')")
          ),

          br(), br(),

          uiOutput(outputId = ns("enrichment_cadra_error_message"))

        )

      })

      # Add Enrichment Score to CaDrA's Input Score ####
      observeEvent(input$add_to_input_scores, {

        enrichment_scores <- gVal$gsva_search_result %>% as.data.frame(.) %>% tibble::rownames_to_column(., var="Geneset")
        gsva_select_all <- isolate({ input$checkbox_all })
        row <- isolate({ input$gsva_selected_row_number })
        
        if(gsva_select_all == TRUE){
          row <- 1:nrow(enrichment_scores)
        }else{
          row <- isolate({ input$gsva_selected_row_number })
        }
        
        if(length(row) > 0){
          
          score_dir <- file.path(dirname(datalist), "GSVA")
          
          if(!dir.exists(score_dir)){
            dir.create(score_dir)
          }
          
          selected_fs = isolate({ input$gsva_feature_set })
          selected_fs_name = extdata()$feature_set_name[which(extdata()$feature_set_path == selected_fs)] %>% unique()
          
          selected_gene_expression = isolate({ input$gsva_gene_expression })
          selected_gene_expression_name <- extdata()$gene_expression_name[which(extdata()$gene_expression_path == selected_gene_expression)] %>% unique()
          
          new_input_score <- data.frame(
            feature_set_name = rep(selected_fs_name, length(row)),
            feature_set_path = rep(selected_fs, length(row)),
            input_score_name = NA,
            input_score_path = NA,
            gene_expression_name = rep(selected_gene_expression_name, length(row)),
            gene_expression_path = rep(selected_gene_expression, length(row)),
            stringsAsFactors = FALSE
          )
          
          for(r in 1:length(row)){
            #r=1
            selected_geneset_scores <- enrichment_scores %>% dplyr::select(-Geneset) %>% dplyr::slice(as.numeric(row[r]))
            selected_geneset <- enrichment_scores %>% dplyr::select(Geneset) %>% dplyr::slice(as.numeric(row[r]))
            
            fs_dir <- file.path(score_dir, selected_fs_name)
            
            if(!dir.exists(fs_dir)){
              dir.create(fs_dir)
            }
            
            selected_score_path <- file.path(fs_dir, paste0(selected_geneset, ".rds"))
            
            new_scores <- selected_geneset_scores[1,] %>% unlist() %>%  as.vector()
            names(new_scores) <- colnames(selected_geneset_scores)
            
            saveRDS(new_scores, selected_score_path)
            
            new_input_score$input_score_name[r] <- selected_geneset
            new_input_score$input_score_path[r] <- selected_score_path
          }
          
          new_input_score <- new_input_score %>% dplyr::mutate_all(., as.character)
          
          if(tools::file_ext(datalist) == "csv"){
            datalist_table <- read.csv(datalist, header=TRUE) %>% dplyr::mutate_all(., as.character)
          }else if(tools::file_ext(datalist) == "rds"){
            datalist_table <- readRDS(datalist) %>% dplyr::mutate_all(., as.character)
          }
          
          # Save the updated list
          updated_datalist <- datalist_table %>%
            dplyr::bind_rows(new_input_score) %>% 
            dplyr::distinct(., .keep_all=TRUE)
          
          # Check if external data exists in package
          if(tools::file_ext(datalist) == "csv"){
            write.csv(updated_datalist, datalist, row.names = F)
          }else if(tools::file_ext(datalist) == "rds"){
            saveRDS(updated_datalist, datalist)
          }
          
          updated_extdata <- get_extdata(datalist)
          
          extdata(updated_extdata)
          
          genelist_names <- paste0(new_input_score$input_score_name, collapse = ", ")
          
          enrichment_table_message(paste0("<em style='font-weights: bold;'>", genelist_names, "</em>", ifelse(length(genelist_names) == 1, " has", " have"), " been added as <em style='font-weights: bold; color: red;'>'Input Score'</em> for <em style='font-weights: bold; color: red;'>'", selected_fs_name , "'</em>."))
          
        }else{
          
          enrichment_table_message("<span style='color: red; font-weights: bold;'>Select a geneset above to add to CaDrA's Input Score</span>")
          
        }

      }, ignoreInit = TRUE)

      #
      # Render GSVA's enrichment error messages ####
      #

      output$enrichment_cadra_error_message <- shiny::renderUI({

        req(enrichment_table_message())

        # Display error message
        p(style="margin-bottom: 10px;", HTML(enrichment_table_message()))
        
      })
      
    }
  )
  
}

#' Run both Shiny UI and Server Modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist A data frame or path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist must contains the following variables:
#' 'feature_set' (required), 'feature_set_path' (repuired), 
#' 'input_score_name' (required), input_score_path' (required), 
#' 'gene_expression_name' (optional), 'gene_expression_path' (optional)
#' 
#' Default is NULL. If NULL, the app will start with DEFAULT dataset included 
#' in CaDrA package.
#' 
#' @return Shiny application 
#'
#' @examples
#' 
#' # Load R library
#' library(shiny)
#' 
#' id = "myapp"
#' 
#' # Create a Shiny app
#' app <- CaDrA.shiny::CaDrA_App(id = id)
#' 
#' # Launch and deploy Shiny app (NOT RUN)
#' # shiny::runApp(app, host='0.0.0.0', port=3838)
#' 
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @export
CaDrA_App <- function(id, datalist=NULL) {
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("CaDrA: Candidate Drivers Analysis"),
    shiny::helpText("Multi-Omic Search for Candidate Drivers of Functional Signatures"),
    CaDrA_UI(id = id, datalist = datalist)
  )
  
  server <- function(input, output, session) {
    CaDrA_Server(id = id, datalist = datalist)
  }
  
  shiny::shinyApp(ui=ui, server=server)
  
}







