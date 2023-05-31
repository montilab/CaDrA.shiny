
# function to hover columns in data table
create_hover_txt <- function(table){
  
  column_names <- colnames(table)
  
  th_tr <- lapply(seq_along(column_names), function(l){ 
    title <- column_names[l]
    name <- ifelse(nchar(title) > 4, paste0(substr(title, 1, 4), "..."), title)
    th <- sprintf('<th title = "%s">%s</th>\n', title, name) 
  }) %>% purrr::flatten_chr() %>% paste0(., collapse = "")
  
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
global_feature_set_paths <- list(
  "CCLE SCNAs + Mutations" =  system.file("data/CCLE_MUT_SCNA.rda", package = "CaDrA"),
  "TCGA BRCA SCNAs + Mutations" = system.file("data/BRCA_GISTIC_MUT_SIG.rda", package = "CaDrA"),
  "Simulated Feature Set" = system.file("data/sim_FS.rda", package = "CaDrA")
)

# Global input scores ####
global_input_score_paths <- list(
  "B-catenin Activity in CCLE" = system.file("data/CTNBB1_reporter.rda", package = "CaDrA"),
  "YAP/TAZ Activity in TCGA BrCa" = system.file("data/TAZYAP_BRCA_ACTIVITY.rda", package = "CaDrA"),
  "Simulated Input Scores" =  system.file("data/sim_Scores.rda", package = "CaDrA")
)

# Global gene expression ####
global_gene_expression_paths <- list(
  "CCLE_MUT_SCNA" = NA,
  "BRCA_GISTIC_MUT_SIG" = NA,
  "sim_FS" = NA
)
  
# Required column names for datalist_file
datalist_colnames <- c('feature_set_names', 'feature_set_paths', 'input_score_names', 'input_score_paths', 'gene_expression_names', 'gene_expression_paths')

# function to obtain the external data
get_extdata <- function(datalist_file=NULL, global_feature_set_paths, global_score_choices, global_gene_expression_paths){
  
  global_app_options <- data.frame(
    feature_set_names = names(global_feature_set_paths), 
    feature_set_paths = global_feature_set_paths %>% unlist(), 
    input_score_names = names(global_input_score_paths), 
    input_score_paths = global_input_score_paths %>% unlist(), 
    gene_expression_names = names(global_gene_expression_paths),
    gene_expression_paths = global_gene_expression_paths %>% unlist()
  )
  
  if(is.null(datalist_file)){
    return(global_app_options)
  }
  
  # Check if external data exists in package
  if(file.exists(datalist_file)){
    
    if(tools::file_ext(datalist_file) == "csv"){
      datalist_file <- utils::read.csv(datalist_file, header=TRUE) %>% dplyr::mutate_all(., as.character)
    }else if(tools::file_ext(datalist_file) == "rds"){
      datalist_file <- base::readRDS(datalist_file) %>% dplyr::mutate_all(., as.character)
    }else{
      stop("datalist file file must have a csv or rds format")
    }
    
    if(all(datalist_colnames %in% colnames(datalist_file))){
      
      datalist_file <- datalist_file[which(datalist_file$feature_set_paths != "" & !is.na(datalist_file$feature_set_paths) & datalist_file$feature_set_names != "" & !is.na(datalist_file$feature_set_names)),]
      
      app_options <- global_app_options %>% 
        dplyr::bind_rows(datalist_file)
      
      return(app_options)
      
    }else{
      
      stop("The provided datalist file must contain the following column names: ", paste0(datalist_colnames, collapse=", "))
      
    }
    
  }else{
    
    stop("File does not exist at ", datalist_file)
    
  }
  
}

#' Shiny UI modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist_file A path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist_file must contains the following variables:
#' 'feature_set' (required), 'feature_set_paths' (repuired), 
#' 'input_score_names' (required), input_score_paths' (required), 
#' 'gene_expression_names' (optional), 'gene_expression_paths' (optional)
#' 
#' Default is NULL. If NULL, the app will start with DEFAULT dataset included 
#' in CaDrA package.
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
#' @import DT htmltools
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' 
#' @export 
CaDrA_UI <- function(id, datalist_file=NULL){
  
  # Combine extdata with global expression set and scores dataset if it was provided
  extdata <- get_extdata(datalist_file, global_feature_set_paths, global_score_choices, global_gene_expression_paths)

  cadra_fs_choices <- extdata %>% dplyr::distinct(feature_set_names, .keep_all=TRUE)
  cadra_feature_set_paths <- cadra_fs_choices$feature_set_paths
  names(cadra_feature_set_paths) <- cadra_fs_choices$feature_set_names

  gsva_fs_choices <- extdata %>% dplyr::distinct(feature_set_names, .keep_all=TRUE) %>% dplyr::filter(gene_expression_paths != "" & !is.na(gene_expression_paths))
  gsva_feature_set_paths <- gsva_fs_choices$feature_set_paths
  names(gsva_feature_set_paths) <- gsva_fs_choices$feature_set_names

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
            width = 4,
            class = "side-bar-options",

            h3("CaDrA Options", style="text-align: center;"),

            br(),

            selectizeInput(
              inputId = ns("feature_set"),
              label = "Feature Set",
              choices = c(cadra_feature_set_paths, "Import Data"),
              width = "100%"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("feature_set")),

              fileInput(
                inputId = ns("ES_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Feature Set file:"),
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
                  'frame including a \'Features\' column name ',
                  'that contains unique names or labels to ',
                  'search for best features. Otherwise, \'Feature ',
                  'Set\' must be an object of class SummarizedExperiment ',
                  'from SummarizedExperiment package.\">?</a>')),
                choices = c(".csv", ".rds"),
                selected = ".csv",
                inline = TRUE
              )
            ),

            selectInput(
              inputId = ns("input_score"),
              label = "Input Score",
              choices = c("Please select an option below" = ""),
              width = "100%"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", 
                                  ns("input_score")),

              fileInput(
                inputId = ns("input_score_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Input Score file:"),
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
                  '(Samples and Scores) and the \'Samples\' column ',
                  'must match the colnames of \'Feature Set\'. ',
                  'Otherwise, \'Input Score\' must be a list of  ',
                  'vectors and have names or labels that match the ',
                  'colnames of the \'Feature Set\'.\">?</a>')),
                choices = c(".csv", ".rds"),
                selected = ".csv",
                inline = TRUE
              )
            ),

            numericInput(
              inputId = ns("min_cutoff"),
              label = HTML(paste0(
                '<strong>Min Event Frequency (n)</strong> ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"Minimum number of \'occurrences\' a feature ',
                '(e.g., a mutation) must have to be included in the ',
                '\'Feature Set\`. Features with fewer events than the ',
                'specified number will be removed.\n\nNOTE: \'Min event ',
                'frequency\' must be >= 5.\">?</a>')),
              value = 30,
              min = 5,
              max = Inf,
              step = 1,
              width = "100%"
            ),

            numericInput(
              inputId = ns("max_cutoff"),
              label = HTML(paste0(
                '<strong>Max Event Frequency (%)</strong> ',
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

            selectInput(
              inputId = ns("method"),
              label = strong(span(style="color:red;", "*"),
                             "Scoring method:"),
              choices = c("ks_pval", "ks_score", "wilcox_pval", 
                          "wilcox_score", "revealer"),
              selected = "ks", 
              width = "100%"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'ks_pval' || 
                                  input['%s'] == 'ks_score'", 
                                  ns("method"), ns("method")),
              
              checkboxInput(
                inputId = ns("weighted_ks"),
                label = "Compute weighted KS?",
                value = FALSE
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("weighted_ks")),
                
                fileInput(
                  inputId = ns("weight_file"),
                  label = strong(span(style = "color: red;", "*"),
                                 "Choose a weight file:"),
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
                    'with two columns (Samples and Weights) and ',
                    'the \'Samples\' column must match the colnames of ',
                    '\'Feature Set\'. Otherwise, \'Weights\' file',
                    'must contain a list of vectors and have names or ',
                    'labels that match the colnames of \'Feature Set\'.\">?</a>')),
                  choices=c(".csv", ".rds"),
                  selected = ".csv",
                  inline = TRUE
                )
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] == 'ks_pval' |
                                  input['%s'] == 'ks_score' |
                                  input['%s'] == 'wilcox_pval' |
                                  input['%s'] == 'wilcox_score'",
                                  ns("method"), ns("method"),
                                  ns("method"), ns("method")),
              selectInput(
                inputId = ns("alternative"),
                label = strong(span(style="color:red;", "*"), "Alternative:"),
                choices = c("less", "two.sided", "greater"),
                selected = "less", width = "100%"
              )
            ),
            
            radioButtons(
              inputId = ns("search_method"),
              label = strong(span(style="color:red;", "*"), "Search method"),
              choices=c("forward and backward"="both",
                        "forward only"="forward"),
              selected = "both", inline = TRUE
            ),
            
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

            radioButtons(
              inputId = ns("initial_seed"),
              label = HTML(paste0(
                '<span style=\"color:red;\">*</span> Search modality ',
                '<a class="tooltip-txt" data-html="true" ',
                'data-tooltip-toggle="tooltip" data-placement="top" ',
                'title=\"\'Top N\' repeats the search starting from ',
                'each of the top N scoring features. \'Custom seeds\' ',
                'repeats the search starting from each of the custom ',
                'seeds. WARNING: If number of seeds specified is greater ',
                'than 10, this may result in a longer search time.\">?</a>')),
              choices = c("Top N seeds"="top_N_seeds", "Custom seeds"="search_start_seeds"),
              selected = "top_N_seeds",
              inline = TRUE
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'top_N_seeds'",
                                  ns("initial_seed")),
              numericInput(
                inputId = ns("top_N"),
                label = strong(span(style = "color:red;", "*"),
                               paste0("Top N value")),
                min = 1,
                max = 100,
                step = 1,
                value = 10,
                width = "100%"
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'search_start_seeds'",
                                  ns("initial_seed")),

              textAreaInput(
                inputId = ns("search_start"),
                label = strong(span(style = "color:red;", "*"),
                               paste0('Enter a list of character strings ',
                                      '(separated by commas) corresponding ',
                                      'to feature names within the ',
                                      '\'Feature Set\' object')),
                value="",
                width="100%"
              )
            ),

            checkboxInput(
              inputId = ns("permutation_test"),
              label = strong("Perform permutation testing?"),
              value = FALSE
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("permutation_test")),

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

              numericInput(
                inputId = ns("ncores"),
                label = strong(span(style="color:red;", "*"),
                               paste0("Number of cores to perform parallelization for permutation testing")),
                min = 1,
                max = Inf,
                step = 1,
                value = 1,
                width = "100%"
              )
            ),

            br(),

            uiOutput(outputId = ns("error_message")),

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
            width = 8,

            div(
              uiOutput(outputId = ns("instructions"))
            ),

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
                p(class = "loading_text", "Running Permutation Testing...")
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
            width = 4,
            class = "side-bar-options",

            h3("GSVA Options", style="text-align: center;"),

            br(),

            h4("Feature Set:"),

            selectizeInput(
              inputId = ns("gsva_feature_set"),
              label = NULL,
              choices = c(gsva_feature_set_paths, "Import Data"),
              width = "600px"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", ns("gsva_feature_set")),

              fileInput(
                inputId = ns("gsva_feature_set_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Feature Set file:"),
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
                  'frame including a \'Features\' column name ',
                  'that contains unique names or labels to ',
                  'search for best features. Otherwise, \'Feature ',
                  'Set\' must be an object of class SummarizedExperiment ',
                  'from SummarizedExperiment package.\">?</a>')),
                choices = c(".csv", ".rds"),
                selected = ".csv",
                inline = TRUE,
                width = "600px"
              )
            ),

            h4("Gene Expression:"),

            selectInput(
              inputId = ns("gsva_gene_expression"),
              label = NULL,
              choices = c("Please select an option below" = ""),
              width = "600px"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Import Data'", ns("gsva_gene_expression")),

              fileInput(
                inputId = ns("gsva_gene_expression_file"),
                label = strong(span(style = "color: red;", "*"),
                               "Input Score file:"),
                width = "600px"
              ),

              radioButtons(
                inputId = ns("gsva_gene_expression_file_type"),
                label = HTML(paste0(
                  'File type ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement=',
                  '"top" title=\"NOTE: If file is in csv format, ',
                  'then the \'Input Score\' file ',
                  'must be a data frame with two columns ',
                  '(Samples and Scores) and the \'Samples\' column ',
                  'must match the colnames of \'Feature Set\'. ',
                  'Otherwise, \'Input Score\' must be a list of  ',
                  'vectors and have names or labels that match the ',
                  'colnames of the \'Feature Set\'.\">?</a>')),
                choices = c(".csv", ".rds"),
                selected = ".csv",
                inline = TRUE,
                width = "600px"
              )
            ),

            h4("Geneset:"),

            fileInput(
              inputId = ns("gsva_geneset_file"),
              label = strong(span(style = "color: red;", "*"),
                             "Choose a geneset file to import:"),
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
                'frame including a \'Features\' column name ',
                'that contains unique names or labels to ',
                'search for best features. Otherwise, \'Feature ',
                'Set\' must be an object of class SummarizedExperiment ',
                'from SummarizedExperiment package.\">?</a>')),
              choices = c(".csv", ".gmt"),
              selected = ".csv",
              inline = TRUE,
              width = "600px"
            ),


            br(),

            uiOutput(outputId = ns("gsva_error_message")),

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
            width =8,

            div(
              uiOutput(outputId = ns("gsva_instructions"))
            ),

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

            div(
              uiOutput(outputId = ns("add_enrichment_scores")),
            )

          )
        ),

        ##### DOWNLOAD DATASET #######
        tabPanel(
          title = "Dataset",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "database", lib = "font-awesome"),

          column(
            width = 4,
            class = "side-bar-options",

            h2("Download Feature Set"),

            selectizeInput(
              inputId = ns("download_fs_options"),
              label = NULL,
              choices = cadra_feature_set_paths,
              width = "600px"
            ),

            selectInput(
              inputId = ns("download_fs_type"),
              label = "Type of Data to Download:",
              choices = c("Feature Set", "Sample Names", "Feature Names"),
              width = "600px"
            ),

            div(
              id = ns("input_score_dl"), style="display: none;",
              checkboxInput(
                inputId = ns("include_scores"),
                label = HTML(paste0(
                  'Include Input Scores ',
                  '<a class="tooltip-txt" data-html="true" ',
                  'data-tooltip-toggle="tooltip" data-placement="top" ',
                  'title=\"Whether to download \'Input Scores\' that is associated with \'Feature Set\'\">?</a>')),
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

        ##### HELP TAB #######
        tabPanel(
          title = "Help",
          style = "padding: 5px 10px 10px 10px;",
          icon = icon(name = "question", lib = "font-awesome"),

          htmltools::includeMarkdown(file.path(system.file("README.md", package = "CaDrA.shiny")))

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
          ),
          
          br()
          
        )
      )
    )
  )
  
}

#' Shiny Server modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist_file A path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist_file must contains the following variables:
#' 'feature_set' (required), 'feature_set_paths' (repuired), 
#' 'input_score_names' (required), input_score_paths' (required), 
#' 'gene_expression_names' (optional), 'gene_expression_paths' (optional)
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
#' @import CaDrA DT GSVA GSEABase htmltools methods parallel SummarizedExperiment tibble tools utils
#' @rawNamespace import(dplyr, except = c(union, intersect, setdiff))
#' 
#' @export 
CaDrA_Server <- function(id, datalist_file=NULL){
  
  shiny::moduleServer(
    id,
    function(input, output, session){

      ## Extract extdata ####
      extdata <- shiny::reactiveVal(get_extdata(datalist_file, global_feature_set_paths, global_score_choices, global_gene_expression_paths))

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
      error_message <- shiny::reactiveVal()

      ## Create reactive values for GSVA analysis ####
      gVal <- shiny::reactiveValues()
      gVal$gsva_search_process <- NULL
      gVal$gsva_search_obs <- NULL
      gsva_instructions_message <- shiny::reactiveVal(TRUE)
      gsva_error_message <- shiny::reactiveVal()
      enrichment_table_message <- shiny::reactiveVal()

      # Prevent Shiny from grayed out
      autoInvalidate <- reactiveTimer(10000)
      
      observe({
        autoInvalidate()
        cat(".")
      })
      
      ## Output instructions message for running CaDrA search ####
      output$instructions <- shiny::renderUI({

        req(instructions_message())

        div(
          h2("Instructions"),

          tags$pre(
            tags$code(
              "
              Select the 'CaDrA Options' on the left and click 'RUN' at the bottom
              "
            )
          )
        )
      })

      ## Output instructions message for running GSVA analysis ####
      output$gsva_instructions <- shiny::renderUI({

        req(gsva_instructions_message())

        div(
          h2("Instructions"),

          tags$pre(
            tags$code(
              "Select the `GSVA options` on the left and click 'RUN' at the bottom"
            )
          )
        )
      })

      ## Updates feature set and input score choices for running CaDrA search ####
      observeEvent({
        input$feature_set
        extdata()
      }, {

        selected_fs <- isolate({ input$feature_set })

        if(selected_fs != "Import Data"){

          input_score_data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs) %>% dplyr::distinct(input_score_paths, .keep_all=TRUE)
          input_score_selection <- input_score_data$input_score_paths
          names(input_score_selection) <- input_score_data$input_score_names

          # print(input_score_selection)

          if(all(is.na(input_score_selection)) || all(input_score_selection == "")){
            updateSelectInput(session, inputId = "input_score", choices = "Import Data")
          }else{
            input_score_selection <- input_score_selection[which(!is.na(input_score_selection) & input_score_selection != "")]
            updateSelectInput(session, inputId = "input_score", choices = c(input_score_selection, "Import Data"), selected = input_score_selection[1])
          }

          if(tools::file_ext(selected_fs) == "rda"){
            selected_fs <- base::load(selected_fs)
          }

          if(selected_fs == "BRCA_GISTIC_MUT_SIG"){
            updateNumericInput(session, inputId = "min_cutoff", value = 30)
          }else {
            updateNumericInput(session, inputId = "min_cutoff", value = 5)
          }

        }else{

          updateSelectInput(session, inputId = "input_score", choices = "Import Data")

        }

      })

      ## Updates feature set and gene expression set choices for running GSVA analysis ####
      observeEvent({
        input$gsva_feature_set
        extdata()
      }, {

        selected_fs <- isolate({ input$gsva_feature_set })

        if(selected_fs != "Import Data"){

          gene_expression_data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs) %>% dplyr::distinct(gene_expression_paths, .keep_all=TRUE)
          gene_expression_selection <- gene_expression_data$gene_expression_paths
          names(gene_expression_selection) <- gene_expression_data$gene_expression_names

          if(all(is.na(gene_expression_selection)) || all(gene_expression_selection == "")){
            updateSelectInput(session, inputId = "gsva_gene_expression", choices = "Import Data")
          }else{
            gene_expression_selection <- gene_expression_selection[which(!is.na(gene_expression_selection) & gene_expression_selection != "")]
            updateSelectInput(session, inputId = "gsva_gene_expression", choices = c(gene_expression_selection, "Import Data"), selected = gene_expression_selection[1])
          }

        }else{

          updateSelectInput(session, inputId = "gsva_gene_expression", choices = "Import Data")

        }

      })

      ## Updates feature set, input score, and gene expression set choices for downloading dataset ####
      observeEvent({
        input$download_fs_options
        extdata()
      }, {

        ns <- session$ns

        selected_fs <- isolate({ input$download_fs_options })

        input_score_data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs) %>% dplyr::distinct(input_score_paths, .keep_all=TRUE)
        input_score_selection <- input_score_data$input_score_paths

        gene_expression_data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs) %>% dplyr::distinct(gene_expression_paths, .keep_all=TRUE)
        gene_expression_selection <- gene_expression_data$gene_expression_paths

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
          filename <- extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)] %>% unique()
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

            data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs & !is.na(input_score_paths) & input_score_paths != "") %>% dplyr::distinct(input_score_paths, .keep_all=TRUE)
            input_score_paths <- data$input_score_paths
            input_score_names <- data$input_score_names

            for(d in 1:length(input_score_paths)){
              #d=1;
              if(tools::file_ext(input_score_paths[d]) == "rda"){
                envir_name <- base::load(input_score_paths[d])
                input_score <- base::get(envir_name)
              }else{
                input_score <- base::readRDS(input_score_paths[d])
              }
              dl_data_names <- c(names(dl_data), input_score_names[d])
              dl_data <- c(dl_data, list(input_score))
              names(dl_data) <- dl_data_names
            }

          }

          if(include_gene_expression){

            data <- extdata() %>% dplyr::filter(feature_set_paths == selected_fs & !is.na(gene_expression_paths) & gene_expression_paths != "") %>% dplyr::distinct(gene_expression_paths, .keep_all=TRUE)
            gene_expression_paths <- data$gene_expression_paths
            gene_expression_names <- data$gene_expression_names

            for(d in 1:length(gene_expression_paths)){
              #d=1;
              #print(gene_expression_paths[d])
              if(tools::file_ext(gene_expression_paths[d]) == "rda"){
                envir_name <- base::load(gene_expression_paths[d])
                ge_set <- base::get(envir_name)
              }else{
                ge_set <- base::readRDS(gene_expression_paths[d])
              }
              dl_data_names <- c(names(dl_data), gene_expression_names[d])
              dl_data <- c(dl_data, list(gene_expression=ge_set))
              names(dl_data) <- dl_data_names
            }

          }

          saveRDS(dl_data, file)

        }
      )

      #
      # Start CaDrA Search ####
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
        error_message(NULL)

        ## Show cadra loading icon
        session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("loading_icon"), display="yes"))

        ## Get feature set ####
        feature_set <- isolate({ input$feature_set })

        if(feature_set == "Import Data"){

          inputfile <- input$ES_file;
          inputtype <- input$ES_file_type;

          if(is.null(inputfile)){
            error_message("Please choose a 'Feature Set' file to import.")
            return(NULL)
          }

          csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
          rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

          if(inputtype %in% ".csv" & length(csv_ext) > 0){

            # Read in the FS file
            FS <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

            if("Features" %in% colnames(FS)){

              # Convert FS to SummarizedExperiment object
              FS <- FS %>% tibble::column_to_rownames(var="Features") %>%
                dplyr::mutate_all(as.numeric) %>% 
                as.matrix() %>% 
                SummarizedExperiment::SummarizedExperiment(
                  assays=SimpleList(counts=.),
                  rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
                  colData=data.frame(samples=colnames(.), row.names = colnames(.))
                )
              
            }else{
              
              error_message("The 'Feature Set' file must contain a 'Features' column name which contains unique names or labels for the features.")
              return(NULL)

            }

          }else if (inputtype %in% ".rds" & length(rds_ext) > 0){

            FS <- base::readRDS(inputfile$datapath)

          }else{

            error_message("Incorrect file format. Please check your 'Feature Set' file again.")
            return(NULL)

          }

        }else{

            if(tools::file_ext(feature_set) == "rda"){
              envir_name <- base::load(feature_set)
              FS <- base::get(envir_name)
            }else{
              FS <- base::readRDS(feature_set)
            }

        }

        ## Get input score ####
        input_score <- isolate({ input$input_score })

        if(input_score == "Import Data"){

          inputfile <- input$input_score_file;
          inputtype <- input$input_score_file_type;

          if(is.null(inputfile)){
            error_message("Please choose a 'Input Score' file to import.")
            return(NULL)
          }

          csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
          rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

          if(inputtype %in% ".csv" & length(csv_ext) > 0) {

            dat <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

            if(all(c("Samples", "Scores") %in% colnames(dat))){
              input_score <- as.numeric(dat$Scores)
              names(input_score) <- as.character(dat$Samples)
            }else{
              error_message("The 'Input Score' file must be a data frame with two columns: Samples and Scores.")
              return(NULL)
            }

          } else if (inputtype %in% ".rds" & length(rds_ext) > 0){

            input_score <- base::readRDS(inputfile$datapath)

          } else {

            error_message("Incorrect file format. Please check your 'Input Score' file again.")
            return(NULL)

          }

        }else{

          if(tools::file_ext(input_score) == "rda"){
            envir_name <- base::load(input_score)
            input_score <- base::get(envir_name)
          }else{
            input_score <- base::readRDS(input_score)
          }

        }
        
        ## Getting the overlapping samples btw input scores and FS
        overlap <- intersect(names(input_score), colnames(FS))
        input_score <- input_score[overlap]
        FS <- FS[,overlap]
        
        # Obtain pre-filter data's minimum cutoff parameter ####
        min_cutoff <- as.integer(input$min_cutoff)

        #print(sprintf("minimum cutoff: %s", min_cutoff))

        if(is.na(min_cutoff) || length(min_cutoff)==0 || min_cutoff < 5){

          error_message("Please specify an integer value for Min Event Frequency >= 5 \n")
          return(NULL)

        } else {

          if(min_cutoff > ncol(FS)){
            error_message(sprintf("There are not enough samples  in \'Feature Set\' to meet Min Event Frequency = %s \n", min_cutoff))
            return(NULL)
          }

          percent_min_cutoff <- round(min_cutoff/ncol(FS), 2)

        }

        # Obtain pre-filter data's maximum cutoff parameter ####
        max_cutoff <- as.numeric(input$max_cutoff)

        #print(sprintf("maximum cutoff: %s", max_cutoff))

        if(is.na(max_cutoff) || length(max_cutoff)==0 ||
           max_cutoff <= 0 || max_cutoff > 100){
          error_message("Please specify a value for Max Event Frequency between 1 and 100\n")
          return(NULL)
        } else {
          max_cutoff <- max_cutoff/100
        }

        ## Keep a record of the number of features in original FS
        n_orig_features <- nrow(FS)

        ## Pre-filter FS based on occurrence frequency ####
        FS <- CaDrA::prefilter_data(
          FS = FS,
          max_cutoff = max_cutoff,
          min_cutoff = percent_min_cutoff
        )

        # Make sure matrix is not empty after removing uninformative features
        if(nrow(assay(FS)) == 0){
          error_message("Features filtering based on given 'Min Event Frequency' and 'Max Event Frequency' yield an empty \'Feature Set\'.\n")
          return(NULL)
        }

        # Check if the FS is a SummarizedExperiment class object
        if(!is(FS, "SummarizedExperiment")){
          error_message("'FS' must be a SummarizedExperiment class object.")
          return(NULL)
        }

        # Check if the dataset has only binary 0 or 1 values
        if(!all(assay(FS) %in% c(0,1))){
          error_message("The \'Feature Set\' (FS) must contain only binary values (0/1) with no NAs.\n")
          return(NULL)
        }

        # Make sure the input FS has row names for features tracking
        if(is.null(rownames(FS))){
          error_message("The FS object does not have rownames or featureData to track the features by. Please provide unique features or rownames for the \'Feature Set\'.\n")
          return(NULL)
        }

        # Make sure the FS object has row names for features tracking
        if(is.null(colnames(FS))){
          error_message("The FS object does not have column names to ",
                        "track samples by. Please provide unique sample names ",
                        "for the FS object.\n")
          return(NULL)
        }

        # Check if input_score is provided and contains a vector of continuous values
        # (with no NAs). Additionally, check if it has names or labels that match
        # the column names of the FS object
        if(length(input_score) == 0 || any(!is.numeric(input_score)) ||
           any(is.na(input_score)) || is.null(names(input_score)) ||
           any(!names(input_score) %in% colnames(assay(FS)))){
          error_message("input_score must contain a vector of continuous scores ",
                        "(with no NAs), and its vector names or labels must match the column ",
                        "names of the FS object.\n")
          return(NULL)
        }

        # Check if the features have either all 0s or 1s values
        if(any(rowSums(assay(FS)) %in% c(0, ncol(assay(FS))))){
          error_message("The FS object has features that are either all 0s or 1s. ",
                        "These features must be removed from the FS object as ",
                        "they are uninformative.")
          return(NULL)
        }

        # Get scoring method ####
        method <- input$method;

        # Whether to perform a weighted-KS test ####
        if(method == "ks_pval" || method == "ks_score"){

          if(input$weighted_ks == TRUE){

            inputfile <- input$weight_file;
            inputtype <- input$weight_file_type;

            if(is.null(inputfile)){
              error_message("Please choose a 'Weighted KS' file to import.")
              return(NULL)
            }

            csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
            rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

            if(inputtype %in% ".csv" & length(csv_ext) > 0){

              dat <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

              if(all(c("Samples", "Weights") %in% colnames(dat))){
                weight <- as.numeric(dat$Weights)
                names(weight) <- as.character(dat$Samples)
              }else{
                error_message("The 'Weighted KS' file must be a data.frame with two columns: Samples and Weights.")
                return(NULL)
              }

            }else if(inputtype %in% ".rds" & length(rds_ext) > 0){

              weight <- base::readRDS(inputfile$datapath)

            }else{

              error_message("Incorrect file format. Please check your 'Weighted KS' file again.")
              return(NULL)

            }

            # Check weight is provided and are continuous values with no NAs
            if(length(weight) == 0 || any(!is.numeric(weight)) || any(is.na(weight))){
              error_message("weight must be a vector of continous values (with no NAs) with the vector names matching the colnames of the \'Feature Set\'.\n")
              return(NULL)
            }

            # Make sure the weight has names or labels that are the same as the colnames of FS
            if(is.null(names(weight))){
              error_message("The weight object must have names or labels to track the samples by. Please provide unique sample names or labels that match the colnames of the \'Feature Set\'.\n")
              return(NULL)
            }

            # Make sure the weight has the same length as number of samples in FS
            if(length(weight) != ncol(FS)){

              error_message("The weight must have the same length as the number of columns in the \'Feature Set\'.\n")
              return(NULL)

            }else{

              if(any(!names(weight) %in% colnames(FS))) {
                error_message("The weight object must have names or labels that match the colnames of the \'Feature Set\'.\n")
                return(NULL)
              }

              # match colnames of Feature Set with names of provided weight
              weight <- weight[colnames(FS)]

            }
          } else {

            weight <- NULL

          }
        }

        ## Get alternative hypothesis from a given scoring method ####
        if(method %in% c("ks_pval", "ks_score", "wilcox_pval", "wilcox_score")){
          alternative <- input$alternative
        }

        ## Get search method ####
        search_method <- input$search_method;

        ## Get max size ####
        max_size <- as.integer(input$max_size)

        if(is.na(max_size) || length(max_size) == 0 || max_size <= 0){
          error_message("Please specify an integer value specifies a maximum size that a meta-feature can extend to do for a given search (max_size must be >= 1).\n")
          return(NULL)
        }

        if(max_size > nrow(FS)){
          error_message("Please specify a \'Max meta-feature size\' lesser than the number of features in the Feature Set.\n")
          return(NULL)
        }

        ## Get search start ####
        initial_seed <- input$initial_seed

        if(initial_seed == "top_N_seeds"){

          search_start <- NULL
          top_N <- as.integer(input$top_N)

          if(is.na(top_N) || length(top_N) == 0 || top_N <= 0){
            error_message("Please specify an integer value to evaluate over Top N features (top_N must be >= 1).\n")
            return(NULL)
          }

          if(top_N > nrow(FS)){
            error_message("Please specify a Top N value lesser than the number of features in the Feature Set.\n")
            return(NULL)
          }

        }else{

          search_start <- strsplit(as.character(input$search_start), ",", fixed=TRUE) %>% unlist() %>% trimws()
          top_N <- NULL

          if(length(search_start) == 0 || any(!search_start %in% rownames(FS))){
            error_message("The provided starting features: ",
                          paste0(search_start[which(!search_start %in% rownames(FS))], collapse=", "),
                          " does not exist among the row names of FS object.\n")
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
            error_message("Please specify an INTEGER number of permutations (n_perm must be >= 1).\n")
            return(NULL)
          }

          ## Get the number of cores ####
          ncores <- as.integer(input$ncores)

          if(is.na(ncores) || length(ncores)==0 || ncores <= 0){
            error_message("Please specify the number of parallelization cores for permutation testing (ncores must be >= 1).\n")
            return(NULL)
          }

          if(ncores > num_of_cores){
            error_message(paste0("There are ONLY ", num_of_cores, " cores available on the system. Please specify the number of parallelization cores for permutation testing (ncores <= ", num_of_cores, ")."))
            return(NULL)
          }

          ## Perform permutation-based testings ####
          rVal$cadra_permutation_process <- parallel::mcparallel({

            CaDrA::CaDrA(
              FS = FS,
              input_score = input_score,
              method = method,
              custom_function = NULL,
              custom_parameters = NULL,
              alternative = alternative,
              weight = weight,
              top_N = top_N,
              search_start = search_start,
              search_method = search_method,
              max_size = max_size,
              n_perm = n_perm,
              smooth = TRUE,
              obs_best_score = NULL,
              plot = FALSE,
              ncores = ncores,
              cache_path = NULL
            )

          })

          #print(paste0("cadra permutation process: ", rVal$cadra_permutation_process$pid, " started"))

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
            min_cutoff,
            percent_min_cutoff*100,
            max_cutoff*100,
            max_cutoff*100,
            format(nrow(FS), big.mark = ","),
            format(n_orig_features, big.mark = ","),
            format(ncol(FS), big.mark =",")
          )
        )

        feature_set_data(assay(FS))
        input_score_data(input_score)

        # Compute candidate search ####
        rVal$candidate_search_process <- parallel::mcparallel({

          CaDrA::candidate_search(
            FS = FS,
            input_score = input_score,
            method = method,
            custom_function = NULL,
            custom_parameters = NULL,
            alternative = alternative,
            weight = weight,
            search_start = search_start,
            top_N = top_N,
            search_method = search_method,
            max_size = max_size,
            best_score_only = FALSE,
            do_plot = FALSE
          )

        })

        #print(paste0("candidate search process: ", rVal$candidate_search_process$pid, " started"))

        error_message("NONE")

      })

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

        error_message("Your process has been interrupted")

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
      # Start GSVA Analysis ####
      #

      observeEvent(input$run_gsva, {

        ns <- session$ns

        if(!is.null(gVal$gsva_search_process)) return(NULL)

        gVal$gsva_search_result <- NULL
        gsva_instructions_message(FALSE)
        gsva_error_message(NULL)
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
            gsva_error_message("Please choose a 'Feature Set' file to import.")
            return(NULL)
          }

          csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
          rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

          if(inputtype %in% ".csv" & length(csv_ext) > 0){

            # read in the FS file
            FS <- utils::read.csv(inputfile$datapath, header=TRUE, check.names=FALSE)

            if("Features" %in% colnames(FS)){

              # Convert FS to SummarizedExperiment class object
              # Convert FS to SummarizedExperiment object
              FS <- FS %>% tibble::column_to_rownames(var="Features")  %>%
                dplyr::mutate_all(as.numeric) %>% 
                as.matrix() %>% 
                SummarizedExperiment::SummarizedExperiment(
                  assays=SimpleList(counts=.),
                  rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
                  colData=data.frame(samples=colnames(.), row.names = colnames(.))
                )
              
            }else{
              
              gsva_error_message("The 'Feature Set' file must contain a 'Features' column name that contains unique names or labels to search for best features.")
              return(NULL)
              
            }
            
          }else if (inputtype %in% ".rds" & length(rds_ext) > 0){
            
            feature_set <- base::readRDS(inputfile$datapath)

          }else{

            gsva_error_message("Incorrect file format. Please check your 'Feature Set' file again.")
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
            gsva_error_message("Please choose a 'gene expression' file to import.")
            return(NULL)
          }

          csv_ext <-  grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
          rds_ext <-  grep(toupper(".rds"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

          if(inputtype %in% ".csv" & length(csv_ext) > 0) {

            gene_expression <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

            if("Features" %in% colnames(gene_expression)){

              gene_expression <- gene_expression %>% 
                tibble::column_to_rownames(var="Features")  %>%
                dplyr::mutate_all(as.numeric) %>%
                as.matrix() %>% 
                SummarizedExperiment::SummarizedExperiment(
                  assays=SimpleList(counts=.),
                  rowData=data.frame(features=rownames(.), row.names = rownames(.)), 
                  colData=data.frame(samples=colnames(.), row.names = colnames(.))
                )
              
            }else{

              gsva_error_message("The 'Feature Set' file must contain a 'Features' column name that contains unique names or labels to search for best features.")
              return(NULL)

            }

          }else if(inputtype %in% ".rds" & length(rds_ext) > 0){

            gene_expression <- base::readRDS(inputfile$datapath)

          }else {

            gsva_error_message("Incorrect file format. Please check your 'Gene Expression' file again.")
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
          gsva_error_message("Please choose a 'geneset' file to import.")
          return(NULL)
        }

        csv_ext <- grep(toupper(".csv"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)
        gmt_ext <- grep(toupper(".gmt"), toupper(substr(inputfile$datapath, nchar(inputfile$datapath)-4, nchar(inputfile$datapath))), fixed = TRUE)

        if(inputtype %in% ".csv" & length(csv_ext) > 0){

          genelist <- utils::read.csv(inputfile$datapath, header = TRUE, check.names = FALSE)

          # Getting gene list for each geneset collection
          for(u in 1:ncol(genelist)){
            #u=1;
            genesetname <- c(genesetname, colnames(genelist)[u])
            genesetcollection <- c(genesetcollection, list(genelist=genelist[,u]))
            names(genesetcollection) <- genesetname
          }

        }else if(inputtype %in% ".gmt" & length(gmt_ext) > 0){

          genesetcollection <- GSEABase::getGmt(inputfile$datapath)

        }else{

          gsva_error_message("Incorrect file format. Please check your 'Geneset' file again.")
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

        gsva_error_message("NONE")

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
        gsva_error_message("Your process has been interrupted")

        ## Hide cadra loading icon
        session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gsva_loading_icon"), display="no"))

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

      output$gsva_error_message <- shiny::renderUI({

        req(gsva_error_message())

        ns <- session$ns

        if(gsva_error_message() != "NONE"){

          ## Update loading icon
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("gsva_loading_icon"), display="no"))

          # Show instructions
          gsva_instructions_message(TRUE)

          # Display error message
          p(style="color: red; font-weight: bold; margin-bottom: 10px;", gsva_error_message())

        }

      })

      # Title for GSVA ####
      output$gsva_enrichment_title <- shiny::renderUI({

        req(gVal$gsva_search_result)
        
        selected_fs = isolate({ input$gsva_feature_set })
        selected_fs_name = extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)] %>% unique()
        
        h3(paste0("Enrichment Scores for ", selected_fs_name))

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
            title = "Download Enrichment Scores",
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
          fsname <- extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)]
          paste0("CaDrA-", fsname, "-GSVA.csv")
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
          fsname <- extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)]
          paste0("CaDrA-", fsname, "-GSVA.rds")
        },

        content = function(file) {

          # Get gsva result to SummarizedExperiment object
          geneset <- gVal$gsva_search_result %>% 
            as.matrix() %>% 
            SummarizedExperiment::SummarizedExperiment(
              assays=SimpleList(counts=FS),
              rowData=data.frame(features=rownames(FS), row.names = rownames(FS)), 
              colData=data.frame(samples=colnames(FS), row.names = colnames(FS))
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
            label = strong("Add to CaDrA's Input Scores"),
            style = "background: blue; color: white;",
            onclick = paste0("gsva_selected_checkbox('", id, "')")
          ),

          br(), br(),

          uiOutput(outputId = ns("enrichment_error_message"))

        )

      })

      # Add enrichment scores to CaDrA's input scores ####
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
          
          score_dir <- file.path(system.file(package = "CaDrA.shiny"), "extdata", "input_score")
          
          if(!dir.exists(score_dir)){
            dir.create(score_dir)
          }
          
          selected_fs = isolate({ input$gsva_feature_set })
          selected_fs_name = extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)] %>% unique()
          
          selected_gene_expression = isolate({ input$gsva_gene_expression })
          selected_gene_expression_name <- extdata()$gene_expression_names[which(extdata()$gene_expression_paths == selected_gene_expression)] %>% unique()
          
          new_input_score <- data.frame(
            feature_set_names = rep(selected_fs_name, length(row)),
            feature_set_paths = rep(selected_fs, length(row)),
            input_score_names = NA,
            input_score_paths = NA,
            gene_expression_names = rep(selected_gene_expression_name, length(row)),
            gene_expression_paths = rep(selected_gene_expression, length(row)),
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
            
            new_input_score$input_score_names[r] <- selected_geneset
            new_input_score$input_score_paths[r] <- selected_score_path
          }
          
          new_input_score <- new_input_score %>% dplyr::mutate_all(., as.character)
          
          if(tools::file_ext(datalist_file) == "csv"){
            datalist_table <- read.csv(datalist_file, header=TRUE) %>% dplyr::mutate_all(., as.character)
          }else if(tools::file_ext(datalist_file) == "rds"){
            datalist_table <- readRDS(datalist_file) %>% dplyr::mutate_all(., as.character)
          }
          
          # Save the updated list
          updated_datalist <- datalist_table %>%
            dplyr::bind_rows(new_input_score) %>% 
            dplyr::distinct(., .keep_all=TRUE)
          
          # Check if external data exists in package
          if(tools::file_ext(datalist_file) == "csv"){
            write.csv(updated_datalist, datalist_file, row.names = F)
          }else if(tools::file_ext(datalist_file) == "rds"){
            saveRDS(updated_datalist, datalist_file)
          }
          
          updated_extdata <- get_extdata(datalist_file, global_feature_set_paths, global_input_score_paths, global_gene_expression_paths)
          
          extdata(updated_extdata)
          
          genelist_names <- paste0(new_input_score$input_score_names, collapse = ", ")
          
          enrichment_table_message(paste0("<em style='font-weight: bold;'>", genelist_names, "</em>", ifelse(length(genelist_names) == 1, " has", " have"), " been added as 'Input Scores' for <em style='font-weight: bold;'>", selected_fs_name , "</em>."))
          
        }else{
          
          enrichment_table_message("<span style='color: red; font-weight: bold;'>Select a geneset above to add to CaDrA's Input Scores</span>")
          
        }

      }, ignoreInit = TRUE)

      #
      # Render GSVA's enrichment error messages ####
      #

      output$enrichment_error_message <- shiny::renderUI({

        req(enrichment_table_message())

        # Display error message
        p(style="margin-bottom: 10px;", HTML(enrichment_table_message()))
        
      })

      #
      # Render CaDrA's error messages ####
      #

      output$error_message <- shiny::renderUI({

        req(error_message())

        ns <- session$ns

        if(error_message() != "NONE"){

          ## Update loading icon
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("loading_icon"), display="no"))

          ## Update loading icon for permutation test
          session$sendCustomMessage(type = "ToggleOperation", message = list(id=ns("permutation_loading_icon"), display="no"))

          # Show instructions
          instructions_message(TRUE)

          # Display error message
          p(style="color: red; font-weight: bold; margin-bottom: 10px;", error_message())

        }

      })

      ## Feature Set Tittle for CaDrA Search ####
      output$featureData_title <- shiny::renderUI({

        req(rVal$candidate_search_result, feature_set_description())

        ns <- session$ns

        description <- feature_set_description()

        selected_fs <- isolate({ input$feature_set })

        if (selected_fs == "Import Data"){
          title <- "Dataset: Imported Data"
        }else{
          title <- paste0("Feature Set: ", extdata()$feature_set_names[which(extdata()$feature_set_paths == selected_fs)] %>% unique())
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
          paste0("CaDrA-Filtered-Features-Set.csv")
        },

        content = function(file) {
          FS_table <- feature_set_data() %>% as.data.frame(.) %>% rownames_to_column(var="Features")
          write.csv(FS_table, file, row.names=FALSE)
        }
      )

      ## Title for Best Meta-Feature ####
      output$bestFeatureData_title <- shiny::renderUI({

        req(rVal$candidate_search_result)

        h3("Best Meta-Feature Set")

      })

      ## Output Best Meta-Feature FS ####
      output$bestFeatureData <- DT::renderDataTable({

        req(rVal$candidate_search_result)

        ns <- session$ns

        topn_best_meta <- topn_best(topn_list=rVal$candidate_search_result)

        ES_table <- topn_best_meta[["feature_set"]] %>% assay(.) %>% as.data.frame(.)

        hover_columns <- create_hover_txt(table = ES_table)

        table <- ES_table  %>%
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
          FS_table <- topn_best_meta[["feature_set"]] %>% assay(.) %>% as.data.frame(.) %>% tibble::rownames_to_column(., var="Features")
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
          FS_table <- topn_best_meta[["feature_set"]]
          saveRDS(FS_table, file)
        }

      )

      ## Title for Input Scores ####
      output$inputScoreData_title <- shiny::renderUI({

        req(rVal$candidate_search_result, input_score_data())

        selected_input_score <- isolate({ input$input_score })

        if(selected_input_score == "Import Data"){
          title <- "Imported Data"
        }else{
          title <- extdata()$input_score_names[which(extdata()$input_score_paths == selected_input_score)]
        }

        h3("Input Score:", title)

      })

      ## Output Input Scores ####
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
            title = "Download Observed Input Scores",
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
          paste0("CaDrA-Observed-Input-Scores.csv")
        },

        content = function(file) {

          input_score <- input_score_data() %>% signif(., digits = 4)

          score_table <- data.frame(
            Samples = names(input_score),
            Scores = input_score,
            stringsAsFactors = FALSE
          )

          write.csv(score_table, file, row.names=FALSE)

        }
      )

      ## Download input score in RDS format ####
      output$downloadScoreRDS <- downloadHandler(

        filename = function() {
          paste0("CaDrA-Observed-Input-Scores.rds")
        },

        content = function(file) {

          input_score <- input_score_data() %>% signif(., digits = 4)

          score_table <- data.frame(
            Samples = names(input_score),
            Scores = input_score,
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
               "NOTE: Cannot plot overlap matrix with provided top N seed = 1 or the number of provided feature names = 1.")
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

        h3("Permutation-Based Testing")

      })

      ## Output permutation plot ####
      output$permutation_plot <- shiny::renderPlot({

        req(rVal$cadra_permutation_result)

        perm_res <- rVal$cadra_permutation_result

        permutation_plot(perm_res)

      })

    }
  )
  
}

#' Run both Shiny UI and Server Modules 
#' 
#' @param id A unique namespace identifier
#' @param datalist_file A path to data file (in cvs or rds format) listing 
#' the absolute paths of necessary files to start the app. 
#' 
#' The datalist_file must contains the following variables:
#' 'feature_set' (required), 'feature_set_paths' (repuired), 
#' 'input_score_names' (required), input_score_paths' (required), 
#' 'gene_expression_names' (optional), 'gene_expression_paths' (optional)
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
#' @export
CaDrA_App <- function(id, datalist_file) {
  
  ui <- shiny::fluidPage(
    titlePanel("CaDrA: Candidate Drivers Analysis"),
    helpText("Multi-Omic Search for Candidate Drivers of Functional Signatures"),
    CaDrA_UI(id = id, datalist_file = datalist_file)
  )
  
  server <- function(input, output, session) {
    CaDrA_Server(id = id, datalist_file = datalist_file)
  }
  
  shiny::shinyApp(ui=ui, server=server)
  
}







