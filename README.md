
# CaDrA

Candidate Drivers Analysis: Multi-Omic Search for Candidate Drivers of Functional Signatures

**CaDrA** is an R package that supports a heuristic search framework aimed at identifying candidate drivers of a molecular phenotype of interest. 

The main function takes two inputs:

i) A binary multi-omics dataset represented as a **SummarizedExperiment** class object (where the rows are 1/0 vectors indicating the presence/absence of 'omics' features such as somatic mutations, copy number alterations, epigenetic marks, etc., and the columns represent the samples)

ii) A molecular phenotype represented as a vector of continuous scores (sample-specific scores representing a phenotypic readout of interest, such as protein expression, pathway activity, etc.). 

Based on this input, **CaDrA** implements a forward/backward search algorithm to find the set of features that together is maximally associated with the observed input scores, based on one of several scoring functions (*Kolmogorov-Smirnov*, *Conditional Mutual Information*, *Wilcoxon*, *custom-defined scoring function*), making it useful to find complementary omics features likely driving the input molecular phenotype.

For more information, please see the associated manuscript [Kartha et al. (2019)](https://www.frontiersin.org/articles/10.3389/fgene.2019.00121/full)

## (1) Installation

```r
library(devtools)
devtools::install_github("montilab/CaDrA")
```

## (2) Quickstart

```r
library(CaDrA)
library(SummarizedExperiment)
```

## (3) CaDrA Query of BRCA YAP/TAZ Activity

### (i) Load & Format Data Inputs

```{r load.data}

## Read in BRCA GISTIC+Mutation object
data(BRCA_GISTIC_MUT_SIG)
eset_mut_scna <- BRCA_GISTIC_MUT_SIG

## Read in input score
data(TAZYAP_BRCA_ACTIVITY)
input_score <- TAZYAP_BRCA_ACTIVITY

## Samples to keep based on the overlap between the two inputs
overlap <- intersect(names(input_score), colnames(eset_mut_scna))
eset_mut_scna <- eset_mut_scna[,overlap]
input_score <- input_score[overlap]

## Binarize FS to only have 0's and 1's
assay(eset_mut_scna)[assay(eset_mut_scna) > 1] <- 1.0

## Pre-filter FS based on occurrence frequency
eset_mut_scna_flt <- CaDrA::prefilter_data(
  FS = eset_mut_scna,
  max_cutoff = 0.6,  # max event frequency (60%)
  min_cutoff = 0.03  # min event frequency (3%)
)  

```

### (ii) Run CaDrA

Here, we repeat the candidate search starting from each of the top 'N' features and report the combined results as a heatmap (to summarize the number of times each feature is selected across repeated runs). 

IMPORTANT NOTE: The legacy function `topn_eval()` is equivalent to the recommended `candidate_search()` function

```{r cadra}

topn_res <- CaDrA::candidate_search(
  FS = eset_mut_scna_flt,
  input_score = input_score,
  method = "ks_pval",          # Use Kolmogorow-Smirnow scoring function 
  weight = NULL,               # If weights is provided, perform a weighted-KS test
  alternative = "less",        # Use one-sided hypothesis testing
  search_method = "both",      # Apply both forward and backward search
  top_N = 7,                   # Evaluate top 7 starting points for each search
  max_size = 7,                # Maximum size a meta-feature matrix can extend to
  do_plot = FALSE,             # Plot after finding the best features
  best_score_only = FALSE      # Return meta-feature set, observed input scores and calculated best score
)

```

### (iii) Visualize Best Results

```{r visualize.best}

## Fetch the meta-feature set corresponding to its best scores over top N features searches
topn_best_meta <- CaDrA::topn_best(topn_res)

# Visualize the best results with the meta-feature plot
CaDrA::meta_plot(topn_best_list = topn_best_meta, input_score_label = "YAP/TAZ Activity")

```

### (iv) Summarize Top N Results

```{r summarize}

# Evaluate results across top N features you started from
CaDrA::topn_plot(topn_res) 

```

# CaDrA-shiny
A web interface for interacting with the R package CaDrA

## Web Interface
[https://cadra.bu.edu/](https://cadra.bu.edu/)

### (1) Installation

``` r
library(devtools)
devtools::install_github("montilab/CaDrA-shiny")
```

### (2) Quickstart

``` r
library(CaDrA-shiny)
```

### (3) Launch CaDrA's Shiny app

``` r
id <- "myapp"
`CaDrA-shiny`::CaDrA_App(id=id)
```



