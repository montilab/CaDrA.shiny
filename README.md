
<!-- README.md is generated from README.Rmd. Please edit this file -->
<p style="font-size: 40pt; front-weight: bold; padding-bottom: 0px; margin-bottom: 0;">
CaDrA.shiny
</p>
<hr style="padding-top: 0; margin-top: 0;">

![build](https://github.com/montilab/cadra/workflows/rcmdcheck/badge.svg)
![Gitter](https://img.shields.io/gitter/room/montilab/cadra.shiny)
![GitHub
issues](https://img.shields.io/github/issues/montilab/cadra.shiny)
![GitHub last
commit](https://img.shields.io/github/last-commit/montilab/cadra.shiny)

An R Shiny Dashboard for Interacting with
**[CaDrA](https://montilab.github.io/CaDrA/)** Package

### **CaDrA: <https://montilab.github.io/CaDrA/>**

### **Web Portal: <https://cadra.bu.edu/>**

## Overview

**CaDrA-shiny** is an interactive R Shiny dashboard developed to allow
users to directly interact with the **CaDrA** package. **CaDrA** is an R
package that supports a heuristic search framework aimed at identifying
candidate drivers of a molecular phenotype of interest (visit [our
Github repo](https://github.com/montilab/CaDrA) for more details).

The CaDrA’s shiny dashboard has two distinctive features:

1.  Run **CaDrA** search to identify candidate drivers of a molecular
    phenotype of interest.
2.  Run **GSVA** to estimate aggregate enrichment scores by projecting a
    (gene) expression dataset onto a given gene set or signature,
    usually representing a molecular phenotype.

Data visualizations include:

- Meta-feature plot.
- Kolmogorov Smirnov (KS) enrichment plot.
- Top N candidates overlapping heatmap.
- Permutation plot.

The **CaDrA.shiny** package is already containerized using **Docker**
and can be deployed on any Cloud-based services.

#### **Docker image: [montilab/cadra-shiny](https://hub.docker.com/r/montilab/cadra-shiny)**

#### Useful Guides

- <a href="articles/docker.html" target="_blank">Containerizing
  CaDrA.shiny with Docker</a>
- <a href="articles/docker-compose.html" target="_blank">Launching
  CaDrA’s Shiny Dashboard with Compose</a>

### (1) Installation

``` r
library(devtools)
devtools::install_github("montilab/CaDrA.shiny")
```

### (2) Load packages

``` r
library(CaDrA.shiny)
library(CaDrA)
library(GSVA)
library(knitr)
library(hypeR)
```

### (3) Run CaDrA with dataset downloaded from CaDrA Portal

Here, we show how to run CaDrA on a dataset downloaded from the CaDrA
Portal, using input_score’s derived by applying GSVA to the downloaded
gene expression dataset and the
[Hallmarks](https://www.gsea-msigdb.org/gsea/msigdb/human/genesets.jsp?collection=H)
genesets. We will run a CaDrA search to look for genetic drivers of the
*“Epithelial Mesenchymal Transition”* (EMT)-derived score.

#### (i) Retrieve a list of descriptors of pre-processed feature sets available on the portal

``` r
## Get a list of descriptors of feature sets available on CaDrA Portal
fs_list <- CaDrA.shiny::get_feature_set(order_by="asc")
```

``` r
## Show the description of the first few feature sets
knitr::kable(head(fs_list))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
description
</th>
<th style="text-align:left;">
feature_set_name
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ACC
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
<tr>
<td style="text-align:left;">
BLCA
</td>
<td style="text-align:left;">
TCGA_BLCA_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
<tr>
<td style="text-align:left;">
BRCA
</td>
<td style="text-align:left;">
TCGA_BRCA_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
<tr>
<td style="text-align:left;">
CESC
</td>
<td style="text-align:left;">
TCGA_CESC_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
<tr>
<td style="text-align:left;">
CHOL
</td>
<td style="text-align:left;">
TCGA_CHOL_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
<tr>
<td style="text-align:left;">
COADREAD
</td>
<td style="text-align:left;">
TCGA_COADREAD_2016_01_28_GISTIC_MUT_SIG
</td>
</tr>
</tbody>
</table>

#### (ii) Retrieve datasets from the portal

``` r
## Retrieve the ACC dataset (both genetic feature set and gene expression)
datasets <- CaDrA.shiny::pull_datasets(
  feature_set = "TCGA_ACC_2016_01_28_GISTIC_MUT_SIG",
  include_gene_expression = TRUE
)
datasets
```

    $feature_set
    class: RangedSummarizedExperiment 
    dim: 7119 88 
    metadata(3): experimentData annotation protocolData
    assays(1): exprs
    rownames(7119): Amp1q22 Amp4p16.3 ... ZZEF1 ZZZ3
    rowData names(1): Feature
    colnames(88): TCGA-OR-A5J1-01 TCGA-OR-A5J2-01 ... TCGA-PK-A5HB-01
      TCGA-PK-A5HC-01
    colData names(0):

    $gene_expression
    class: RangedSummarizedExperiment 
    dim: 19777 79 
    metadata(3): experimentData annotation protocolData
    assays(1): exprs
    rownames(19777): IGF2 DLK1 ... TGM6 SPANXA2
    rowData names(1): Genes
    colnames(79): TCGA-OR-A5J1-01 TCGA-OR-A5J2-01 ... TCGA-PK-A5HA-01
      TCGA-PK-A5HB-01
    colData names(1): Samples

#### (iii) Run GSVA on the downloaded expression dataset

``` r
## download MSigDB’s Hallmark genesets
hallmarks <- msigdb_gsets("Homo sapiens", "H", clean=TRUE)$genesets # returns 50 genesets 

# Compute the gsva scores of the 50 hallmark genesets
input_score_matrix <- GSVA::gsva(
  expr = SummarizedExperiment::assay(datasets$gene_expression), 
  gset.idx.list = hallmarks,
  method = "gsva",
  mx.diff = TRUE,
  verbose = FALSE
)
```

``` r
## Show few entries of the returned hallmark-by-sample matrix
knitr::kable(input_score_matrix[1:5,1:5])
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
TCGA-OR-A5J1-01
</th>
<th style="text-align:right;">
TCGA-OR-A5J2-01
</th>
<th style="text-align:right;">
TCGA-OR-A5J3-01
</th>
<th style="text-align:right;">
TCGA-OR-A5J5-01
</th>
<th style="text-align:right;">
TCGA-OR-A5J6-01
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Adipogenesis
</td>
<td style="text-align:right;">
-0.0534716
</td>
<td style="text-align:right;">
-0.2768861
</td>
<td style="text-align:right;">
0.0953223
</td>
<td style="text-align:right;">
-0.4063615
</td>
<td style="text-align:right;">
0.4394275
</td>
</tr>
<tr>
<td style="text-align:left;">
Allograft Rejection
</td>
<td style="text-align:right;">
-0.0744033
</td>
<td style="text-align:right;">
-0.0621729
</td>
<td style="text-align:right;">
-0.3521640
</td>
<td style="text-align:right;">
-0.3018557
</td>
<td style="text-align:right;">
0.1431379
</td>
</tr>
<tr>
<td style="text-align:left;">
Androgen Response
</td>
<td style="text-align:right;">
-0.1832658
</td>
<td style="text-align:right;">
0.0855770
</td>
<td style="text-align:right;">
0.1389136
</td>
<td style="text-align:right;">
-0.5098008
</td>
<td style="text-align:right;">
-0.0719647
</td>
</tr>
<tr>
<td style="text-align:left;">
Angiogenesis
</td>
<td style="text-align:right;">
-0.1895439
</td>
<td style="text-align:right;">
0.2684503
</td>
<td style="text-align:right;">
0.2747169
</td>
<td style="text-align:right;">
-0.1479879
</td>
<td style="text-align:right;">
-0.4408048
</td>
</tr>
<tr>
<td style="text-align:left;">
Apical Junction
</td>
<td style="text-align:right;">
-0.1188141
</td>
<td style="text-align:right;">
0.1778803
</td>
<td style="text-align:right;">
0.0797656
</td>
<td style="text-align:right;">
0.0144147
</td>
<td style="text-align:right;">
-0.1364014
</td>
</tr>
</tbody>
</table>

#### (iv) Run candidate search with input scores obtained in (iii)

``` r
## Samples to keep based on the overlap between the two inputs
sample_overlap <- intersect(colnames(input_score_matrix), colnames(datasets$feature_set))
input_score <- input_score_matrix["Epithelial Mesenchymal Transition", sample_overlap]
FS <- datasets$feature_set[, sample_overlap, drop = FALSE]

## Pre-filter FS based on occurrence frequency
FS_filtered <- CaDrA::prefilter_data(
  FS = FS,
  max_cutoff = 0.6,  # max event frequency (60%)
  min_cutoff = 0.03  # min event frequency (3%)
)  
## Run candidate search
topn_result <- CaDrA::candidate_search(
  FS = FS_filtered,
  input_score = input_score,
  method = "ks_pval",      # Use Kolmogorov-Smirnov scoring function 
  weights = NULL,          # If weights are provided, perform a weighted-KS (gsea-like) test
  alternative = "less",    # Use one-sided hypothesis testing
  search_method = "both",  # Apply both forward and backward search
  top_N = 1,               # Perform only one search (starting from top scoring feature)
  max_size = 7,            # Maximum number of features to include in the returned meta-feature
  do_plot = FALSE,         # Plot after finding the best features
  best_score_only = FALSE  # Return meta-feature set, observed input scores, and calculated best score
)
```

### (v) Visualize Best Results

``` r
## Fetch the meta-feature yielding the best score over N searches
## .. (in this example, only N=1 search was performed)
topn_best_meta <- CaDrA::topn_best(topn_result)

## Visualize the best results with the meta-feature plot
CaDrA::meta_plot(topn_best_list = topn_best_meta, input_score_label = NULL)
```

![](./man/figures/unnamed-chunk-8-1.png)<!-- -->

### (vi) Compute permutation-based p-value

``` r
## Permutation seed (for reproducible results)
set.seed(123)

## Run CaDrA Search 1000 times on permuted input scores to estimate the null
perm_res <- CaDrA::CaDrA(
  FS = FS_filtered, 
  input_score = input_score, 
  method = "ks_pval",
  top_N = 1,
  max_size = 7,
  search_method = "both",     
  n_perm = 1000,
  ncores = 1
)
## Visualize permutation results
permutation_plot(perm_res = perm_res)
```

![](./man/figures/unnamed-chunk-9-1.png)<!-- -->

### (4) Launch CaDrA’s Shiny App with your pre-proccessed dataset

Here, we show how to launch a local instance of the CaDrA Portal, which
will be populated with a user-selected set of datasets. In the example,
a single dataset (ACC) will be uploaded to the portal.

#### (i) Pull pre-processed feature sets using our REST API

``` r
# Download feature sets and return a datalist with appropriate paths to dataset
mydatafile <- CaDrA.shiny::download_feature_sets(
  #feature_set = fs_list$feature_set_name,  # this would download all TCGA datasets
  feature_set = "TCGA_ACC_2016_01_28_GISTIC_MUT_SIG",
  include_input_score = TRUE,
  include_gene_expression = TRUE,
  out_dir = file.path(Sys.getenv("HOME"),"Github") # specify your folder of choice here
)
```

``` r
# Look at the top 6 rows
knitr::kable(head(mydatafile))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
feature_set_name
</th>
<th style="text-align:left;">
feature_set_path
</th>
<th style="text-align:left;">
input_score_name
</th>
<th style="text-align:left;">
input_score_path
</th>
<th style="text-align:left;">
gene_expression_name
</th>
<th style="text-align:left;">
gene_expression_path
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/NA
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
HALLMARK_TNFA_SIGNALING_VIA_NFKB
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_TNFA_SIGNALING_VIA_NFKB.rds
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
HALLMARK_HYPOXIA
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_HYPOXIA.rds
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
HALLMARK_CHOLESTEROL_HOMEOSTASIS
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_CHOLESTEROL_HOMEOSTASIS.rds
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
HALLMARK_MITOTIC_SPINDLE
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_MITOTIC_SPINDLE.rds
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
<tr>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_GISTIC_MUT_SIG
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds
</td>
<td style="text-align:left;">
HALLMARK_WNT_BETA_CATENIN_SIGNALING
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_WNT_BETA_CATENIN_SIGNALING.rds
</td>
<td style="text-align:left;">
TCGA_ACC_2016_01_28_Gene_Expression
</td>
<td style="text-align:left;">
/Users/smonti/Github/download-fs-2023-10-11/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds
</td>
</tr>
</tbody>
</table>

#### (iii) Launch CaDrA’s Shiny app with the downloaded dataset

``` r
## Launch CaDrA's Shiny app with your downloaded datalist retrieved from (ii)
app <- CaDrA.shiny::CaDrA_App(id="myapp", datalist=mydatafile)

## Launch app on localhost with port 3838
shiny::runApp(app, host='0.0.0.0', port=3838)
```

# A Glimpse of CaDrA’s Dashboard

There are five tabs on CaDrA’s Dashboard. Explore each tab and see what
they do:

![](man/figures/tabs.png)

- <a href="articles/run-cadra-tab.html" target="_blank">Run CaDrA</a>
- <a href="articles/run-gsva-tab.html" target="_blank">Run GSVA</a>
- <a href="articles/api.html" target="_blank">Download</a>
- Help
- Publication
- Contract Us

# Getting Help

To get help with **CaDrA**, visit our [Github
dicussion](https://github.com/montilab/CaDrA/discussions) or [Github
issues](https://github.com/montilab/CaDrA/issues).

To get help with **CaDrA.shiny**, visit our [Github
dicussion](https://github.com/montilab/CaDrA.shiny/discussions) or
[Github issues](https://github.com/montilab/CaDrA.shiny/issues).
