
<!-- README.md is generated from README.Rmd. Please edit this file -->

# CaDrA.shiny

An R Shiny Dashboard for Interacting with CaDrA package

## **Documentation: <https://montilab.github.io/CaDrA/>**

## **Web Portal: <https://cadra.bu.edu/>**

## Overview

**CaDrA-shiny** is an interactive Shiny dashboard that was developed to
allow users to directly interacting with **CaDrA** package. **CaDrA** is
an R package that supports a heuristic search framework aimed at
identifying candidate drivers of a molecular phenotype of interest
(visit [our Github repo](https://github.com/montilab/CaDrA) for more
details).

The CaDrA shiny dashboard has two distinctive features:

1.  Run **CaDrA** to search for candidate drivers of a molecular
    phenotype of interest
2.  Run **GSVA** to obtain the relative enrichment scores for a given
    gene sets, then subsequently, these scores are used to run **CaDrA**
    to identify complementary features that likely driving the input of
    molecular phenotype.

Data visualization includes:

- Meta-feature plot
- KS enrichment plot
- Top N candidates overlapping heatmap
- Permutation-based testing plot

The **CaDrA.shiny** package is containerized using **Docker** and can be
deployed on any Cloud-based services.

**Docker Image:
[montilab/cadra-shiny](https://hub.docker.com/r/montilab/cadra-shiny)**

### (1) Installation

``` r
library(devtools)
devtools::install_github("montilab/CaDrA.shiny")
```

### (2) Quickstart

``` r
library(CaDrA.shiny)
```

### (3) Launch Shiny app

``` r
id <- "myapp"
CaDrA.shiny::CaDrA_App(id=id)
```
