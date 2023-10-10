
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

**CaDrA-shiny** is an interactive R Shiny dashboard that is developed to
allow users to directly interacting with **CaDrA** package. **CaDrA** is
an R package that supports a heuristic search framework aimed at
identifying candidate drivers of a molecular phenotype of interest
(visit [our Github repo](https://github.com/montilab/CaDrA) for more
details).

The CaDrA’s shiny dashboard has two distinctive features:

1.  Run **CaDrA** search to identify candidate drivers of a molecular
    phenotype of interest
2.  Run **GSVA** to obtain relative enrichment scores for a given gene
    sets, then subsequently, these scores are used to run **CaDrA**
    search to look for complementary features that likely driving the
    input of molecular phenotype.

Data visualization includes:

- Meta-feature plot
- Kolmogorov Smirnov (KS) enrichment plot
- Top N candidates overlapping heatmap
- Permutation plot

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
```

### (4) Run CaDrA with dataset downloaded from CaDrA Portal

#### (i) Retrieve a list of pre-processed feature sets available on the portal

``` r
# Get a list of feature sets available on CaDrA Portal
fs_list <- CaDrA.shiny::get_feature_set(order_by="asc")
```

``` r
# Look at the first 6 feature sets
knitr::kable(head(fs_list))
```

| description | feature_set_name                        |
|:------------|:----------------------------------------|
| ACC         | TCGA_ACC_2016_01_28_GISTIC_MUT_SIG      |
| BLCA        | TCGA_BLCA_2016_01_28_GISTIC_MUT_SIG     |
| BRCA        | TCGA_BRCA_2016_01_28_GISTIC_MUT_SIG     |
| CESC        | TCGA_CESC_2016_01_28_GISTIC_MUT_SIG     |
| CHOL        | TCGA_CHOL_2016_01_28_GISTIC_MUT_SIG     |
| COADREAD    | TCGA_COADREAD_2016_01_28_GISTIC_MUT_SIG |

#### (ii) Pull down datasets from the portal

``` r
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

#### (iii) Run GSVA with downloaded datasets

``` r
# Get the first 10 features of the feature set
feature_list <- rownames(datasets$gene_expression)[1:10]
names(feature_list) <- feature_list

# Compute the gsva scores of the first 10 features
input_score_list <- GSVA::gsva(
  expr = SummarizedExperiment::assay(datasets$gene_expression), 
  gset.idx.list = as.list(feature_list),
  method = "gsva",
  mx.diff = TRUE
)
```

    Warning: useNames = NA is deprecated. Instead, specify either useNames = TRUE
    or useNames = TRUE.

    Warning in .gsva(expr, mapped.gset.idx.list, method, kcdf, rnaseq, abs.ranking,
    : Some gene sets have size one. Consider setting 'min.sz > 1'.

    Estimating GSVA scores for 10 gene sets.
    Estimating ECDFs with Gaussian kernels
      |                                                                              |                                                                      |   0%  |                                                                              |=======                                                               |  10%  |                                                                              |==============                                                        |  20%  |                                                                              |=====================                                                 |  30%  |                                                                              |============================                                          |  40%  |                                                                              |===================================                                   |  50%  |                                                                              |==========================================                            |  60%  |                                                                              |=================================================                     |  70%  |                                                                              |========================================================              |  80%  |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%

``` r
# Look at the gsva scores of the first 10 features
knitr::kable(input_score_list)
```

|         | TCGA-OR-A5J1-01 | TCGA-OR-A5J2-01 | TCGA-OR-A5J3-01 | TCGA-OR-A5J5-01 | TCGA-OR-A5J6-01 | TCGA-OR-A5J7-01 | TCGA-OR-A5J8-01 | TCGA-OR-A5J9-01 | TCGA-OR-A5JA-01 | TCGA-OR-A5JB-01 | TCGA-OR-A5JC-01 | TCGA-OR-A5JD-01 | TCGA-OR-A5JE-01 | TCGA-OR-A5JF-01 | TCGA-OR-A5JG-01 | TCGA-OR-A5JI-01 | TCGA-OR-A5JJ-01 | TCGA-OR-A5JK-01 | TCGA-OR-A5JL-01 | TCGA-OR-A5JM-01 | TCGA-OR-A5JO-01 | TCGA-OR-A5JP-01 | TCGA-OR-A5JQ-01 | TCGA-OR-A5JR-01 | TCGA-OR-A5JS-01 | TCGA-OR-A5JT-01 | TCGA-OR-A5JV-01 | TCGA-OR-A5JW-01 | TCGA-OR-A5JX-01 | TCGA-OR-A5JY-01 | TCGA-OR-A5JZ-01 | TCGA-OR-A5K0-01 | TCGA-OR-A5K1-01 | TCGA-OR-A5K2-01 | TCGA-OR-A5K3-01 | TCGA-OR-A5K4-01 | TCGA-OR-A5K5-01 | TCGA-OR-A5K6-01 | TCGA-OR-A5K8-01 | TCGA-OR-A5K9-01 | TCGA-OR-A5KO-01 | TCGA-OR-A5KT-01 | TCGA-OR-A5KU-01 | TCGA-OR-A5KV-01 | TCGA-OR-A5KW-01 | TCGA-OR-A5KX-01 | TCGA-OR-A5KY-01 | TCGA-OR-A5KZ-01 | TCGA-OR-A5L3-01 | TCGA-OR-A5L4-01 | TCGA-OR-A5L5-01 | TCGA-OR-A5L6-01 | TCGA-OR-A5L8-01 | TCGA-OR-A5L9-01 | TCGA-OR-A5LA-01 | TCGA-OR-A5LB-01 | TCGA-OR-A5LC-01 | TCGA-OR-A5LD-01 | TCGA-OR-A5LE-01 | TCGA-OR-A5LG-01 | TCGA-OR-A5LH-01 | TCGA-OR-A5LJ-01 | TCGA-OR-A5LK-01 | TCGA-OR-A5LL-01 | TCGA-OR-A5LM-01 | TCGA-OR-A5LN-01 | TCGA-OR-A5LO-01 | TCGA-OR-A5LP-01 | TCGA-OR-A5LR-01 | TCGA-OR-A5LS-01 | TCGA-OR-A5LT-01 | TCGA-OU-A5PI-01 | TCGA-P6-A5OF-01 | TCGA-P6-A5OG-01 | TCGA-PA-A5YG-01 | TCGA-PK-A5H8-01 | TCGA-PK-A5H9-01 | TCGA-PK-A5HA-01 | TCGA-PK-A5HB-01 |
|:--------|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|----------------:|
| IGF2    |       0.4401294 |       0.3702468 |      -0.8832929 |      -0.2458536 |       0.8877427 |       0.0593649 |       0.3953277 |      -0.4104976 |      -0.4707727 |      -0.7734628 |       0.9533778 |      -0.8892597 |       0.4402306 |      -0.9711772 |      -0.9181837 |       0.4406351 |      -0.6980178 |       0.0839401 |       0.2690129 |       0.5054612 |      -0.8243325 |       0.2347290 |      -0.3618528 |       0.5295307 |      -0.8326254 |       0.8352549 |       0.1472492 |      -0.8497168 |       0.5430825 |       0.5303398 |       0.2945995 |      -0.8086570 |       0.8111853 |       0.7093447 |       0.5689725 |       0.1854773 |       0.6341019 |      -0.9241505 |      -0.5950647 |      -0.7335154 |       0.3203883 |      -0.8618528 |      -0.0843447 |       0.2522249 |       0.2451456 |       0.4707727 |      -0.1843649 |      -0.3805623 |      -0.8130057 |       0.8610437 |       0.1230785 |       0.5800971 |      -0.3089604 |       0.9808859 |       0.2535396 |      -0.9542880 |      -0.5532969 |       0.9047330 |       0.6999393 |       0.4234426 |      -0.6408778 |      -0.9073625 |       0.8366707 |      -0.8083536 |       0.7804409 |       0.8600324 |      -0.4331513 |      -0.5918285 |       0.7663835 |      -0.6307646 |       0.5888956 |      -0.8806634 |      -0.6570591 |      -0.8323220 |      -0.7879248 |       0.9581311 |       0.7633495 |       0.4181837 |      -0.2944984 |
| DLK1    |      -0.8167476 |       0.2766990 |       0.6021440 |       0.2727549 |      -0.1587783 |       0.5433859 |      -0.7791262 |       0.0063714 |      -0.2169296 |      -0.7237055 |       0.8574029 |      -0.9221278 |       0.5488471 |      -0.5813107 |      -0.0378236 |      -0.0884911 |      -0.6826456 |      -0.9099919 |       0.4448827 |       0.3636731 |      -0.4898867 |       0.6155947 |      -0.8851133 |       0.9775485 |       0.5748382 |       0.9702670 |       0.7764968 |      -0.8875405 |       0.9266788 |      -0.4021036 |       0.7919701 |      -0.7417071 |       0.7821602 |       0.3325243 |       0.9064523 |       0.4542880 |      -0.2426173 |      -0.5329693 |       0.7339199 |       0.4340615 |       0.6879045 |      -0.3300971 |      -0.8983617 |       0.2336165 |       0.6127629 |       0.8644822 |      -0.9063511 |      -0.0846481 |      -0.3273665 |       0.2683050 |       0.5775688 |       0.9885720 |       0.3366707 |       0.7794296 |       0.7994539 |      -0.6146845 |       0.4589401 |       0.3300971 |       0.5629045 |       0.5417678 |       0.1412824 |      -0.8849110 |      -0.5604773 |      -0.5352953 |       0.3054207 |      -0.7467638 |      -0.7470672 |      -0.8794498 |       0.7968244 |       0.4102953 |      -0.8351537 |      -0.8407160 |      -0.8941141 |      -0.7960154 |      -0.8741909 |       0.7875202 |      -0.4432646 |      -0.8739887 |      -0.8169498 |
| SLPI    |      -0.0002023 |      -0.0769620 |       0.1256068 |      -0.1709142 |       0.0237662 |      -0.0207322 |      -0.1002225 |      -0.0270024 |      -0.2147047 |       0.3164442 |      -0.1724312 |       0.2180421 |       0.0310477 |      -0.1306634 |       0.0273058 |      -0.0341828 |      -0.1614078 |      -0.1673746 |       0.0369134 |       0.0468244 |      -0.0373180 |       0.0088997 |       0.1345065 |       0.7971278 |       0.0031351 |       0.8340413 |      -0.0196197 |       0.0001011 |      -0.0723099 |      -0.0670510 |       0.7637540 |       0.0643204 |       0.1724312 |       0.0547128 |       0.0122371 |      -0.0131472 |       0.0157767 |       0.0326659 |       0.1316748 |       0.1046723 |      -0.0635113 |       0.0079895 |       0.0262945 |       0.0179005 |       0.0255866 |      -0.0073827 |      -0.2035801 |      -0.0188107 |       0.1456311 |       0.1708131 |       0.1554409 |       0.3934061 |       0.0229571 |       0.3049150 |       0.9801780 |      -0.0159790 |       0.0351942 |       0.1177184 |       0.0904126 |       0.0449029 |      -0.1576659 |      -0.1345065 |       1.0000000 |       0.1449231 |      -0.0483414 |       0.2185477 |      -0.1052791 |       0.0192152 |      -0.0402508 |      -0.0287217 |       0.2595065 |      -0.0548139 |      -0.1726335 |      -0.1082120 |      -0.1131675 |      -0.1069984 |       0.0703883 |       0.1746561 |      -0.0373180 |
| CYP17A1 |      -0.6969053 |      -0.7985437 |       0.7181432 |       0.3725728 |       0.9116100 |       0.9454895 |      -0.7607201 |      -0.8793487 |       0.7041869 |      -0.6896238 |       0.8898665 |      -0.8990696 |       0.7537419 |       0.4558050 |       0.4320388 |      -0.8639765 |      -0.7299757 |       0.6067961 |       0.4569175 |       0.8028924 |      -0.3462783 |       0.6895227 |      -0.9077670 |      -0.7513147 |       0.0721076 |      -0.8803600 |      -0.6814320 |       0.9846278 |      -0.3159385 |       0.1653519 |      -0.7147047 |       0.0502629 |      -0.8314118 |       0.9946400 |       0.8239280 |      -0.4113066 |      -0.2422128 |       0.5946602 |       0.9111044 |       0.4799757 |       0.6538228 |      -0.0563309 |      -0.8270631 |       0.3503236 |       0.8722694 |       0.3771238 |       0.4638956 |      -0.6853762 |       0.9092840 |       0.5811084 |       0.1147856 |      -0.3168487 |      -0.3707524 |      -0.0931432 |      -0.7940939 |       0.4205097 |       0.5064725 |       0.2547532 |      -0.3812702 |       0.5333738 |       0.3402104 |      -0.5954693 |      -0.8454693 |      -0.2829693 |       0.6429005 |      -0.7975324 |      -0.4244539 |      -0.8887540 |      -0.1952872 |       0.0599717 |      -0.8045105 |      -0.1849717 |       0.2918689 |      -0.7659790 |      -0.9092840 |       0.7615291 |       0.2512136 |      -0.8452670 |       0.5505663 |
| APOE    |      -0.1803196 |      -0.6128641 |      -0.4194984 |      -0.6391586 |       0.8217031 |       0.5306432 |      -0.2172330 |      -0.7652710 |      -0.7114684 |      -0.3883495 |      -0.7744741 |      -0.3639765 |       0.8833940 |      -0.6774879 |       0.0106189 |      -0.7067152 |       0.4881675 |       0.0781756 |       0.5188107 |       0.2430218 |       0.7106594 |       0.5033374 |      -0.7819579 |      -0.7189523 |       0.4754248 |      -0.6784992 |       0.0555218 |      -0.1930623 |      -0.7035801 |       0.2753843 |      -0.5426780 |      -0.5612864 |       0.8901699 |       0.6743528 |      -0.6406756 |      -0.3967435 |       0.4016990 |       0.7560680 |      -0.3283778 |       0.7676982 |      -0.1610032 |       0.7563714 |      -0.7325040 |       0.5760518 |       0.6666667 |       0.9186893 |       0.7473706 |       0.5132484 |       0.5117314 |      -0.7355380 |       0.9492314 |       0.8867314 |      -0.3579086 |       0.9508495 |       0.9913026 |       0.5176982 |       0.5774676 |      -0.0948625 |       0.6444175 |       0.6436084 |       0.8061286 |      -0.2902508 |      -0.4731998 |      -0.4710761 |      -0.7086367 |      -0.6211570 |      -0.7385720 |      -0.7729571 |      -0.7375607 |      -0.6744539 |      -0.6547330 |      -0.6015372 |      -0.3387945 |      -0.4594458 |      -0.7472694 |      -0.7824636 |       0.5069782 |      -0.6221683 |      -0.6593851 |
| GNAS    |       0.7006472 |       0.2992516 |      -0.9201052 |       0.1205502 |       0.9617718 |      -0.7514159 |      -0.5064725 |       0.0026294 |      -0.4866505 |      -0.3576052 |       0.9666262 |      -0.4566141 |       0.5860639 |      -0.7251214 |       0.3312095 |       0.3988673 |      -0.5838390 |       0.3057241 |      -0.7379652 |      -0.1771845 |      -0.6789037 |       0.5789846 |      -0.0644215 |       0.3849110 |      -0.2518204 |       0.7432241 |       0.1994337 |      -0.7504045 |       0.7108617 |      -0.8690332 |       0.3867314 |      -0.7735639 |       0.5579490 |       0.3154328 |       0.6065939 |       0.8241303 |      -0.1879045 |      -0.7301780 |       0.6178196 |      -0.7737662 |      -0.8206917 |      -0.0325647 |      -0.5313511 |      -0.5694782 |      -0.7927791 |       0.3799555 |       0.3040049 |      -0.3867314 |       0.4507484 |       0.5963794 |       0.6836570 |       0.8881472 |      -0.6559466 |       1.0000000 |       0.7808455 |      -0.0494539 |      -0.5160801 |       0.8352549 |      -0.1978155 |       0.4158576 |      -0.6318770 |       0.3835963 |       0.7389765 |      -0.6894215 |       0.7182443 |       0.7246157 |      -0.8772249 |      -0.4662217 |       0.8840008 |      -0.4432646 |      -0.4576254 |      -0.7127832 |      -0.8280744 |      -0.7997573 |      -0.4835154 |       0.9363875 |      -0.4552994 |       0.3902710 |      -0.5186084 |
| CYP11B1 |      -0.5205299 |      -0.6237864 |       0.2543487 |      -0.6217638 |       0.8689320 |       0.4973706 |      -0.5522856 |      -0.6813309 |      -0.6277306 |      -0.4671319 |      -0.7740696 |       0.0820186 |       0.5138552 |      -0.5703883 |       0.1642395 |      -0.4666262 |      -0.7197613 |      -0.5893002 |      -0.5992112 |      -0.2876214 |      -0.5298341 |      -0.4471076 |       0.3426375 |       0.9892799 |       0.7404935 |      -0.5914239 |      -0.4991909 |      -0.6246966 |      -0.4548948 |      -0.5763552 |       0.8351537 |       0.6470469 |       0.9423544 |      -0.5484426 |      -0.6488673 |       0.1315736 |       0.1549353 |       0.5001011 |      -0.2592031 |       0.8537621 |       0.4827063 |       0.6650485 |      -0.6087176 |       0.5034385 |       0.4042273 |      -0.6081108 |      -0.7667880 |      -0.4982807 |       0.9267799 |       0.8286812 |       0.9736044 |       0.5149676 |      -0.4532767 |       0.3055218 |       0.4566141 |      -0.6905340 |       0.7756877 |      -0.4019013 |       0.5384304 |       0.7838794 |      -0.6642395 |      -0.6766788 |      -0.6418892 |       0.6402710 |      -0.7048948 |       0.9914037 |      -0.6525081 |       0.7271440 |       0.8383900 |      -0.6363269 |      -0.5449029 |      -0.6411812 |      -0.0801982 |      -0.5727144 |      -0.1860841 |       0.8790453 |       0.5594660 |      -0.5070793 |      -0.0300364 |
| H19     |      -0.2256270 |      -0.2907565 |      -0.2759911 |      -0.3691343 |       0.9669296 |      -0.2515170 |      -0.2109628 |      -0.2523261 |      -0.4289037 |       0.0275081 |       0.0913228 |      -0.3474919 |      -0.1927589 |       0.7929814 |       0.9033172 |      -0.2885316 |      -0.3695388 |      -0.2687095 |       0.1050769 |       0.8855178 |       0.1480583 |      -0.1005259 |      -0.1765777 |      -0.3217031 |       0.9077670 |      -0.2160194 |      -0.2751820 |      -0.2371561 |       0.3518406 |      -0.0452063 |      -0.1925566 |      -0.0988066 |      -0.2637540 |      -0.1106392 |      -0.2243123 |       0.3224110 |       0.3206917 |      -0.1927589 |      -0.1278317 |      -0.1314725 |      -0.3032969 |       0.5347896 |      -0.1183252 |      -0.1454288 |      -0.1464401 |       0.9163633 |       0.9928196 |       0.2333131 |       0.8437500 |       0.1503843 |       0.3064320 |      -0.2315939 |      -0.1791060 |       0.1256068 |       0.2444377 |       0.8856189 |      -0.0072816 |      -0.0566343 |       0.4490291 |       0.4865494 |      -0.3824838 |      -0.3554814 |      -0.2823625 |       0.2029733 |      -0.2049960 |      -0.1686893 |      -0.2555623 |      -0.3689320 |      -0.3315129 |      -0.2805421 |      -0.1616100 |      -0.2999595 |      -0.4043285 |       0.1849717 |      -0.3693366 |      -0.2621359 |       0.2428196 |      -0.2588997 |      -0.0967840 |
| STAR    |      -0.6474515 |      -0.9513552 |       0.3937095 |       0.0956715 |       0.8720672 |      -0.2739684 |      -0.9701659 |      -0.4038228 |      -0.8336367 |      -0.9524676 |       0.5162824 |       0.7966222 |       0.2252225 |      -0.6632282 |       0.3213997 |       0.2508091 |      -0.9290049 |       0.4546926 |       0.6518002 |      -0.1562500 |      -0.3920914 |       0.6868932 |       0.1998382 |       0.9944377 |      -0.4323422 |       0.8508293 |       0.5694782 |      -0.7348301 |       0.5271036 |      -0.3455704 |       0.9345672 |      -0.5298341 |       0.9928196 |       0.0295307 |       0.7524272 |       0.4251618 |      -0.8474919 |      -0.5884911 |      -0.4834142 |       0.6266181 |      -0.8148261 |       0.7772047 |       0.5059668 |      -0.4052387 |       0.2443366 |      -0.8682241 |       0.1602953 |      -0.2815534 |       0.6305623 |       0.1745550 |       0.6275283 |       0.5332727 |      -0.6748584 |       0.8404126 |       0.6945793 |      -0.6120550 |       0.2407969 |      -0.3561893 |      -0.0953681 |      -0.8234223 |       0.7863066 |      -0.9549960 |      -0.9358819 |      -0.6134709 |       0.8530542 |       0.9472087 |      -0.7586974 |       0.2128843 |       0.8073422 |       0.4067557 |      -0.3071400 |      -0.3892597 |      -0.5813107 |      -0.9701659 |      -0.5517799 |       0.6811286 |      -0.0937500 |      -0.3424353 |       0.1866909 |
| SPARC   |      -0.6340008 |       0.2386731 |      -0.6689927 |      -0.6576659 |      -0.6491707 |       0.8573018 |       0.2562702 |       0.8573018 |       0.1078074 |      -0.9711772 |       0.0815129 |      -0.9327468 |       0.9521642 |       0.2475728 |       0.4807848 |       0.6421926 |      -0.1665655 |      -0.9555016 |      -0.6918487 |       0.8752023 |       0.1758697 |       0.9073625 |       0.9031149 |       0.3836974 |      -0.9439725 |       0.6522047 |      -0.7620348 |       0.6541262 |      -0.6246966 |       0.5751416 |       0.6870955 |       0.6495752 |       0.6647451 |       0.4130259 |      -0.5255866 |      -0.1411812 |       0.7419094 |      -0.6234830 |       0.5525890 |       0.6797128 |       0.5986044 |      -0.0569377 |       0.2871157 |       0.2604167 |       0.4634911 |      -0.2283576 |       0.7947006 |       0.7189523 |      -0.8567961 |       0.4986853 |      -0.6102346 |      -0.1852751 |      -0.3676173 |      -0.6854773 |      -0.4724919 |      -0.5320591 |       0.4966626 |      -0.2114684 |       0.9911003 |      -0.6451254 |      -0.9223301 |      -0.9137338 |      -0.8039037 |      -0.9263754 |      -0.4467031 |      -0.4385113 |      -0.7150081 |      -0.1727346 |       0.4948422 |       0.3865291 |       0.9385113 |      -0.0617921 |       0.2429207 |       0.3013754 |      -0.9388147 |      -0.4343649 |      -0.7628439 |      -0.8661003 |       0.7707322 |

#### (iv) Run candidate search with input scores obtained in (iii)

``` r
## Samples to keep based on the overlap between the two inputs
overlap <- intersect(names(input_score_list[1,]), colnames(datasets$feature_set))
input_score <- input_score_list[1, overlap]
FS <- datasets$feature_set[, overlap, drop=FALSE]

## Pre-filter FS based on occurrence frequency
FS_filtered <- CaDrA::prefilter_data(
  FS = FS,
  max_cutoff = 0.6,  # max event frequency (60%)
  min_cutoff = 0.03  # min event frequency (3%)
)  

# Run candidate search
topn_result <- CaDrA::candidate_search(
  FS = FS_filtered,
  input_score = input_score,
  method = "ks_pval",          # Use Kolmogorow-Smirnow scoring function 
  weights = NULL,              # If weights is provided, perform a weighted-KS test
  alternative = "less",        # Use one-sided hypothesis testing
  search_method = "both",      # Apply both forward and backward search
  top_N = 1,                   # Evaluate top 1 starting points for each search
  max_size = 7,                # Maximum size a meta-feature matrix can extend to
  do_plot = FALSE,             # Plot after finding the best features
  best_score_only = FALSE      # Return meta-feature set, observed input scores and calculated best score
)
```

### (v) Visualize Best Results

``` r
## Fetch the meta-feature set corresponding to its best scores over top N features searches
topn_best_meta <- CaDrA::topn_best(topn_result)

# Visualize the best results with the meta-feature plot
CaDrA::meta_plot(topn_best_list = topn_best_meta, input_score_label = NULL)
```

![](./man/figures/unnamed-chunk-8-1.png)<!-- -->

``` r
# Set seed for permutation
set.seed(123)

# Run CaDrA Search
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

# Visualize permutation results
permutation_plot(perm_res = perm_res)
```

![](./man/figures/unnamed-chunk-9-1.png)<!-- -->

### (4) Launch CaDrA’s Shiny App with your pre-proccessed dataset

#### (i) Pull pre-processed feature sets using our REST API

``` r
# Download feature sets and return a datalist with appropriate paths to dataset
mydatafile <- CaDrA.shiny::download_feature_sets(
  feature_set = "TCGA_ACC_2016_01_28_GISTIC_MUT_SIG",
  include_input_score = TRUE,
  include_gene_expression = TRUE,
  out_dir = "~/Github"
)
```

``` r
# Look at the top 6 rows
knitr::kable(head(mydatafile))
```

| feature_set_name                   | feature_set_path                                                                                                      | input_score_name                    | input_score_path                                                                                                       | gene_expression_name                | gene_expression_path                                                                                                       |
|:-----------------------------------|:----------------------------------------------------------------------------------------------------------------------|:------------------------------------|:-----------------------------------------------------------------------------------------------------------------------|:------------------------------------|:---------------------------------------------------------------------------------------------------------------------------|
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | NA                                  | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/NA                                      | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | HALLMARK_TNFA_SIGNALING_VIA_NFKB    | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_TNFA_SIGNALING_VIA_NFKB.rds    | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | HALLMARK_HYPOXIA                    | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_HYPOXIA.rds                    | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | HALLMARK_CHOLESTEROL_HOMEOSTASIS    | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_CHOLESTEROL_HOMEOSTASIS.rds    | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | HALLMARK_MITOTIC_SPINDLE            | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_MITOTIC_SPINDLE.rds            | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |
| TCGA_ACC_2016_01_28_GISTIC_MUT_SIG | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/feature_set/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG.rds | HALLMARK_WNT_BETA_CATENIN_SIGNALING | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/input_score/HALLMARK_WNT_BETA_CATENIN_SIGNALING.rds | TCGA_ACC_2016_01_28_Gene_Expression | ~/Github/download-fs-2023-10-10/TCGA_ACC_2016_01_28_GISTIC_MUT_SIG/gene_expression/TCGA_ACC_2016_01_28_Gene_Expression.rds |

#### (iii) Launch CaDrA’s app with the downloaded dataset

``` r
# Launch CaDrA's Shiny app with your downloaded datalist retrieved from (ii)
app <- CaDrA.shiny::CaDrA_App(id="myapp", datalist=mydatafile)

# Launch app on localhost with port 3838
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
