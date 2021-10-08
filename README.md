
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Shiny server for the metaPR2 database

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/191439796.svg)](https://zenodo.org/badge/latestdoi/191439796)
<!-- badges: end -->

An [interactive database](https://app.metapr2.org/) of eukaryotic
metabarcodes compiled from the literature.

### Presentation

MetaPR2 is a database of published 18S rRNA metabarcodes. This R package
launches a shiny application that allows to interact with the database
by mapping, searching and downloading the barcodes.

### How run the metaPR2 shiny server

#### Step 1 - Download the R package from GitHub

<https://github.com/vaulot/metapr2-shiny/archive/refs/heads/main.zip>

#### Step 2 - Install package on your computer

-   Unpack Zip file

-   Launch shiny\_metapr2.Rproj

-   (Skip this step) If you are running Windows install Rtools

    -   <https://cran.r-project.org/bin/windows/Rtools/>

#### Step 3 - Install the following libraries

``` r
### Shiny
install.packages("shiny" ) 
install.packages("shinyvalidate" ) # For validation of input (easier to use than shinyFeedback)
install.packages("shinycssloaders" ) # For the spinning wheel
install.packages("shinyWidgets" ) # for Extra widgets - https://dreamrs.github.io/shinyWidgets/index.html
install.packages("markdown" ) # To display text boxes in md
install.packages("DT" ) # For table display

### Tidyverse
install.packages("dplyr" )
install.packages("tidyr" )
install.packages("stringr" )
install.packages("forcats" )

### graphics
install.packages("ggplot2" )
install.packages("viridis" )
install.packages("patchwork" )
install.packages("treemapify" )

### maps
install.packages("leaflet" )
install.packages("leaflet.minicharts" ) # To do nice pie charts

### Misc
install.packages("rio" )


### Bioinformatics

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("Biostrings")
BiocManager::install("phyloseq")
```

#### Step 4 - Launch the metaPR2 shiny server

Run the following line from R studio

``` r
install.packages("pkgload")

pkgload::load_all(".")

metapr2App()
```

#### Stop the Shiny application

-   Close the browser window

OR

-   Hit the stop button at the top right of the bottom left window

#### In case Shiny application crashes

-   Hit the stop button at the top right of the bottom left window
-   Relaunch `metapr2App()`

### Errors

Please report errors in the [Issues page of the metaPR2 primer
database](https://xxx).

### Citation

Vaulot, D. et al. (2021). [metaPR2 : An interactive 18S rRNA metabarcode
database](). Unpublished

### Resources

-   Website: <https://app.metapr2.org/>
-   Docker: <https://hub.docker.com/repository/docker/vaulot/metapr2>
-   Source code: <https://github.com/pr2database/metapr2>

### Maintainer

-   Daniel Vaulot: <vaulot@gmail.com>

### Contributors

-   

### Versions

1.0.0 - 2021-xx-xx \* Initial version
