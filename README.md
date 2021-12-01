
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaPR2 <img src="inst/img/metapr2_logo.png" align="right" />

## A database of 18S rRNA metabarcodes

**Database version**: 1.0.0 - 41 datasets

**Shiny application version**: 1.0.2

### Presentation

MetaPR2 is a database of published 18S rRNA metabarcodes that have been
reprocessed and assigned using PR2.

### Acessing the database

Access to the database to map, search and download the barcodes can be
done in three different ways:

1 - Using a [web interface](http://shiny.metapr2.org).

2 - Download the R package and launch the shiny application.

3 - Download and run a Docker container

#### 1 - Web interface

<http://shiny.metapr2.org>

#### 2 - metaPR2 shiny R package

*(NOT YET AVAILABLE)*

Install the package from GitHub and launch function metapr2App()

``` r
install.packages(devtools)

devtools::install_github("pr2database/metapr2-shiny")

metapr2::metapr2App()
```

#### 3 - metaPR2 Docker container

*(NOT YET AVAILABLE)*

Will be available from Docker repository

### Help

Extensive help is provided
[here](https://pr2database.github.io/metapr2-shiny/articles/).

### Errors

Please report errors in the [Issues page of the metaPR2
database](https://github.com/pr2database/metapr2-shiny/issues).

### Citation

Vaulot, D. et al. (2021). metaPR2 : An interactive 18S rRNA metabarcode
database. Unpublished

### Maintainer

-   Daniel Vaulot: <vaulot@gmail.com>

### Contributors

-   Daniel Vaulot, CNRS Roscoff, NTU-ASE Geek lab
-   Adriana Lopes dos Santos, NTU-ASE Geek lab
-   Clarence Sim, NTU-ASE Geek lab
-   Denise Ong, NTU-ASE Geek lab
-   Bryan Teo, NTU-ASE Geek lab
-   Mahwash Jamy, Uppsala University Sweden
-   Charlie Biwer, Uppsala University Sweden
