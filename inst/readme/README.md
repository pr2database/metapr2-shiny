
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaPR2 <img src="https://github.com/pr2database/metapr2-shiny/blob/main/inst/img/metapr2_logo.png?raw=true" align="right" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/410160328.svg)](https://zenodo.org/badge/latestdoi/410160328)
![Release](https://img.shields.io/badge/release-3.0.0-blue.svg)
![Date](https://img.shields.io/badge/date-26%20Sep%202025-lightgrey.svg)

<!-- badges: end -->

## A database of 18S rRNA metabarcodes

**Version**: 3.0.0

- Data sets: 67
- Samples: 7 766
- ASVs clustered: 94 373
- ASVs unclustered: 113 989

**Release date**: 2023-09-26

### Presentation

MetaPR2 is a database of published 18S rRNA metabarcodes that have been
reprocessed and assigned using PR2.

### Acessing the database

Access to the database to map, search and download the barcodes can be
done in three different ways:

1.  Using a [web interface](http://shiny.metapr2.org).

2.  Run as docker container. \<= **This is the best way**

3.  Download the R package and launch the shiny application (hardest).

Please privilege method \# 2 (Docker Image) if you are going to use
metapr2 extensively or for a course as the server will crash if too many
users are logged.

#### 1 - Web interface

- Launch in your browser: <http://shiny.metapr2.org>
- Help : <https://pr2database.github.io/metapr2-shiny/articles/>

#### 2 - metaPR2 Docker container (Easier)

Available from [Docker
repository](https://hub.docker.com/repository/docker/vaulot/metapr2)

- Install docker on your computer: <https://docs.docker.com/desktop/>

- At shell prompt (can be Linux or Windows Powershell)

``` bash
# Download container
docker pull vaulot/metapr2:latest

# Launch container
docker run --rm -p 8080:8080 vaulot/metapr2
```

- In your browser: <http://localhost:8080/>

#### 3 - metaPR2 shiny R package (Harder)

Note: *You may run into trouble because quite a few packages need to be
installed.*

##### 3.1 - Install the R package from GitHub

- Launch R or R Studio
- Run the following lines.You may have to install some CRAN packages
  required by metapr2 if they are not installed on your machine.

``` r
install.packages("devtools")
install.packages("remotes")

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

remotes::install_version("qs", version = "0.27.3")
remotes::install_version("blaster", version = "1.0.7")

BiocManager::install("phyloseq")
BiocManager::install("Biostrings")

devtools::install_github("pr2database/metapr2-shiny")
```

##### 3.2 - Launch shiny interface with function run_app()

``` r
metapr2::run_app()
```

<!-- This is commented out.
&#10; -->

### Help

Extensive help is provided
[here](https://pr2database.github.io/metapr2-shiny/articles/).

### Errors

Please report errors in the [Issues page of the metaPR2
database](https://github.com/pr2database/metapr2-shiny/issues).

### Citation

Vaulot, D., Sim, C.W.H., Ong, D., Teo, B., Biwer, C., Jamy, M., Lopes
dos Santos, A., 2022. metaPR$^{2}$: a database of eukaryotic 18S rRNA
metabarcodes with an emphasis on protists. Molecular Ecology Resources
22, 3188–3201. <https://doi.org/10.1111/1755-0998.13674>

### Resources

- Website: <https://shiny.metapr2.org/>

- Source code: <https://github.com/pr2database/metapr2-shiny>

### Maintainer

- Daniel Vaulot: <vaulot@gmail.com>

### Contributors

- Daniel Vaulot, CNRS Roscoff, U. of Oslo
- Adriana Lopes dos Santos, U. of Oslo
- Clarence Sim, NTU-ASE Geek lab
- Denise Ong, U. of Oslo
- Bryan Teo, NTU-ASE Geek lab
- Charlie Biwer, Uppsala University Sweden
- Mahwash Jamy, Uppsala University Sweden
