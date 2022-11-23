
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metaPR2 <img src="https://github.com/pr2database/metapr2-shiny/blob/main/inst/img/metapr2_logo.png?raw=true" align="right" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/410160328.svg)](https://zenodo.org/badge/latestdoi/410160328)
![Release](https://img.shields.io/badge/release-1.0.4-blue.svg)
![Date](https://img.shields.io/badge/date-30%23Nov%202022-lightgrey.svg)

<!-- badges: end -->

## A database of 18S rRNA metabarcodes

**Database version**: 2.0 - 59 datasets

**Shiny application version**: 1.0.4

### Presentation

MetaPR2 is a database of published 18S rRNA metabarcodes that have been
reprocessed and assigned using PR2.

### Acessing the database

Access to the database to map, search and download the barcodes can be
done in three different ways:

- Using a [web interface](http://shiny.metapr2.org).

- Download the R package and launch the shiny application.

<!-- This is commented out.
3 - Download and run a Docker container 
 -->

#### 1 - Web interface

- Launch in your browser: <http://shiny.metapr2.org>
- Help : <https://pr2database.github.io/metapr2-shiny/articles/>

#### 2 - metaPR2 shiny R package

##### Install the package from GitHub

``` r
install.packages(devtools)

devtools::install_github("pr2database/metapr2-shiny")
```

Note: You may have to install some packages required by metapr2 if they
are not installed on your machine

##### Alternatively install from [R-universe](https://pr2database.r-universe.dev/ui#packages)

No need in this case to install package `devetools`

``` r
# Enable repository from pr2database
options(repos = c(
  pr2database = 'https://pr2database.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install metapr2 in R
install.packages('metapr2')
```

##### Launch shiny interface with function run_app()

``` r
metapr2::run_app()
```

<!-- This is commented out.
#### 3 - metaPR2 Docker container

Available from Docker repository: https://hub.docker.com/repository/docker/vaulot/metapr2

* Install docker on your computer: https://docs.docker.com/desktop/

* At shell prompt (can be Linux or Windows Powershell)


```bash
# Download container
docker pull vaulot/metapr2:v1.0.2

# Launch container
docker run --rm -p 8080:8080 metapr2

```

* In your browser: http://localhost:8080/
 
 -->

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

- Docker (only version \<= 1.0.2):
  <https://hub.docker.com/repository/docker/vaulot/metapr2>

### Maintainer

- Daniel Vaulot: <vaulot@gmail.com>

### Contributors

- Daniel Vaulot, CNRS Roscoff, NTU-ASE Geek lab
- Adriana Lopes dos Santos, NTU-ASE Geek lab
- Clarence Sim, NTU-ASE Geek lab
- Denise Ong, NTU-ASE Geek lab
- Bryan Teo, NTU-ASE Geek lab
- Charlie Biwer, Uppsala University Sweden
- Mahwash Jamy, Uppsala University Sweden
