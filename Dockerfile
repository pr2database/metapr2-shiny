# Date 2026-02-21

# Notes: 
# - Necessary to list all libraries
# - Use shiny image smaller

# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.4

# Install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    build-essential \
    libglpk40 \
    wget \
    unzip \
    zlib1g-dev \
    bzip2 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf
ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R

# Install the R library

RUN install2.r --error --skipinstalled \
    dplyr \
    DT \
    forcats \
    ggforce \
    ggplot2 \
    leaflet \
    leaflet.minicharts \
    lubridate \
    patchwork \
    pkgload \
    plotly \
    lobstr \
    purrr \
    qs2 \
    rio \
    scrypt \
    shiny \
    shinycssloaders \
    shinydisconnect \
    shinylogs \
    shinymanager \
    shinyvalidate \
    shinyWidgets \
    stringr \
    tidyr \
    treemapify \
    viridis 


# Package for installing other packages
RUN install2.r --error --skipinstalled \
    BiocManager \
    remotes

# Bioconductor and older versions
RUN R -e "BiocManager::install('Biostrings',ask=F)"
RUN R -e "BiocManager::install('phyloseq',ask=F)"
RUN R -e "remotes::install_version('blaster', version ='1.0.7')"

# copy necessary files

WORKDIR /srv/shiny-server

COPY DESCRIPTION ./
COPY NAMESPACE ./
COPY .Rbuildignore ./

COPY README.md ./
COPY *.R ./

COPY /R ./R
COPY /inst/img  ./inst/img
COPY /inst/readme  ./inst/readme
COPY /inst/data-qs  ./inst/data-qs
COPY /inst/data-qs-pelago  ./inst/data-qs-pelago
COPY /inst/data-qs-pacbio  ./inst/data-qs-pacbio
COPY /inst/data-qs-nansen  ./inst/data-qs-nansen
COPY /inst/data-qs-5.0  ./inst/data-qs-5.0

# For testing
# CMD Rscript R/test.R


# expose port

EXPOSE 8080

USER shiny

# Better to use
CMD ["/usr/bin/shiny-server"]
