# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny 

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Necessary for phyloseq: https://stackoverflow.com/questions/68036714/r-docker-load-failed-for-networkd3-in-dyn-load
RUN apt-get update && apt-get install -y build-essential libglpk40

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf
ENV _R_SHLIB_STRIP_=true
COPY Rprofile.site /etc/R

# Install the R library

RUN install2.r --error --skipinstalled \
    shiny \
    shinyvalidate \
    shinycssloaders \
    shinyWidgets \
    dplyr \
    tidyr \
    forcats \
    stringr \
    jsonlite \
    ggplot2 \
    viridis \
    patchwork \
    treemapify \
    leaflet \
    leaflet.minicharts \
    pkgload \
    markdown \
    DT \
    rio \
    BiocManager 

RUN R -e "BiocManager::install('Biostrings',ask=F)"
RUN R -e "BiocManager::install('phyloseq',ask=F)"

# copy necessary files

WORKDIR /srv/shiny-server

COPY DESCRIPTION ./
COPY NAMESPACE ./
COPY .Rbuildignore ./

COPY README.md ./
COPY *.R ./

COPY /R ./R
COPY /data  ./data
COPY /inst  ./inst

# For testing
# CMD Rscript R/test.R


# expose port

EXPOSE 8080

USER shiny

# Better to use
CMD ["/usr/bin/shiny-server"]
