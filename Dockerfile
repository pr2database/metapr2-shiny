# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:4.4 

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
RUN R -e "remotes::install_version('qs', version ='0.27.3')"
RUN R -e "remotes::install_version('blaster', version ='1.0.7')"



# Install vsearch
# https://github.com/FredHutch/docker-vsearch/blob/master/Dockerfile

# Install prerequisites
# RUN apt-get install -y build-essential wget unzip python2.7 python-dev python-pip bats zlib1g-dev bzip2

# Add files
# RUN mkdir /usr/vsearch

# Get the binary from the latest release
# RUN cd /usr/vsearch && \
#	wget https://github.com/torognes/vsearch/releases/download/v2.18.0/vsearch-2.18.0-linux-x86_64.tar.gz && \
#	tar xzvf vsearch-2.18.0-linux-x86_64.tar.gz && \
#	cd vsearch-2.18.0-linux-x86_64 && \
#	ln -s /usr/vsearch/vsearch-2.18.0-linux-x86_64/bin/vsearch /usr/local/bin

# copy necessary files

WORKDIR /srv/shiny-server

COPY DESCRIPTION ./
COPY NAMESPACE ./
COPY .Rbuildignore ./

COPY README.md ./
COPY *.R ./

COPY /R ./R
COPY /inst/data-qs  ./inst/data-qs
COPY /inst/img  ./inst/img
COPY /inst/readme  ./inst/readme

# For testing
# CMD Rscript R/test.R


# expose port

EXPOSE 8080

USER shiny

# Better to use
CMD ["/usr/bin/shiny-server"]
