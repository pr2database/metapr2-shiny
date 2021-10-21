# https://www.theorsociety.com/media/3832/data-visualisation-workshop-uploading-a-shiny-app-to-a-server-_14062018122240.pdf

sudo apt-get update
# sudo apt-get upgrade

sudo apt-get install r-base r-base-dev

sudo su - \
-c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""

sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.17.973-amd64.deb
sudo gdebi shiny-server-1.5.17.973-amd64.deb

install2.r --error --skipinstalled \
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