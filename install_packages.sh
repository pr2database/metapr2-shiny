### Shiny
sudo su - -c "R -e \"install.packages('shinyvalidate', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinycssloaders', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinyWidgets', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('markdown', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('DT', repos='https://cran.rstudio.com/')\""


### Tidyverse


sudo su - -c "R -e \"install.packages('stringr', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('forcats', repos='https://cran.rstudio.com/')\""

### graphics
sudo su - -c "R -e \"install.packages('ggplot2', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('viridis', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('patchwork', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('treemapify', repos='https://cran.rstudio.com/')\""

### maps
sudo su - -c "R -e \"install.packages('leaflet', repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('leaflet.minicharts', repos='https://cran.rstudio.com/')\""

### Misc
sudo su - -c "R -e \"install.packages('rio', repos='https://cran.rstudio.com/')\""


# To load package

sudo su - -c "R -e \"install.packages('pkgload', repos='https://cran.rstudio.com/')\""

### Bioinformatics

sudo su - -c "R -e \"install.packages('BiocManager', repos='https://cran.rstudio.com/')\""

sudo su - -c "R -e \"BiocManager::install('Biostrings')\""

### Phyloseq

sudo su - -c "R -e \"BiocManager::install('phyloseq')\""