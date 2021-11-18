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


# To load package

install.packages("pkgload")

### Bioinformatics

install.packages("BiocManager")


BiocManager::install("Biostrings")

### Phyloseq


BiocManager::install('phyloseq')