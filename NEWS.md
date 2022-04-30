# metapr2 1.0.3

Released: 2022-04-30

### Database 

#### version 1.1 - 41 datasets
* Tara Ocean V9 samples have been reprocessed using the dada2 pipeline.  In version 1.0, the original swarms were used instead of ASVs.

### Tabs of application

#### Datasets
* Selected datasets appear first ordered by dataset_id
* Search error fixed

<!--- 

### Taxonomy

### Treemaps

### Maps

### Barplots

### Diversity

-->

#### Query
* A fasta formatted sequence with header can now be used.

#### Download
* The zipped file now contains a fasta file with the asv_code and the taxonomy in the header.

---

# metapr2 1.0.2

Released: 2021-12-14

### Tabs of application

#### Datasets
* Settings (datasets, type of samples) can be saved and recalled

#### Taxonomy
* Now more than one taxon can be selected
* Three divisions can be removed (Fungi, Metazoa and Streptophyta)
* Taxa (selected and excluded) can be saved and recalled
* It is necessary to press the "Validate taxa" to replot after changing taxo selection

#### Treemaps
* Color of taxa now match other panels
* Add a treemap of ASVs number

#### Maps
* Add topography
* Add equator, tropics and polar circle

#### Barplots
* Add number of samples for each bar

#### Diversity Alpha
* Use Violin + Sina plot for discrete variable
* Allow discretization of continuous variables (e.g. depth, latitude)

---

# metapr2 1.0.1

Released: 2021-11-22

### Tabs of application

#### Documentation 
* Using pkgdown: https://pr2database.github.io/metapr2-shiny/

#### Barplots
* Make interactive (R plotly library)
* Add coloring by ecological function
* Add time series

---

# metapr2 1.0.0

Released: 2021-11-19

* Initial release

### Database 

#### version 1.0 - 41 datasets
* Tara Ocean V9 samples have been not been reprocessed and the original swarms are used instead.
