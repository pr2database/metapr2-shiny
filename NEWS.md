# metapr2 2.0.0

Released: 2022-11-23

### Database 

#### 59 datasets

18 new datasets

##### Polar
* Arctic - 2012 (Kilias 2020)
* Amundsen_Sea ASPIRE cruise - 2010-2011
* Fram Strait - 2014
* Palmer Station Antarctic - LTER - 2014
* Arctic and Scotian Shelf - 2009-2011
* Baffin Bay - 2008-2018
* Southern Ocean - 2017
* Fram observatory 2016
* Antarctic Peninsula - 2012-2016

##### Oceanic
* Atlantic transect cruise PS113 - 2018
* South West Pacific NIWA - 2009_2018

##### Coastal
* Roscoff Astan - 2009-2011
* Roscoff Astan - 2012-2016
* SE Asia Tsunami deposits
* Baltic Sea Gdansk Gulf - 2012 Hapto
* Coral Infecting Apicomplexan
* Zostera marina - British Columbia - 2015
* Coral Singapore - 2018

##### Clustering
An option is now provided to use either all ASVs or clustered ASVs on the [welcome screen](https://pr2database.github.io/metapr2-shiny/articles/vignette-data-processing.html).  ASVs are clustered at 100% identity with VSEARCH --id 1.00 See the [metaPR2 paper](https://doi.org/10.1111/1755-0998.13674) for more information. 

It is also possible to use version 1.0 of the database by entering `v1` on the [welcome screen](https://pr2database.github.io/metapr2-shiny/articles/vignette-data-processing.html).

![](https://pr2database.github.io/metapr2-shiny/articles/img/welcome_01.png)

### Web application

#### New Panel: Taxonomy
This new [panel](https://pr2database.github.io/metapr2-shiny/articles/vignette-taxonomy.html) provides a table with all the taxa present in the current metaPR2 version with the number of ASV for each species.  The table can be easily searched.

![](https://pr2database.github.io/metapr2-shiny/articles/img/taxonomy_01.png)

#### Minor changes
* Information about database is provided on left panel
* New button to disconnect the application and reloading.
* Maximum number of samples for Phyloseq: 2000.
* Taxonomy is constructed from all the samples and not only samples selected.
* [Option to use clustered ASVs](https://pr2database.github.io/metapr2-shiny/articles/vignette-data-processing.html) in Welcome panel.

* **Barplots**. The right side of the graph indicates, for each parameter range, the number of samples that fall into that range as well as the number of samples that contain the taxa selected.

![](https://pr2database.github.io/metapr2-shiny/articles/img/bar_08.png)

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
