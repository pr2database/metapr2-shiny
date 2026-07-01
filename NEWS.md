# metapr2 4.0.0

Released: 2026-07-01

### Database 

#### Statistics

- Data sets: 187
- Samples: 26 315
- ASVs clustered: 229 793
- ASVs clustered: 275 680

#### New entries
* __120 new datasets__
* __18 549 new samples__

This release includes all EukBanks datasets that contained more than 20 samples.

#### Citations
Please refer to the web interface to see the reference for each dataset.  

Berney, C., Mahé, F., Henry, N., Lara, E., de Vargas, C., & consortium, E. (2023). EukBank 18S V4 dataset. Zenodo. https://doi.org/10.5281/zenodo.7804946



---

# metapr2 3.0.2

Released: 2026-04-15

### Web application and R package

* Slight modification of interface
* Include rRNA gene choice (nuclear 18S, plastid 16S or nuclear operon).

---


# metapr2 3.0.1

Released: 2026-02-18

### Web application and R package

* Use the [qs2 package](https://github.com/qsbase/qs2) instead of qs to read the data
* Available as a [Docker repository](https://hub.docker.com/repository/docker/vaulot/metapr2) (preferred solution for off-line use)

---

# metapr2 3.0.0

Released: 2025-09-26

### Database 

#### Statistics

* Data sets: 67
* Samples: 7 766
* ASVs clustered: 94 373
* ASVs unclustered: 113 989

#### New entries
* __8 new datasets__
* __1650 new samples__

##### Polar
* Arctic - Green Edge cruise - 2016
* Arctic - AWI data set (Greenland, Svalbard, Norway)
* Arctic - MicroPolar cruise - 2014

##### Oceanic
* South West Pacific NIWA - 2018

#### Citations
* Sim et al. 2025. Temporal dynamics and biogeography of sympagic and planktonic photosynthetic microbial eukaryotes during the under-ice Arctic bloom. ISME Communications. ycaf075.
* Ribeiro et al. 2024. Arctic phytoplankton microdiversity across the marginal ice zone: Subspecies vulnerability to sea-ice loss. Elementa: Science of the Anthropocene. 12:00109.
* Egge et al. 2021. An 18S V4 rRNA metabarcoding dataset of protist diversity in the Atlantic inflow to the Arctic Ocean, through the year and down to 1000 m depth. Earth System Science Data. 13:4913–28.
* Hörstmann et al. 2024. Biogeographic gradients of picoplankton diversity indicate increasing dominance of prokaryotes in warmer Arctic fjords. Commun Biol. 7:1–11. 
* Šupraha et al.. 2022. Diversity and biogeography of planktonic diatoms in Svalbard fjords: the role of dispersal and Arctic endemism in phytoplankton community structuring. Elementa: Science of the Anthropocene. 10:00117.
* Ong et al. (2025), Consistent cell-specific carbon fixation rates by small eukaryotic phytoplankton in contrasting nutrient-limited conditions. Limnol Oceanogr, 70: 162-177. https://doi.org/10.1002/lno.12751
* Décima et al. 2023. Salp blooms drive strong increases in passive carbon export in the Southern Ocean. Nat Commun. 14:1–16.


### Web application and R package

* Column added in the dataset table to provide a link to the Project accession number
* Update in the map panel 
  * select circle size
  * allow continuous view across the globe when panning

---

# metapr2 2.1.1

Released: 2023-05-17

### Web application and R package

* Bug fixed
  * ASVs could not be selected on the left menu (Taxonomy)
  
* Other changes
  * Bootstrap values are now exported in the ASV table

---

# metapr2 2.1.0

Released: 2023-05-16

### Web application and R package

* Only the latest version (2.1) of the database is provided.
* Taxonomy table reflects the groups selected on the side panel

### Database version 2.1

* Metabarcodes are now assigned with PR2 version 5.0.0 (9 taxonomy levels).

---

# metapr2 2.0.1

Released: 2023-02-17

### Web application and R package

Two bugs fixed:

* Error linked to version 4.2 of R giving error instead of warning for `if` when the condition a dimension > 1
* Some long sequences where giving an error in the Query panel

---

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

---

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
