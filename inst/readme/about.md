## Shiny server for the metaPR2 database

An interactive database of eukaryotic metabarcodes compiled from the literature. 

### Presentation

MetaPR2 is a database of **18S rRNA metabarcodes** originating from published studies. 

This R package launches a shiny application that allows to interact with the database by mapping, searching and downloading the barcodes.

### User interface
#### Left panel

Choose samples and taxonomy

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class="ox-hugo-table basic-styling">
<div></div>
<div class="table-caption">
  <span class="table-number"></span>
</div>

Select | Choices 
--- | --- 
**Datasets** | Access through panel "Datasets". Choices are OSD, Malaspina and Tara
**Samples** | DNA or RNA
 | Substrates (in our case only water)
 | Fraction: pico 0.2-3 µm / Total 0.2-100 or 200 µm
 | Depth levels: surface (0-10 m), euphotic (10-250 m), bathypelagic (250-1000 m), bathypelagic (> 1000 m)
**Minimum number of reads per ASV** | 100-10,000 - If you increase this number the number of ASVs will decrease.
**Taxa** | supergroup, division, class, order, family, genus, species

</div>

#### Right panels

Analysize the selected data.

<style>
.basic-styling td,
.basic-styling th {
  border: 1px solid #999;
  padding: 0.5rem;
}
</style>

<div class="ox-hugo-table basic-styling">
<div></div>
<div class="table-caption">
  <span class="table-number"></span>
</div>

panel | content 
--- | --- 
**About** | Basic information on the metapr2 database.
**Datasets** | Select and list datasets used.
**Treemap** | Create treemaps (square pie-chart) provinding the proportion of each taxon for the samples and datasets selected
**Map** | Maps of read abundance. For a given taxonomic level you can visualize the proportions of the taxonomic levels below (e.g. species if you are at the genus level) or alternatively the main taxon at each station (e.g. the mainn species if you are the genus level). Size of circle is proportional to contribution of taxon to total eukaryotes.
**Barplot** | Composition for specific variables (e.g. depth_level).
**Alpha diversity** | Alpha diversity indices (e.g. Shannon, Simpson).
**Beta diversity** | Mutivariate analyses (e.g. NMDS).
**Query** | Enter a sequence and search for metabarcode that are similar to your sequence using a BLAST like search (not used for course).
**Download** | Download data for selected samples/taxa as well as the whole dataset as a phyloseq file.

</div>

### Data pre-processing

* Raw fastq files were downloaded from NCBI

* All datasets were processed with cutapdapt to remove primers and the dada2 R package to compute ASVs.

* Assignment was done with dada2 assignTaxa using the 18S PR2 4.14.0 as reference

* ASVs with less 100 reads total and with bootstrap value at the supergroup level < 90 were not considered.

* Total read number per sample has been normalized to 100 with 3 decimals so that the value displayed in the different panels correspond to % of total eukaryotic reads.