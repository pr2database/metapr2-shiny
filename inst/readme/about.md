## Shiny server for the metaPR2 database

An [interactive database](https://app.metapr2.org/) of eukaryotic metabarcodes compiled from the literature. 

### Presentation

MetaPR2 is a database of published 18S rRNA metabarcodes. This R package launches a shiny application that allows to interact with the database by mapping, searching and downloading the barcodes.

### User interface
#### Left panels

The left panels allow to choose: 
* specific datasets (e.g. Tara, OSd, Malaspina)
* specific samples (e.g. different depth levels)
* Specific taxa

#### Right panels

Provide results from analysis

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
**Datasets** | List of datasets used.
**Download** | Allows you to download data for selected samples/taxa as well as the whole dataset as a phyloseq file.
**Treemap** | Create treemap graphs for samples and taxon selected
**Map** | Maps of relative abundance. Taxonomic level can be chosen as well as sample types (RNA/DNA, substrate, depth layer...).
**Barplot** | Composition based on specific variables (e.g. depth_level).
**Alpha diversity** | Alpha diversity incies (e.g. Shannon, Simpson).
**Beta diversity** | Mutivariate analyses.
**Query** | Enter a sequence and search for metabarcode that are similar to your sequence using BLAST like search.

</div>

### Data processing

* All datasets were processed with cutapdapt and the dada2 R package.

* Assignement was done with dada2 assignTaxa using PR2 4.12 as reference

* ASVs with less 100 reads total and with bootstrap value at the supergroup level < 90 were removed.

* Total read number per sample has been normalized to 100 with 3 decimals so that the value displayed in the different panels correspond to % of total eukaryotic reads.