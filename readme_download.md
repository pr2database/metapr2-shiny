Five files are provided.  They can be linked by key fields.  They only contain the datasets, sample type and taxa selected.

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

file | content | key fields
--- | --- | ---
datasets.xlsx | Information on the different datasets selected including reference and GenBank id | dataset_id
samples.xlxs | List of samples selected with medadata | file_code
asv.xlsx | ASV selected with taxonomy and sequence | asv_code
asv_reads.xlsx | Number of reads for each ASV and each sample (long form) | asv_code, file_code
asv_map.xlsx | Summary file used to draw the map

</div>

<!--- https://stackoverflow.com/questions/52239087/table-in-r-markdown-is-not-printing-correctly-in-shiny-application -->