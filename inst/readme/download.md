#### Selected damples, datasets and taxa

Five files are provided.  They can be linked by key fields.  They only contain the datasets, sample type and taxa selected.

<style>
.basic-styling td,
.basic-styling th {
  border: 0px solid #999;
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
samples.xlsx | List of samples selected with medadata | file_code
asv.xlsx | ASV selected with taxonomy and sequence | asv_code
asv_reads.tsv.gz | Fraction of reads, related to the total number of eukaryotic reads in the sample, for each ASV and each sample (long form). | asv_code, file_code
 | ! **This file can be very big** if you download all datasets and all taxa |

</div>

<!--- https://stackoverflow.com/questions/52239087/table-in-r-markdown-is-not-printing-correctly-in-shiny-application -->