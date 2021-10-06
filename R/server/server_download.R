
# Download files (zip) ----------------------------------------------------
# 
# See:  https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny


output$download_datasets <- downloadHandler(
  
  filename = function() {str_c("metapr2_datasets_", taxo()$name, "_", Sys.Date(), ".zip")},
  
  content = function(path) {
    
    tmpdir <- tempdir()
    file_datasets <- str_c(tmpdir, "/datasets.xlsx")
    file_samples <- str_c(tmpdir, "/samples.xlsx")
    file_asv <- str_c(tmpdir, "/asv.xlsx")
    file_asv_reads <- str_c(tmpdir, "/asv_reads.xlsx")
    file_asv_map <- str_c(tmpdir, "/asv_map.xlsx")
    files = c(file_datasets, file_samples, file_asv, file_asv_reads, file_asv_map)
    
    rio::export(datasets_selected(), file=file_datasets)
    rio::export(samples_selected(), file=file_samples)
    rio::export(fasta_selected_taxa_one(), file=file_asv)
    rio::export(df_selected_taxa_one(), file=file_asv_reads)
    rio::export(df_map(), file=file_asv_map)
    
    system2("zip", args=(paste("--junk-paths", path,files,sep=" "))) # remove the paths of the files
  }
)   


# Download phyloseq -------------------------------------------------------


output$download_phyloseq <- downloadHandler(
  
  filename = function() {str_c("metapr2_phyloseq.rds")},
  
  content = function(file) {
    
    rio::export(asv_set$ps, file = file) 
  }
)   


# UI ----------------------------------------------------------------------


output$ui_download <- renderUI({
  tagList(
    includeMarkdown("readme/download.md"),
    p(),
    downloadButton('download_datasets', 'Download selected data as xlsx'),
    p(),
    h4("Phyloseq file - All data"),
    renderPrint(print(asv_set$ps)),
    h5("Taxonomic ranks"),
    renderPrint(phyloseq::rank_names(asv_set$ps)),
    h5("Variables"),
    renderPrint(phyloseq::sample_variables(asv_set$ps)),
    downloadButton('download_phyloseq', 'Download all data as phyloseq file (rds)'),
    p()
  )
  })