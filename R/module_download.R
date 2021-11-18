# UI ----------------------------------------------------------------------

downloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_download'))
  )
}



# Server ------------------------------------------------------------------


downloadServer <- function(id, datasets_selected, samples_selected, df_selected, taxo, messages) {
  # stopifnot(is.reactive(taxo))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)

  # Download files (zip) ----------------------------------------------------
  # 
  # See:  https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    
    fasta_selected <- reactive({
      req(df_selected())
      asv_set$fasta %>% 
        filter(asv_code %in% df_selected()$asv_code)
    })
    
    ps_selected <- reactive({
      req(n_samples_valid())
      ps <- make_phyloseq(samples_selected(), df_selected(), fasta_selected())
      print(ps)
    })
    
    n_samples_max = 5000
    
    n_samples_valid <- reactive({nrow(samples_selected()) <= n_samples_max})
    
    output$sample_number <- renderText({stringr::str_c("Number of samples: <b>", nrow(samples_selected()), 
                                                       if_else(n_samples_valid(), "</b>", " - Too many for phyloseq download!!</b> - Must be below <b>5000 !</b>"),
                                                       sep=" ")})
  
  
  output$download_df <- downloadHandler(
      
      filename = function() {str_c("metapr2_ASVs_selected_abundance_", taxo()$name, "_", Sys.Date(), ".tsv.gz")},
      
      content = function(path) {
        
        # cat("Download gz - Mem Gb: ", pryr::mem_used()/10^9, "\n")
        
        data.table::fwrite(df_selected(), file=path,  sep = "\t", compress="gzip")
      }
    ) 
    
  output$download_phyloseq <- downloadHandler(
    
    filename = function() {str_c("metapr2_phyloseq_", taxo()$name, "_", Sys.Date(), ".rds")},
    
    content = function(path) {
      
      # req(n_samples_valid())
      # req(fasta_selected())
      
      # ps <- make_phyloseq(samples_selected(), df_selected(), fasta_selected())
      # print(ps)
      
      # cat("Download ps - Mem Gb: ", pryr::mem_used()/10^9, "\n")
      
      rio::export(ps_selected(), file=path)
    }
  ) 
  
  
  output$download_datasets_zip <- downloadHandler(

    filename = function() {str_c("metapr2_datasets_", taxo()$name, "_", Sys.Date(), ".zip")},

    content = function(path) {

      tmpdir <- tempdir()
      file_datasets <- str_c(tmpdir, "/datasets.xlsx")
      file_samples <- str_c(tmpdir, "/samples.xlsx")
      file_asv <- str_c(tmpdir, "/asv.xlsx")
      # file_asv_reads <- str_c(tmpdir, "/asv_reads.xlsx")
      files = c(file_datasets, file_samples, file_asv)

      rio::export(datasets_selected(), file=file_datasets, overwrite = TRUE)
      rio::export(samples_selected(), file=file_samples, overwrite = TRUE)
      rio::export(fasta_selected(), file=file_asv, overwrite = TRUE)
      # rio::export(df_selected(), file=file_asv_reads, overwrite = TRUE)

      system2("zip", args=(paste("--junk-paths", path, files,sep=" "))) # remove the paths of the files
    }
  )

  
  # # Download phyloseq -------------------------------------------------------
  # 
  # 
  # output$download_phyloseq <- downloadHandler(
  #   
  #   filename = function() {str_c("metapr2_phyloseq.rds")},
  #   
  #   content = function(file) {
  #     
  #     rio::export(asv_set$ps, file = file) 
  #   }
  # )   
  
  
  # UI ----------------------------------------------------------------------
  
  
  output$ui_download <- renderUI({
    tagList(
      # includeMarkdown("readme/download.md"),
      includeMarkdown(system.file("readme", 'download.md', package = "metapr2")),
      if(nrow(df_selected()) == 0) {messages$no_data},
      p(),
      htmlOutput(ns("sample_number")),
      p(),
      if(n_samples_valid()) renderPrint(print(ps_selected())),
      # https://cran.r-project.org/web/packages/dipsaus/vignettes/shiny_customized_widgets.html
      # https://github.com/rstudio/shiny/issues/1675
      fluidRow(
        column(3, downloadButton(ns('download_datasets_zip'), 'Download datasets, samples and ASVs (zip)', class = "btn-primary")),
        column(4, downloadButton(ns('download_df'), 'Download ASVs abundance (tsv.gz - can be very big)', class = "btn-danger")),
        column(4, if(n_samples_valid()) downloadButton(ns('download_phyloseq'), 'Download phyloseq file (rds)', class = "btn-primary")),
      )
      ,
      p(),
      p(),
      hr(),
      p(),
      # if (nrow(samples_selected()<= 1000)) {
      #   tagList(
      #       h4("Phyloseq file (if less than 1000 samples)"),
      #       renderPrint(print(asv_set$ps)),
      #       h5("Taxonomic ranks"),
      #       renderPrint(phyloseq::rank_names(asv_set$ps)),
      #       h5("Variables"),
      #       renderPrint(phyloseq::sample_variables(asv_set$ps)),
      #       downloadButton(ns('download_phyloseq'), 'Download all data as phyloseq file (rds)'),
      #       p()
      #   )
      # }
    )
    })

  })
  
} 