
table_datasets <- select(asv_set$datasets, dataset_id, dataset_name, region, paper_reference)

# See:
# Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width

output$table_datasets <- DT::renderDT(table_datasets, 
                                      rownames = TRUE,
                                      selection = list(selected = 1:nrow(table_datasets)),
                                      options = list(pageLength = 20,
                                                     columnDefs = list(list(width = '200px', targets = c(1)),
                                                                       list(width = '500px', targets = c(4)))
                                                     )
                                      )

# Select the datasets based on the table but must make sure that the table has been displayed

datasets_id_selected <- reactive({
  if(!is.null(input$table_datasets_rows_selected)) { 
    table_datasets %>% 
      dplyr::slice(input$table_datasets_rows_selected) %>% 
      pull(dataset_id) 
    } else {
      table_datasets %>% 
        pull(dataset_id)
    }
})

datasets_selected <- reactive({
  asv_set$datasets %>%
    filter(dataset_id %in% datasets_id_selected()
    ) })

# Select samples based on different parameters and datasets

samples_selected <- reactive({
  asv_set$samples %>%
    filter(gene_region %in% input$gene_region,
           DNA_RNA %in% input$DNA_RNA,
           depth_level %in% input$depth_level,
           fraction_name %in% input$fraction_name,
           substrate %in% input$substrate,
           dataset_id %in% datasets_id_selected()
    ) })

df_selected_taxa_all <- reactive({
  asv_set$df %>%
    filter(file_code %in% samples_selected()$file_code)
})

df_selected_taxa_one <- reactive({
  df_selected_taxa_all() %>%
    inner_join(select(fasta_selected_taxa_one(), any_of(taxo_levels)))
})

fasta_selected_taxa_one <- reactive({
  asv_set$fasta %>% 
    filter(!!as.symbol(taxo()$level)  %in% taxo()$name )
})

# To display datasets selected
output$datasets_id_selected = renderText(datasets_id_selected())

# Downlaod files (zip)
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
