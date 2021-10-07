
# Validate sample selection -----------------------------------------------

iv_samples <- shinyvalidate::InputValidator$new()

iv_samples$add_rule("DNA_RNA", shinyvalidate::sv_required(message = "Choose at least one DNA or RNA"))
iv_samples$add_rule("substrate", shinyvalidate::sv_required(message = "Choose at least one substrate"))
iv_samples$add_rule("fraction_name", shinyvalidate::sv_required(message = "Choose at least one fraction"))
iv_samples$add_rule("depth_level", shinyvalidate::sv_required(message = "Choose at least one depth level"))
iv_samples$add_rule("datasets_selected_id", shinyvalidate::sv_required(message = "Choose at least one dataset"))

iv_samples$enable()


# Create table of datasets -------------------------------------------------


table_datasets <- reactive ({
  DT::datatable(asv_set$datasets %>% 
    select(dataset_id, dataset_name, region, paper_reference) %>%
    mutate(selected = ifelse(dataset_id %in% input$datasets_selected_id,TRUE, FALSE)),
    rownames = FALSE
  ) %>% DT::formatStyle("selected",  target = 'row',
                        backgroundColor = DT::styleEqual(c(FALSE, TRUE), c('white', 'yellow'))
                        )
  })

# See:
# Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width

output$table_datasets <- DT::renderDT(table_datasets())

# Select the datasets based on the table but must make sure that the table has been displayed

# datasets_id_selected <- reactive({
#   if(!is.null(input$table_datasets_rows_selected)) { 
#     table_datasets %>% 
#       dplyr::slice(input$table_datasets_rows_selected) %>% 
#       pull(dataset_id) 
#     } else {
#       table_datasets %>% 
#         pull(dataset_id)
#     }
# })



# Update Sample Checkboxes With the values possible for the datasets selected ---

update_checkbox <- function(variable, datasets_id) {
  values <- asv_set$samples %>% 
    filter(dataset_id %in% datasets_id) %>% 
    pull(!!as.symbol(variable)) %>% 
    unique()
  
  updateCheckboxGroupInput(inputId = variable, 
                           choices = values,
                           selected = values,
                           inline = TRUE)
}

observeEvent(input$datasets_selected_id,{
  update_checkbox("DNA_RNA", input$datasets_selected_id)
  update_checkbox("substrate", input$datasets_selected_id)
  update_checkbox("fraction_name", input$datasets_selected_id)
  update_checkbox("depth_level", input$datasets_selected_id)
  
  
})


# Update the datasets df -------------------------------------------------------

datasets_selected <- reactive({
  req(iv_samples$is_valid())
  asv_set$datasets %>%
    filter(dataset_id %in% input$datasets_selected_id) 
  })

# Display datasets selected


output$datasets_selected_id = renderText(input$datasets_selected_id)


# Build the whole dataset

df_full <- asv_set$df %>% 
  left_join(asv_set$samples) %>% 
  left_join(select(asv_set$fasta, asv_code, kingdom:species)) %>% 
  filter(!is.na(kingdom)) %>% # Some asvs are missing from the FASTA table... (to be checked)
  mutate(depth_level = forcats::fct_relevel(depth_level, 
                                   levels = c("bathypelagic", "mesopelagic", "euphotic", "surface")))


# Select samples based on different parameters and datasets --------------------

samples_selected <- reactive({
  
  # First check some samples are chosen
  req(iv_samples$is_valid())
  
  asv_set$samples %>%
    filter( #gene_region %in% input$gene_region,
           DNA_RNA %in% input$DNA_RNA,
           depth_level %in% input$depth_level,
           fraction_name %in% input$fraction_name,
           substrate %in% input$substrate,
           dataset_id %in% input$datasets_selected_id
    ) })

# To use in the future, the full dataset
df_selected <- reactive({
  # First check some samples are chosen
  req(iv_samples$is_valid())
  
  df_full %>%
    filter( #gene_region %in% input$gene_region,
      DNA_RNA %in% input$DNA_RNA,
      depth_level %in% input$depth_level,
      fraction_name %in% input$fraction_name,
      substrate %in% input$substrate,
      dataset_id %in% input$datasets_selected_id,
      !!as.symbol(taxo()$level) %in% taxo()$name
    ) })

# Filter asv_set$df for samples selected ---------------------------------------
# -- This contains only 3 columns (file_code, asv_code, n_reads)

df_selected_taxa_all <- reactive({
  req(samples_selected())
  asv_set$df %>%
    filter(file_code %in% samples_selected()$file_code)
})

# Filter asv_set$df for taxa selected AND merge with taxonomy
# Mutate asv_code to keep only 8 characters 


# Then filter for the taxon selected and joind with fasta taxonomy--------------
# -- This contains only columns (file_code, asv_code, n_reads, taxonomy)

df_selected_taxa_one <- reactive({
  req(samples_selected())
  df_selected_taxa_all() %>%
    inner_join(select(fasta_selected_taxa_one(), any_of(global$taxo_levels))) %>% 
    mutate(asv_code = str_sub(asv_code, 1,8)) 
})


# Filter dasta df by taxon selected ---------------------------------------


fasta_selected_taxa_one <- reactive({
  asv_set$fasta %>% 
    filter(!!as.symbol(taxo()$level)  %in% taxo()$name )
})



