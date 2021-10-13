# UI ----------------------------------------------------------------------

dataset_reads_min <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      ns("reads_min"),
      "Minimum number of total reads per ASV",
      100,
      min = 100,
      max = 10000,
      step = NA,
      width = NULL
    )
  )
}


dataset_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_ps_alpha')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_alpha')))
  )
}

samples_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('ui_ps_beta')),
    shinycssloaders::withSpinner(uiOutput(ns('graph_ps_beta')))
  )
}


# Server ------------------------------------------------------------------


datasetServer <- function(id, ps_selected, taxo) {
  # stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)



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
    select(dataset_id, dataset_name, region, paper_reference, sample_number, asv_number, n_reads_mean) %>%
    mutate(selected = ifelse(dataset_id %in% input$datasets_selected_id,TRUE, FALSE)),
    rownames = FALSE
  ) %>% DT::formatStyle("selected",  target = 'row',
                        backgroundColor = DT::styleEqual(c(FALSE, TRUE), c('white', 'lightcyan'))
                        )
  })

# See:
# Column width: https://stackoverflow.com/questions/25205410/r-shiny-set-datatable-column-width

output$table_datasets <- DT::renderDT(table_datasets())


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

# The full data frame filtered by samples, datasets and taxonomy ---------------

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



# Filter phyloseq by samples and taxon selected ---------------------------------------

ps_selected <- reactive({ 
  
  req(iv_samples$is_valid())
  
  print("filtering PS")
  
  ps_select (ps = asv_set$ps, 
             DNA_RNA = input$DNA_RNA, depth_level = input$depth_level, 
             fraction_name = input$fraction_name, substrate = input$substrate, 
             datasets_selected_id = input$datasets_selected_id,
             ps_reads_min = input$ps_reads_min, 
             taxo_level = taxo()$level, taxo_name = taxo()$name)
})

  })
  
}  
